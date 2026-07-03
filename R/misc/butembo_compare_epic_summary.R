# script to compare the difference between the summary_positives et la Master linelist
#
# Field-by-field comparison of every case between two sources:
#   - pos_summary : the positives summary (summary_positives.xlsx)
#   - epic_ll     : the Epicentre master / narrative linelist (LL-epic)
# Variable correspondence is driven by the "match_dir" sheet of the summary file.
# Output: one long table with, per case and per mapped variable, the value on
# each side and a TRUE/FALSE flag indicating whether they agree.

source(here::here("R", "0_global.R"))

#* IMPORT DATA ------------------------------------------------------------

#* Matching dictionary ---------------------------------------------------
# Two columns: pos_summary <-> epic_ll. NA on a side = field has no counterpart.
match_dir <- rio::import(pos_summary_path, which = "match_dir") |>
  filter(include) |>
  as_tibble()

#* Positives summary (raw) -----------------------------------------------
# Raw summary (not the cleaned/recoded rds) so column names match the dictionary.
# Light recoding to align coding with epic_ll and avoid spurious mismatches.
pos_summary <- rio::import(pos_summary_path) |>
  as_tibble() |>
  mutate(
    infection_butembo = case_match(
      infection_butembo,
      "TRUE" ~ "Oui",
      "FALSE" ~ "Non"
    ),
    community_death = case_match(
      community_death,
      "TRUE" ~ "Oui",
      "FALSE" ~ "Non"
    ),
    sex = case_match(sex, "H" ~ "M", .default = sex),
    adm1_name = if_else(adm1_name == "Nord-Kivu", "COD Nord-Kivu", adm1_name)
  )

#* Epicentre master / narrative linelist ---------------------------------
latest_narr_ll <- fs::dir_ls(fs::path(
  butembo_project_data_path,
  "LL-epic",
  "narrative LL",
  "export_ll"
)) |>
  max()

epic_ll <- rpxl::rp_xlsb(latest_narr_ll, password = "ebolaExport", sheet = 1) |>
  as_tibble() |>
  # date_exit_eff is a raw Excel serial; Excel's day 0 is 1899-12-30
  mutate(date_exit_eff = as.Date(date_exit_eff, origin = "1899-12-30"))


#* COMPARE BOTH FILES ------------------------------------------

# Case identifier on each side (dictionary row 1: epi_id <-> patient_site_id)
JOIN_POS <- "epi_id"
JOIN_EPIC <- "patient_site_id"

# Column names of each source (precomputed: the dictionary columns are also
# named pos_summary / epic_ll and would mask the data frames inside filter()).
pos_names <- names(pos_summary)
epic_names <- names(epic_ll)

# Keep only variable pairs that (a) have a counterpart on both sides and
# (b) actually exist as columns in their dataset; drop the join key itself.
pairs <- match_dir |>
  filter(!is.na(pos_summary), !is.na(epic_ll)) |>
  filter(
    pos_summary %in% pos_names,
    epic_ll %in% epic_names,
    pos_summary != JOIN_POS
  ) |>
  # epic_ll name used as the canonical variable label (data are migrated there)
  mutate(variable = epic_ll)

# Long form of each source, keyed by case id + canonical variable name.
# Everything coerced to character so a single value column holds any type.
# Cases with a blank/missing id get a unique per-source sentinel: they never
# collide with each other and never join across sources, so they surface as
# one-sided rows instead of producing a cartesian self-join.
pos_long <- pos_summary |>
  mutate(across(everything(), as.character)) |>
  mutate(
    .id = na_if(str_squish(.data[[JOIN_POS]]), ""),
    .id = if_else(is.na(.id), paste0("__pos_noid_", row_number()), .id)
  ) |>
  select(.id, all_of(pairs$pos_summary)) |>
  pivot_longer(
    -.id,
    names_to = "pos_summary",
    values_to = "value_pos"
  ) |>
  left_join(pairs |> select(pos_summary, variable), by = "pos_summary") |>
  select(.id, variable, value_pos)

epic_long <- epic_ll |>
  mutate(across(everything(), as.character)) |>
  mutate(
    .id = na_if(str_squish(.data[[JOIN_EPIC]]), ""),
    .id = if_else(is.na(.id), paste0("__epic_noid_", row_number()), .id)
  ) |>
  select(.id, all_of(unique(pairs$epic_ll))) |>
  pivot_longer(
    -.id,
    names_to = "epic_ll",
    values_to = "value_epic"
  ) |>
  # one epic column can map to several pos variables (e.g. lab ids) -> many-to-many
  left_join(
    pairs |> select(epic_ll, variable),
    by = "epic_ll",
    relationship = "many-to-many"
  ) |>
  select(.id, variable, value_epic)

# Case order as the ids appear in epic_ll (the migration target); pos-only and
# id-less cases, absent from epic, sort after this in their existing order.
epic_id_order <- unique(epic_long$.id)

# One row per (case, variable) with both values side by side and a match flag.
# full_join keeps cases/variables present in only one source (value = NA there).
comparison <- full_join(
  pos_long,
  epic_long,
  by = c(".id", "variable"),
  # id_lab_1 + id_lab_2 (pos) both map to lab_id_1 (epic) -> many-to-many expected
  relationship = "many-to-many"
) |>
  mutate(
    # blank strings -> NA so they compare as missing, not as ""
    value_pos = na_if(str_squish(value_pos), ""),
    value_epic = na_if(str_squish(value_epic), ""),
    # TRUE when equal, or both missing; FALSE on any discrepancy
    match = (value_pos == value_epic) |
      (is.na(value_pos) & is.na(value_epic)),
    match = tidyr::replace_na(match, FALSE)
  ) |>
  arrange(factor(.id, levels = union(epic_id_order, .id)), variable)

#* EXPORT MISMATCHES ------------------------------------------

# Only the discrepant fields, kept in id order so each case's rows sit together.
mismatches <- comparison |>
  filter(!match) |>
  select(.id, variable, value_pos, value_epic)

# qxl with group = .id stripes the sheet by case: alternating cases get a
# shaded band, so it's easy to scan one patient's discrepancies at a glance.
qxl::qxl(
  mismatches,
  file = fs::path(out_dir, "butembo_compare_mismatches.xlsx"),
  group = ".id",
  filter = TRUE
)
