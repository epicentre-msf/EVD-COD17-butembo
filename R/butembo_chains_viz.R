# Visualisation of Butembo transmission chains with epicontacts

source(here::here("R", "0_global.R"))
library(tidyverse)
library(epicontacts)

ll <- readRDS(latest_narr_ll_clean)$data

trans_dat <- readRDS(latest_transmission_clean)

#! BUILD EPICONTACTS OBJECT ------------------------------

# drop pairs with a missing infector/infectee before building the network
epi_contacts <- trans_dat |>
  filter(!is.na(from) & !is.na(to)) |>
  select(from, to, certainty, start_exposure, end_exposure)

# linelist restricted to the cases that appear in at least one pair, plus the
# attributes we want to display / colour on
chain_ids <- union(epi_contacts$from, epi_contacts$to)

epi_ll <- ll |>
  filter(!is.na(pid) & pid %in% chain_ids) |>
  select(
    pid,
    date_symptom_onset,
    sex,
    age,
    age_group,
    adm2_name,
    adm3_name,
    type_of_exit,
    infection_butembo
  )

epi <- make_epicontacts(
  linelist = epi_ll,
  contacts = epi_contacts,
  id = "pid",
  from = "from",
  to = "to",
  directed = TRUE
)

#! STATIC CHAINS BY ONSET DATE (ggplot) ------------------------------

# nodes = cases at their onset date, coloured by outcome; links styled by the
# certainty of the reported pair (solid = known contact, dashed = hypothesis)
outcome_pal <- c(
  "Actif" = "#4c72b0",
  "Guéri" = "#55a868",
  "Abandon" = "#9aa0a6",
  "Décédé" = "#c44e52"
)

certainty_lty <- c(
  "known contact" = "solid",
  "hypothesis" = "dashed"
)

chains_gg <- epicontacts::vis_temporal_static(
  epi,
  x_axis = "date_symptom_onset",
  network_shape = "rectangle",
  node_order = "date_symptom_onset",
  rank_contact = "date_symptom_onset",
  parent_pos = "top",
  node_color = "type_of_exit",
  node_size = 3.5,
  edge_color = "certainty",
  position_dodge = TRUE
)

# vis_temporal_static maps certainty to edge colour; move that encoding to the
# edge linetype instead, thin the links and draw them in a single neutral grey
for (i in 1:2) {
  chains_gg$layers[[i]]$mapping$linetype <- chains_gg$layers[[i]]$mapping$colour
  chains_gg$layers[[i]]$mapping$colour <- NULL
  chains_gg$layers[[i]]$aes_params$colour <- "grey40"
  chains_gg$layers[[i]]$aes_params$size <- NULL
  chains_gg$layers[[i]]$aes_params$linewidth <- 0.35
}

# smaller arrowheads
chains_gg$layers[[2]]$geom_params$arrow$length <- grid::unit(0.008, "npc")

chains_gg <- chains_gg +
  scale_fill_manual("Statut", values = outcome_pal) +
  scale_linetype_manual(
    "Lien",
    values = certainty_lty,
    labels = c("known contact" = "Contact connu", "hypothesis" = "Hypothèse")
  ) +
  scale_x_date(
    "Date de début des symptômes",
    date_labels = "%d\n%b",
    date_breaks = "1 week"
  ) +
  labs(y = NULL) +
  theme_evd() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 11, vjust = 0.5),
    legend.text = element_text(size = 11, vjust = 0.5)
  )

chains_gg

ggsave(
  fs::path(out_dir, "butembo_transmission_chains.png"),
  chains_gg,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)
