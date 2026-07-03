# Epishiny module for North-Kivu
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(epishiny))

# then set your options. the options below are the defaults
options(
  epishiny.na.label = "(Missing)", # label to be used for NA values in outputs
  epishiny.count.label = "Cases", # if data is un-aggregated, the label to represent row counts
  epishiny.week.letter = "W", # letter to represent 'Week'. Change to S for 'Semaine' etc
  epishiny.week.start = 1 # day the epiweek starts on. 1 = Monday, 7 = Sunday
)

#* PATH ------------------------------------------------
source(here::here("R", "0_global.R"))

#* Admin ------------------------------------------------
# import shapefiles
adm1_nk <- readRDS("local/geobase/COD_adm1.rds") |>
  filter(adm1_name %in% c("Nord-Kivu"))
adm2_nk <- readRDS("local/geobase/COD_adm2.rds") |>
  filter(adm1_name %in% c("Nord-Kivu"))
adm3_nk <- readRDS("local/geobase/COD_adm3.rds") |>
  filter(adm2_name %in% c("Butembo", "Katwa"))

#* Import DATA  ------------------------------------------------
# import the narrative linelist
latest_narr_ll <- fs::dir_ls(fs::path(
  butembo_project_data_path,
  "LL-epic",
  "narrative LL",
  "export_ll"
)) |>
  max()

nk_ll <- rpxl::rp_xlsb(latest_narr_ll, password = "ebolaExport", sheet = 1) |>
  as_tibble() |>
  remove_empty()


# EVD case-classification palette (shared with the static NK outputs in
# nk_description.R). epishiny assigns `group_pal` colours *positionally* to the
# factor levels of the grouping variable, so we order EVD_status as a factor and
# derive the palette in the same order. Building from `present` keeps the colour
# -> status mapping correct even when a class (e.g. "Probable") is absent.
evd_status_cols <- c(
  "Confirmé" = "#9e2a2b", # dark red
  "Probable" = "#e09f3e", # amber
  "Suspect" = "#bdbdbd", # grey
  "Non cas" = "#6d85b6" # muted blue
)

evd_present <- intersect(names(evd_status_cols), unique(nk_ll$EVD_status))
nk_ll <- nk_ll |>
  mutate(EVD_status = factor(EVD_status, levels = evd_present))
evd_pal <- unname(evd_status_cols[evd_present])

nk_ll_evd <- nk_ll |> filter(EVD_status %in% c("Suspect", "Confirmé"))
nk_ll_conf <- nk_ll |> filter(EVD_status %in% c("Confirmé"))

nk_ll_conf |>
  select(
    epi_id,
    isolation_site_id,
    date_notification,
    date_symptom_onset,
    type_of_exit
  )

#* Geo data ------------------------------------------------

nk_ll <- nk_ll |>
  left_join(
    select(adm1_nk, adm1_name, adm1_pcode__onset = adm1_pcode),
    join_by(adm1_name__onset == adm1_name)
  ) |>
  left_join(
    select(adm2_nk, adm2_name, adm2_pcode__onset = adm2_pcode),
    join_by(adm2_name__onset == adm2_name)
  ) |>
  left_join(
    select(adm3_nk, adm3_name, adm3_pcode__onset = adm3_pcode),
    join_by(adm3_name__onset == adm3_name)
  )


geo_data <- list(
  geo_layer(
    layer_name = "Province", # name of the boundary layer
    sf = adm1_nk, # sf object with boundary polygons
    name_var = "adm1_name", # column with place names
    pop_var = "adm1_pop", # column with population data (optional)
    join_by = c("pcode" = "adm1_pcode__onset") # geo to data join vars: LHS = sf, RHS = data
  ),
  geo_layer(
    layer_name = "Health Zone",
    sf = adm2_nk,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode__onset")
  ),
  geo_layer(
    layer_name = "Health Area",
    sf = adm3_nk,
    name_var = "adm3_name",
    join_by = c("pcode" = "adm3_pcode__onset")
  )
)

# define date variables in data as named list to be used in app
date_vars <- c(
  "Date of notification" = "date_notification",
  "Date of onset" = "date_symptom_onset",
  "Date of admission" = "date_admission_eff",
  "Date of exit" = "date_exit_eff"
)

# define categorical grouping variables
# in data as named list to be used in app
group_vars <- c(
  "Health zone" = "adm2_name__onset",
  "Health area" = "adm3_name__onset",
  "Health structure" = "isolation_site_id",
  "Sex" = "sex",
  "EVD status" = "EVD_status",
  "Outcome" = "type_of_exit"
)

# serve www/ (saved reference leaflet map + its dependency folder) to the browser
addResourcePath("refmap", "www")

# ! UI ---------------------------------------------------------------

# user interface
ui <- page_navbar(
  title = "Butembo EVD situation",
  # sidebar: single control — which case classifications to display
  sidebar = sidebar(
    title = "Display options",
    shinyWidgets::checkboxGroupButtons(
      inputId = "status_display",
      label = "Case classification",
      choices = c("Confirmé", "Suspect"),
      selected = c("Confirmé", "Suspect"),
      direction = "vertical",
      checkIcon = list(yes = shiny::icon("check"))
    )
  ),
  # main dashboard: map (left) + time/person stack (right)
  nav_panel(
    title = "Dashboard",
    layout_columns(
      col_widths = c(6, 6),
      # left: tabbed map card — interactive cases map + saved reference map
      navset_card_tab(
        full_screen = TRUE,
        title = "Place",
        nav_panel(
          "Cases",
          place_ui(
            id = "map",
            geo_data = geo_data,
            group_vars = group_vars
          )
        ),
        nav_panel(
          "Health facilities",
          tags$iframe(
            src = "refmap/reference_map_health_facilities.html",
            style = "border:none; width:100%; height:100%;"
          )
        )
      ),
      # right: time over person (stacked)
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        time_ui(
          id = "curve",
          title = "Time",
          date_vars = date_vars,
          group_vars = group_vars,
          date_interval_default = "day",
          group_var_default = "EVD_status",
          ratio_line_lab = "Show CFR line?"
        ),
        person_ui(id = "age_sex")
      )
    )
  ),
  # dedicated tab: full confirmed-case line list
  nav_panel(
    title = "Positive cases",
    card(
      full_screen = TRUE,
      card_header("Confirmed cases"),
      DT::DTOutput("summary_table")
    )
  )
)

# app server
server <- function(input, output, session) {
  # single filter: which case classifications to display across the dashboard
  app_df <- reactive({
    shiny::req(input$status_display)
    nk_ll |> filter(EVD_status %in% input$status_display)
  })

  # subtitle shown on the epishiny outputs reflecting the active selection
  filter_info <- reactive({
    paste0(
      "<b>Displaying</b>: ",
      paste(input$status_display, collapse = ", ")
    )
  })

  # "Positive cases" tab: the full confirmed line list, independent of the
  # display toggle. Latest notifications first.
  output$summary_table <- DT::renderDT({
    nk_ll_conf |>
      select(
        epi_id,
        isolation_site_id,
        patient_name,
        date_notification,
        date_symptom_onset,
        type_of_exit
      ) |>
      arrange(desc(date_notification)) |>
      DT::datatable(
        rownames = FALSE,
        # "compact" tightens row padding; smaller font fits more rows on screen
        class = "compact stripe hover row-border",
        colnames = c(
          "Epi ID" = "epi_id",
          "Health structure" = "isolation_site_id",
          "Name" = "patient_name",
          "Notification" = "date_notification",
          "Symptom onset" = "date_symptom_onset",
          "Outcome" = "type_of_exit"
        ),
        filter = "top",
        options = list(
          order = list(list(3, "desc")), # default sort: Notification, latest first
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
          scrollX = TRUE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "  $(this.api().table().container()).css({'font-size': '0.8em'});",
            "}"
          )
        )
      )
  })
  place_server(
    id = "map",
    df = app_df,
    geo_data = geo_data,
    group_vars = group_vars,
    filter_info = filter_info
  )
  # epishiny 0.1.0 has no default-layer / default-grouping / default-date args,
  # so set them once on startup via the modules' namespaced inputs.
  observe({
    shinyWidgets::updateRadioGroupButtons(
      session,
      "map-geo_level",
      selected = "Health Area"
    )
    updateSelectInput(session, "map-var", selected = "EVD_status")
    # epicurve date axis defaults to date of symptom onset
    updateSelectInput(session, "curve-date", selected = "date_symptom_onset")
  }) |>
    bindEvent(TRUE, once = TRUE)

  # zoom the map to the Butembo / Katwa extent once it is initialised
  # (map_zoom fires when leaflet first reports its view).
  nk_bbox <- sf::st_bbox(adm3_nk)
  observe({
    leaflet::leafletProxy("map-map", session) |>
      leaflet::fitBounds(
        lng1 = nk_bbox[["xmin"]],
        lat1 = nk_bbox[["ymin"]],
        lng2 = nk_bbox[["xmax"]],
        lat2 = nk_bbox[["ymax"]]
      )
  }) |>
    bindEvent(input[["map-map_zoom"]], once = TRUE)

  time_server(
    id = "curve",
    df = app_df,
    date_vars = date_vars,
    group_vars = group_vars,
    group_pal = evd_pal,
    show_ratio = TRUE,
    ratio_var = "type_of_exit",
    ratio_lab = "CFR",
    ratio_numer = "Died",
    ratio_denom = c("Died", "Cured", "Lost to follow-up"),
    filter_info = filter_info
  )

  person_server(
    id = "age_sex",
    df = app_df,
    age_var = "age",
    sex_var = "sex",
    male_level = "Male",
    female_level = "Female",
    filter_info = filter_info
  )
}

# launch app
if (interactive()) {
  shinyApp(ui, server)
}
