# app server
server <- function(input, output, session) {
  # "Positive cases" tab: confirmed line list, latest notifications first
  output$summary_table <- DT::renderDT({
    but_ll_conf |>
      select(
        unique_id,
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
          "Epi ID" = "unique_id",
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
  # the sidebar owns the data: every module below plots filter_data$df, and
  # map / bar clicks feed back as extra filter chips. bar_click and map_click are
  # forward references - epishiny only forces them inside reactive contexts.
  filter_data <- filter_server(
    id = "filter",
    df = but_ll,
    date_vars = date_vars,
    group_vars = group_vars,
    time_filter = bar_click,
    place_filter = map_click
  )

  map_click <- place_server(
    id = "map",
    df = filter_data$df,
    geo_data = geo_data,
    group_vars = group_vars,
    time_filter = bar_click,
    filter_info = filter_data$filter_info
  )

  bar_click <- time_server(
    id = "curve",
    df = filter_data$df,
    date_vars = date_vars,
    group_vars = group_vars,
    group_pal = evd_pal,
    show_ratio = TRUE,
    ratio_var = "type_of_exit",
    ratio_lab = "CFR",
    ratio_numer = "Died",
    ratio_denom = c("Died", "Recovered", "Abandoned"),
    place_filter = map_click,
    filter_info = filter_data$filter_info
  )

  mod_vb_server(
    id = "vb",
    df = filter_data$df,
    time_filter = bar_click,
    place_filter = map_click
  )

  person_server(
    id = "age_sex",
    df = filter_data$df,
    age_var = "age",
    sex_var = "sex",
    male_level = "Male",
    female_level = "Female",
    time_filter = bar_click,
    place_filter = map_click,
    filter_info = filter_data$filter_info
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
    # epicurve date axis defaults to date of notification
    updateSelectInput(session, "curve-date", selected = "date_lab_result_1")
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
}
