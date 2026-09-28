# app server
server <- function(input, output, session) {
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
    base_maps = c(
      "CartoDB.Voyager",
      "CartoDB.Positron",
      "OpenStreetMap",
      "OpenStreetMap.HOT"
    ),
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
      selected = "Health Zone"
    )
    updateSelectInput(session, "map-var", selected = "EVD_status")
    # epicurve date axis defaults to date of notification
    updateSelectInput(session, "curve-date", selected = "date_lab_result_1")
  }) |>
    bindEvent(TRUE, once = TRUE)

  nk_bbox <- sf::st_bbox(app_data$admin_data$adm2[
    app_data$admin_data$adm2 %in% c("Butembo", "Katwa")
  ])

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
