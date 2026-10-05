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

  # global period filter, on top of the sidebar/click filters, anchored to
  # the most recent lab result date in the data rather than Sys.Date()
  df_period <- reactive({
    d <- filter_data$df()
    per <- input$period %||% "total"
    if (identical(per, "total")) {
      return(d)
    }
    n_days <- c(last_7d = 7, last_14d = 14, last_21d = 21)[[per]]
    max_date <- max(d$date_lab_result_1, na.rm = TRUE)
    out <- dplyr::filter(d, date_lab_result_1 >= max_date - (n_days - 1))
    message(
      "period ", per, ": kept ", nrow(out), " of ", nrow(d),
      " rows (", sum(is.na(d$date_lab_result_1)), " without lab date)"
    )
    out
  })

  map_click <- mod_map_place_server(
    id = "map",
    df = df_period,
    geo_data_res = geo_data_res,
    geo_data_notif = geo_data,
    facilities = facilities,
    time_filter = bar_click,
    filter_info = filter_data$filter_info
  )

  bar_click <- time_server(
    id = "curve",
    df = df_period,
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

  # time_server takes one fixed palette, so send level colours for the chosen group
  observeEvent(input[["curve-group"]], {
    grp <- input[["curve-group"]]
    session$sendCustomMessage(
      "recolour",
      list(
        id = "curve-chart",
        palette = as.list(c(
          group_pals[[grp]],
          setNames(na_col, getOption("epishiny.na.label"))
        ))
      )
    )
  })

  mod_vb_server(
    id = "vb",
    df = df_period,
    time_filter = bar_click,
    place_filter = map_click
  )

  person_server(
    id = "age_sex",
    df = df_period,
    age_var = "age",
    sex_var = "sex",
    male_level = "Male",
    female_level = "Female",
    time_filter = bar_click,
    place_filter = map_click,
    filter_info = filter_data$filter_info
  )

  mod_delay_server(
    id = "delay",
    df = df_period,
    date_vars = delay_date_vars,
    group_vars = group_vars
  )

  mod_epicurve_hz_server(
    "epicurve_hz",
    df = df_period,
    group_vars = group_vars
  )

  mod_facilities_server(
    "facilities",
    df = df_period,
    hf_visits = hf_visits,
    hf_cases = hf_cases,
    flow_locations = flow_locations,
    flow_edges = flow_edges,
    adm2 = app_data$admin_data$adm2,
    adm3 = app_data$admin_data$adm3
  )

  mod_quality_server("quality", quality = quality)

  mod_lab_server("lab", shiny::reactive(lab_data))

  # epishiny 0.1.0 has no default-date arg, so set it once on startup
  observe({
    # epicurve date axis defaults to date of notification
    updateSelectInput(session, "curve-date", selected = "date_lab_result_1")
  }) |>
    bindEvent(TRUE, once = TRUE)
}
