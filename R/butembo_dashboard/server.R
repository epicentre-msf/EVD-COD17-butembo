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

  # NULL for "all", so patients without a lab date are kept
  iso_week_rng <- reactive({
    wk <- input$iso_week %||% "all"
    if (identical(wk, "all")) {
      return(NULL)
    }
    as.Date(wk) + c(0, 6)
  })

  # global period filter, on top of the sidebar/click filters, anchored to
  # the most recent lab result date in the data rather than Sys.Date()
  df_period <- reactive({
    d <- filter_data$df()
    rng <- iso_week_rng()
    if (!is.null(rng)) {
      n_before <- nrow(d)
      d <- dplyr::filter(
        d,
        date_lab_result_1 >= rng[1],
        date_lab_result_1 <= rng[2]
      )
      message(
        "iso week: kept ", nrow(d), " of ", n_before,
        " rows (dropped those outside ", rng[1], " to ", rng[2],
        " or without lab date)"
      )
    }
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

  # lab samples follow the same period, anchored on their own latest result
  lab_period <- reactive({
    lab <- lab_data
    rng <- iso_week_rng()
    if (!is.null(rng)) {
      n_before <- nrow(lab)
      lab <- dplyr::filter(
        lab,
        date_lab_result >= rng[1],
        date_lab_result <= rng[2]
      )
      message(
        "lab iso week: kept ", nrow(lab), " of ", n_before,
        " samples (dropped those outside ", rng[1], " to ", rng[2],
        " or without result date)"
      )
    }
    per <- input$period %||% "total"
    if (identical(per, "total")) {
      return(lab)
    }
    n_days <- c(last_7d = 7, last_14d = 14, last_21d = 21)[[per]]
    max_date <- max(lab$date_lab_result, na.rm = TRUE)
    out <- dplyr::filter(
      lab,
      date_lab_result >= max_date - (n_days - 1)
    )
    message(
      "lab period ", per, ": kept ", nrow(out), " of ", nrow(lab),
      " samples (", sum(is.na(lab$date_lab_result)), " without result date)"
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
    na_colour = na_col,
    show_ratio = TRUE,
    ratio_var = "type_of_exit",
    ratio_lab = "Létalité",
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
        na_col = na_col,
        # every no-value label is appended, since most palettes lack them
        palette = as.list(c(
          group_pals[[grp]][!names(group_pals[[grp]]) %in% na_labels],
          setNames(
            rep(na_col, length(na_labels) + 1),
            c(getOption("epishiny.na.label"), na_labels)
          )
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

  mod_zs_table_server("zs_table", df = df_period)

  person_server(
    id = "age_sex",
    df = df_period,
    age_var = "age",
    sex_var = "sex",
    age_var_lab = "Âge (années)",
    age_group_lab = "Groupe d'âge",
    male_level = "Male",
    female_level = "Female",
    colours = unname(group_pals$sex[c("Male", "Female")]),
    time_filter = bar_click,
    place_filter = map_click,
    filter_info = filter_data$filter_info
  )

  # epishiny has a Counts / Percent toggle but starts on counts
  session$onFlushed(
    \() {
      shinyWidgets::updateRadioGroupButtons(
        session,
        "age_sex-cnt_pcnt",
        selected = "n_prop"
      )
    },
    once = TRUE
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

  mod_breakdown_server(
    "breakdown",
    df = df_period,
    group_vars = group_vars,
    time_filter = bar_click,
    place_filter = map_click,
    filter_info = filter_data$filter_info
  )

  mod_facilities_server(
    "facilities",
    df = df_period,
    hf_visits = hf_visits,
    hf_geo = hf_geo,
    flow_locations = flow_locations,
    flow_edges = flow_edges,
    adm2 = app_data$admin_data$adm2,
    adm3 = app_data$admin_data$adm3
  )

  # make_epicontacts needs unique node ids and both ends of every edge present
  chain_ll <- shiny::reactive({
    d <- dplyr::distinct(df_period(), unique_id, .keep_all = TRUE)
    message("chains: ", nrow(d), " nodes of ", nrow(df_period()), " rows")
    d
  })

  chain_contacts <- shiny::reactive({
    ids <- chain_ll()$unique_id
    out <- dplyr::filter(chain_events, from_uid %in% ids, to_uid %in% ids)
    message("chains: ", nrow(out), " of ", nrow(chain_events), " edges kept")
    out
  })

  mod_chains_vb_server(
    "chains_vb",
    ll = chain_ll
  )

  mod_chains_summary_server("chains_summary", ll = chain_ll)

  # mod_contacts_server(
    # "chains",
    # df = chain_ll,
    # contact_df = chain_contacts,
    # id_var = "unique_id",
    # from_var = "from_uid",
    # to_var = "to_uid",
    # node_vars = chain_node_vars,
    # tooltip_vars = chain_tooltip_vars
  # )

  mod_quality_server("quality", quality = quality)

  mod_lab_server("lab", lab_period)

  # epishiny 0.1.0 has no default-date arg, so set it once on startup
  observe({
    # epicurve date axis defaults to date of notification
    updateSelectInput(session, "curve-date", selected = "date_lab_result_1")
  }) |>
    bindEvent(TRUE, once = TRUE)
}
