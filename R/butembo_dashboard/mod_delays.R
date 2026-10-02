#' Delay Module UI
#'
#' Creates the UI components for the delay visualization module, including
#' distribution of delays, and mean/median time between key events.
#'
#' @rdname delay
#'
#' @param id Module ID, must be the same in both the UI and server function to link the two.
#' @param title Title of the module (default: "Delays").
#' @param icon A Bootstrap icon for the module header (default: hourglass-split).
#' @param opts_btn_lab Label for the options button (default: "Options").
#' @param sidebar_title Title displayed at the top of the options sidebar (default: NULL).
#' @param sidebar_width Width of the options sidebar in pixels (default: 250).
#'
#' @return A Shiny UI object containing navigation panels for a bar chart and timeline visualization.
#'
#' @importFrom shiny NS selectInput sliderInput bslib::input_switch conditionalPanel radioButtons actionLink updateSelectInput
#' @importFrom bslib navset_card_tab nav_panel card_body sidebar tooltip
#' @importFrom highcharter highchartOutput
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets checkboxGroupButtons
#'
#' @export
mod_delay_ui <- function(
  id,
  opts_btn_lab = "Options",
  sidebar_title = NULL,
  sidebar_width = 250
) {
  ns <- shiny::NS(id)

  pkg_deps <- c("highcharter", "bslib", "shinyWidgets")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(pkg_deps, reason = "to use the delay module.")
  }

  inputs_ui <- tagList(
    selectInput(
      ns("group_var"),
      label = "Select grouping variable",
      choices = NULL,
      width = "100%",
      selectize = FALSE
    ),
    sliderInput(
      ns("co_value"),
      label = "Maximum delay",
      min = 0,
      max = 30,
      value = 30,
      step = 1,
      width = "100%"
    ),
    div(
      id = ns("bar_inputs"),
      selectInput(
        ns("delays"),
        label = "Select delay",
        choices = NULL,
        width = "100%",
        selectize = FALSE
      ),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("show_stat"),
        size = "sm",
        label = "Select statistics to display:",
        choices = c("Mean" = "mean", "Median" = "median"),
        selected = c("mean", "median")
      ),
      bslib::input_switch(
        ns("fit_dist"),
        label = "Fit Distribution",
        value = FALSE
      ),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("fit_dist")),
        selectInput(
          ns("which_dist"),
          label = "Select Distribution",
          choices = c("gamma", "weibull", "lnorm"),
          selected = "gamma",
          multiple = TRUE,
          width = "100%"
        )
      )
    ),
    div(
      id = ns("timeline_inputs"),
      selectInput(
        ns("dates"),
        label = "Select events",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      radioButtons(
        inputId = ns("mean_median"),
        label = "Select Statistic",
        choices = c("Mean" = "mean", "Median" = "median"),
        selected = "mean",
        inline = TRUE
      ),
      bslib::input_switch(
        ns("display_lab"),
        label = "Display labels",
        value = TRUE
      )
    ),
    div(
      id = ns("trend_inputs"),
      selectInput(
        ns("trend_delays"),
        label = "Select delays",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      radioButtons(
        inputId = ns("trend_agg"),
        label = "Aggregate by",
        choices = c("Week" = "week", "Month" = "month"),
        selected = "week",
        inline = TRUE
      )
    )
  )

  bslib::navset_card_tab(
    wrapper = function(...) {
      bslib::card_body(..., padding = 0, class = "delay-container")
    },
    id = ns("tabs"),

    sidebar = bslib::sidebar(
      id = ns("delay_sidebar"),
      title = sidebar_title,
      width = sidebar_width,
      position = "right",
      open = "open",
      inputs_ui
    ),

    bslib::nav_panel(
      title = tags$span(bsicons::bs_icon("bar-chart-fill"), "Distribution"),
      value = "bar_nav",
      highcharter::highchartOutput(ns("delay_barchart"), height = "100%")
    ),

    bslib::nav_panel(
      title = tags$span(bsicons::bs_icon("calendar-week-fill"), "Timeline"),
      value = "timeline_nav",
      highcharter::highchartOutput(ns("delay_timeline"))
    ),

    bslib::nav_panel(
      title = tags$span(bsicons::bs_icon("graph-up"), "Trend"),
      value = "trend_nav",
      highcharter::highchartOutput(ns("delay_trend"))
    )
  )
}

#' Delay Module Server
#'
#' Manages the server-side logic for the delay visualization module, handling
#' user interactions, data transformations, and plotting.
#'
#' @param id  Module ID, must be the same in both the UI and server function to link the two.
#' @param df data frame containing the event data.
#' @param date_vars A vector of date-related variable names for event selection, in the chronological order. If named vector then names are used in selection/display
#' @param group_vars A vector of categorical variables available for grouping.
#'
#' @return A Shiny module server function that generates reactive plots and updates UI elements.
#'
#'
#' @export
mod_delay_server <- function(id, df, date_vars, group_vars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Toggle options sidebar when gear icon clicked
      # observeEvent(input$toggle_sidebar, {
      #   bslib::sidebar_toggle("delay_sidebar")
      # })

      observe({
        shinyjs::toggle("bar_inputs", condition = input$tabs == "bar_nav")
      })

      observe({
        shinyjs::toggle(
          "timeline_inputs",
          condition = input$tabs == "timeline_nav"
        )
      })

      observe({
        shinyjs::toggle("trend_inputs", condition = input$tabs == "trend_nav")
      })

      # grouping is not used by the trend lines
      observe({
        shinyjs::toggle("group_var", condition = input$tabs != "trend_nav")
      })

      # all possible date combinations
      date_combinations <- combn(date_vars, 2, simplify = FALSE)

      # options for input
      delay_choices <- purrr::map_chr(
        date_combinations,
        ~ paste(.x, collapse = "__")
      )
      names(delay_choices) <- purrr::map_chr(
        date_combinations,
        \(x) {
          labs <- if (rlang::is_named(x)) names(x) else x
          paste(labs, collapse = " to ")
        }
      )

      # Update input choices
      observeEvent(delay_choices, {
        updateSelectInput(
          session,
          "delays",
          choices = delay_choices
        )
      })

      trend_choices <- delay_choices[
        delay_choices %in%
          c(
            "date_symptom_onset__date_notification",
            "date_symptom_onset__date_admission_eff",
            "date_symptom_onset__date_exit_eff",
            "date_notification__date_lab_result_1",
            "date_admission_eff__date_exit_eff"
          )
      ]

      observeEvent(trend_choices, {
        updateSelectInput(
          session,
          "trend_delays",
          choices = trend_choices,
          selected = trend_choices
        )
      })

      # Update group variables choices
      observeEvent(group_vars, {
        updateSelectInput(
          session,
          "group_var",
          choices = c("None" = "none", group_vars),
          selected = "none"
        )
      })

      group_var <- reactive({
        if (input$group_var == "none") NULL else input$group_var
      })

      # calculate delays
      delay_df <- reactive({
        req(input$group_var)
        get_delay_df(df(), date_vars, group_var = group_var())
      })

      # Plot barchart
      output$delay_barchart <- highcharter::renderHighchart({
        req(input$delays)

        plot_delay_bar(
          delay_df(),
          co_value = input$co_value,
          delay = input$delays,
          delay_choices = delay_choices,
          fit_dist = input$fit_dist,
          which_dist = input$which_dist,
          group_var = group_var(),
          show_stat = input$show_stat
        )
      })

      # update dates choices
      observeEvent(date_vars, {
        updateSelectInput(
          session,
          "dates",
          choices = date_vars,
          selected = date_vars
        )
      })

      # Reactive to map the selected value to the corresponding label
      selected_label <- reactive({
        names(date_vars[date_vars %in% input$dates]) # Map selected value to its label
      })

      choice_dates <- reactive(setNames(input$dates, selected_label()))

      # Plot Timeline
      output$delay_timeline <- highcharter::renderHighchart({
        plot_delay_timeline(
          delay_df = delay_df(),
          statistic = input$mean_median,
          date_var_seq = choice_dates(),
          co_value = input$co_value,
          display_lab = input$display_lab,
          group_var = group_var()
        )
      })

      # Plot trend of mean delays by onset date
      output$delay_trend <- highcharter::renderHighchart({
        req(input$trend_delays)

        # rows are unchanged by get_delay_df, so onset date binds safely
        trend_df <- dplyr::bind_cols(
          get_delay_df(df(), date_vars),
          date_symptom_onset = as.Date(df()$date_symptom_onset)
        )

        plot_delay_trend(
          trend_df,
          delays = input$trend_delays,
          delay_choices = delay_choices,
          agg = input$trend_agg,
          co_value = input$co_value
        )
      })
    }
  )
}

#' Plot mean delays over time
#'
#' One line per delay: mean delay (days) by week or month of symptom onset.
#' Delays outside 0 to `co_value` are excluded and counted in the credits.
#'
#' @param trend_df Data frame of delay pair columns plus `date_symptom_onset`.
#' @param delays Delay column names to plot.
#' @param delay_choices Named vector mapping delay columns to display labels.
#' @param agg Either "week" (ISO, Monday start) or "month".
#' @param co_value Maximum valid delay in days.
#'
#' @return A highchart object.
#' @export
plot_delay_trend <- function(
  trend_df,
  delays,
  delay_choices,
  agg = c("week", "month"),
  co_value = 30
) {
  agg <- match.arg(agg)

  long <- trend_df |>
    dplyr::select(date_symptom_onset, dplyr::all_of(delays)) |>
    tidyr::pivot_longer(
      dplyr::all_of(delays),
      names_to = "delay",
      values_to = "timespan"
    )

  valid <- long |>
    dplyr::filter(
      !is.na(date_symptom_onset),
      !is.na(timespan),
      dplyr::between(timespan, 0, co_value)
    )
  n_removed <- sum(!is.na(long$timespan)) - nrow(valid)

  summ <- valid |>
    dplyr::mutate(
      bin = lubridate::floor_date(
        date_symptom_onset,
        unit = agg,
        week_start = 1
      )
    ) |>
    dplyr::summarise(
      mean = mean(timespan),
      n = dplyr::n(),
      .by = c(delay, bin)
    ) |>
    dplyr::arrange(delay, bin)

  # Set1 caps at 9 colours but five events give 10 delay pairs
  pal <- grDevices::hcl.colors(length(delays), "Dark 3")
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line", zoomType = "x") |>
    highcharter::hc_xAxis(type = "datetime", title = list(text = NULL)) |>
    highcharter::hc_yAxis(
      title = list(text = "Mean delay (days)"),
      min = 0
    ) |>
    highcharter::hc_legend(enabled = TRUE) |>
    highcharter::hc_exporting(enabled = FALSE) |>
    highcharter::hc_tooltip(
      shared = FALSE,
      useHTML = TRUE,
      formatter = highcharter::JS(
        "function() {
          return '<b>' + this.series.name + '</b><br/>' +
            Highcharts.dateFormat('%e %b %Y', this.x) + '<br/>' +
            'Mean: ' + Highcharts.numberFormat(this.y, 1) + ' days<br/>' +
            'n = ' + this.point.n;
        }"
      )
    ) |>
    highcharter::hc_credits(
      enabled = TRUE,
      text = paste0(n_removed, " delays removed (negative or over cut-off)")
    )

  for (i in seq_along(delays)) {
    d <- summ |>
      dplyr::filter(delay == delays[[i]]) |>
      dplyr::mutate(x = highcharter::datetime_to_timestamp(bin))
    # a delay with no valid rows has nothing to draw
    if (nrow(d) == 0) {
      next
    }
    label <- names(delay_choices)[match(delays[[i]], delay_choices)]
    hc <- hc |>
      highcharter::hc_add_series(
        data = d,
        type = "line",
        highcharter::hcaes(x = x, y = mean),
        name = label,
        color = pal[[i]],
        marker = list(enabled = TRUE, radius = 3)
      )
  }

  hc
}

#' Get Delay DataFrame
#'
#' This function calculates the delay between all combinations of two date variables in a data frame.
#' It returns a new data frame containing the delay for each unique pair of dates. The delay is calculated
#' as the difference (in days) between the two dates. Additionally, it optionally includes a grouping variable
#' from the input data frame.
#'
#' @param dat A data frame containing the date variables and optionally a grouping variable.
#' @param date_vars A character vector of the names of the date variables for which the delays should be calculated.
#' @param group_var (Optional) A character string specifying the name of a grouping variable to include in the result.
#' If not provided, no grouping variable will be included in the output.
#'
#' @return A data frame containing the delay for each unique pair of date variables in `date_vars` as columns.
#' If `group_var` is provided, it is also included as an additional column in the result.
#'
#' @examples
#' # Example data frame
#' dat <- data.frame(
#'   id = 1:5,
#'   start_date = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05")),
#'   end_date = as.Date(c("2025-01-05", "2025-01-06", "2025-01-07", "2025-01-08", "2025-01-09"))
#' )
#'
#' # Example usage
#' get_delay_df(dat, date_vars = c("start_date", "end_date"))
#'
#' # With a grouping variable
#' get_delay_df(dat, date_vars = c("start_date", "end_date"), group_var = "id")
#'
#' @importFrom dplyr mutate across transmute select bind_cols
#' @importFrom purrr map
#' @importFrom rlang .data
#' @export
get_delay_df <- function(dat, date_vars, group_var = NULL) {
  # Get all unique pairs of date variables
  date_combinations <- combn(date_vars, 2, simplify = FALSE)

  # prep already saved these columns (add_delay_pairs in R/utils.R)
  pair_cols <- purrr::map_chr(
    date_combinations,
    \(d) paste0(d[[1]], "__", d[[2]])
  )
  if (all(pair_cols %in% names(dat))) {
    return(dplyr::select(dat, dplyr::all_of(c(pair_cols, group_var))))
  }

  dat <- dat |>
    # Ensure dates are in Date format - or ask for it to be date ?
    dplyr::mutate(dplyr::across(dplyr::all_of(date_vars), as.Date))

  # Function to calculate delay between two date variables
  calculate_delay <- function(x, dates) {
    var_name <- paste0(dates[[1]], "__", dates[[2]])
    x |>
      dplyr::transmute(
        !!var_name := as.integer(.data[[dates[[2]]]] - .data[[dates[[1]]]])
      )
  }

  # apply delay function to all combination and get df
  final_df <- purrr::map(
    date_combinations,
    ~ calculate_delay(dat, .x)
  ) |>
    dplyr::bind_cols() |>
    dplyr::bind_cols(dplyr::select(dat, dplyr::all_of(group_var)))

  return(final_df)
}

#' Plot Delay Distributions
#'
#' This function generates a bar plot of delay distributions from a provided data frame containing delay data.
#' The plot shows the counts of delays within specified ranges, and optionally fits and overlays a distribution (e.g., Gamma, Weibull, Lognormal).
#' It can also display the distribution parameters and log-likelihood values in tooltips for the fitted distributions.
#'
#' @param delay_df A data frame containing the delay data and optionally a grouping variable.
#' @param delay A character string specifying the name of the column in `delay_df` that contains the delay values.
#' @param delay_choices A named character vector mapping the delay variable names to their display labels.
#'   The names should correspond to the column names in `delay_df`, and the values are used for display in the plot title and tooltips.
#' @param co_value A numeric value specifying the threshold for counting delays as outliers (default is 30).
#' @param group_var (Optional) A character string specifying the name of a grouping variable in `delay_df`. The function will produce separate bars for each group.
#' @param fit_dist A logical value indicating whether to fit and display a distribution curve (default is `FALSE`).
#' @param which_dist A character vector specifying which distributions to fit. Options are "gamma", "weibull", and "lnorm" (default is `c("gamma", "weibull", "lnorm")`).
#'
#' @return A highchart object showing the distribution of delays, with optional overlays for fitted distributions.
#' If `fit_dist` is `TRUE`, the plot includes density curves based on the fitted distributions.
#' Tooltips display distribution parameters and log-likelihood values if fitted distributions are included.
#'
#' @examples
#' # Example data frame
#' dat <- data.frame(
#'   id = rep(1:3, each = 5),
#'   delay = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Plot delay distributions without fitting a distribution
#' plot_delay_bar(dat, delay = "delay")
#'
#' # Plot delay distributions with fitted distributions (Gamma, Weibull, Lognormal)
#' plot_delay_bar(dat, delay = "delay", fit_dist = TRUE)
#'
#' # Plot with grouping variable
#' plot_delay_bar(dat, delay = "delay", group_var = "id", fit_dist = TRUE)
#'
#' @importFrom dplyr select rename summarise filter count mutate
#' @importFrom fitdistrplus fitdist
#' @importFrom highcharter highchart hc_chart hc_add_series hc_tooltip hc_yAxis_multiples hc_xAxis hc_plotOptions hc_credits
#' @importFrom rlang sym
#' @importFrom glue glue
#' @export
plot_delay_bar <- function(
  delay_df,
  delay,
  delay_choices,
  co_value = 30,
  group_var = NULL,
  fit_dist = FALSE,
  which_dist = c("gamma", "weibull", "lnorm"),
  show_stat = c("mean", "median")
) {
  if (length(group_var) > 1) {
    stop("Please provide only one group variable")
  }

  if (length(group_var) && !any(names(delay_df) %in% group_var)) {
    stop("Please provide a valid group variable")
  }

  #make group_var useable as variable name
  if (!is.null(group_var)) {
    group_var_sym <- rlang::sym(group_var)
  }

  # Pivot delay_df
  df <- delay_df |>
    dplyr::select(dplyr::all_of(c(delay, group_var))) |>
    # rename the delay to timespan
    dplyr::rename("timespan" = 1)

  # get invalid df
  invalid_df <- df |>
    dplyr::summarise(
      N = dplyr::n(),
      n_na = sum(is.na(timespan)),
      n_invalid = sum(timespan < 0, na.rm = TRUE),
      n_co = sum(timespan >= co_value, na.rm = TRUE),
      n_valid = N - (n_na + n_invalid + n_co)
    )

  # filter only valid ranges
  df_clean <- dplyr::filter(
    df,
    dplyr::between(timespan, 0, co_value)
  )

  # Fit a distribution to data
  if (fit_dist) {
    # extract values used for fitting, remove 0s
    values <- df_clean |> dplyr::filter(timespan > 0) |> dplyr::pull(timespan)

    if (length(values) < 10) {
      message("Not enough data to fit a distribution")
      fit_dist <- FALSE
    }
  }

  if (fit_dist) {
    fit_results <- list()

    for (i in which_dist) {
      fit <- fitdistrplus::fitdist(values, distr = i)
      fit_results[[i]] <- fit
    }

    # structure the results
    results_list <- lapply(names(fit_results), function(d) {
      fit <- fit_results[[d]]
      param_df <- as.data.frame(t(fit$estimate)) # Convert parameter estimates into a dataframe
      param_df$distribution <- d
      param_df$loglik <- fit$loglik
      return(param_df)
    })

    # Bind all results into a single data frame
    results_df <- dplyr::bind_rows(results_list) |>
      dplyr::relocate(distribution, loglik) # Ensure distribution name is the first column

    #function to compute density
    compute_density <- function(dist_name, params) {
      if (dist_name == "gamma") {
        return(dgamma(
          value_seq,
          shape = params["shape"],
          rate = params["rate"]
        ))
      } else if (dist_name == "weibull") {
        return(dweibull(
          value_seq,
          shape = params["shape"],
          scale = params["scale"]
        ))
      } else if (dist_name == "lnorm") {
        return(dlnorm(
          value_seq,
          meanlog = params["meanlog"],
          sdlog = params["sdlog"]
        ))
      } else {
        return(rep(NA, length(value_seq))) # Return NA if the distribution is not recognized
      }
    }
    # Create a sequence of values to compute densities
    value_seq <- seq(min(values), max(values), length.out = 100)

    # Compute densities
    density_list <- lapply(1:nrow(results_df), function(i) {
      dist_name <- results_df$distribution[i]
      params <- results_df[i, -1, drop = FALSE] # Extract only parameter values
      params <- as.numeric(params) # Convert to numeric
      names(params) <- names(results_df)[-1] # Assign parameter names

      tooltip_text <- paste0(
        "<b>Distribution:</b> ",
        dist_name,
        "<br>",
        if (!is.na(params["shape"])) {
          paste0("<b>Shape:</b> ", round(params["shape"], 4), "<br>")
        } else {
          ""
        },
        if (!is.na(params["rate"])) {
          paste0("<b>Rate:</b> ", round(params["rate"], 4), "<br>")
        } else {
          ""
        },
        if (!is.na(params["scale"])) {
          paste0("<b>Scale:</b> ", round(params["scale"], 4), "<br>")
        } else {
          ""
        },
        if (!is.na(params["meanlog"])) {
          paste0("<b>Meanlog:</b> ", round(params["meanlog"], 4), "<br>")
        } else {
          ""
        },
        if (!is.na(params["sdlog"])) {
          paste0("<b>Sdlog:</b> ", round(params["sdlog"], 4), "<br>")
        } else {
          ""
        },
        "<br>",
        "<b>Log-likelihood:</b> ",
        round(results_df$loglik[i], 4)
      )

      data.frame(
        values = value_seq,
        density = compute_density(dist_name, params),
        distribution = dist_name,
        tooltip = tooltip_text
      )
    })

    dist_df <- dplyr::bind_rows(density_list)
  }

  # get summary stat
  stat_df <- df_clean |>
    dplyr::summarise(
      min = min(timespan, na.rm = TRUE),
      mean = mean(timespan),
      median = median(timespan),
      max = max(timespan)
    )

  # get hc_df
  # full integer sequence so every day from 0 to co_value appears on the axis
  max_observed <- max(df_clean$timespan, na.rm = TRUE)
  axis_max <- min(max_observed, co_value)
  full_range <- tibble::tibble(timespan = 0:axis_max)

  if (is.null(group_var)) {
    hc_df <- df_clean |>
      dplyr::count(timespan) |>
      dplyr::right_join(full_range, by = "timespan") |>
      dplyr::mutate(
        n = tidyr::replace_na(n, 0L),
        timespan = as.integer(timespan)
      ) |>
      dplyr::arrange(timespan) |>
      dplyr::mutate(
        tooltip = paste0(
          "<b>Delay:</b> ",
          timespan,
          " days",
          "<br>",
          n,
          " occurences"
        )
      )
  } else {
    # expand to full range × all groups so no day is skipped per group
    all_groups <- dplyr::distinct(df_clean, .data[[group_var]])
    full_grid <- tidyr::crossing(all_groups, full_range)

    hc_df <- df_clean |>
      dplyr::count(.data[[group_var]], timespan) |>
      dplyr::right_join(full_grid, by = c(group_var, "timespan")) |>
      dplyr::mutate(
        n = tidyr::replace_na(n, 0L),
        timespan = as.integer(timespan)
      ) |>
      dplyr::arrange(timespan) |>
      dplyr::mutate(
        tooltip = paste0(
          "<b>",
          .data[[group_var]],
          "</b><br>",
          "<b>Delay:</b> ",
          timespan,
          " days",
          "<br>",
          n,
          " occurences"
        )
      )
  }

  if (is.null(group_var)) {
    hc <- highcharter::highchart() |>
      highcharter::hc_chart(type = "column", zoomtype = "x") |>
      highcharter::hc_add_series(
        data = hc_df,
        highcharter::hcaes(x = as.factor(timespan), y = n),
        name = "count",
        type = "column",
        color = "lightgray"
      ) |>
      highcharter::hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = '{point.tooltip}'
      )
  } else {
    pal <- group_colours(group_var, levels(factor(hc_df[[group_var]])))
    hc <- highcharter::highchart() |>
      highcharter::hc_chart(type = "column", zoomtype = "x") |>
      highcharter::hc_add_series(
        data = hc_df,
        highcharter::hcaes(
          x = as.factor(timespan),
          y = n,
          group = !!group_var_sym
        ),
        type = "column",
        stacking = "normal"
      ) |>
      highcharter::hc_colors(pal) |>
      highcharter::hc_legend(
        title = list(text = NULL),
        layout = "vertical",
        align = "right",
        verticalAlign = "top",
        x = -10,
        y = 40,
        itemStyle = list(textOverflow = "ellipsis", width = 150)
      ) |>
      highcharter::hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = '{point.tooltip}'
      )
  }

  # Add fitted distribution
  if (fit_dist) {
    hc <- hc |>
      highcharter::hc_yAxis_multiples(
        list(title = list(text = "Counts"), opposite = FALSE),
        list(title = list(text = "Density"), opposite = TRUE)
      ) |>
      # Add Gamma Density Curve
      highcharter::hc_add_series(
        type = "spline",
        data = dist_df,
        marker = list(enabled = FALSE),
        highcharter::hcaes(
          x = values,
          y = density,
          group = distribution,
          tooltip = tooltip
        ),
        lineWidth = 2,
        yAxis = 1,
        zIndex = 5
      ) |>
      highcharter::hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = '{point.tooltip}'
      )
  }

  # Stat line ?
  stat_line <- list()

  if ("mean" %in% show_stat) {
    stat_line <- append(
      stat_line,
      list(
        list(
          color = "red",
          zIndex = 1,
          value = stat_df$mean,
          label = list(
            text = "Mean",
            verticalAlign = "top",
            textAlign = "left"
          )
        )
      )
    )
  }

  if ("median" %in% show_stat) {
    stat_line <- append(
      stat_line,
      list(
        list(
          color = "red",
          zIndex = 1,
          value = stat_df$median,
          label = list(
            text = "Median",
            verticalAlign = "top",
            textAlign = "left"
          )
        )
      )
    )
  }

  hc <- hc |>
    highcharter::hc_title(
      text = names(delay_choices[delay_choices %in% delay])
    ) |>
    highcharter::hc_xAxis(
      type = "category",
      categories = as.character(0:axis_max), # explicit full sequence, numeric order
      title = list(text = "Delay (days)"),
      allowDecimals = FALSE,
      plotLines = stat_line
    ) |>
    highcharter::hc_plotOptions(
      column = list(
        pointPadding = 0, # No spacing within individual bars
        groupPadding = 0, # No spacing between different bars
        # shadow = FALSE,
        borderWidth = .5,
        borderColor = "white"
      )
    ) |>
    highcharter::hc_credits(
      enabled = TRUE,
      text = glue::glue(
        "Using {invalid_df$n_valid} valid delays ({invalid_df$n_na} missing, {invalid_df$n_co + invalid_df$n_invalid} invalid)"
      )
    )

  return(hc)
}

#' Plot Timeline of Delay Intervals
#'
#' This function creates a timeline plot to visualize the delays between successive date variables in a dataset. It generates a highcharter plot where each date pair is represented as a horizontal bar, and the events are shown as scatter points. The function supports optional grouping of data and displays various statistics related to the delays.
#'
#' @param delay_df A data frame containing the delay data, with at least the date variables specified in `date_var_seq`.
#' @param statistic A string indicating the statistic to display. It must be one of "mean" or "median" for the timespans between date intervals.
#' @param date_var_seq A character vector containing the sequence of date variables. The number of date variables must be at least 2.
#' @param co_value A numeric value representing the cut-off for valid delays. Delays exceeding this value will be considered invalid.
#' @param group_var An optional string specifying the name of a grouping variable in the data. If provided, the data will be grouped by this variable for visualization.
#' @param color_pal A character vector of colors to be used for the date intervals. The length of this vector should match the number of intervals in `date_var_seq` (i.e., `length(date_var_seq) - 1`).
#' @param display_lab A logical value indicating whether to display labels for the events in the plot. Defaults to `TRUE`.
#'
#' @return A highcharter object representing the timeline plot.
#'
#' @details The function processes the input data frame to calculate delay timespans between each pair of date variables in `date_var_seq`. It then creates a horizontal bar plot for each valid interval, with optional grouping and statistical labels. Tooltips are included to show detailed information about each event.
#'
#' The plot also includes scatter points to represent the events at each stage in the timeline. The `color_pal` argument allows users to specify a custom color palette for the intervals, while the `group_var` argument enables grouping by a specific variable.
#'
#' The timeline plot helps in visualizing the distribution and relationships of delay intervals across different stages in a process.
#'
#' @importFrom dplyr select mutate filter summarise count arrange pivot_longer
#' @importFrom tidyr pivot_longer separate
#' @importFrom stringr str_c
#' @importFrom forcats fct_reorder
#' @importFrom highcharter highchart hc_add_series hc_tooltip hc_xAxis hc_yAxis hc_plotOptions hc_credits
#' @importFrom glue glue
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot_delay_timeline(
#'   delay_df = my_data,
#'   statistic = "mean",
#'   date_var_seq = c("start_date", "mid_date", "end_date"),
#'   co_value = 30,
#'   group_var = "group",
#'   color_pal = c("#FFEDA0", "#FEB24C", "#F03B20"),
#'   display_lab = TRUE
#' )
#' }
#'
#' @export
plot_delay_timeline <- function(
  delay_df,
  statistic,
  date_var_seq,
  co_value,
  group_var = NULL,
  display_lab = TRUE
) {
  max <- length(date_var_seq)

  steps <- paste0(date_var_seq[-max], "__", date_var_seq[-1])

  if (max == 1) {
    stop("Please provide at least two date variables")
  }

  #make group_var useable as variable name
  if (!is.null(group_var)) {
    group_var_sym <- rlang::sym(group_var)
  }

  # Pivot delay_df
  delay_long <- delay_df |>
    dplyr::select(dplyr::all_of(c(steps, group_var))) |>
    tidyr::pivot_longer(
      dplyr::all_of(steps),
      names_to = "dates",
      values_to = "timespan"
    ) |>
    dplyr::mutate(timespan = timespan)

  # get invalid df
  invalid_df <- delay_long |>
    dplyr::summarise(
      .by = dates,
      N = dplyr::n(),
      n_na = sum(is.na(timespan)),
      n_invalid = sum(timespan < 0, na.rm = TRUE),
      n_co = sum(timespan >= co_value, na.rm = TRUE),
      n_valid = N - (n_na + n_invalid + n_co)
    )

  # filter only valid ranges
  delay_fil <- dplyr::filter(
    delay_long,
    dplyr::between(timespan, 0, co_value)
  )

  # get summary stat and process for plot
  xrange_df <- delay_fil |>
    dplyr::summarise(
      .by = c(dates, group_var),
      n_valid = dplyr::n(),
      min = min(timespan, na.rm = TRUE),
      mean = mean(timespan) |> round(1),
      median = median(timespan) |> round(1),
      max = max(timespan)
    ) |>
    tidyr::pivot_longer(
      c(mean, median),
      names_to = "stat",
      values_to = "days"
    ) |>
    # dplyr::mutate(days = as.integer(days)) |>
    # create the labels and graph value
    dplyr::mutate(
      label = ifelse(
        days == 0,
        "< 1 day",
        ifelse(days == 1, paste0(days, " day"), paste0(days, " days"))
      ),
      graph_value = days + 1
    ) |>
    tidyr::separate(dates, c("first", "second"), sep = "__", remove = FALSE) |>
    dplyr::mutate(
      first = factor(
        first,
        levels = date_var_seq,
        label = labels(date_var_seq)
      ),
      second = factor(
        second,
        levels = date_var_seq,
        label = labels(date_var_seq)
      )
    ) |>
    dplyr::arrange(first, .by_group = TRUE) |>
    dplyr::mutate(
      .by = c(stat, group_var),
      end = cumsum(graph_value),
      start = dplyr::lag(end),
      start = dplyr::if_else(is.na(start), 0, start),
      #default position will be overide if group_var
      position = 0
    )

  # make the group variable as factor
  if (!is.null(group_var)) {
    xrange_final <- xrange_df |>
      dplyr::mutate(group = factor(!!group_var_sym)) |>
      dplyr::mutate(
        .by = group,
        sum = sum(graph_value[stat == statistic], na.rm = TRUE)
      ) |>
      dplyr::arrange(sum) |>
      dplyr::mutate(group = forcats::fct_reorder(group, sum)) |>
      dplyr::mutate(
        .by = group,
        position = dplyr::cur_group_id() - 1,
        group = forcats::fct_reorder(group, position)
      ) |>
      dplyr::mutate(
        .by = c(first, second, min, max, n_valid, group),
        tooltip_label = paste0(
          "<b>",
          group,
          "</b><br><b>",
          first,
          " - ",
          second,
          "</b><br> Range: ",
          min,
          " - ",
          max,
          "<br>Median: ",
          label[stat == "median"],
          "<br>Mean: ",
          label[stat == "mean"],
          "<br><hr><i>based on ",
          n_valid,
          " valid intervals"
        )
      ) |>
      dplyr::filter(stat == statistic)
  } else {
    xrange_final <- xrange_df |>
      dplyr::mutate(
        .by = c(first, second, min, max, n_valid),
        tooltip_label = paste0(
          "<br><b>",
          first,
          " - ",
          second,
          "</b><br> Range: ",
          min,
          " - ",
          max,
          "<br>Median: ",
          label[stat == "median"],
          "<br>Mean: ",
          label[stat == "mean"],
          "<br><hr><i>based on ",
          n_valid,
          " valid intervals"
        )
      ) |>
      dplyr::filter(stat == statistic)
  }

  # Create a df of the events to be plotted as scatter and labels
  last_event <- names(tail(date_var_seq, 1))

  events_df <- xrange_final |>
    dplyr::select(dplyr::any_of(c(
      "first",
      "second",
      "start",
      "end",
      "graph_value",
      "group",
      "position"
    ))) |>
    dplyr::mutate(
      event = purrr::map(first, ~.x),
      event_2 = purrr::map2(first, second, ~ c(.x, .y)),
      event_final = dplyr::if_else(second == last_event, event_2, event)
    ) |>
    tidyr::unnest(event_final) |>
    dplyr::mutate(x = dplyr::if_else(event_final == last_event, end, start)) |>
    dplyr::select(event_final, x, position)

  if (!is.null(group_var)) {
    group_cat <- levels(xrange_final[xrange_final$stat == statistic, ]$group)
  }

  # PLOT TIMELINE ======================================================================================================================

  # color palette
  generate_palette <- function(n) {
    # Choose a palette from RColorBrewer
    palette <- RColorBrewer::brewer.pal(n, "Set1") # You can choose from other palettes like "Set3", "Paired", etc.
    return(palette)
  }

  color_pal <- suppressWarnings(generate_palette(length(steps)))

  if (length(steps) == 2) {
    color_pal <- color_pal[-1]
  }

  val_col <- data.frame(value = unique(xrange_final$first), col = color_pal)

  xrange_final$color <- val_col$col[match(xrange_final$first, val_col$value)]

  hc <- highcharter::highchart() |>
    highcharter::hc_add_series(
      "xrange",
      name = "timespan",
      data = xrange_final,
      highcharter::hcaes(x = start, x2 = end, y = position, color = color),
      colorByPoint = TRUE,
      enableMouseTracking = TRUE,
      showInLegend = FALSE
    ) |>
    highcharter::hc_add_series(
      "scatter",
      name = "event",
      data = events_df,
      highcharter::hcaes(x = x, y = position),
      color = "darkred",
      showInLegend = FALSE,
      enableMouseTracking = FALSE
    ) |>
    highcharter::hc_xAxis(
      visible = FALSE,
      # extend the limit of the x axis to display the last event label
      max = max(events_df$x) + 1
    ) |>
    highcharter::hc_yAxis(
      visible = !is.null(group_var),
      categories = if (!is.null(group_var)) {
        group_cat
      },
      title = list(text = NULL),
      labels = list(
        enabled = !is.null(group_var),
        style = list(fontSize = "11px", color = "black")
      )
    ) |>
    highcharter::hc_plotOptions(
      scatter = list(
        dataLabels = list(
          enabled = display_lab,
          y = -15,
          align = "left",
          allowOverlap = TRUE,
          rotation = -20,
          style = list(fontSize = "10px", color = "black"),
          formatter = highcharter::JS(
            "function(){
              outHTML =  this.point.event_final
              return(outHTML) 
             }"
          ),
          marker = list(radius = 10)
        )
      ),
      xrange = list(
        pointWidth = 15,
        opacity = .7,
        dataLabels = list(
          enabled = TRUE,
          y = +15,
          style = list(fontSize = "9px", color = "#34495E"),
          formatter = highcharter::JS(
            "function(){
              outHTML =  this.point.label
              return(outHTML)
             }"
          )
        )
      )
    ) |>
    highcharter::hc_tooltip(
      useHTML = T,
      formatter = highcharter::JS(
        "function(){
           outHTML = this.point.tooltip_label
           return(outHTML)
         }"
      )
    ) |>
    highcharter::hc_exporting(enabled = FALSE) |>
    highcharter::hc_title(text = NULL) |>
    highcharter::hc_credits(
      enabled = TRUE,
      text = glue::glue(
        "{sum(c(invalid_df$n_invalid, invalid_df$n_co), na.rm = TRUE)} ranges removed ({sum(na.rm = TRUE, invalid_df$n_invalid)} negatives, {sum(na.rm = TRUE, invalid_df$n_co)} over cut-Off)"
      )
    )
  return(hc)
}
