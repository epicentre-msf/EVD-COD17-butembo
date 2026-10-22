# Breakdown module: bar chart and table of counts by one categorical variable,
# optionally split by a second. Ported from evd-2026-app (mod_crosstab.R).

BREAKDOWN_NO_GROUP <- "none"
BREAKDOWN_OTHER <- "(Autre)"
BREAKDOWN_OTHER_COL <- "#B5B5B5CC"
BREAKDOWN_TOP_N_DEFAULT <- 20L

# factor level order is meaningful, so these start on "Ordre" not "Fréquence"
breakdown_ordered_vars <- c("age_group", "sex", "type_of_exit")

border_right <- list(borderRight = "1px solid rgba(0, 0, 0, 0.1)")
border_right_bold <- list(
  borderRight = "1px solid rgba(0, 0, 0, 0.1)",
  fontWeight = "bold"
)

rt_footer_sum <- function() {
  reactable::JS(
    "function(column, state) {
      let total = 0
      state.sortedData.forEach(function(row) {
        total += row[column.id]
      })
      return total.toLocaleString('fr-FR')
    }"
  )
}

# share is of the row total, read from the sibling `denom` column
rt_add_n_percent <- function(denom = ".total") {
  reactable::JS(sprintf(
    "function(cellInfo) {
      const v = cellInfo.value
      if (v === null || v === undefined) return '—'
      const s = v.toLocaleString('fr-FR')
      const d = cellInfo.row['%s']
      return d ? s + ' (' + (100 * v / d).toFixed(0) + '%%)' : s
    }",
    denom
  ))
}

# a pooled percentage is not the sum of per-row percentages
rt_footer_n_percent <- function(numer, denom = ".total") {
  reactable::JS(sprintf(
    "function(column, state) {
      let n = 0, d = 0
      state.sortedData.forEach(function(row) {
        n += row['%s'] || 0
        d += row['%s'] || 0
      })
      return n.toLocaleString('fr-FR') + (d ? ' (' + (100 * n / d).toFixed(0) + '%%)' : '')
    }",
    numer,
    denom
  ))
}

rt_n_percent_coldef <- function(col, denom = ".total") {
  reactable::colDef(
    cell = rt_add_n_percent(denom),
    footer = rt_footer_n_percent(col, denom)
  )
}

breakdown_input_label <- function(label, text) {
  htmltools::tagList(
    label,
    htmltools::tags$style(
      ".tooltip-bd-input .tooltip-inner { max-width: 320px; text-align: left; }"
    ),
    bslib::tooltip(
      bsicons::bs_icon("info-circle", class = "ms-1 text-primary", size = "0.9em"),
      htmltools::HTML(text),
      options = list(customClass = "tooltip-bd-input")
    )
  )
}

# ! UI -------------------------------------------------------------------------

mod_breakdown_ui <- function(id, sidebar_width = 250) {
  ns <- NS(id)

  inputs_ui <- tagList(
    htmltools::div(
      class = "text-muted small px-2 pt-2",
      "Répartition des cas selon une variable catégorielle."
    ),
    selectInput(
      ns("x_var"),
      label = "Variable",
      choices = NULL,
      selectize = FALSE,
      width = "100%"
    ),
    selectInput(
      ns("group_var"),
      label = "Grouper par",
      choices = NULL,
      selectize = FALSE,
      width = "100%"
    ),
    shinyWidgets::radioGroupButtons(
      inputId = ns("sort"),
      label = breakdown_input_label(
        "Trier les catégories par",
        "(Autre) et (Manquant) sont toujours affichés en dernier"
      ),
      size = "sm",
      justified = TRUE,
      choices = c("Fréquence" = "freq", "Ordre" = "order"),
      selected = "freq"
    ),
    numericInput(
      ns("top_n"),
      label = breakdown_input_label(
        "Nombre maximum de catégories",
        paste(
          "Au-delà, seules les plus fréquentes sont affichées et les autres",
          "sont regroupées en (Autre). Les valeurs manquantes sont toujours affichées"
        )
      ),
      value = BREAKDOWN_TOP_N_DEFAULT,
      min = 2,
      max = 200,
      step = 5,
      width = "100%"
    )
  )

  bslib::navset_card_tab(
    id = ns("tabs"),
    height = "80vh",
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = sidebar_width,
      position = "right",
      open = "open",
      inputs_ui
    ),
    bslib::nav_panel(
      title = tags$span(bsicons::bs_icon("bar-chart"), "Graphique"),
      class = "p-0",
      highcharter::highchartOutput(ns("chart"), height = "100%")
    ),
    bslib::nav_panel(
      title = tags$span(bsicons::bs_icon("table"), "Tableau"),
      class = "p-0",
      # note must be a direct sibling of the table so the table keeps filling
      bslib::as_fill_carrier(htmltools::div(
        reactable::reactableOutput(ns("tbl")),
        htmltools::div(
          class = "text-muted small px-2 pt-2",
          textOutput(ns("tbl_note"), inline = TRUE)
        )
      ))
    )
  )
}

# ! SERVER ---------------------------------------------------------------------

mod_breakdown_server <- function(
  id,
  df,
  group_vars,
  time_filter = shiny::reactiveVal(),
  place_filter = shiny::reactiveVal(),
  filter_info = shiny::reactiveVal()
) {
  moduleServer(id, function(input, output, session) {
    na_lab <- getOption("epishiny.na.label", "(Manquant)")

    x_choices <- reactive(group_vars[group_vars %in% names(df())])

    update_group_choices <- function() {
      ch <- c(
        "Aucun groupe" = BREAKDOWN_NO_GROUP,
        x_choices()[!x_choices() %in% input$x_var]
      )
      sel <- if (isTRUE(input$group_var %in% ch)) {
        input$group_var
      } else {
        BREAKDOWN_NO_GROUP
      }
      updateSelectInput(session, "group_var", choices = ch, selected = sel)
    }

    choices_sent <- reactiveVal(NULL)
    observe({
      ch <- x_choices()
      if (identical(ch, choices_sent())) {
        return()
      }
      choices_sent(ch)
      sel <- if (isTRUE(input$x_var %in% ch)) {
        input$x_var
      } else if ("age_group" %in% ch) {
        "age_group"
      } else {
        ch[1]
      }
      updateSelectInput(session, "x_var", choices = ch, selected = sel)
      update_group_choices()
    })

    # reactiveVal so the first render after a variable change is already sorted right
    sort_mode <- reactiveVal("freq")

    observeEvent(input$x_var, {
      m <- if (input$x_var %in% breakdown_ordered_vars) "order" else "freq"
      sort_mode(m)
      shinyWidgets::updateRadioGroupButtons(session, "sort", selected = m)
      update_group_choices()
    })

    observeEvent(input$sort, sort_mode(input$sort))

    # input$group_var can equal input$x_var for one render on first flush
    group_var <- reactive({
      g <- input$group_var
      if (
        !length(g) ||
          g %in% c(BREAKDOWN_NO_GROUP, "") ||
          identical(g, input$x_var)
      ) {
        NULL
      } else {
        g
      }
    })

    top_n <- shiny::debounce(
      reactive({
        n <- suppressWarnings(as.integer(input$top_n))
        if (!length(n) || is.na(n)) {
          BREAKDOWN_TOP_N_DEFAULT
        } else {
          max(2L, min(200L, n))
        }
      }),
      400
    )

    # df is already filtered by the sidebar and the click filters
    ct <- reactive({
      shiny::req(input$x_var, input$x_var %in% x_choices())
      d <- df()
      shiny::validate(shiny::need(nrow(d) > 0, "Aucune donnée à afficher"))
      all_counts <- breakdown_counts(
        d,
        input$x_var,
        group_var(),
        sort_mode = sort_mode(),
        na_lab = na_lab
      )
      shown <- breakdown_lump(
        all_counts,
        input$x_var,
        group_var(),
        top_n(),
        na_lab = na_lab
      )
      n_diff <- nlevels(all_counts[[input$x_var]]) -
        nlevels(shown[[input$x_var]])
      list(
        counts = shown,
        n_lumped = if (n_diff > 0) n_diff + 1L else 0L
      )
    })

    trunc_note <- reactive({
      n <- ct()$n_lumped
      if (n == 0L) {
        NULL
      } else {
        sprintf("Les %s plus petites catégories sont regroupées en (Autre).", n)
      }
    })

    x_lab <- reactive(epishiny:::get_label(input$x_var, x_choices()))

    group_lab <- reactive({
      g <- group_var()
      if (is.null(g)) NULL else epishiny:::get_label(g, x_choices())
    })

    caption <- reactive({
      epishiny:::format_filter_info(filter_info(), time_filter(), place_filter())
    })

    output$chart <- highcharter::renderHighchart({
      build_breakdown_hc(
        ct()$counts,
        x_var = input$x_var,
        group_var = group_var(),
        x_lab = x_lab(),
        group_lab = group_lab(),
        credits = trunc_note(),
        caption = caption(),
        na_lab = na_lab
      )
    })

    output$tbl <- reactable::renderReactable({
      build_breakdown_rt(
        ct()$counts,
        x_var = input$x_var,
        group_var = group_var(),
        x_lab = x_lab(),
        group_lab = group_lab()
      )
    })

    output$tbl_note <- renderText({
      paste(c(breakdown_pct_note(group_var()), trunc_note()), collapse = " ")
    })
  })
}

# ! DATA PREP ------------------------------------------------------------------

# One row per x level (x group level). Missing is an explicit level, never dropped.
breakdown_counts <- function(
  d,
  x_var,
  group_var = NULL,
  sort_mode = c("freq", "order"),
  na_lab = getOption("epishiny.na.label", "(Manquant)")
) {
  sort_mode <- match.arg(sort_mode)
  if (!is.null(group_var) && identical(group_var, x_var)) {
    group_var <- NULL
  }
  cols <- c(x_var, group_var)

  out <- d |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      \(x) breakdown_to_level(x, na_lab)
    )) |>
    dplyr::summarise(n = dplyr::n(), .by = dplyr::all_of(cols))

  for (v in cols) {
    if (sort_mode == "freq") {
      out[[v]] <- factor(
        as.character(out[[v]]),
        levels = breakdown_count_order(out, v, na_lab)
      )
    }
    out[[v]] <- droplevels(out[[v]])
  }

  stopifnot(sum(out$n) == nrow(d))
  dplyr::arrange(out, .data[[x_var]])
}

breakdown_to_level <- function(x, na_lab) {
  f <- droplevels(as.factor(x))
  if (anyNA(f)) {
    f <- forcats::fct_na_value_to_level(f, na_lab)
  }
  if (na_lab %in% levels(f)) {
    f <- forcats::fct_relevel(f, na_lab, after = Inf)
  }
  f
}

breakdown_count_order <- function(counts, var, na_lab) {
  ord <- counts |>
    dplyr::summarise(tot = sum(n), .by = dplyr::all_of(var)) |>
    dplyr::arrange(dplyr::desc(tot))
  lv <- as.character(ord[[var]])
  c(setdiff(lv, na_lab), intersect(na_lab, lv))
}

# Fold small levels into (Autre) rather than dropping, so denominators stay whole.
breakdown_lump <- function(counts, x_var, group_var, top_n, na_lab) {
  lv <- levels(counts[[x_var]])
  if (!length(top_n) || is.na(top_n) || length(setdiff(lv, na_lab)) <= top_n) {
    return(counts)
  }
  keep <- utils::head(
    setdiff(breakdown_count_order(counts, x_var, na_lab), na_lab),
    top_n
  )
  x <- as.character(counts[[x_var]])
  x[!x %in% c(keep, na_lab)] <- BREAKDOWN_OTHER
  counts[[x_var]] <- factor(
    x,
    levels = c(intersect(lv, keep), BREAKDOWN_OTHER, intersect(lv, na_lab))
  )
  counts |>
    dplyr::summarise(
      n = sum(n),
      .by = dplyr::all_of(c(x_var, group_var))
    ) |>
    dplyr::arrange(.data[[x_var]])
}

# `.total` rather than `Total` so a group level called "Total" cannot collide
breakdown_wide <- function(counts, x_var, group_var) {
  counts |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(group_var),
      values_from = "n",
      values_fill = 0L,
      names_sort = TRUE
    ) |>
    dplyr::mutate(
      .total = as.integer(rowSums(dplyr::pick(-dplyr::all_of(x_var))))
    )
}

# ! CHART ----------------------------------------------------------------------

breakdown_stacked_tooltip <- function() {
  paste0(
    '<span style="color:{point.color}">●</span> {series.name}: ',
    "<b>{point.y:,.0f} ({point.percentage:.1f}%)</b><br/>"
  )
}

breakdown_legend_title <- function(group_lab) {
  if (!length(group_lab)) {
    return(NULL)
  }
  sprintf(
    paste0(
      '%s<br/><span style="font-size: 9px; color: #666; font-weight: normal">',
      "(Cliquer pour masquer)</span>"
    ),
    group_lab
  )
}

build_breakdown_hc <- function(
  counts,
  x_var,
  group_var = NULL,
  x_lab = x_var,
  group_lab = NULL,
  count_lab = getOption("epishiny.count.label", "Cas"),
  credits = NULL,
  caption = NULL,
  na_lab = getOption("epishiny.na.label", "(Manquant)")
) {
  cats <- levels(counts[[x_var]])

  at_cats <- function(d) {
    as.numeric(tidyr::replace_na(
      d$n[match(cats, as.character(d[[x_var]]))],
      0
    ))
  }

  if (is.null(group_var)) {
    # grey for "no value" bars so they do not read as a category
    base_col <- epishiny:::prepare_palette(1)[1]
    point_cols <- dplyr::case_when(
      cats %in% c(na_lab, na_labels) ~ paste0(na_col, "CC"),
      cats == BREAKDOWN_OTHER ~ BREAKDOWN_OTHER_COL,
      .default = base_col
    )
    series_list <- list(list(
      name = count_lab,
      showInLegend = FALSE,
      data = purrr::map2(
        at_cats(counts),
        point_cols,
        \(y, col) list(y = y, color = col, pct = 100 * y / sum(counts$n))
      )
    ))
  } else {
    lvls <- levels(counts[[group_var]])
    pal <- group_colours(group_var, lvls, missing_label = na_lab)
    pal[lvls == BREAKDOWN_OTHER] <- BREAKDOWN_OTHER_COL
    series_list <- purrr::map(seq_along(lvls), function(i) {
      list(
        name = lvls[i],
        color = pal[i],
        data = as.list(at_cats(
          counts[as.character(counts[[group_var]]) == lvls[i], ]
        ))
      )
    })
  }

  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "bar", zoomType = "x") |>
    highcharter::hc_title(text = NULL) |>
    highcharter::hc_xAxis(
      categories = as.list(as.character(cats)),
      title = list(text = x_lab, style = list(fontSize = "13px")),
      labels = list(style = list(fontSize = "14px")),
      crosshair = TRUE
    ) |>
    highcharter::hc_yAxis(
      title = list(text = count_lab, style = list(fontSize = "16px")),
      labels = list(style = list(fontSize = "13px")),
      allowDecimals = FALSE,
      maxPadding = 0.2
    ) |>
    highcharter::hc_plotOptions(
      bar = list(stacking = "normal", dataLabels = list(enabled = FALSE))
    ) |>
    highcharter::hc_add_series_list(series_list) |>
    highcharter::hc_tooltip(
      shared = TRUE,
      headerFormat = "<b>{point.key}</b><br/>",
      pointFormat = if (is.null(group_var)) {
        paste0(
          '<span style="color:{point.color}">●</span> {series.name}: ',
          "<b>{point.y:,.0f} ({point.pct:.1f}%)</b><br/>"
        )
      } else {
        breakdown_stacked_tooltip()
      },
      footerFormat = if (is.null(group_var)) {
        ""
      } else {
        "Total : <b>{point.total:,.0f}</b>"
      }
    ) |>
    highcharter::hc_legend(
      enabled = !is.null(group_var),
      title = list(text = breakdown_legend_title(group_lab)),
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      x = -10,
      y = 40,
      itemStyle = list(textOverflow = "ellipsis", width = 150)
    ) |>
    highcharter::hc_credits(
      enabled = length(credits) > 0,
      text = if (length(credits)) credits else ""
    )

  epishiny:::my_hc_export(hc, caption = caption)
}

# ! TABLE ----------------------------------------------------------------------

build_breakdown_rt <- function(
  counts,
  x_var,
  group_var = NULL,
  x_lab = x_var,
  group_lab = NULL,
  count_lab = getOption("epishiny.count.label", "Cas")
) {
  x_coldef <- stats::setNames(
    list(reactable::colDef(
      name = x_lab,
      align = "left",
      minWidth = 160,
      sticky = "left",
      footer = "Total",
      style = border_right,
      footerStyle = border_right_bold
    )),
    x_var
  )

  rt <- function(d, cols, col_groups = NULL) {
    reactable::reactable(
      d,
      highlight = TRUE,
      compact = TRUE,
      pagination = FALSE,
      height = "100%",
      theme = rt_theme(),
      defaultColDef = reactable::colDef(
        align = "right",
        na = "—",
        footerStyle = list(fontWeight = "bold")
      ),
      columns = c(x_coldef, cols),
      columnGroups = col_groups
    )
  }

  if (is.null(group_var)) {
    d <- counts |> dplyr::mutate(percent = n / sum(n))
    return(rt(
      d,
      list(
        n = reactable::colDef(
          name = count_lab,
          footer = rt_footer_sum(),
          format = reactable::colFormat(separators = TRUE, locales = "fr-FR")
        ),
        percent = reactable::colDef(
          name = "%",
          format = reactable::colFormat(percent = TRUE, digits = 1)
        )
      )
    ))
  }

  d <- breakdown_wide(counts, x_var, group_var)
  grp_cols <- setdiff(names(d), c(x_var, ".total"))
  rt(
    d,
    c(
      lapply(stats::setNames(nm = grp_cols), rt_n_percent_coldef),
      list(
        .total = reactable::colDef(
          name = "Total",
          style = list(fontWeight = 600),
          footer = rt_footer_sum(),
          format = reactable::colFormat(separators = TRUE, locales = "fr-FR")
        )
      )
    ),
    col_groups = list(reactable::colGroup(
      name = group_lab %||% group_var,
      columns = grp_cols
    ))
  )
}

breakdown_pct_note <- function(group_var) {
  if (is.null(group_var)) {
    "Les pourcentages sont calculés sur le total de la colonne."
  } else {
    paste(
      "Les pourcentages des cellules sont calculés sur le total de la ligne.",
      "Les valeurs manquantes sont affichées comme une catégorie."
    )
  }
}
