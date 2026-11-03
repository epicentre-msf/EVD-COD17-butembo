# Transmission network, adapted from epishiny.contacts. Trimmed to the options
# this dashboard uses, with fixed sizes and a short node tooltip.

# fixed symbols for known variables; other levels take the next free palette shape
contacts_fixed_shapes <- list(
  sex = c(Male = "square", Female = "dot")
)

contacts_shape_palette <- c(
  "dot",
  "square",
  "triangle",
  "triangleDown",
  "diamond",
  "star"
)

# node_vars / edge_vars / date_vars: named vectors, label = column name.
mod_contacts_ui <- function(
  id,
  node_vars,
  edge_vars,
  date_vars,
  title = "Chaînes de transmission",
  node_color = "isolated",
  node_shape = "sex"
) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex align-items-center",
      tags$span(bsicons::bs_icon("diagram-3-fill"), title, class = "me-auto pe-2"),
      downloadButton(
        ns("export"),
        label = NULL,
        icon = shiny::icon("download"),
        class = "btn-sm btn-light me-1",
        title = "Exporter le réseau en HTML"
      ),
      bslib::tooltip(
        actionLink(
          ns("toggle_sidebar"),
          label = bsicons::bs_icon("gear", size = "1.2em"),
          class = "ms-2 text-primary"
        ),
        "Options"
      )
    ),
    bslib::card_body(
      padding = 0,
      bslib::layout_sidebar(
        padding = 0,
        gap = 0,
        sidebar = bslib::sidebar(
          id = ns("sidebar"),
          title = "Options",
          position = "right",
          open = "closed",
          width = 250,
          selectInput(
            ns("node_color"),
            "Couleur des nœuds",
            choices = c("Aucun" = "none", node_vars),
            selected = node_color
          ),
          # choices narrowed server-side to variables with 3 levels or fewer
          selectInput(
            ns("node_shape"),
            "Symbole des nœuds",
            choices = c("Aucun" = "none", node_vars),
            selected = node_shape
          ),
          selectInput(
            ns("edge_color"),
            "Couleur des liens",
            choices = c("Aucun" = "none", edge_vars)
          ),
          hr(),
          selectInput(
            ns("x_axis"),
            "Axe des x",
            choices = c("Aucun" = "none", date_vars)
          )
        ),
        uiOutput(ns("network_ui"))
      )
    )
  )
}

# df: one row per node; contact_df: one row per link. tooltip_vars: named
# vector, label = node column. "id" is the node id, whatever id_var was called.
# Sizes are c(undated, dated): a dated x-axis crowds the network.
# height is a CSS size for the plot, per view.
mod_contacts_server <- function(
  id,
  df,
  contact_df,
  id_var,
  from_var,
  to_var,
  node_vars,
  tooltip_vars,
  node_size = c(18, 10),
  edge_width = c(6, 3),
  n_date_break = 10,
  node_shape = "sex",
  height = c(undated = "80vh", dated = "130vh")
) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$toggle_sidebar, {
      bslib::sidebar_toggle("sidebar")
    })

    epi_r <- reactive({
      ll <- df()
      ct <- dplyr::semi_join(
        contact_df(),
        ll,
        by = setNames(id_var, to_var)
      )
      epicontacts::make_epicontacts(
        linelist = ll,
        contacts = ct,
        id = id_var,
        from = from_var,
        to = to_var,
        directed = TRUE
      )
    })

    observe({
      ll <- epi_r()$linelist
      few_levels <- vapply(
        node_vars,
        \(v) dplyr::n_distinct(ll[[v]], na.rm = TRUE) <= 3L,
        logical(1)
      )
      updateSelectInput(
        session,
        "node_shape",
        choices = c("Aucun" = "none", node_vars[few_levels]),
        selected = isolate(input$node_shape) %||% node_shape
      )
    })

    none_to_null <- \(x) if (is.null(x) || identical(x, "none")) NULL else x

    vis <- reactive({
      req(input$x_axis)
      x_axis <- none_to_null(input$x_axis)
      dated <- !is.null(x_axis)
      contacts_plot(
        epi = epi_r(),
        x_axis = x_axis,
        node_size = node_size[[if (dated) 2 else 1]],
        node_color = none_to_null(input$node_color),
        node_shape = none_to_null(input$node_shape),
        edge_color = none_to_null(input$edge_color),
        edge_width = edge_width[[if (dated) 2 else 1]],
        network_shape = "rectangle",
        n_date_break = n_date_break,
        tooltip_vars = tooltip_vars
      )
    })

    # taller when dated: the time axis spreads nodes out and crams at 80vh
    output$network_ui <- renderUI({
      dated <- !is.null(input$x_axis) && input$x_axis != "none"
      visNetwork::visNetworkOutput(
        session$ns("contact_network"),
        height = height[[if (dated) "dated" else "undated"]]
      )
    })

    output$contact_network <- visNetwork::renderVisNetwork(vis())

    output$export <- downloadHandler(
      filename = \() {
        paste0("reseau_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = \(file) htmlwidgets::saveWidget(vis(), file, selfcontained = TRUE)
    )
  })
}

contacts_plot <- function(
  epi,
  x_axis,
  node_size,
  node_color,
  node_shape,
  edge_color,
  edge_width,
  network_shape,
  n_date_break,
  tooltip_vars
) {
  # annot = NULL: the default tooltip lists every column, replaced below
  vis <- plot(
    epi,
    x_axis = x_axis,
    node_size = node_size,
    node_color = node_color,
    edge_color = edge_color,
    edge_width = edge_width,
    network_shape = network_shape,
    parent_pos = "top",
    timeline = NULL,
    arrow_size = 0.5,
    font_size = 20,
    date_labels = "%d\n%B",
    n_break = n_date_break,
    position_dodge = TRUE,
    selector = FALSE,
    annot = NULL
  )

  parts <- lapply(seq_along(tooltip_vars), \(i) {
    v <- as.character(vis$x$nodes[[tooltip_vars[[i]]]])
    paste0("<b>", names(tooltip_vars)[[i]], "</b> : ", dplyr::coalesce(v, "—"))
  })
  title <- do.call(paste, c(parts, sep = "<br>"))
  # a dated x-axis adds helper nodes for the ticks, which get no tooltip
  is_case <- vis$x$nodes$id %in% epi$linelist$id
  vis$x$nodes$title <- ifelse(is_case, title, NA)

  # ids crowd the dated view; the tick helper nodes keep their date labels
  if (!is.null(x_axis)) {
    vis$x$nodes$label[is_case] <- ""
  }

  shape_map <- NULL
  if (!is.null(node_shape)) {
    lvls <- unique(stats::na.omit(vis$x$nodes[[node_shape]]))
    fixed <- contacts_fixed_shapes[[node_shape]][
      intersect(lvls, names(contacts_fixed_shapes[[node_shape]]))
    ]
    spare <- setdiff(contacts_shape_palette, fixed)
    other <- setdiff(as.character(lvls), names(fixed))
    shape_map <- c(fixed, setNames(spare[seq_along(other)], other))
    vis$x$nodes$shape <- shape_map[as.character(vis$x$nodes[[node_shape]])]
  }

  if (is.null(node_color)) {
    vis$x$nodes$color.background <- "#bddcf2"
    vis$x$nodes$color.highlight.background <- "#bddcf2"
  }

  legend_nodes <- NULL
  if (!is.null(node_color)) {
    color_df <- unique(vis$x$nodes[, c(node_color, "color.background"), drop = FALSE])
    color_df <- color_df[!is.na(color_df[[node_color]]), ]
    entry_shape <- if (identical(node_shape, node_color)) {
      shape_map[as.character(color_df[[node_color]])]
    } else {
      "dot"
    }
    legend_nodes <- data.frame(
      label = as.character(color_df[[node_color]]),
      color = color_df$color.background,
      shape = entry_shape,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(node_shape) && !identical(node_shape, node_color)) {
    legend_nodes <- rbind(
      legend_nodes,
      data.frame(
        label = names(shape_map),
        color = "#bddcf2",
        shape = unname(shape_map),
        stringsAsFactors = FALSE
      )
    )
  }
  if (!is.null(legend_nodes)) {
    vis <- visNetwork::visLegend(
      vis,
      useGroups = FALSE,
      addNodes = legend_nodes,
      position = "right",
      width = 0.15
    )
  }

  vis
}
