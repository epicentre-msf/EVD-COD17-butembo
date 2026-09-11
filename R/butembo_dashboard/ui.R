# user interface
ui <- page_navbar(
  title = "Butembo EVD situation",
  id = "nav",
  theme = bslib::bs_theme(version = 5, preset = "shiny"),
  fillable = TRUE,
  gap = 10,
  header = tagList(
    tags$head(
      shiny::useBusyIndicators(),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "assets/styles.css"
      )
    )
  ),
  nav_panel(
    title = tags$span(bsicons::bs_icon("clipboard-data"), "Surveillance data"),
    value = "surveillance",
    bslib::layout_sidebar(
      gap = 10,
      padding = NULL,
      sidebar = filter_ui(
        id = "filter",
        date_vars = date_vars,
        group_vars = group_vars,
        wrapper = function(...) {
          bslib::sidebar(..., id = "filter", bg = "#fff", width = 265)
        }
      ),
      mod_vb_ui("vb"),
      layout_columns(
        col_widths = c(6, 6),
        # left: interactive cases map
        place_ui(
          id = "map",
          title = "Place",
          geo_data = geo_data,
          group_vars = group_vars
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
            date_interval_default = "week",
            group_var_default = "type_of_exit",
            ratio_line_lab = "Show CFR line?"
          ),
          person_ui(id = "age_sex")
        )
      )
    ),
  ),

  # Lab panel
  nav_panel(
    title = tags$span(bsicons::bs_icon("clipboard-data"), "Lab data"),
    value = "lab"
  ),
  nav_spacer(),
  nav_item(
    tags$a(
      tags$img(
        src = "assets/img/epicentre_logo_narrow.png",
        alt = "Epicentre Logo",
        height = "40px"
      ),
      class = "py-0",
      title = "Epicentre",
      href = "https://epicentre.msf.org/",
      target = "_blank"
    )
  ),
  nav_item(
    tags$a(
      tags$img(
        src = "assets/img/msf_logo.png",
        alt = "MSF Logo",
        height = "40px"
      ),
      class = "py-0",
      title = "MSF",
      href = "https://msf.org/",
      target = "_blank"
    )
  )
)
