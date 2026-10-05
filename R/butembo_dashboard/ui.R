# user interface
ui <- page_navbar(
  title = "Butembo BVD Surveillance",
  id = "nav",
  theme = bslib::bs_theme(version = 5, preset = "shiny"),
  fillable = c("lab", "quality"),
  gap = 10,
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      shiny::useBusyIndicators(),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0(
          "assets/styles.css?v=",
          as.integer(file.mtime("www/styles.css"))
        )
      ),
      tags$script(src = "assets/recolour.js")
    )
  ),
  nav_menu(
    title = tags$span(bsicons::bs_icon("clipboard-data"), "Surveillance data"),
    nav_panel(
      title = "Dashboard",
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
        tags$div(
          class = "d-flex justify-content-start mb-2 map-timeperiod",
          shiny::selectizeInput(
            "period",
            label = NULL,
            choices = c(
              "Last 7d" = "last_7d",
              "Last 14d" = "last_14d",
              "Last 21d" = "last_21d",
              "Total" = "total"
            ),
            selected = "total",
            width = "170px",
            options = list(
              openOnFocus = FALSE,
              render = I(
                "{ item: function(item, escape) {
                return '<div><b>Period</b>: <span class=\"map-dd-value\">' + escape(item.label) + '</span></div>';
              } }"
              )
            )
          )
        ),
        mod_vb_ui("vb"),
        layout_columns(
          col_widths = c(6, 6),
          height = "80vh",
          min_height = 600,
          # left: interactive cases map
          mod_map_place_ui(id = "map"),
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
        ),
        bslib::card(
          full_screen = TRUE,
          min_height = 650,
          bslib::card_header("Delays between key events"),
          mod_delay_ui("delay")
        )
      )
    ),
    nav_panel(
      title = "Stratified Epicurves",
      value = "epicurve_hz",
      mod_epicurve_hz_ui("epicurve_hz", group_vars = group_vars)
    ),
    nav_panel(
      title = "Health facilities",
      value = "facilities",
      mod_facilities_ui("facilities")
    )
  ),

  mod_lab_ui("lab"),

  mod_quality_ui("quality", quality),

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
