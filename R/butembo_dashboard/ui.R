# global period filter, shown at the top of the sidebar on every tab
period_select <- tags$div(
  class = "d-flex justify-content-start mb-2 map-timeperiod",
  shiny::selectizeInput(
    "period",
    label = NULL,
    choices = c(
      "7 derniers jours" = "last_7d",
      "14 derniers jours" = "last_14d",
      "21 derniers jours" = "last_21d",
      "Total" = "total"
    ),
    selected = "total",
    width = "100%",
    options = list(
      openOnFocus = FALSE,
      render = I(
        "{ item: function(item, escape) {
        return '<div><b>Période</b>: <span class=\"map-dd-value\">' + escape(item.label) + '</span></div>';
      } }"
      )
    )
  )
)

# user interface
ui <- page_navbar(
  title = "Surveillance BVD Butembo",
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
  sidebar = filter_ui(
    id = "filter",
    date_vars = date_vars,
    group_vars = group_vars,
    date_filters_lab = "Filtres de dates",
    missing_dates_lab = "Inclure les patients sans date ?",
    group_filters_lab = "Filtres de groupes",
    filter_btn_lab = "Appliquer",
    filter_btn_tooltip = "Cliquer ici pour appliquer les filtres et mettre à jour les graphiques",
    reset_btn_lab = "Réinitialiser",
    title = tags$span(bsicons::bs_icon("filter"), "Filtres"),
    wrapper = function(...) {
      # period first: it also drives the lab tab, which the filters below do not
      bslib::sidebar(
        period_select,
        ...,
        id = "filter",
        bg = "#fff",
        width = 265
      )
    }
  ),
  nav_menu(
    title = tags$span(bsicons::bs_icon("clipboard-data"), "Données de surveillance"),
    nav_panel(
      title = "Tableau de bord",
      value = "surveillance",
      mod_vb_ui("vb"),
      layout_columns(
        col_widths = c(4, 8),
        height = "45vh",
        min_height = 380,
        mod_zs_table_ui("zs_table"),
        do.call(
          time_ui,
          c(
            list(
              id = "curve",
              title = "Temps",
              date_vars = date_vars,
              group_vars = group_vars,
              date_interval_default = "week",
              group_var_default = "dead_upon_notif_grp",
              ratio_line_lab = "Afficher la courbe de létalité ?"
            ),
            time_ui_labs
          )
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        height = "80vh",
        min_height = 600,
        mod_map_place_ui(id = "map"),
        person_ui(
          id = "age_sex",
          title = "Personne",
          opts_btn_lab = "Options",
          count_vars_lab = "Indicateur",
          age_breaks_lab = "Tranches d'âge",
          age_breaks_help = "Valeurs séparées par des virgules (p. ex. 0, 5, 18, 25, 35, 50)",
          age_breaks_apply_lab = "Appliquer les tranches"
        )
      )
    ),
    nav_panel(
      title = "Courbes épidémiques stratifiées",
      value = "epicurve_hz",
      mod_epicurve_hz_ui("epicurve_hz", group_vars = group_vars)
    ),
    nav_panel(
      title = "Formations sanitaires",
      value = "facilities",
      mod_facilities_ui("facilities")
    )
  ),

  nav_panel(
    title = "Délais",
    value = "delays",
    bslib::card(
      full_screen = TRUE,
      height = "80vh",
      min_height = 600,
      bslib::card_header("Délais entre événements clés"),
      mod_delay_ui("delay")
    )
  ),

  nav_panel(
    title = "Répartition",
    value = "breakdown",
    mod_breakdown_ui("breakdown")
  ),

  mod_lab_ui("lab"),

  mod_quality_ui("quality", quality),

  nav_spacer(),
  nav_item(
    tags$a(
      tags$img(
        src = "assets/img/epicentre_logo_narrow.png",
        alt = "Logo Epicentre",
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
        alt = "Logo MSF",
        height = "40px"
      ),
      class = "py-0",
      title = "MSF",
      href = "https://msf.org/",
      target = "_blank"
    )
  )
)
