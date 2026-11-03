# Value boxes above the transmission tab: cases linked to an infector, known
# mode of infection, and contact known and followed.

mod_chains_vb_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 3, 3, 3),
    fill = FALSE,
    class = "pt-2 hgap-tight",
    value_box(
      title = vb_title_info(
        "Cas liés à un infecteur",
        paste0(
          "Cas dont le nom de l'infecteur est renseigné.<br>",
          "Un seul infecteur par cas ; pourcentage sur les cas filtrés."
        )
      ),
      value = textOutput(ns("linked"), inline = TRUE),
      showcase_layout = "left center",
      class = "vb-accent vb-chain-linked",
      height = "85px"
    ),
    value_box(
      title = "Mode d'infection connu",
      value = textOutput(ns("mode"), inline = TRUE),
      showcase_layout = "left center",
      class = "vb-accent vb-chain-mode",
      height = "85px"
    ),
    value_box(
      title = "Contact connu",
      value = textOutput(ns("contact_known"), inline = TRUE),
      showcase_layout = "left center",
      class = "vb-accent vb-chain-contact-known",
      height = "85px"
    ),
    value_box(
      title = "Contact suivi",
      value = textOutput(ns("contact_followed"), inline = TRUE),
      showcase_layout = "left center",
      class = "vb-accent vb-chain-contact-followed",
      height = "85px"
    )
  )
}

# ll: one row per unique_id.
mod_chains_vb_server <- function(id, ll) {
  moduleServer(id, function(input, output, session) {
    stats <- reactive({
      d <- ll()
      # the name is the link for now: pid is missing for some named infectors
      n_linked <- sum(!is.na(d$infector_name_1))
      # a missing type reads "Unknown" or "Inconnu", so NA alone would count everyone
      n_contact_known <- sum(d$contact_tracing_known_yn %in% "Yes")
      n_contact_followed <- sum(d$contact_tracing_followed_yn %in% "Yes")
      n_mode <- sum(
        !is.na(d$transmission_type_1) &
          !d$transmission_type_1 %in% c("Inconnu", "Unknown", getOption("epishiny.na.label"))
      )

      list(
        n = nrow(d),
        n_linked = n_linked,
        n_mode = n_mode,
        n_contact_known = n_contact_known,
        n_contact_followed = n_contact_followed
      )
    })

    n_pct <- \(n, denom) paste0(scales::number(n), " (", vb_pct(n, denom), ")")

    output$linked <- renderText(n_pct(stats()$n_linked, stats()$n))

    output$mode <- renderText(n_pct(stats()$n_mode, stats()$n))

    output$contact_known <- renderText(
      n_pct(stats()$n_contact_known, stats()$n)
    )
    output$contact_followed <- renderText(
      n_pct(stats()$n_contact_followed, stats()$n)
    )
  })
}
