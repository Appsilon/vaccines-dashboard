# Box imports -----------------------------------------------------------------

box::use(
  shiny[
    NS, moduleServer, reactive, reactiveVal, observeEvent,
    tagList, a, br,
  ],
  shiny.blueprint[
    Navbar, NavbarGroup, NavbarHeading, Button.shinyInput, NavbarDivider,
    Alert, Blockquote, H5,
    triggerEvent, renderReact, reactOutput,
  ],
)

# -----------------------------------------------------------------------------
#
#
# UI

#' @export
navbar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    reactOutput(ns("source_info")),
    Navbar(
      NavbarGroup(
        NavbarHeading("Do we trust Science? Vaccines Case")
      ),
      NavbarGroup(
        align = "right",
        a(
          Button.shinyInput(
            inputId = ns("mail"),
            text = "Send feedback",
            icon = "envelope",
            minimal = TRUE
          ),
          href = paste0("mailto:", "hello@appsilon.com")
        ),
        NavbarDivider(),
        Button.shinyInput(
          inputId = ns("source"),
          text = "Source",
          icon = "database",
          minimal = TRUE
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
#
#
# Server

#' @export
navbar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    isOpen <- reactiveVal(FALSE)
    observeEvent(input$source, isOpen(TRUE))
    observeEvent(input$close_info, isOpen(FALSE))

    output$source_info <- renderReact({
      Alert(
        usePortal = FALSE,
        confirmButtonText = "Got it",
        isOpen = isOpen(),
        onClose = triggerEvent(ns("close_info")),
        style = "min-width: 600px;",
        tagList(
          H5("Our World in Data - Vaccination"),
          Blockquote(
            "Samantha Vanderslott, Bernadeta Dadonaite and Max Roser (2013) - 'Vaccination'.
             Published online at OurWorldInData.org. Retrieved from:
             'https://ourworldindata.org/vaccination' [Online Resource]"
          ),
          br(),
          H5("World Health Organization – Immunization surveillance, assessment and monitoring"),
          Blockquote(
            "Data: Immunization coverage, system indicators and schedule, and disease incidence
            for WHO member nations - 1980 onwards for many countries,
            Available at: 'https://www.who.int/data/gho/data/themes/immunization'"
          ),
          br(),
          H5("UNICEF"),
          Blockquote(
            # nolint start: line_length_linter
            "Data: Percent of one-year-olds immunized for UN member nations - 1980 onwards for many countries,
            Available at: Online from UNICEF 'http://web.archive.org/web/20150709064229/http://data.unicef.org/child-health/immunization'.
            Also available via Gapminder 'https://www.gapminder.org/data/' (search “vaccine” to find the data)."
            # nolint end
          ),
          br(),
          H5("Wellcome Global Monitor 2018"),
          Blockquote(
            "The Wellcome Global Monitor is the world’s largest study into how people around
            the world think and feel about science and major health challenges.
            Published online at Wellcome.org. Retrieved from:
            'https://wellcome.org/reports/wellcome-global-monitor/2018'"
          )
        )
      )
    })
  })
}
