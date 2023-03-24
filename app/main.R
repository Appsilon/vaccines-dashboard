# Box imports -----------------------------------------------------------------

box::use(
  shiny[NS, moduleServer, fluidPage, tags, div, tagList]
)

box::use(
  app/view/navbar[navbar_ui, navbar_server],
  app/view/main[main_server, main_ui],
  app/view/footer[footer],
)

# -----------------------------------------------------------------------------
#
#
# UI

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tagList(
      navbar_ui(ns("navbar")),
      main_ui(ns("main")),
      div(class = "footer", footer)
    )
  )
}

# -----------------------------------------------------------------------------
#
#
# Server

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    navbar_server("navbar")
    main_server("main")
  })
}
