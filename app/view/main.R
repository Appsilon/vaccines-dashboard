# Box imports -----------------------------------------------------------------
box::use(
  shiny[
    NS, moduleServer, reactive, reactiveVal, observeEvent, req,
    renderUI, uiOutput, br, fluidRow, column, tagList, div, tags,
  ],
  shiny.blueprint[
    Callout, Collapse, Button.shinyInput, Card, H6,
    reactOutput, renderReact,
  ],
  shinyjs[useShinyjs, runjs],
  glue[glue],
)

# Box imports: application ----------------------------------------------------

box::use(
  app/logic/utils_tree[tree_server, tree_ui],
  app/logic/utils_menu[menu_server, menu_ui],
  app/view/main_country[main_country_server, main_country_ui],
  app/view/main_world[main_world_server, main_world_ui],
)

# -----------------------------------------------------------------------------
#
#
# UI

#' @export
main_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    # reactOutput(ns("main_tabs")),
    #---
    main_world_ui(ns("world")),
    br(),
    Card(
      elevation = 2, interactive = TRUE,
      fluidRow(
        column(
          6,
          div(
            style = "display: flex; align-items: flex-start;",
            H6(
              "Choose Country",
              tags$br(),
              tags$span(
                "(for more detailed information)",
                style = "font-weight: normal; font-style: italic;"
              ),
              style = "margin: 0 10px 0 0; padding-top: 7px; text-align: right;"
            ),
            tree_ui(ns("vac_country")),
          )
        ),
        column(
          6,
          Callout(
            style = "min-height: 40px;",
            uiOutput(ns("chosen_country"))
          )
        )
      )
    ),
    reactOutput(ns("ui_detail"))
  )
}

# -----------------------------------------------------------------------------
#
#
# Server

#' @export
main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # currentTab <- reactiveVal("tab_world")
    # observeEvent(input$select_tab, currentTab(input$select_tab))
    #
    # output$main_tabs <- renderReact(
    #   Tabs(
    #     selectedTabId = currentTab(),
    #     onChange = setInput(ns("select_tab")),
    #     Tab(id = "tab_world",
    #         title = Button("World", minimal = TRUE, icon = "globe"),
    #         panel = main_world_ui(ns("world"))
    #     ),
    #     Tab(id = "tab_country",
    #         title = Button("Country", minimal = TRUE, icon = "polygon-filter"),
    #         panel = main_country_ui(ns("country"))
    #     )
    #   )
    # )
    # ---
    main_world_server("world")
    vac_country <- tree_server("vac_country")
    main_country_server("country", vac_country = vac_country)

    show_detail <- reactiveVal(FALSE)
    # observeEvent(input$show_detail, show_detail(!show_detail()))
    output$ui_detail <- renderReact({
      if (!is.null(vac_country())) {
        runjs(glue(
          "document.getElementById(\"{ns(\"ui_detail\")}\").scrollIntoView();"
        ))
      }
      Collapse(
        isOpen = !is.null(vac_country()),
        main_country_ui(ns("country"))
      )
    })

    output$chosen_country <- renderUI({
      req(vac_country())
      tags$span(
        tags$b(
          vac_country()
        ),
        "was chosen! Scroll down to see the plots"
      )
    })
  })
}

# if (interactive()) {
#   shiny::shinyApp(
#     main_ui("app"),
#     function(input, output) main_server("app")
#   )
# }
