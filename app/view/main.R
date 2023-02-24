# Box imports -----------------------------------------------------------------
box::use(
  shiny[
    NS, moduleServer, reactive, reactiveVal, observeEvent, shinyApp, req,
    renderText, htmlOutput, br, fluidRow, column,
  ],
  
  shiny.blueprint[
    Callout, Collapse, Button.shinyInput, Card,
    reactOutput, renderReact,
  ]
)

box::use(
  app/help/utils_tree[tree_server, tree_ui],
  app/help/utils_menu[menu_server, menu_ui],
  app/view/main_country[main_country_server, main_country_ui],
)

# UI ---------------------------------------------------------------------------

#' @export
main_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
            style = "display: flex; align-items: center;",
            H6("Country", style = "margin: 0 10px 0 0;"),
            tree_ui(ns("vac_country")),
          )
        ),
        column(
          6,
          Callout(
            style = "min-height: 40px;",
            htmlOutput(ns("chosen_country"))
          )
        )
      )   
    ),
    br(),
    Button.shinyInput(
      inputId = ns("show_detail"),
      rightIcon = "arrow-down",
      text = "Show country charts",
      minimal = TRUE,
      outlined = TRUE),
    reactOutput(ns("ui_detail"))
  )
  
}

# Server ----------------------------------------------------------------------

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
    observeEvent(input$show_detail, show_detail(!show_detail()))
    output$ui_detail<- renderReact({
      Collapse(
        isOpen = show_detail(),
        main_country_ui(ns("country"))
      )
    })
    
    output$chosen_country <- renderText({
      req(vac_country())
      paste0("<b>", vac_country(), "</b> was chosen!")
    })
  })
}

if (interactive()) {
  shinyApp(main_ui("app"), function(input, output) main_server("app"))
}
