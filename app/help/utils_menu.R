# Box imports -----------------------------------------------------------------

box::use(
  shiny[
    NS, moduleServer, tagList, reactive, reactiveVal, observeEvent
  ],
  
  shiny.blueprint[
    Menu, MenuItem, MenuDivider, setInput, renderReact, reactOutput, triggerEvent
  ],
)

#' @export
menu_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    Menu(
      style = "min-width: 200px",
      # className = "bp4-elevation-2",
      MenuItem(
        
        text = "Wellcome Global Monitor 2018",
        MenuItem(
          style = "min-width: 200px",
          text = "Index",
          MenuItem(text = "Wellcome Trust in Science Index", 
                   onClick = triggerEvent(ns("WTScI")),
                   intent = "primary")
        ),
        MenuDivider(),
        MenuItem(
          text = "Trust",
          MenuItem(
            style = "min-width: 300px",
            text = "Which of the following people do you trust for medical/health advice?", 
            onClick = triggerEvent(ns("Q20")),
            multiline = TRUE,
            intent = "primary"),
          MenuItem(
            text = "How much do you trust medical & health advice from this country's government?", 
            onClick = triggerEvent(ns("Q21")),
            multiline = TRUE,
            intent = "primary"),
          MenuItem(
            text = "How much do you trust medical & health advice from doctors/nurses?", 
            onClick = triggerEvent(ns("Q22")),
            multiline = TRUE,
            intent = "primary"),
        ),
        MenuDivider(),
        MenuItem(
          text = "Vaccines",
          MenuItem(
            style = "min-width: 300px",
            text = "Before today, had you ever heard of a vaccine?", 
            onClick = triggerEvent(ns("Q23")),
            multiline = TRUE,
            intent = "primary"),
          MenuDivider(),
          MenuItem(
            text = "How much do you agree that vaccines are important for children to have?",
            onClick = triggerEvent(ns("Q24")),
            multiline = TRUE,
            intent = "primary"),
          MenuItem(
            text = "How much do you agree that vaccines are safe?", 
            onClick = triggerEvent(ns("Q25")),
            multiline = TRUE,
            intent = "primary"),
          MenuItem(
            text = "How much do you agree that vaccines are effective?", 
            onClick = triggerEvent(ns("Q26")),
            multiline = TRUE,
            intent = "primary")
        )
      )
    )
  )
}

#' @export
menu_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    stat <- reactiveVal("WTScI")
    
    observeEvent(input$WTScI, {
      stat("WTScI")
    })
    
    observeEvent(input$Q20, {
      stat("Q20")
    })
    observeEvent(input$Q21, {
      stat("Q21")
    })
    observeEvent(input$Q22, {
      stat("Q22")
    })
    observeEvent(input$Q23, {
      stat("Q23")
    })
    observeEvent(input$Q24, {
      stat("Q24")
    })
    observeEvent(input$Q25, {
      stat("Q25")
    })
    observeEvent(input$Q26, {
      stat("Q26")
    })
    
    return(stat)
  })
}

if (interactive()) shinyApp(ui("app"), function(input, output) server("app"))