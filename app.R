# Box imports -----------------------------------------------------------------

box::use(
  shiny[
    fluidPage, tags, includeCSS, div, shinyApp
  ],
)

box::use(
  app/view/navbar[navbar_ui, navbar_server],
  app/view/footer[footer],
  app/view/main[main_server, main_ui],
)

ui <- fluidPage(
  
  tags$head(includeCSS("./www/style.css")),

  tagList(
      navbar_ui("navbar"),
      main_ui("main"),
      div(class = "footer", footer)
  )
)

server <- function(input, output, session) {
  navbar_server("navbar")
  main_server("main")
}

shinyApp(ui, server)
