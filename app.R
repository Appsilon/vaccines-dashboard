library(shiny)
library(shiny.blueprint)
library(shiny.react)

source("./app/view/navbar.R")
source("./app/view/main.R")
source("./app/view/footer.R")


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
