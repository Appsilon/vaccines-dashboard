main_country_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # fluidRow(
    #   column(12,
    #          Card(
    #            elevation = 2, interactive = TRUE,
    #            div(style = "display: flex; align-items: center;",
    #                H6(
    #                  "Country",
    #                  style = "margin: 0 10px 0 0;"),
    #                HTMLSelect.shinyInput(
    #                  inputId = ns("vac_country"),
    #                  options = ls_country,
    #                  value = "Poland",
    #                  minimal = TRUE)
    #            )
    #          )
    #   )
    # ),
    fluidRow(
      column(6,
             Card(
               elevation = 2, interactive = TRUE,
               style = "height: 580px",
               # plot
               textOutput(ns("vac_country_title")),
               plotly::plotlyOutput(ns("vac_country_lineplot"), height = "500px")
             )
      ),
      column(6,
             Card(
               elevation = 2, interactive = TRUE,
               style = "height: 580px",
               # plot
               tabsetPanel(
                 selected = "Trust",
                 tabPanel("Trust Index", value = "InxDet",
                          plotly::plotlyOutput(ns("WTScIdet_country_barplot"), height = "500px")
                 ),
                 tabPanel("Trust", value = "Trust",
                          plotly::plotlyOutput(ns("trust_country_barplot_1"), height = "200px"),
                          plotly::plotlyOutput(ns("trust_country_barplot_2"), height = "300px")
                 ),
                 tabPanel("Vaccines", value = "Vac",
                          plotly::plotlyOutput(ns("vac_country_barplot_1"), height = "140px"),
                          plotly::plotlyOutput(ns("vac_country_barplot_2"), height = "360px")
                 ),
                 tabPanel("Vaccines safety", value = "VacDet",
                          plotly::plotlyOutput(ns("Q25det_country_barplot"), height = "500px")
                 )
               )
             )
      )
    )
  )
}


main_country_server <- function(id, vac_country) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # lineplot for country
    # vac_country <- reactive({
    #   input$vac_country
    # })

    vac_country_title <- reactive({
      req(vac_country())
      
      prep_country_title(country = vac_country())
    })
    
    vac_country_data <- reactive({
      req(vac_country())
      
      prep_country_data(country = vac_country(), src_data = tab_vaccines)
    })
    
    output$vac_country_title <- renderText({
      vac_country_title()
    })
    
    output$vac_country_lineplot <- plotly::renderPlotly({
      
      plot_country_vaccines(plt_data = vac_country_data(),
                            ls_col = ls_colors[["vac"]])
    })
    
    # barplot
    WTScIdet_country_detail <- reactive({
      req(vac_country())
      
      prep_country_detail(country = vac_country(), 
                          src_data = detail_responses_country[["WTScI"]])
    })
    
    output$WTScIdet_country_barplot <- plotly::renderPlotly({
      plot_country_detail(
        plt_data = WTScIdet_country_detail(),
        ls_col = ls_colors[["WTScI"]],
        plt_title = "Wellcome Trust in Science Index")
    })
    # ---
    trust_country_questions_1 <- reactive({
      req(vac_country())
      
      prep_country_questions(country = vac_country(), 
                             src_data = wgm_responses_country,
                             num_q = c("Q20"))
    })
    
    output$trust_country_barplot_1 <- plotly::renderPlotly({
      plot_country_questions(
        plt_data = trust_country_questions_1(),
        ls_col = ls_colors,
        num_q = c("Q20"),
        title_tab = wgm_questions,
        plt_height = 200)
    })
    
    trust_country_questions_2 <- reactive({
      req(vac_country())
      
      prep_country_questions(country = vac_country(), 
                             src_data = wgm_responses_country,
                             num_q = c("Q21", "Q22"))
    })
    
    output$trust_country_barplot_2 <- plotly::renderPlotly({
      plot_country_questions(
        plt_data = trust_country_questions_2(),
        ls_col = ls_colors,
        num_q = c("Q21", "Q22"),
        title_tab = wgm_questions,
        plt_height = 130)
    })
    
    # ---
    vac_country_questions_1 <- reactive({
      req(vac_country())
      
      prep_country_questions(country = vac_country(), 
                             src_data = wgm_responses_country,
                             num_q = c("Q23"))
    })
    
    output$vac_country_barplot_1 <- plotly::renderPlotly({
      plot_country_questions(
        plt_data = vac_country_questions_1(),
        ls_col = ls_colors,
        num_q = c("Q23"),
        title_tab = wgm_questions,
        plt_height = 140)
    })
    
    vac_country_questions_2 <- reactive({
      req(vac_country())

      prep_country_questions(country = vac_country(), 
                             src_data = wgm_responses_country,
                             num_q = c("Q24", "Q25", "Q26"))
    })
    
    output$vac_country_barplot_2 <- plotly::renderPlotly({
      plot_country_questions(
        plt_data = vac_country_questions_2(),
        ls_col = ls_colors,
        num_q = c("Q24", "Q25", "Q26"),
        title_tab = wgm_questions,
        plt_height = 120)
    })
    
    # ---
    Q25det_country_detail <- reactive({
      req(vac_country())
     
      prep_country_detail(country = vac_country(), 
                          src_data = detail_responses_country[["Q25"]])
    })
    
    output$Q25det_country_barplot <- plotly::renderPlotly({
      plot_country_detail(
        plt_data = Q25det_country_detail(),
        ls_col = ls_colors[["Q25"]],
        plt_title = "How much do you agree that vaccines are safe?")
    })
    
    #-
  })
}

# shinyApp(main_country_ui("app"), function(input, output) main_country_server("app", reactive({"Poland"})))
