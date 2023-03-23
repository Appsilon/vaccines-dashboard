# Box imports -----------------------------------------------------------------

box::use(
  shiny[
    NS, moduleServer, reactive, reactiveVal, observeEvent, req,
    tagList, fluidRow, column, div, textOutput, tabsetPanel, renderText,
    tabPanel, updateTabsetPanel, mainPanel,
  ],
  shiny.blueprint[
    Card, H6, HTMLSelect.shinyInput,
    reactOutput, renderReact,
  ],
  plotly[plotlyOutput, renderPlotly]
)

# Box imports: application ----------------------------------------------------

box::use(
  app/logic/utils_fun_plot[
    plot_country_vaccines, plot_country_detail, plot_country_questions
  ],
  app/logic/utils_prep_data[
    prep_country_title, prep_country_data, prep_country_detail,
    prep_country_questions
  ],
  app/logic/data_import[
    detail_responses_country, wgm_responses_country, wgm_questions,
    tab_vaccines, ls_colors,
  ],
)

# -----------------------------------------------------------------------------
#
#
# UI


main_country_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        6,
        Card(
          elevation = 2, interactive = TRUE,
          style = "height: 580px",
          # plot
          textOutput(ns("vac_country_title")),
          plotlyOutput(ns("vac_country_lineplot"), height = "500px")
        )
      ),
      column(
        6,
        Card(
          elevation = 2,
          interactive = TRUE,
          style = "height: 580px",
          # plot
          tabsetPanel(
            id = "country_tabs",
            selected = "Trust",
            type = "tabs",
            tabPanel(
              "Trust Index",
              value = "InxDet",
              plotlyOutput(
                ns("WTScIdet_country_barplot"),
                height = "500px"
              )
            ),
            tabPanel(
              "Trust",
              value = "Trust",
              plotlyOutput(
                ns("trust_country_barplot_1"),
                height = "200px"
              ),
              plotlyOutput(
                ns("trust_country_barplot_2"),
                height = "300px"
              )
            ),
            tabPanel(
              "Vaccines",
              value = "Vac",
              plotlyOutput(
                ns("vac_country_barplot_1"),
                height = "140px"
              ),
              plotlyOutput(
                ns("vac_country_barplot_2"),
                height = "360px"
              )
            ),
            tabPanel(
              "Vaccines safety",
              value = "VacDet",
              plotlyOutput(
                ns("Q25det_country_barplot"),
                height = "500px"
              )
            )
          )
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
#
#
# Server

main_country_server <- function(id, vac_country) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    output$vac_country_lineplot <- renderPlotly({
      plot_country_vaccines(
        plt_data = vac_country_data(),
        ls_col = ls_colors[["vac"]]
      )
    })

    # barplot
    WTScIdet_country_detail <- reactive({
      req(vac_country())

      prep_country_detail(
        country = vac_country(),
        src_data = detail_responses_country[["WTScI"]]
      )
    })

    output$WTScIdet_country_barplot <- renderPlotly({
      plot_country_detail(
        plt_data = WTScIdet_country_detail(),
        ls_col = ls_colors[["WTScI"]],
        plt_title = "Wellcome Trust in Science Index"
      )
    })

    # ---
    trust_country_questions_1 <- reactive({
      req(vac_country())

      prep_country_questions(
        country = vac_country(),
        src_data = wgm_responses_country,
        num_q = c("Q20")
      )
    })

    output$trust_country_barplot_1 <- renderPlotly({
      plot_country_questions(
        plt_data = trust_country_questions_1(),
        ls_col = ls_colors,
        num_q = c("Q20"),
        title_tab = wgm_questions,
        plt_height = 200
      )
    })

    trust_country_questions_2 <- reactive({
      req(vac_country())

      prep_country_questions(
        country = vac_country(),
        src_data = wgm_responses_country,
        num_q = c("Q21", "Q22")
      )
    })

    output$trust_country_barplot_2 <- renderPlotly({
      plot_country_questions(
        plt_data = trust_country_questions_2(),
        ls_col = ls_colors,
        num_q = c("Q21", "Q22"),
        title_tab = wgm_questions,
        plt_height = 130
      )
    })

    # ---
    vac_country_questions_1 <- reactive({
      req(vac_country())

      prep_country_questions(
        country = vac_country(),
        src_data = wgm_responses_country,
        num_q = c("Q23")
      )
    })

    output$vac_country_barplot_1 <- renderPlotly({
      plot_country_questions(
        plt_data = vac_country_questions_1(),
        ls_col = ls_colors,
        num_q = c("Q23"),
        title_tab = wgm_questions,
        plt_height = 140
      )
    })

    vac_country_questions_2 <- reactive({
      req(vac_country())

      prep_country_questions(
        country = vac_country(),
        src_data = wgm_responses_country,
        num_q = c("Q24", "Q25", "Q26")
      )
    })

    output$vac_country_barplot_2 <- renderPlotly({
      plot_country_questions(
        plt_data = vac_country_questions_2(),
        ls_col = ls_colors,
        num_q = c("Q24", "Q25", "Q26"),
        title_tab = wgm_questions,
        plt_height = 120
      )
    })

    # ---
    Q25det_country_detail <- reactive({
      req(vac_country())

      prep_country_detail(
        country = vac_country(),
        src_data = detail_responses_country[["Q25"]]
      )
    })

    output$Q25det_country_barplot <- renderPlotly({
      plot_country_detail(
        plt_data = Q25det_country_detail(),
        ls_col = ls_colors[["Q25"]],
        plt_title = "How much do you agree that vaccines are safe?"
      )
    })

    #-
  })
}
