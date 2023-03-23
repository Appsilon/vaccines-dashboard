# Box imports -----------------------------------------------------------------

box::use(
  shiny[
    NS, moduleServer, reactive, reactiveVal, observeEvent,
    tagList, fluidRow, column, div, br, textOutput, renderText, req, hr,
  ],
  leaflet[renderLeaflet, leafletOutput],
  shiny.blueprint[
    Card, Text, H6, HTMLSelect.shinyInput, Button.shinyInput, Drawer,
    triggerEvent, renderReact, reactOutput,
  ],
  dplyr[select, pull],
  tidyr[drop_na],
)

# Box imports: application ----------------------------------------------------

box::use(
  app/logic/utils_drawer[prep_vac_txt, drawer_questions],
  app/logic/utils_menu[menu_ui, menu_server],
  app/logic/utils_fun_plot[map_trust, map_vaccines],
  app/logic/data_import[
    ls_vac, tab_vaccines, world_country, wgm_responses_map, ls_colors,
  ],
  app/logic/utils_prep_data[
    prep_trust_title, prep_vac_data, prep_trust_data, prep_vac_title,
  ],
)

# -----------------------------------------------------------------------------
#
#
# UI

main_world_ui <- function(id) {
  ns <- NS(id)

  tagList(
    reactOutput(ns("vac_drawer")),
    reactOutput(ns("trust_drawer")),
    fluidRow(
      column(
        12,
        Card(
          elevation = 2, interactive = TRUE,
          fluidRow(
            column(
              6,
              Text(
                "Today vaccines protect millions of people around the world
                   from infectious diseases."
              ),
              Text(
                "Immunization among 1-year-olds is an essential component for
                 reducing under-five mortality.
                 It is also a good indicator of health system performance."
              )
            ),
            column(
              6,
              Text(
                "The London-based research charity The Wellcome Trust published their
                  Wellcome Global Monitor in 2019 on attitudes to science and major
                  health challenges. It is the world’s largest study of its kind,
                  surveying over 140,000 people from over 140 countries."
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        6,
        Card(
          elevation = 2, interactive = TRUE,
          # inputs
          div(
            style = "display: flex; align-items: center; height: 40px;",
            H6(
              "Vacines type",
              style = "margin: 0 10px 0 0;"
            ),
            HTMLSelect.shinyInput(
              inputId = ns("vac_type"),
              options = ls_vac,
              value = "BCG",
              minimal = TRUE
            ),
            Button.shinyInput(
              inputId = ns("vac_drawer_btn"),
              icon = "drawer-right",
              intent = "primary",
              style = "margin: 0 30px 0 10px;"
            ),
            H6(
              "Year",
              style = "margin: 0 10px 0 0;"
            ),
            reactOutput(ns("vac_year_dropdown"))
          ),
          br(),
          # map
          div(
            style = "min-height: 40px;",
            textOutput(ns("vac_map_title"))
          ),
          br(),
          leaflet::leafletOutput(ns("vac_map"))
        )
      ),
      column(
        6,
        Card(
          elevation = 2, interactive = TRUE,
          # input
          div(
            style = "display: flex; align-items: center;",
            H6(
              "Statistic",
              style = "margin: 0 10px 0 0;"
            ),
            # HTMLSelect.shinyInput(
            #   inputId = ns("wgm_stat"),
            #   options = ls_stat,
            #   value = "WTScI",
            #   minimal = TRUE
            # )
            menu_ui(ns("wgm_stat")),
            Button.shinyInput(
              inputId = ns("trust_drawer_btn"),
              icon = "drawer-right",
              intent = "primary",
              style = "margin: 0 30px 0 10px;"
            )
          ),
          br(),
          # map
          div(
            style = "min-height: 40px;",
            textOutput(ns("trust_map_title"))
          ),
          br(),
          leaflet::leafletOutput(ns("trust_map")),
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
#
#
# Server

main_world_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # vaccines map
    vac_type <- reactive({
      input$vac_type
    })

    ls_year <- reactive({
      tab_vaccines |>
        select(c("Entity", "Year", vac_type())) |>
        drop_na(vac_type()) |>
        pull(Year) |>
        unique() |>
        sort() |>
        as.character()
    })

    output$vac_year_dropdown <- renderReact({
      HTMLSelect.shinyInput(
        inputId = ns("vac_year"),
        options = ls_year(),
        value = "2018",
        minimal = TRUE
      )
    })

    vac_year <- reactive({
      # ... because value = "2018" in output$vac_year_dropdown does not work.
      if (input$vac_year %in% ls_year() && length(input$vac_year) > 0) {
        input$vac_year
      } else {
        as.character(min(as.numeric(ls_year())))
      }
    }) # |> debounce(1000)

    vac_data_title <- reactive({
      req(vac_type())
      req(vac_year())

      prep_vac_title(vac = vac_type(), year = vac_year())
    })

    vac_data <- reactive({
      req(vac_type())
      req(vac_year())

      prep_vac_data(vac = vac_type(), year = vac_year(), src_data = tab_vaccines)
    })

    output$vac_map_title <- renderText({
      vac_data_title()
    })

    output$vac_map <- leaflet::renderLeaflet({
      map_vaccines(vac_data(), world_country = world_country)
    })

    # drawer vac_drawer
    isOpen_vac_drawer <- reactiveVal(FALSE)
    observeEvent(input$vac_drawer_btn, isOpen_vac_drawer(!isOpen_vac_drawer()))
    observeEvent(input$dismiss_vac_drawer, isOpen_vac_drawer(FALSE))
    drawer_title <- reactive({
      vac_type()
    })

    drawer_body <- reactive({
      prep_vac_txt(vac_type())
    })

    output$vac_drawer <- renderReact({
      Drawer(
        isOpen = isOpen_vac_drawer(),
        onClose = triggerEvent(ns("dismiss_vac_drawer")),
        usePortal = FALSE,
        title = drawer_title(),
        icon = "info-sign",
        canEscapeKeyClose = TRUE,
        canOutsideClickClose = TRUE,
        div(
          class = "bp4-dialog-body",
          drawer_body()
        )
      )
    })

    # drawer trust_drawer
    isOpen_trust_drawer <- reactiveVal(FALSE)
    observeEvent(input$trust_drawer_btn, isOpen_trust_drawer(!isOpen_trust_drawer()))
    observeEvent(input$dismiss_trust_drawer, isOpen_trust_drawer(FALSE))
    drawer_title <- reactive({
      vac_type()
    })

    output$trust_drawer <- renderReact({
      Drawer(
        isOpen = isOpen_trust_drawer(),
        onClose = triggerEvent(ns("dismiss_trust_drawer")),
        usePortal = FALSE,
        title = "Wellcome Global Monitor 2018",
        icon = "info-sign",
        canEscapeKeyClose = TRUE,
        canOutsideClickClose = TRUE,
        div(
          class = "bp4-dialog-body",
          Text(
            "The Wellcome Global Monitor is the world’s largest study into
            how people around the world think and feel about science and
            major health challenges. "
          ),
          br(),
          Text(
            "The questionnaire used in this study can be found here
            'https://cms.wellcome.org/sites/default/files/wgm2018-questionnaire.pdf'"
          ),
          hr(),
          drawer_questions
        )
      )
    })

    # trust map
    trust_stat <- menu_server("wgm_stat")

    trust_data <- reactive({
      req(trust_stat())

      prep_trust_data(q_stat = trust_stat(), src_data = wgm_responses_map)
    })

    trust_data_title <- reactive({
      req(trust_stat())

      prep_trust_title(q_stat = trust_stat(), src_stat = wgm_questions)
    })

    output$trust_map_title <- renderText({
      trust_data_title()
    })

    output$trust_map <- leaflet::renderLeaflet({
      map_trust(trust_data(),
        world_country = world_country,
        ls_col = ls_colors[[trust_stat()]]
      )
    })
  })
}

# if (interactive()) {
#   shiny::shinyApp(
#     main_world_ui("app"),
#     function(input, output) main_world_server("app")
#   )
# }
