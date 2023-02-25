# Box imports -----------------------------------------------------------------

box::use(
  shiny[
    NS, moduleServer, tagList, reactive, reactiveVal, observeEvent
  ],
  shiny.blueprint[
    Tree, setInput, renderReact, reactOutput
  ],
  purrr[
    list_modify, map
  ],
)

# -----------------------------------------------------------------------------

ls_grp_country <- list(
  "North Africa" = c("Algeria", "Egypt", "Libya", "Morocco", "Tunisia"),
  "Eastern Africa" = c(
    "Burundi", "Comoros", "Ethiopia", "Kenya", "Madagascar", "Malawi", "Mauritius",
    "Mozambique", "Rwanda", "United Republic of Tanzania", "Uganda", "Zambia", "Zimbabwe"),
  "Central Africa" = c("Cameroon", "Chad", "Republic of the Congo", "Gabon"),
  "Southern Africa" = c("Botswana", "Namibia", "South Africa"),
  "Western Africa" = c(
    "Benin", "Burkina Faso", "Ghana", "Guinea", "Liberia", "Mali", "Mauritania", 
    "Niger", "Nigeria", "Senegal", "Sierra Leone", "Gambia", "Togo"),
  "Central America and Mexico" = c(
    "Costa Rica", "Dominican Republic", "El Salvador", 
    "Guatemala", "Haiti", "Honduras", "Mexico", "Nicaragua", "Panama"),
  "South America" = c(
    "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", 
    "Peru", "Uruguay", "Venezuela"),
  "Northern America" = c("Canada", "United States of America"),
  "Central Asia" = c(
    "Armenia", "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Tajikistan", 
    "Turkmenistan", "Uzbekistan"),
  "East Asia" = c("China", "Japan", "Mongolia", "South Korea", "Taiwan"),
  "Southeast Asia" = c(
    "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", 
    "Singapore", "Thailand", "Vietnam"),
  "South Asia" = c(
    "Afghanistan", "Bangladesh", "India", "Iran", "Nepal", "Pakistan", "Sri Lanka"),
  "Middle East" = c(
    "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Saudi Arabia", "Turkey", 
    "United Arab Emirates", "Yemen"),
  "Eastern Europe" = c(
    "Belarus", "Bulgaria", "Czech Republic", "Hungary", "Moldova", "Poland", 
    "Romania", "Russia", "Slovakia", "Ukraine"),
  "Northern Europe" = c(
    "Denmark", "Estonia", "Finland", "Iceland", "Ireland", "Latvia", "Lithuania",
    "Norway", "Sweden", "United Kingdom"),
  "Southern Europe" = c(
    "Albania", "Bosnia and Herzegovina", "Croatia", "Cyprus", "Greece", "Italy", 
    "Malta", "Macedonia", "Montenegro", "Portugal", "Republic of Serbia", "Slovenia", "Spain"),
  "Western Europe" = c(
    "Austria", "Belgium", "France", "Germany", "Luxembourg", "Netherlands", "Switzerland"),
  "Australia and New Zealand" = c("Australia", "New Zealand")
)

continent <- list(
  "Africa" = c("North Africa", "Eastern Africa", "Central Africa", "Southern Africa", "Western Africa"),           
  "South America" = c("Central America and Mexico", "South America"),
  "Northern America" = c("Northern America"),
  "Asia" = c("Central Asia", "East Asia", "Southeast Asia", "South Asia", "Middle East"),              
  "Europe" = c("Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe"),
  "Australia" = c("Australia and New Zealand" )
)

fun_grp_country <- function(lbl) {
  ls_grp <- ls_grp_country[[lbl]]
  lapply(
    seq_along(ls_grp), function(i) list(
      id = ls_grp[i], label = ls_grp[i])
  )
}

treeList <- list(
  list(
    id = "0", label = "World",
    childNodes = 
      lapply(seq_along(continent), function(i) list(
        id = as.character(i), label = names(continent)[i],
        childNodes = 
          lapply(seq_along(continent[[i]]), function(j) list(
            id = paste0(i, j), label = continent[[i]][j],
            childNodes = fun_grp_country(continent[[i]][j])
          ))
      ))
  )
)

# ---

modifyTree <- function(tree, ids, props) {
  if (!is.null(tree)) map(tree, function(node) {
    if (node$id %in% ids) {
      node <- list_modify(node, !!!props)
    }
    node$childNodes <- modifyTree(node$childNodes, ids, props)
    node
  })
}

#' @export
tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactOutput(ns("tree"))
  )
}

#' @export
tree_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    treeReactive <- reactiveVal(treeList)
    observeEvent(input$expand, {
      treeReactive(
        modifyTree(treeReactive(), ids = input$expand$id, props = list(isExpanded = TRUE))
      )
    })
    observeEvent(input$collapse, {
      treeReactive(
        modifyTree(treeReactive(), ids = input$collapse$id, props = list(isExpanded = FALSE))
      )
    })

    output$tree <- renderReact({
      Tree(
        contents = treeReactive(),
        onNodeExpand = setInput(ns("expand")),
        onNodeCollapse = setInput(ns("collapse")),
        onNodeClick = setInput(ns("click"))
      )
    })
    
    selected_country <- reactive({
      if (input$click$id %in% unlist(ls_grp_country, use.names = FALSE) && 
          length(input$click$id)) {
        input$click$id
      } else if (length(input$click$id)) {
        # "Poland" # development trick to select Poland when clicking any node
        NULL
      }
    })
    
    # return
    return(selected_country)
  })
}

# shiny::shinyApp(tree_ui("app"), function(input, output) tree_server("app"))
