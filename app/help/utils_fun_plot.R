map_vaccines <- function(plt_data,
                         world_country = world_country) {
  
  checkmate::assert_data_frame(plt_data)
  checkmate::assert_true(
    all(c("Entity", "Vaccinated") %in% colnames(plt_data)))
  checkmate::assert_class(world_country, "SpatialPolygonsDataFrame")
  
  pal_leg <- leaflet::colorNumeric("YlOrRd", domain = plt_data$Vaccinated)
  fun_pal <- function(country) {
    ls <- leaflet::colorNumeric(
      "YlOrRd", 
      domain = plt_data$Vaccinated)(plt_data$Vaccinated) |> 
      setNames(plt_data$Entity)
    
    unname(ls[country])
  }
  
  fun_lbl <- function(country) {
    ls <- sprintf(
      "<strong>Country: %s</strong><br/>One-year-olds immunized: %g%%",
      plt_data$Entity, plt_data$Vaccinated) |>
      setNames(plt_data$Entity)
    
    unname(ls[country]) |> lapply(htmltools::HTML)
  }
  
  map <- leaflet::leaflet(world_country) |> leaflet::addTiles() |> 
    leaflet::addProviderTiles(provider = "Stamen.Toner",
                              options = leaflet::providerTileOptions(opacity = 0.4)) |>
    leaflet::setView(lat = 10, lng = 0, zoom = 2) |>
    leaflet::addPolygons(
      fillColor = ~fun_pal(world_country$name),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
        weight = 2,
        color = "black",
        fillOpacity = 0.5,
        bringToFront = TRUE),
      label = ~fun_lbl(world_country$name),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) |>
    leaflet::addLegend(
      pal = pal_leg, 
      values = ~plt_data$Vaccinated, 
      opacity = 1,  
      title = "%", 
      position = "bottomleft")
  
  map
}


map_trust <- function(plt_data,
                      world_country = world_country,
                      ls_col) {

  checkmate::assert_data_frame(plt_data)
  checkmate::assert_true(all(c("Entity", "Response") %in% colnames(plt_data)))
  checkmate::assert_class(world_country, "SpatialPolygonsDataFrame")
  checkmate::assert_character(ls_col, pattern = "^#[[:xdigit:]]{6}$")
 
  plt_data$Response <- factor(plt_data$Response, levels = names(ls_col))
  
  pal_leg <- leaflet::colorFactor(ls_col, levels = names(ls_col))

  fun_pal <- function(country) {
    ls <- leaflet::colorFactor(ls_col,
                               domain = plt_data$Response)(plt_data$Response) |> 
      setNames(plt_data$Entity)
    
    unname(ls[country])
  }
  
  fun_opa <- function(country) {
    ls <- plt_data$Result |> setNames(plt_data$Entity)
    
    unname(ls[country])
  }
  
  fun_lbl <- function(country) {
    ls <- plt_data$Label |> setNames(plt_data$Entity)
    
    unname(ls[country]) |> lapply(htmltools::HTML)
  }
  
  map <- leaflet::leaflet(world_country) |> leaflet::addTiles() |> 
    leaflet::addProviderTiles(provider = "Stamen.Toner",
                              options = leaflet::providerTileOptions(opacity = 0.4)) |>
    leaflet::setView(lat = 10, lng = 0, zoom = 2) |>
    leaflet::addPolygons(
      fillColor = ~fun_pal(world_country$name),
      weight = 1,
      opacity = ~fun_opa(world_country$name),
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
        weight = 2,
        color = "black",
        fillOpacity = 0.5,
        bringToFront = TRUE),
      label = ~fun_lbl(world_country$name),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) |>
    leaflet::addLegend(
      pal = pal_leg,
      values = names(ls_col),
      title = "Response", 
      opacity = 1,
      position = "bottomleft")
  
  map
}

plot_country_vaccines <- function(plt_data,
                                  ls_col = NULL) {
  
  checkmate::assert_data_frame(plt_data)
  checkmate::assert_true(
    all(c("Entity", "Vaccine", "Vaccinated", "Year") %in% colnames(plt_data)))
  checkmate::assert_character(ls_col, pattern = "^#[[:xdigit:]]{6}$", null.ok = TRUE)
  
  plt_data |> plotly::plot_ly(
    x = ~Year, y = ~Vaccinated, color = ~Vaccine,
    colors = ls_col,
    line = list(width = 3),
    type = "scatter", mode = "lines",
    hoverinfo = "text",
    hovertext = ~paste0(
      "<b>Vaccine</b>: ", Vaccine, "<br>",
      "<b>Year</b>: ", Year, "<br>",
      "<b>Share of one-year-olds vaccinated</b>: ", Vaccinated, "%")) |>
    plotly::layout(
      yaxis = list(ticksuffix = "%"),
      xaxis = list(showspikes = TRUE, spikemode = "across", spikethickness = 1, 
                   spikecolor = "#444", spikedash = "solid"),
      legend = list(orientation = "h", bgcolor = "rgba(0, 0, 0, 0)",
           xanchor = "center", x = 0.5, y = -0.2)) |>
    plotly::config(displayModeBar = FALSE)
  
}


plot_country_row <- function(plt_data,
                             var_y,
                             ls_col = NULL,
                             plt_title = NULL,
                             with_legend = TRUE,
                             plt_height = NULL) {
  
  checkmate::assert_data_frame(plt_data)
  checkmate::assert_true(
    all(c(var_y, "Response", "Result") %in% colnames(plt_data)))
  checkmate::assert_character(ls_col, pattern = "^#[[:xdigit:]]{6}$", null.ok = TRUE)
  checkmate::assert_string(plt_title, null.ok = TRUE)
  checkmate::assert_flag(with_legend)
  checkmate::assert_numeric(plt_height, null.ok = TRUE)
  
  plt <- plt_data |> 
    plotly::plot_ly(
      y = ~plt_data[[var_y]], x = ~Result, color = ~Response,
      colors = ls_col, height = plt_height, 
      marker = list(line = list(color = "black", width = 0.5)),
      type = "bar", legendgroup = ~Response, showlegend = with_legend,
      hoverinfo = "text",
      hovertext = ~paste0(
        "<b>", Response, "</b>: ", Result, "%")
    ) |>
    plotly::layout(
      xaxis = list(title = "", ticksuffix = "%"),
      yaxis = list(title = ""),
      legend = list(orientation = "h", bgcolor = "rgba(0, 0, 0, 0)",
                    y = -0.2),
      barmode = "stack", bargap = 0.5) |>
    plotly::config(displayModeBar = FALSE)
  
  if (!is.null(plt_title)) {
    plt <- plt |>
      plotly::layout(
        annotations = list(
          yanchor = "bottom", xanchor = "left", align = "left",
          yref = "paper", xref = "paper", y = 1.02, x = 0.01, 
          showarrow = FALSE, font = list(size = 14),
          text = paste0("<b>", plt_title, "</b>")),
        margin = list(t = 50))
  }

  plt
}


plot_country_questions <- function(plt_data,
                                   num_q,
                                   ls_col = NULL,
                                   title_tab = NULL,
                                   plt_height = NULL) {
  
  checkmate::assert_list(plt_data)
  checkmate::assert_true(all(unlist(num_q) %in% names(plt_data)))
  checkmate::assert_true(
    all(c("Entity", "Response", "Result") %in% colnames(dplyr::bind_rows(plt_data))))
  checkmate::assert_character(
    unlist(ls_col), pattern = "^#[[:xdigit:]]{6}$", null.ok = TRUE)
  checkmate::assert_data_frame(title_tab, null.ok = TRUE)
  checkmate::assert_numeric(plt_height, null.ok = TRUE)

  ls_plt <- lapply(num_q, 
                   function(q) plot_country_row(
                     plt_data[[q]], 
                     var_y = "Entity",
                     ls_col = ls_col[[q]],
                     plt_title = title_tab[title_tab$value == q,]$label,
                     with_legend = FALSE,
                     plt_height = length(num_q) * plt_height) 
  )
  ls_plt[[1]]$"x"$"attrs"[[1]]$"showlegend" <- TRUE

  sub_plt <- plotly::subplot(ls_plt, margin = 0.04,
                             nrows = length(ls_plt), shareX = TRUE)
  
  sub_plt
}

plot_country_detail <- function(plt_data,
                                ls_col = NULL,
                                plt_title = NULL) {
  
  checkmate::assert_data_frame(plt_data)
  checkmate::assert_true(
    all(c("Group", "Response", "Result") %in% colnames(plt_data)))
  checkmate::assert_string(plt_title, null.ok = TRUE)
  
  if (is.null(ls_col)) ls_col <- c(
    RColorBrewer::brewer.pal(n = length(unique(plt_data$Response)) - 1, "RdYlBu"), "#BABABA")
  
  # plot by group 
  plt_national <- plt_data |> 
    dplyr::filter(Group == "National Total") |>
    plot_country_row(ls_col = ls_col,
                     var_y = "Group")
  
  plt_gender <- plt_data |> 
    dplyr::filter(Group %in% c("Female", "Male")) |>
    plot_country_row(ls_col = ls_col,
                     var_y = "Group",
                     with_legend = FALSE)
  plt_age_group <- plt_data |> 
    dplyr::filter(Group %in% c("15-29", "30-49", "50-99")) |>
    plot_country_row(ls_col = ls_col,
                     var_y = "Group",
                     with_legend = FALSE)
  
  
  plt_sub <- plotly::subplot(plt_gender, plt_national, plt_age_group,
                             nrows = 3, shareX = TRUE)
  
  if (!is.null(plt_title)) {
    plt_sub <- plt_sub |>
      plotly::layout(
        annotations = list(
          yanchor = "bottom", xanchor = "left", align = "left",
          yref = "paper", xref = "paper", y = 1.05, x = 0, 
          showarrow = FALSE, font = list(size = 14),
          text =  paste0("<b>", plt_title, "</b>")),
        margin = list(t = 50),
        xaxis = list(showspikes = TRUE, spikemode = "across", spikethickness = 1, 
                     spikecolor = "#444", spikedash = "solid"))
  }

  plt_sub
}
