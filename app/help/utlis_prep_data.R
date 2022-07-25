prep_vac_data <- function(vac,
                          year,
                          src_data) {
  
  checkmate::assert_string(vac)
  checkmate::assert_subset(vac, choices = colnames(src_data))
  checkmate::assert_string(year, n.chars = 4)
  checkmate::assert_data_frame(src_data)
  
  src_data |>
    dplyr::filter(Year == as.numeric(year)) |>
    dplyr::select(c("Entity", "Vaccinated" = all_of(vac)))
  
}


prep_vac_title <- function(vac,
                           year) {
  
  checkmate::assert_string(vac)
  checkmate::assert_subset(
    vac, 
    choices = c("MCV1", "BCG", "Pol3", "HepB3", "DTP3", "RotaC", "PCV3"))
  checkmate::assert_string(year, n.chars = 4)
  
  disease <- switch(
    vac,
    "MCV1" = "measles",
    "BCG" = "tuberculosis",
    "Pol3" = "polio",
    "HepB3" = "hepatitis B",
    "DTP3" =  "diphtheria, tetanus, and pertussis",
    "RotaC" = "rotavirus",
    "PCV3" = "pneumococcal"
  )
  
  plt_title <- paste0(
    "Share of one-year-olds vaccinated against ", disease,  
    "(", vac, "), ", year)
}


prep_trust_data <- function(q_stat,
                            src_data) {
  
  checkmate::assert_string(q_stat)
  checkmate::assert_subset(q_stat, choices = unique(src_data$Number))
  checkmate::assert_data_frame(src_data)
  
  src_data |> 
    dplyr::filter(Number == q_stat)
}


prep_trust_title <- function(q_stat,
                             src_stat) {
  
  checkmate::assert_string(q_stat)
  checkmate::assert_subset(
    q_stat, 
    c("Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "WTScI"))
  
  if (q_stat == "WTScI") {
    plt_title <- "Most often value of The Wellcome Trust in Science Index"
  } else {
    q <- wgm_questions |> dplyr::filter(value == q_stat) |> dplyr::pull(label)
    
    plt_title <- paste0("Most often answer for: ", q)
  }
  
}


prep_country_data <- function(country,
                              src_data) {
  
  checkmate::assert_string(country)
  checkmate::assert_subset(country, choices = unique(src_data$Entity))
  checkmate::assert_data_frame(src_data)
  
  src_data |>
    dplyr::filter(Entity == country) |>
    tidyr::pivot_longer(
      cols = c("MCV1", "BCG", "Pol3", "HepB3", "DTP3", "RotaC", "PCV3"),
      names_to = "Vaccine",
      values_to = "Vaccinated") |>
    dplyr::mutate(Vaccine = factor(
      Vaccine,
      levels = c("MCV1", "BCG", "Pol3", "HepB3", "DTP3", "RotaC", "PCV3"))) |>
    dplyr::arrange(Entity, Vaccine, Year)
  
}

prep_country_title <- function(country) {
  
  checkmate::assert_string(country)
  
  plt_title <- paste0("Vaccines for ", country)
}


prep_country_questions <- function(country,
                                   src_data,
                                   num_q) {

  checkmate::assert_string(country)
  checkmate::assert_character(num_q)
  checkmate::assert_list(src_data)
  checkmate::assert_true(all(num_q %in% names(src_data)))
  checkmate::assert_subset(num_q, choices = names(src_data))
  
  ls_fin <- list()
  for (i in num_q) {
    ls_fin[[i]] <- src_data[[i]] |> dplyr::filter(Entity == country)
  }
  
  ls_fin
}

prep_country_detail <- function(country,
                                src_data) {
  
  checkmate::assert_string(country)
  checkmate::assert_subset(country, choices = unique(src_data$Entity))
  checkmate::assert_data_frame(src_data)
  
  src_data |>
    dplyr::filter(Entity == country) |>
    dplyr::select(c("Group", "Response", "Result"))
  
}
