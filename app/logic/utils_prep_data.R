# Box imports -----------------------------------------------------------------

box::use(
  checkmate[
    assert_string, assert_character, assert_list, assert_true, assert_subset,
    assert_data_frame,
  ],
  dplyr[filter, select, pull, arrange, mutate],
  tidyr[pivot_longer],
)

box::use(
  app/logic/data_import[wgm_questions],
)

# -----------------------------------------------------------------------------

#' @export
prep_vac_data <- function(vac, year, src_data) {
  assert_string(vac)
  assert_subset(vac, choices = colnames(src_data))
  assert_string(year, n.chars = 4)
  assert_data_frame(src_data)

  src_data |>
    filter(Year == as.numeric(year)) |>
    select(c("Entity", "Vaccinated" = all_of(vac)))
}

#' @export
prep_vac_title <- function(vac, year) {
  assert_string(vac)
  assert_subset(
    vac,
    choices = c("MCV1", "BCG", "Pol3", "HepB3", "DTP3", "RotaC", "PCV3")
  )
  assert_string(year, n.chars = 4)

  disease <- switch(vac,
    "MCV1" = "measles",
    "BCG" = "tuberculosis",
    "Pol3" = "polio",
    "HepB3" = "hepatitis B",
    "DTP3" = "diphtheria, tetanus, and pertussis",
    "RotaC" = "rotavirus",
    "PCV3" = "pneumococcal"
  )

  plt_title <- paste0(
    "Share of one-year-olds vaccinated against ", disease,
    "(", vac, "), ", year
  )
}

#' @export
prep_trust_data <- function(q_stat, src_data) {
  assert_string(q_stat)
  assert_subset(q_stat, choices = unique(src_data$Number))
  assert_data_frame(src_data)

  src_data |>
    filter(Number == q_stat)
}

#' @export
prep_trust_title <- function(q_stat, src_stat) {
  assert_string(q_stat)
  assert_subset(
    q_stat,
    c("Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "WTScI")
  )

  if (q_stat == "WTScI") {
    plt_title <- "Most often value of The Wellcome Trust in Science Index"
  } else {
    q <- wgm_questions |>
      filter(value == q_stat) |>
      pull(label)

    plt_title <- paste0("Most often answer for: ", q)
  }
}

#' @export
prep_country_data <- function(country, src_data) {
  assert_string(country)
  assert_subset(country, choices = unique(src_data$Entity))
  assert_data_frame(src_data)

  src_data |>
    filter(Entity == country) |>
    pivot_longer(
      cols = c("MCV1", "BCG", "Pol3", "HepB3", "DTP3", "RotaC", "PCV3"),
      names_to = "Vaccine",
      values_to = "Vaccinated"
    ) |>
    mutate(Vaccine = factor(
      Vaccine,
      levels = c("MCV1", "BCG", "Pol3", "HepB3", "DTP3", "RotaC", "PCV3")
    )) |>
    arrange(Entity, Vaccine, Year)
}

#' @export
prep_country_title <- function(country) {
  assert_string(country)

  plt_title <- paste0("Vaccines for ", country)
}

#' @export
prep_country_questions <- function(country, src_data, num_q) {
  assert_string(country)
  assert_character(num_q)
  assert_list(src_data)
  assert_true(all(num_q %in% names(src_data)))
  assert_subset(num_q, choices = names(src_data))

  ls_fin <- list()
  for (i in num_q) {
    ls_fin[[i]] <- src_data[[i]] |> filter(Entity == country)
  }

  ls_fin
}

#' @export
prep_country_detail <- function(country, src_data) {
  assert_string(country)
  assert_subset(country, choices = unique(src_data$Entity))
  assert_data_frame(src_data)

  src_data |>
    filter(Entity == country) |>
    select(c("Group", "Response", "Result"))
}
