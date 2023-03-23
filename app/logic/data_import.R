# Box imports -----------------------------------------------------------------

box::use(
  geojsonio[geojson_read],
  readr[read_csv],
  dplyr[select],
)

# -----------------------------------------------------------------------------

#
# data for vaccines -----------------------------------------------------------

#' @export
tab_vaccines <- read_csv("data/tab_vaccines.csv", show_col_types = FALSE)

#' @export
ls_vac <- colnames(tab_vaccines)[!colnames(tab_vaccines) %in% c("Entity", "Code", "Year")]

# data for trust --------------------------------------------------------------

#' @export
wgm_questions <- read_csv("data/wgm_questions.csv", show_col_types = FALSE) |>
  select(value = Number, label = Question)

#' @export
ls_stat <- do.call(
  "mapply",
  c(list, wgm_questions, SIMPLIFY = FALSE, USE.NAMES = FALSE)
)

#' @export
wgm_responses_map <- read_csv("data/wgm_responses_map.csv", show_col_types = FALSE)

# country ---------------------------------------------------------------------
#' @export
wgm_responses_country <- readRDS("./data/wgm_responses_country.RDS")

# detail ----------------------------------------------------------------------
#' @export
detail_responses_country <- readRDS("./data/detail_responses_country.RDS")

# -----------------------------------------------------------------------------
#' data for maps
#' @export
world_country <- geojson_read("./data/countries.geo.json", what = "sp")

# plots -----------------------------------------------------------------------

#' @export
ls_colors <- readRDS("./data/ls_colors.RDS")
