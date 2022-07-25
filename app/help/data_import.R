# data for vaccines ----
tab_vaccines <- readr::read_csv("data/tab_vaccines.csv", show_col_types = FALSE)
ls_vac <- colnames(tab_vaccines)[!colnames(tab_vaccines) %in% c("Entity", "Code", "Year")]

# data for trust ----
wgm_questions <- readr::read_csv("data/wgm_questions.csv", show_col_types = FALSE) |>
  dplyr::select(value = Number, label = Question)
ls_stat <- do.call("mapply", c(list, wgm_questions, SIMPLIFY = FALSE, USE.NAMES = FALSE))

wgm_responses_map <- readr::read_csv("data/wgm_responses_map.csv", show_col_types = FALSE)

# country ----
wgm_responses_country <- readRDS("./data/wgm_responses_country.RDS")

# detail ----
detail_responses_country <- readRDS("./data/detail_responses_country.RDS")

# data for maps ----
world_country <- geojsonio::geojson_read("./data/countries.geo.json", what = "sp")

# plots ----
ls_colors <- readRDS("./data/ls_colors.RDS")

# ls_country
# ls_country_vac <- unique(tab_vaccines$Entity)
# ls_country_res <- unique(wgm_responses_map$Entity)
# ls_country <- sort(intersect(ls_country_vac, ls_country_res))
