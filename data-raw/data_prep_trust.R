# 00 Source ----
# data about trust to science from Wellcome Global Monitor 2018
# https://wellcome.ac.uk/reports/wellcome-global-monitor/2018
# questionnaire https://cms.wellcome.org/sites/default/files/wgm2018-questionnaire.pdf

src_path <- "./data-raw/"
final_path <- "./data/"

wgm2018_1 <- readxl::read_excel(paste0(src_path, "wgm2018-country-level-data-1806.xlsx"))
wgm2018_2 <- readxl::read_excel(paste0(src_path, "wgm2018-country-level-data-1806.xlsx"),
                                sheet = "Question breakdown data")
# 01 Trust ----
# We need only detailed answer for Q25 (Graph 4) and Wellcome Trust in Science Index (Graph 2)
wgm_stat <- wgm2018_1 |> dplyr::filter(Visual %in% c("Graph 4", "Graph 2")) |>
  dplyr::rename(Entity = `Country Name`, Response = `Response Type`, Result = `Result %`) |>
  dplyr::mutate(Result = as.numeric(Result))

# 01_01 country name correction ----
# for name used in map
# USA -> United States of America
# UAE -> United Arab Emirates
# UK -> United Kingdom
# Bosnia Herzegovina -> Bosnia and Herzegovina
# Congo, Rep. -> Republic of the Congo
# Democratic Republic of Congo -> Democratic Republic of the Congo
# Tanzania-> United Republic of Tanzania
# Serbia -> Republic of Serbia
# The Gambia -> Gambia


old_name <- c("USA", "UK", "UAE", 
              "Bosnia Herzegovina", "Congo, Rep.", "Tanzania",
              "Serbia", "The Gambia")
new_name <- c("United States of America", "United Kingdom", "United Arab Emirates", 
              "Bosnia and Herzegovina", "Republic of the Congo", "United Republic of Tanzania",
              "Republic of Serbia", "Gambia")

for (i in seq_along(old_name)) {
  wgm_stat$Entity <- ifelse(wgm_stat$Entity == old_name[i], new_name[i], wgm_stat$Entity)
}

# 01_02 Filter data ----
# We need only detailed answer for Q25 (Graph 4)
Q25_detail <- wgm_stat |> 
  dplyr::filter(Visual == "Graph 4") |> 
  dplyr::select(-c("Visual", "Question/Statistic")) |>
  tidyr::separate(Response, c("Group", "Response"), sep = ": ") |> 
  dplyr::mutate(Response = ifelse(
    Response == "Don't know/Refused", "Don't know/refused", Response)) |>
  dplyr::mutate(Response = factor(
    Response, 
    levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree",
               "Somewhat disagree", "Strongly disagree", "Don't know/refused")))

# and Wellcome Trust in Science Index (Graph 2)
WTScI_detail <- wgm_stat |> 
  dplyr::filter(Visual == "Graph 2") |> 
  dplyr::select(-c("Visual", "Question/Statistic")) |>
  tidyr::separate(Response, c("Group", "Response"), sep = ": ") |> 
  dplyr::mutate(Response = ifelse(
    Response == "Don't know/Refused", "Don't know/refused", Response)) |>
  dplyr::mutate(Response = factor(
    Response,
    levels = c("Low trust", "Medium trust", "High trust", "DK/Refused"),
    labels = c("Low trust", "Medium trust", "High trust", "Don't know/refused")))

detail_responses_country <- list(
  Q25 = Q25_detail,
  WTScI = WTScI_detail)

saveRDS(detail_responses_country, file = paste0(final_path, "detail_responses_country.RDS"))

# 02 Questionnaire ----
#  We need only questions Q20 -> Q26
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

wgm_survey <- wgm2018_2 |> 
  dplyr::filter(`Question Number` %in% c("Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26")) |>
  dplyr::rename(Entity = `Country Name`, Number = `Question Number`, Result = `Response Total %`) |>
  dplyr::mutate(Question = substr(Question, 5, nchar(Question))) |>
  dplyr::mutate(Response = firstup(Response))

# 02_01 country name correction ----
for (i in seq_along(old_name)) {
  wgm_survey$Entity <- ifelse(wgm_survey$Entity == old_name[i], new_name[i], wgm_survey$Entity)
}

# 02_02 Response hierarchy ----
wgm_survey |> dplyr::filter(Number %in% c("Q24", "Q25", "Q26")) |> dplyr::pull(Response) |> unique()

old_response <- c("A lot", "Some", "Neutral", "Not much", "Not at all")
new_response <- c("Strongly agree", "Somewhat agree", "Neither agree nor disagree",
                  "Somewhat disagree", "Strongly disagree")

for (i in seq_along(old_response)) {
  wgm_survey$Response <- ifelse(wgm_survey$Number %in% c("Q24", "Q25", "Q26") &
                                  wgm_survey$Response == old_response[i], new_response[i], wgm_survey$Response)
}


wgm_questions <- wgm_survey |> dplyr::select(Number, Question) |> unique() |>
  rbind(data.frame(Number = "WTScI", Question = "Wellcome Trust in Science Index"))
wgm_responses <- wgm_survey |> dplyr::select(-Question)

readr::write_csv(wgm_questions, file = paste0(final_path, "wgm_questions.csv"))

# 02_03 data to map ----
WTScI_map <- WTScI_detail |>
  dplyr::filter(Group == "National Total") |> 
  dplyr::select(Entity, Response, Result) |>
  dplyr::group_by(Entity) |>
  dplyr::mutate(Label = paste0(Response, ": ", Result, "%")) |>
  dplyr::mutate(Label = paste(Label, collapse = "<br>")) |>
  dplyr::mutate(Label = sprintf("<strong>Country: %s</strong><br/>%s",
                                Entity, Label)) |>
  dplyr::filter(Result == max(Result)) |>
  dplyr::mutate(Number = "WTScI")

wgm_responses_map <- wgm_responses |>
  dplyr::group_by(Entity, Number) |>
  dplyr::mutate(Label = paste0(Response, ": ", Result, "%")) |>
  dplyr::mutate(Label = paste(Label, collapse = "<br>")) |>
  dplyr::mutate(Label = sprintf("<strong>Country: %s</strong><br/>%s",
                                Entity, Label)) |>
  dplyr::filter(Result == max(Result))

wgm_responses_map <- rbind(wgm_responses_map, WTScI_map[colnames(wgm_responses_map)]) |>
  dplyr::arrange(Entity, Number)

readr::write_csv(wgm_responses_map, file = paste0(final_path, "wgm_responses_map.csv"))

# 02_04 data to barplot ----
wgm_responses_country <- vector("list", length = length(wgm_questions$Number))
names(wgm_responses_country) <- wgm_questions$Number

# WTScI
ls_lvl_trust <- c(
  "Low trust", "Medium trust", "High trust", "Don't know/refused")
wgm_responses_trust <- WTScI_detail |>
  dplyr::filter(Group == "National Total") |> 
  dplyr::select(Entity, Response, Result) |>
  dplyr::mutate(Number = "WTScI") |>
  dplyr::mutate(Response = factor(Response, levels = ls_lvl_trust))

wgm_responses_country$WTScI <- wgm_responses_trust

# Q24, Q25, Q26
wgm_survey |> dplyr::filter(Number %in% c("Q24", "Q25", "Q26")) |> dplyr::pull(Response) |> unique()
ls_lvl_agree <- c(
  "Strongly agree", "Somewhat agree", "Neither agree nor disagree",
  "Somewhat disagree", "Strongly disagree", "Don't know/refused")
wgm_responses_agree <- wgm_responses |>
  dplyr::filter(Number %in% c("Q24", "Q25", "Q26")) |>
  dplyr::mutate(Response = factor(Response, levels = ls_lvl_agree))

for (i in c("Q24", "Q25", "Q26")) {
  wgm_responses_country[[i]] <- wgm_responses_agree |> dplyr::filter(Number == i)
}
 
# Q20
wgm_survey |> dplyr::filter(Number %in% c("Q20")) |> dplyr::pull(Response) |> unique()
ls_lvl_person <- c(
  "Family/friends", "Religious leader", "Doctor/nurse", "Famous person",
  "Traditional healer", "Don't know/refused", "N/A") 
wgm_responses_person  <- wgm_responses |>
  dplyr::filter(Number %in% c("Q20")) |>
  dplyr::mutate(Response = factor(Response, levels = ls_lvl_person))

wgm_responses_country$Q20 <- wgm_responses_person

# Q21, Q22
wgm_survey |> dplyr::filter(Number %in% c("Q21", "Q22")) |> dplyr::pull(Response) |> unique()
ls_lvl_advice <- c("A lot", "Some", "Not much", "Not at all", "Don't know/refused")
wgm_responses_advice <- wgm_responses |>
  dplyr::filter(Number %in% c("Q21", "Q22")) |>
  dplyr::mutate(Response = factor(Response, levels = ls_lvl_advice))

for (i in c("Q21", "Q22")) {
  wgm_responses_country[[i]] <- wgm_responses_advice |> dplyr::filter(Number == i)
}

# Q23
wgm_survey |> dplyr::filter(Number %in% c("Q23")) |> dplyr::pull(Response) |> unique()
ls_lvl_YesNo <- c("Yes", "No", "Don't know/refused")
wgm_responses_YesNo <- wgm_responses |>
  dplyr::filter(Number %in% c("Q23")) |>
  dplyr::mutate(Response = factor(Response, levels = ls_lvl_YesNo))

wgm_responses_country$Q23 <- wgm_responses_YesNo

saveRDS(wgm_responses_country, file = paste0(final_path, "wgm_responses_country.RDS"))

# 0x Other ----
# 0x_01 Colors ----
ls_col_vac <- c("MCV1" = "#4E79A7",
                "BCG" = "#F28E2B",
                "Pol3" = "#E15759",
                "HepB3" = "#76B7B2",
                "DTP3" = "#59A14F",
                "RotaC" = "#EDC948",
                "PCV3" = "#B07AA1")

# WTScI
ls_col_trust <- c(RColorBrewer::brewer.pal(n = 3, "OrRd"), "#BABABA")
names(ls_col_trust) <- ls_lvl_trust

# Q24, Q25, Q26
ls_col_agree <- c(RColorBrewer::brewer.pal(n = 5, "RdBu"), "#BABABA")
names(ls_col_agree) <- ls_lvl_agree

# Q20
ls_col_person <- c("#4E79A7", "#E15759", "#59A14F", "#EDC948", "#B07AA1", "#BABABA", "#E0E0E0")
names(ls_col_person) <- ls_lvl_person

# Q21, Q22
ls_col_advice <- ls_col_agree[-c(3)]
names(ls_col_advice) <- ls_lvl_advice

# Q23
ls_col_YesNo <- ls_col_agree[c(1, 5, 6)]
names(ls_col_YesNo) <- ls_lvl_YesNo

# all
wgm_responses_colors <- vector("list", length = length(wgm_questions$Number))
names(wgm_responses_colors) <- wgm_questions$Number
wgm_responses_colors[["Q20"]] <- ls_col_person
for (i in c("Q21", "Q22")) wgm_responses_colors[[i]] <- ls_col_advice
wgm_responses_colors[["Q23"]] <- ls_col_YesNo
for (i in c("Q24", "Q25", "Q26")) wgm_responses_colors[[i]] <- ls_col_agree
wgm_responses_colors[["WTScI"]] <- ls_col_trust

wgm_responses_colors$vac <- ls_col_vac

saveRDS(wgm_responses_colors, file = paste0(final_path, "ls_colors.RDS"))


# 0x_02 Entity Hierarchy ----
# Countries surveyed are grouped into 18 categories:
# https://wellcome.org/reports/wellcome-global-monitor/2018/chapter-1-introduction
category <- list(
  "North Africa" = c("Algeria", "Egypt", "Libya", "Morocco", "Tunisia"),
  "Eastern Africa" = c("Burundi", "Comoros", "Ethiopia", "Kenya", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Rwanda", "Tanzania", "Uganda", "Zambia", "Zimbabwe"),
  "Central Africa" = c("Cameroon", "Chad", "Republic of the Congo", "Gabon"),
  "Southern Africa" = c("Botswana", "Namibia", "South Africa", "eSwatini"),
  "Western Africa" = c("Benin", "Burkina Faso", "Ghana", "Guinea", "Ivory Coast", "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Senegal", "Sierra Leone", "The Gambia", "Togo"),
  "Central America and Mexico" = c("Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Haiti", "Honduras", "Mexico", "Nicaragua", "Panama"),
  "South America" = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela"),
  "Northern America" = c("Canada", "United States"),
  "Central Asia" = c("Armenia", "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan"),
  "East Asia" = c("China", "Japan", "Mongolia", "South Korea", "Taiwan"),
  "Southeast Asia" = c("Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam"),
  "South Asia" = c("Afghanistan", "Bangladesh", "India", "Iran", "Nepal", "Pakistan", "Sri Lanka"),
  "Middle East" = c("Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Palestine", "Saudi Arabia", "Turkey", "United Arab Emirates", "Yemen"),
  "Eastern Europe" = c("Belarus", "Bulgaria", "Czech Republic", "Hungary", "Moldova", "Poland", "Romania", "Russia", "Slovakia", "Ukraine"),
  "Northern Europe" = c("Denmark", "Estonia", "Finland", "Iceland", "Ireland", "Latvia", "Lithuania", "Norway", "Sweden", "United Kingdom"),
  "Southern Europe" = c("Albania", "Bosnia Herzegovina", "Croatia", "Cyprus", "Greece", "Italy", "Malta", "North Macedonia", "Montenegro", "Portugal", "Serbia", "Slovenia", "Spain"),
  "Western Europe" = c("Austria", "Belgium", "France", "Germany", "Luxembourg", "Netherlands", "Switzerland"),
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