# 00 Source ----
# Data was uploaded on 5th July 2022 
# - data about vaccinations - from site Our World In Data,
# section "Vaccination - Coverage, Impact, and Potential"
# https://ourworldindata.org/vaccination#coverage-impact-and-potential
# -- for Deaths caused by vaccine-preventable diseases
# https://ourworldindata.org/grapher/Deaths-caused-by-vaccine-preventable-diseases-over-time
# -- for Preventable child deaths from rotavirus vaccination
# https://ourworldindata.org/grapher/avertable-deaths-from-rotavirus-with-full-vaccine-coverage
# -- for Child deaths from diseases caused by invasive pneumococcus
# https://ourworldindata.org/grapher/child-deaths-from-streptococcus-by-disease
# -- for Diphtheria, Tetanus, and Pertussis vaccine (DTP3)
# https://ourworldindata.org/grapher/share-of-children-immunized-dtp3
# -- for Measles (MCV1)
# https://ourworldindata.org/grapher/share-of-children-vaccinated-against-measles
# -- for Rotavirus (RotaC)
# https://ourworldindata.org/grapher/share-of-one-year-olds-who-received-the-rotavirus-vaccine
# -- for Pneumococcal (PCV3)
# https://ourworldindata.org/grapher/share-of-one-year-olds-who-received-the-final-dose-of-pneumococcal-vaccine
# -- for Hepatitis B (HepB3)
# https://ourworldindata.org/grapher/immunization-hepb3-of-one-year-old-children
# -- for Polio (Pol3)
# https://ourworldindata.org/grapher/polio-vaccine-coverage-of-one-year-olds
# -- for Tuberculosis (BCG)
# https://ourworldindata.org/grapher/bcg-immunization-coverage-for-tb-among-1-year-olds

src_path <- "./data-raw/"
final_path <- "./data/"


# 01 Vaccines ----
measles_1 <- readr::read_csv(paste0(src_path, "share-of-children-vaccinated-against-measles.csv")) |>
  dplyr::rename(MCV1 = `MCV1 (% of one-year-olds immunized)`)
bcg <- readr::read_csv(paste0(src_path, "bcg-immunization-coverage-for-tb-among-1-year-olds.csv")) |>
  dplyr::rename(BCG = `BCG (% of one-year-olds immunized)`)
polio <- readr::read_csv(paste0(src_path, "polio-vaccine-coverage-of-one-year-olds.csv")) |>
  dplyr::rename(Pol3 = `Pol3 (% of one-year-olds immunized)`)
hepb3 <- readr::read_csv(paste0(src_path, "immunization-hepb3-of-one-year-old-children.csv")) |>
  dplyr::rename(HepB3 = `HepB3 (% of one-year-olds immunized)`)
dtp3 <- readr::read_csv(paste0(src_path, "share-of-children-immunized-dtp3.csv")) |>
  dplyr::rename(DTP3 = `DTP3 (% of one-year-olds immunized)`)
rotavirus <- readr::read_csv(paste0(src_path, "share-of-one-year-olds-who-received-the-rotavirus-vaccine.csv")) |>
  dplyr::rename(RotaC = `Indicator:Rotavirus vaccines completed dose (RotaC) immunization coverage among 1-year-olds (%)`)
pneumococcal <- readr::read_csv(paste0(src_path, "share-of-one-year-olds-who-received-the-final-dose-of-pneumococcal-vaccine.csv")) |>
  dplyr::rename(PCV3 = `Indicator:Pneumococcal conjugate vaccines (PCV3) immunization coverage among 1-year-olds (%)`)

# 01_01 main data ----
tab_vaccines <- dplyr::left_join(measles_1, bcg, by = c("Entity", "Code", "Year"))
tab_vaccines <- dplyr::left_join(tab_vaccines, polio, by = c("Entity", "Code", "Year"))
tab_vaccines <- dplyr::left_join(tab_vaccines, hepb3, by = c("Entity", "Code", "Year"))
tab_vaccines <- dplyr::left_join(tab_vaccines, dtp3, by = c("Entity", "Code", "Year"))
tab_vaccines <- dplyr::left_join(tab_vaccines, rotavirus, by = c("Entity", "Code", "Year"))
tab_vaccines <- dplyr::left_join(tab_vaccines, pneumococcal, by = c("Entity", "Code", "Year"))

dim(tab_vaccines)
names(tab_vaccines)



# 01_02 country name correction ----
# for name used in map
# United States -> United States of America
# Czechia -> Czech Republic
# Bolivia (Plurinational State of) -> Bolivia
# Tanzania -> United Republic of Tanzania
# Congo -> Republic of the Congo
# Democratic Republic of Congo -> Democratic Republic of the Congo
# North Macedonia -> Macedonia

old_name <- c("United States", "Czechia", "Bolivia (Plurinational State of)", "Tanzania", 
              "Congo", "Democratic Republic of Congo", "North Macedonia")
new_name <- c("United States of America", "Czech Republic", "Bolivia", "United Republic of Tanzania", 
              "Republic of the Congo", "Democratic Republic of the Congo", "Macedonia")

for (i in seq_along(old_name)) {
  tab_vaccines$Entity <- ifelse(tab_vaccines$Entity == old_name[i], new_name[i], tab_vaccines$Entity)
}

# 01_03 save ----
tab_vaccines <- tab_vaccines |> dplyr::select(-Code)
readr::write_csv(tab_vaccines, file = paste0(final_path, "tab_vaccines.csv"))

# ls_col_vaccines <- c("MCV1" = "#4E79A7", 
#                      "BCG" = "#F28E2B", 
#                      "Pol3" = "#E15759", 
#                      "HepB3" = "#76B7B2", 
#                      "DTP3" = "#59A14F", 
#                      "RotaC" = "#EDC948", 
#                      "PCV3" = "#B07AA1")
