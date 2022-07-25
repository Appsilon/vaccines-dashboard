# Vaccines Dashboard

This dashboard was built for purpose of testing the package `appsilon.blueprint`.
As `appsilon.blueprint` is still under development, only a limited number of 
items could be shown. 

Detailed description can be found:
Project > Appsilon Lab > appsilon.blueprint > demo

## To see app

1. `git clone` this repository
2. install the required packages from Appsilon github

```R
remotes::install_github("Appsilon/shiny.react")
remotes::install_github("Appsilon/appsilon.blueprint")
```

3. restore environment (install all required packages)
```R
renv::restore()
```

4. run app from file `app.R`

## Data

Main source of data used in app
* [Our World In Data - Vaccination](https://ourworldindata.org/vaccination)
* [Wellcome Global Monitor 2018](https://wellcome.org/reports/wellcome-global-monitor/2018)

