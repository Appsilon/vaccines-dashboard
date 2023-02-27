# Vaccines Dashboard

This dashboard was built for purpose of testing the package `shiny.blueprint`.
As `shiny.blueprint` is still under development, only a limited number of items could be shown. 

## How to use Shiny.blueprint

Shiny.blueprint makes available Blueprint _(the react-based UI toolkit)_ for Shiny apps.

We believe that a great UI plays a huge role in the success of application projects. shiny.blueprint gives your apps.

```R
remotes::install_github("Appsilon/shiny.blueprint")
```

## Set-up environment

1. Clone or download the source files for this repository
2. Set-up the environment _(install all required packages)_ via `renv::restore()`
3. run app from file `app.R`

```R
library(shiny)
runApp()
```

## Data sources

Main source of data used in app:

* [Our World In Data - Vaccination](https://ourworldindata.org/vaccination)
* [Wellcome Global Monitor 2018](https://wellcome.org/reports/wellcome-global-monitor/2018)
* [WHO - Immunization](https://www.who.int/data/gho/data/themes/immunization)
* [UNICEF - Immunization](https://data.unicef.org/topic/child-health/immunization/)

See more information on the "Source" link on the top right corner of the demo.

## Appsilon

<img src="https://avatars0.githubusercontent.com/u/6096772" align="right" alt="" width="6%" />

Appsilon is a **Posit (formerly RStudio) Full Service Certified Partner**.<br/>
Learn more
at [appsilon.com](https://appsilon.com).

Get in touch [opensource@appsilon.com](mailto:opensource@appsilon.com)

Check our [Open Source tools](https://shiny.tools).

<a href = "https://appsilon.com/careers/" target="_blank"><img src="http://d2v95fjda94ghc.cloudfront.net/hiring.png" alt="We are hiring!"/></a>
