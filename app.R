# Minimal app.R for Golem on Posit Cloud

required_packages <- c(
  "golem",
  "pkgload",
  "shiny",
  "bslib",
  "DT",
  "dplyr",
  "ggiraph",
  "ggplot2",
  "glue",
  "htmltools",
  "kableExtra",
  "knitr",
  "lubridate",
  "magrittr",
  "readr",
  "rlang",
  "rmarkdown",
  "scales",
  "stringr",
  "tidyr",
  "viridis"
)
to_install <- setdiff(required_packages, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install)
}

library(golem)
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all()
}
run_app()
