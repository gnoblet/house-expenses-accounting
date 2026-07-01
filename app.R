required_packages <- c(
  "golem", "shiny", "bslib", "DT", "dplyr",
  "ggiraph", "ggplot2", "glue", "htmltools",
  "kableExtra", "knitr", "lubridate", "magrittr",
  "readr", "readxl", "rlang", "rmarkdown",
  "scales", "stringr", "tidyr", "viridis"
)
to_install <- setdiff(required_packages, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install)
}
library(golem)
options("golem.app.prod" = TRUE)
run_app()
