# Minimal app.R for Golem on Posit Cloud
if (!requireNamespace("golem", quietly = TRUE)) {
  install.packages("golem")
}
library(golem)
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all()
}
run_app()