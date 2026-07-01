if (nzchar(Sys.which("rv"))) {
  source("rv/scripts/rvr.R")
  source("rv/scripts/activate.R")
} else if (!interactive()) {
  # Deployment environment (e.g. Posit Connect Cloud).
  # Connect Cloud's curated library ships Rcpp 1.0.14; install the correct
  # version before Shiny loads httpuv so the right one is found first.
  install.packages(c("Rcpp", "systemfonts", "cpp11"), quiet = TRUE)
}
options(repos = c(CRAN = "https://p3m.dev/cran/__linux__/manylinux_2_28/latest"))
options(renv.config.pak.enabled = TRUE)
