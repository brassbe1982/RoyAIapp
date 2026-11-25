# Positron / RStudio "Run App" entry point during development.
pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
run_royai_app()
