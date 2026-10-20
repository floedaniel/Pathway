# Source app code for tests (the app is not a package).
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

test_settings <- function(dir = withr::local_tempdir(.local_envir = parent.frame())) {
  s <- load_settings(file.path(root, "config.yml"))
  s$cache$dir <- dir
  s
}
