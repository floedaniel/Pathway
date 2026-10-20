# Deploy Pathwise to Posit Connect Cloud with the rsconnect account stored by
# RStudio. Run from the project root: Rscript dev/deploy.R
# Keys are read from API_kays/ into this R session only and sent with envVars
# (encrypted, not stored in the bundle). If Connect Cloud ignores envVars, set
# EPPO_KEY and OPENAI_API_KEY as secrets in the Connect Cloud UI.

source("R/settings.R")
load_local_keys()
stopifnot(nzchar(Sys.getenv("EPPO_KEY")), nzchar(Sys.getenv("OPENAI_API_KEY")))

files <- list.files(".", recursive = TRUE)
excluded <- "^(API_kays|API_keys|cache|renv/library|renv/staging|renv/sandbox|data/law/raw|old_scripts|tests|dev|docs|rsconnect)/|\\.Rproj$|\\.nb\\.html$|^Pathwise_notes|^manifest\\.json$"
keep <- files[!grepl(excluded, files)]
stopifnot(!any(grepl("API_ka|tore_vkm|EPPO_beta|\\.Renviron", keep)))

rsconnect::deployApp(
  appDir = ".", appFiles = keep, appPrimaryDoc = "app.R",
  appName = "pathwise", appTitle = "Pathwise: FinnPRIO ENT3 import volume",
  account = "danielflow", server = "connect.posit.cloud",
  envVars = c("EPPO_KEY", "OPENAI_API_KEY"),
  forceUpdate = TRUE, launch.browser = FALSE, logLevel = "normal"
)
