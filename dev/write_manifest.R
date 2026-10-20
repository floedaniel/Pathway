# Write manifest.json for Posit Connect Cloud (deploys from the GitHub repo).
# Run from the project root after package changes: Rscript dev/write_manifest.R
# Keys are NOT bundled: set EPPO_KEY and OPENAI_API_KEY as secrets in Connect Cloud.

files <- list.files(".", recursive = TRUE)
excluded <- "^(API_kays|API_keys|cache|renv/library|renv/staging|renv/sandbox|data/law/raw|old_scripts|tests|dev|docs)/|\\.Rproj$|^data/Lids_flora\\.xls$|\\.nb\\.html$|^Pathwise_notes|^manifest\\.json$"
keep <- files[!grepl(excluded, files)]
stopifnot(!any(grepl("API_ka|tore_vkm|EPPO_beta|\\.Renviron", keep)))
rsconnect::writeManifest(appDir = ".", appFiles = keep, appPrimaryDoc = "app.R")
m <- jsonlite::read_json("manifest.json")
cat(length(keep), "files,", length(m$packages), "packages, R", m$platform, "\n")
print(keep)
