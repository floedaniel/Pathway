# Record real EPPO API v2 responses as test fixtures, and survey host lineages.
# Needs EPPO_KEY in the environment. Run from the project root:
#   Rscript dev/record_eppo_fixtures.R [survey_out.csv]
# Fixtures: tests/testthat/fixtures/eppo/<path with / replaced by __>.json
# (bibref fields are dropped to keep files small).

source("R/conditions.R")
source("R/api_eppo.R")

fixture_dir <- "tests/testthat/fixtures/eppo"
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

fixture_pests <- c("IPSXCA", "AROMBU", "PHYTRA", "PHYTIN")
survey_pests <- c("IPSXCA", "DENCPO", "POLGPR", "GIBBCI", "AROMBU", "CERAFP",
                  "PHYTRA", "LYMAMA", "XYLEFA", "PHYTIN")

save_fixture <- function(path, value) {
  if (is.data.frame(value)) value$bibref <- NULL
  f <- file.path(fixture_dir, paste0(gsub("/", "__", path), ".json"))
  jsonlite::write_json(value, f, auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE)
}

fetch_and_save <- function(path, save = TRUE) {
  value <- eppo_fetch(path)
  if (save) save_fixture(path, value %||% list())
  value
}

fetch_and_save("references/distributionStatus")
fetch_and_save("references/pestHostClassification")

survey <- list()
for (pest in survey_pests) {
  save <- pest %in% fixture_pests
  for (ep in c("overview", "distribution", "names")) fetch_and_save(file.path("taxons/taxon", pest, ep), save)
  hosts <- fetch_and_save(file.path("taxons/taxon", pest, "hosts"), save)
  if (!is.data.frame(hosts) || nrow(hosts) == 0) next
  message(pest, ": ", nrow(hosts), " hosts")
  for (i in seq_len(nrow(hosts))) {
    tax <- fetch_and_save(file.path("taxons/taxon", hosts$eppocode[i], "taxonomy"), save)
    rank <- function(type) if (is.data.frame(tax) && any(tax$type == type)) tax$prefname[tax$type == type][1] else NA
    survey[[length(survey) + 1]] <- data.frame(
      pest = pest, host = hosts$eppocode[i], prefname = hosts$prefname[i], class_label = hosts$class_label[i],
      class = rank("Class"), category = rank("Category"), family = rank("Family"), genus = rank("Genus")
    )
  }
}

# Error/edge cases for stop-condition tests
fetch_and_save("taxons/taxon/NOSUCH/overview")

out <- commandArgs(trailingOnly = TRUE)[1]
if (!is.na(out)) write.csv(do.call(rbind, survey), out, row.names = FALSE)
