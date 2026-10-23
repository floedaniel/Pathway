# Compare ENT3 rows of every test pest between two versions of data/commodity_cn8.csv.
# Usage: Rscript dev/sweep_tables.R <old table csv> <output csv>
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
load_local_keys()
settings <- load_settings()
args <- commandArgs(trailingOnly = TRUE)
old_table <- read_commodity_cn8(args[1])
new_table <- read_commodity_cn8()
species <- readxl::read_excel("C:/Users/dafl/Downloads/master_species.xlsx")
codes <- unique(stats::na.omit(species$eppocode))

ent3 <- function(code, tab) {
  r <- tryCatch(run_assessment(code, settings, cn8_table = tab), error = function(e) NULL)
  if (is.null(r) || is.null(r$classes)) return(NULL)
  r$classes[r$classes$role == "ent3", c("pathway", "kg", "class")]
}
rows <- list()
for (code in codes) {
  o <- ent3(code, old_table)
  n <- ent3(code, new_table)
  if (is.null(o) || is.null(n)) next
  m <- merge(o, n, by = "pathway", suffixes = c("_old", "_new"))
  m$code <- code
  rows[[code]] <- m
}
out <- dplyr::bind_rows(rows)
utils::write.csv(out, args[2], row.names = FALSE)
ch <- out[!(out$kg_old %in% out$kg_new & is.na(out$kg_old) == is.na(out$kg_new)) | out$class_old != out$class_new, ]
cat(length(unique(out$code)), "pests;", nrow(ch), "pathway rows changed;", sum(out$class_old != out$class_new), "class changes\n")
