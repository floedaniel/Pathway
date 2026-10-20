# Check every Latin name in data/cn8_host_taxa.csv against the EPPO Global
# Database: the name must resolve to an EPPO code, and it should be EPPO's
# preferred name (host lists use preferred names, so a synonym would never match).
# Run from the project root: Rscript dev/check_host_taxa.R

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
load_local_keys()
settings <- load_settings()

taxa_table <- utils::read.csv("data/cn8_host_taxa.csv", colClasses = "character", encoding = "UTF-8")
names_used <- sort(unique(unlist(strsplit(c(taxa_table$taxa, taxa_table$excludes), ";"))))
names_used <- names_used[nzchar(names_used) & names_used != "*" & !startsWith(names_used, "@")]

res <- lapply(names_used, function(n) {
  codes <- tryCatch(eppo_name2codes(n, settings), error = function(e) NULL)
  if (is.null(codes) || nrow(codes) == 0) return(data.frame(name = n, eppocode = NA, preferred_name = NA, status = "not found"))
  code <- codes$eppocode[1]
  ov <- tryCatch(eppo_overview(code, settings), error = function(e) NULL)
  pref <- if (is.null(ov)) NA else ov$prefname
  data.frame(name = n, eppocode = code, preferred_name = pref,
             status = if (identical(pref, n)) "ok" else "not the preferred name")
})
res <- do.call(rbind, res)
print(table(res$status))
print(res[res$status != "ok", ], row.names = FALSE)
utils::write.csv(res, "dev/check_host_taxa_result.csv", row.names = FALSE, fileEncoding = "UTF-8")
