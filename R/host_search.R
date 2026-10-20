# Host search in SSB table 08801 (user's rule, 2026-10-20: always search SSB for
# the host plants). All 08801 commodity labels valid in the table window are
# searched for each host genus: its Latin name and its Norwegian names
# (data/genus_names_no.csv, from the customs tariff). Every hit is listed with its
# status, so lines outside the ENT3 scope are visible, not silently dropped.

read_genus_names_no <- function(file = "data/genus_names_no.csv") {
  utils::read.csv(file, colClasses = "character", encoding = "UTF-8")
}

host_search_terms <- function(genus, names_no) unique(c(genus, names_no$norwegian[names_no$genus %in% genus]))

# labels: tibble(cn8, label) from ssb_metadata()$cn8. counted: named list of CN8
# ids counted per pathway (pathway id -> cn8). Returns one row per hit.
host_search <- function(labels, genera, names_no, counted, first_year) {
  genera <- unique(stats::na.omit(genera))
  empty <- tibble::tibble(cn8 = character(), code = character(), valid = character(), label = character(),
                          host = character(), term = character(), status = character())
  if (length(genera) == 0) return(empty)
  to <- suppressWarnings(as.integer(sub(".*\\(\\d{4}-(\\d{4})?\\).*", "\\1", labels$label)))
  labels <- labels[is.na(to) | to >= first_year, ]
  labels$valid <- sub("^.*\\((\\d{4}-(\\d{4})?)\\).*$", "\\1", labels$label)
  labels$text <- trimws(sub("\\s*\\(\\d{4}-(\\d{4})?\\).*$", "", labels$label))
  # Label with exclusion clauses cut off ("unntatt bjørk", "unnt av bartrær, eik, ...",
  # "ikke dekkfiner furu/gran"): from the keyword to the next ";" or bracket. A host
  # term that only occurs inside such a clause is excluded by the line.
  labels$positive_text <- gsub("(?<!\\p{L})(unnt|unntatt|ikke)(?!\\p{L})[^;()]*", "", labels$text, perl = TRUE, ignore.case = TRUE)
  esc <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
  rows <- list()
  for (g in genera) {
    for (term in host_search_terms(g, names_no)) {
      word <- paste0("(?<!\\p{L})", esc(term), "(?!\\p{L})")
      hit <- grepl(word, labels$text, ignore.case = TRUE, perl = TRUE)
      if (!any(hit)) next
      x <- labels[hit, ]
      positive <- grepl(word, x$positive_text, ignore.case = TRUE, perl = TRUE)
      rows[[length(rows) + 1]] <- tibble::tibble(cn8 = x$cn8, label = x$text, valid = x$valid, host = g, term = term,
                                                 positive = positive)
    }
  }
  if (length(rows) == 0) return(empty)
  h <- dplyr::bind_rows(rows)
  # per line: excluded only if every matched host term is excluded; keep a positive term if any
  h <- h[order(h$cn8, !h$positive), ]
  h$excludes <- !stats::ave(h$positive, h$cn8, FUN = any)
  h <- h[!duplicated(h$cn8), ]
  in_pathway <- vapply(h$cn8, function(c) {
    p <- names(counted)[vapply(counted, function(v) c %in% v, logical(1))]
    if (length(p) == 0) NA_character_ else paste(pathway_label(p), collapse = ", ")
  }, "", USE.NAMES = FALSE)
  h$code <- cn8_code(h$cn8)
  treated <- grepl("impregnert", h$label, ignore.case = TRUE) & !grepl("uimpregnert|ikke impregnert", h$label, ignore.case = TRUE)
  why <- ifelse(treated, "not counted: impregnated wood is excluded",
                paste0("not counted: heading ", substr(h$code, 1, 4), " is outside the app's CN8 scope"))
  h$status <- ifelse(!is.na(in_pathway), paste0("counted: ", in_pathway),
                     ifelse(h$excludes, "label excludes the host", why))
  h <- h[order(!startsWith(h$status, "counted"), h$status != "label excludes the host", h$cn8), ]
  h[c("cn8", "code", "valid", "label", "host", "term", "status")]
}
