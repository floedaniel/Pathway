# SSB table 08801 (external trade by CN8 commodity and country) via PxWebApi v2.
# Verified 2026-10-20: base https://data.ssb.no/api/pxwebapi/v2, no key,
# max 800,000 cells per call, 40 calls per minute (header X-Ratelimit-Limit).
# Q1 (Mengde1) is kg for all lines used here; only kg is used (decided 2026-10-20),
# Q2 (pieces, m³) is not fetched. Value (Verdi) is NOK. Land codes are ISO2,
# exceptions in data/country_map.csv.

ssb_base_url <- "https://data.ssb.no/api/pxwebapi/v2/tables/08801"
ssb_chunk_size <- 25  # CN8 lines per call: 25 x 262 countries x 2 contents x 10 years < 800,000

ssb_get <- function(path, query = list()) {
  httr2::request(ssb_base_url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_throttle(capacity = 30, fill_time_s = 60) |>
    httr2::req_retry(max_tries = 4, is_transient = function(r) httr2::resp_status(r) %in% c(429, 503)) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

# Table metadata: CN8 labels (with M1/M2 units) and available years. Cached (ssb).
ssb_metadata <- function(settings) {
  entry <- cached("ssb", "metadata_no_en", function() {
    m <- jsonlite::fromJSON(ssb_get("metadata", list(lang = "no")), simplifyVector = FALSE)
    m_en <- jsonlite::fromJSON(ssb_get("metadata", list(lang = "en")), simplifyVector = FALSE)
    labels <- unlist(m$dimension$Varekoder$category$label)
    list(
      cn8 = tibble::tibble(cn8 = names(labels), label = unname(labels),
                           q1_unit = sub(".*M1=([^,]*),.*", "\\1", labels)),
      years = sort(as.integer(unlist(m$dimension$Tid$category$label))),
      countries = unlist(m$dimension$Land$category$label),          # Norwegian (Lovdata search)
      countries_en = unlist(m_en$dimension$Land$category$label),    # English (UI, write-up)
      updated = m$updated
    )
  }, settings)
  structure(entry$value, fetched_at = entry$fetched_at)
}

# Years for table and graph: ssb_years back from the latest complete year.
ssb_window <- function(settings, meta = ssb_metadata(settings)) {
  last <- max(meta$years)
  seq(last - settings$ssb_years + 1, last)
}

# Unit check (Flow step 7): Q1 must be kg.
ssb_check_units <- function(lines, meta) {
  u <- meta$cn8[match(lines$cn8, meta$cn8$cn8), ]
  bad <- !is.na(u$q1_unit) & u$q1_unit != "kg"
  if (any(bad)) {
    stop_app("unit_mismatch", group = lines$commodity_group[bad][1], unit = u$q1_unit[bad][1])
  }
  invisible(TRUE)
}

# Imports for one chunk of CN8 lines, all origin countries. Long tibble, zero rows dropped.
ssb_fetch_chunk <- function(cn8, years) {
  body <- ssb_get("data", list(
    lang = "no",
    `valueCodes[Varekoder]` = paste(cn8, collapse = ","),
    `valueCodes[ImpEks]` = "1",
    `valueCodes[Land]` = "*",
    `valueCodes[ContentsCode]` = "Mengde1,Verdi",
    `valueCodes[Tid]` = paste(years, collapse = ","),
    outputFormat = "json-stat2"
  ))
  d <- rjstat::fromJSONstat(body, naming = "id")
  d <- d[!is.na(d$value) & d$value != 0, ]
  wide <- tidyr::pivot_wider(d[c("Varekoder", "Land", "Tid", "ContentsCode", "value")],
                             names_from = "ContentsCode", values_from = "value", values_fill = 0)
  for (col in setdiff(c("Mengde1", "Verdi"), names(wide))) wide[[col]] <- numeric()
  tibble::tibble(cn8 = wide$Varekoder, country = wide$Land, year = as.integer(wide$Tid),
                 kg = as.numeric(wide$Mengde1), value_nok = as.numeric(wide$Verdi))
}

# Imports for the given CN8 lines over `years`, chunked and cached per chunk.
# countries: origin countries to keep (ISO2). The app passes the countries where
# the pest is present (user's rule, 2026-10-20: only SSB data from those
# countries). Chunks are fetched for all origins and cached that way, so the
# cache is shared between pests; the filter is applied here, before anything
# else sees the data. NULL keeps every origin.
ssb_imports <- function(lines, years, settings, meta = ssb_metadata(settings),
                        fetch_chunk = ssb_fetch_chunk, countries = NULL) {
  ssb_check_units(lines, meta)
  codes <- sort(unique(lines$cn8))
  chunks <- split(codes, ceiling(seq_along(codes) / ssb_chunk_size))
  entries <- lapply(chunks, function(ch) {
    cached("ssb", list("08801", "kg+nok", ch, years), function() fetch_chunk(ch, years), settings)
  })
  out <- dplyr::bind_rows(lapply(entries, `[[`, "value"))
  if (!is.null(countries)) out <- out[out$country %in% countries, ]
  structure(out, fetched_at = min(do.call(base::c, lapply(entries, `[[`, "fetched_at"))))
}

read_commodity_cn8 <- function(file = "data/commodity_cn8.csv") {
  utils::read.csv(file, colClasses = "character", encoding = "UTF-8", na.strings = "")
}

# CN8 lines for a pathway and a set of groups ("both" lines belong to every group).
# "monocot" lines (0601 bulbs, tubers) join non_conifer only when monocot = TRUE,
# i.e. at least one host is a monocot.
# host_genera: named list per group (conifer, non_conifer) of hosts, or NULL for no
# host filter (also used for a group with a host known only above genus). Each host
# is list(taxa, rank): its EPPO name and lineage names from family down (see
# classify_host()). A plain character vector of genus and family names is also
# accepted (each genus one host, the families attached to all).
# Line columns (names are EPPO preferred names, checked by dev/check_host_taxa.R):
# - `genus`: taxa the line covers (genus, species or family, e.g. birch round wood
#   "Betula", tomatoes "Solanum lycopersicum", orchids "Orchidaceae"); the line counts
#   when a host belongs to one of them, or when a genus-level host contains one.
# - `excludes_genera`: a residual line ("other broadleaved", "other Allium"); it
#   counts when some host (within `genus`, if set) is not covered by the list. "*"
#   = residual of the pathway ("vegetables n.e.s."): counts for hosts that no
#   specific line of the pathway covers.
# categories: EPPO host-commodity categories listed for the pest (NULL = no filter);
# a line counts only when one of its `eppo_category` values is listed.
cn8_lines <- function(cn8_table, pathway, groups, monocot = FALSE, host_genera = NULL, categories = NULL) {
  g <- cn8_table$commodity_group
  keep <- g %in% groups | (g == "both" & length(groups) > 0) |
    (g == "monocot" & monocot & "non_conifer" %in% groups)
  if (!is.null(categories)) {
    keep <- keep & vapply(strsplit(cn8_table$eppo_category %||% rep("", nrow(cn8_table)), ";"),
                          function(cc) any(cc %in% categories), logical(1))
  }
  x <- cn8_table[cn8_table$pathway == pathway & keep, ]
  if (is.null(host_genera) || nrow(x) == 0) return(x)
  host_genera <- lapply(host_genera, as_hosts)
  split_taxa <- function(s) if (is.na(s) || !nzchar(s)) character() else norm_taxon(strsplit(s, ";")[[1]])
  only_all <- lapply(x$genus %||% rep(NA, nrow(x)), split_taxa)
  excl_all <- lapply(x$excludes_genera %||% rep(NA, nrow(x)), split_taxa)
  # "*" lines: a host is covered when another line of the pathway names its taxa.
  # A scoped "*" line (with taxa, e.g. 'vegetable seeds n.e.s.') yields only to
  # lines that are not "*"; an unscoped one ('seeds n.e.s.') also to scoped ones.
  star <- vapply(excl_all, identical, logical(1), "*")
  covered_by_specific <- function(h, i) {
    others <- setdiff(which(lengths(only_all) > 0), i)
    if (length(only_all[[i]]) > 0) others <- others[!star[others]]
    any(vapply(others, function(j) host_in(h, only_all[[j]]), logical(1)))
  }
  ok <- vapply(seq_len(nrow(x)), function(i) {
    grp <- x$commodity_group[i]
    # "both" / "monocot" lines: judged on the hosts of all requested groups
    use <- if (grp %in% c("conifer", "non_conifer")) grp else intersect(c("conifer", "non_conifer"), groups)
    use <- intersect(use, names(host_genera))
    if (length(use) == 0 || any(vapply(host_genera[use], is.null, logical(1)))) return(TRUE)
    hosts <- unlist(host_genera[use], recursive = FALSE)
    only <- only_all[[i]]
    excl <- excl_all[[i]]
    if (length(only) > 0) hosts <- Filter(function(h) host_in(h, only), hosts)
    if (identical(excl, "*")) return(any(!vapply(hosts, covered_by_specific, logical(1), i = i)))
    if (length(excl) > 0) return(any(vapply(hosts, function(h) !any(h$taxa %in% excl), logical(1))))
    length(only) == 0 || length(hosts) > 0
  }, logical(1))
  x[ok, ]
}

# Taxon names compared without the hybrid sign ("Citrus x limon" = "Citrus limon",
# "x Citrofortunella" = "Citrofortunella").
norm_taxon <- function(x) trimws(gsub("\\s+", " ", gsub("(^|\\s)[x×]\\s", "\\1", x)))
genus_of <- function(x) sub(" .*$", "", norm_taxon(x))

# Is host h (list(taxa, rank)) within one of `taxa`? Yes when one of its lineage
# names is listed, or, for a genus-level host, when a listed species is in its genus.
host_in <- function(h, taxa) any(h$taxa %in% taxa) || (identical(h$rank, "Genus") && any(genus_of(taxa) %in% h$taxa))

# Hosts of one group in the form cn8_lines() uses.
as_hosts <- function(x) {
  if (is.null(x)) return(x)
  if (is.list(x)) return(lapply(x, function(h) list(taxa = norm_taxon(h$taxa), rank = h$rank)))
  fam <- x[grepl("aceae$", x)]
  lapply(setdiff(x, fam), function(g) list(taxa = norm_taxon(c(g, fam)), rank = "Genus"))
}

# CN8 lines for a pathway from its host selection `info` (see pathway_info() in
# R/pipeline.R); `group` restricts to one host group.
pathway_lines <- function(cn8_table, pathway, info, group = NULL) {
  x <- cn8_lines(cn8_table, pathway, group %||% info$groups, isTRUE(info$monocot), info$host_genera, info$categories)
  x[!x$cn8 %in% info$excluded, ]  # codes the assessor excluded (apply_selection())
}

# Hosts per group for cn8_lines(), each list(taxa, rank) from the EPPO taxonomy
# (classify_host()); without that, genus and family. NULL for a group where some
# host has no genus in its lineage (e.g. a family-level host), so that group is not
# filtered.
host_genera_by_group <- function(classified) {
  if (any(classified$host_class == "all_plants")) return(list(conifer = NULL, non_conifer = NULL))
  out <- lapply(c(conifer = "conifer", non_conifer = "non_conifer"), function(g) {
    sel <- which(classified$host_class == g)
    if (length(sel) == 0) return(list())
    if (anyNA(classified$genus[sel])) return(NULL)
    lapply(sel, function(i) {
      t <- if (!is.null(classified$taxa) && !is.na(classified$taxa[i]) && nzchar(classified$taxa[i]))
        strsplit(classified$taxa[i], ";")[[1]] else stats::na.omit(c(classified$genus[i], classified$family[i]))
      list(taxa = norm_taxon(unique(t)), rank = classified$rank[i] %||% "Genus")
    })
  })
  out[!vapply(out, function(x) !is.null(x) && length(x) == 0, logical(1))]
}
