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
ssb_imports <- function(lines, years, settings, meta = ssb_metadata(settings),
                        fetch_chunk = ssb_fetch_chunk) {
  ssb_check_units(lines, meta)
  codes <- sort(unique(lines$cn8))
  chunks <- split(codes, ceiling(seq_along(codes) / ssb_chunk_size))
  entries <- lapply(chunks, function(ch) {
    cached("ssb", list("08801", "kg+nok", ch, years), function() fetch_chunk(ch, years), settings)
  })
  out <- dplyr::bind_rows(lapply(entries, `[[`, "value"))
  structure(out, fetched_at = min(do.call(c, lapply(entries, `[[`, "fetched_at"))))
}

read_commodity_cn8 <- function(file = "data/commodity_cn8.csv") {
  utils::read.csv(file, colClasses = "character", encoding = "UTF-8", na.strings = "")
}

# CN8 lines for a pathway and a set of groups ("both" lines belong to every group).
# "monocot" lines (0601 bulbs, tubers) join non_conifer only when monocot = TRUE,
# i.e. at least one host is a monocot.
# host_genera: named list per group (conifer, non_conifer) of host genera, or NULL
# for no genus filter (also used for a group with a host known only above genus).
# A genus-specific line (column `genus`, e.g. birch round wood) counts only when a
# host of its group is in that genus; a residual line (`excludes_genera`, e.g.
# "other broadleaved") counts only when a host genus of its group is outside the list.
cn8_lines <- function(cn8_table, pathway, groups, monocot = FALSE, host_genera = NULL) {
  g <- cn8_table$commodity_group
  keep <- g %in% groups | (g == "both" & length(groups) > 0) |
    (g == "monocot" & monocot & "non_conifer" %in% groups)
  x <- cn8_table[cn8_table$pathway == pathway & keep, ]
  if (is.null(host_genera) || nrow(x) == 0) return(x)
  split_genera <- function(s) if (is.na(s) || !nzchar(s)) character() else strsplit(s, ";")[[1]]
  ok <- vapply(seq_len(nrow(x)), function(i) {
    grp <- x$commodity_group[i]
    # "both" / "monocot" lines: judged on the hosts of all requested groups
    use <- if (grp %in% c("conifer", "non_conifer")) grp else intersect(c("conifer", "non_conifer"), groups)
    use <- intersect(use, names(host_genera))
    if (length(use) == 0 || any(vapply(host_genera[use], is.null, logical(1)))) return(TRUE)
    hg <- unlist(host_genera[use])
    only <- split_genera(x$genus[i])
    excl <- split_genera(x$excludes_genera[i])
    if (length(only) > 0) return(any(hg %in% only))
    # residual lines: judged on genera only (family names end in -aceae)
    if (length(excl) > 0) return(any(!hg[!grepl("aceae$", hg)] %in% excl))
    TRUE
  }, logical(1))
  x[ok, ]
}

# Host genera (and their families, for lines that name a family such as palms)
# per group for cn8_lines(); NULL for a group where some host has no genus in its
# lineage (e.g. a family-level host), so that group is not filtered.
host_genera_by_group <- function(classified) {
  out <- lapply(c(conifer = "conifer", non_conifer = "non_conifer"), function(g) {
    sel <- classified$host_class == g
    gen <- classified$genus[sel]
    if (length(gen) == 0) return(character())
    if (anyNA(gen)) NULL else unique(stats::na.omit(c(gen, classified$family[sel])))
  })
  out[!vapply(out, function(x) !is.null(x) && length(x) == 0, logical(1))]
}
