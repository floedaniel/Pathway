# SSB table 08801 (external trade by CN8 commodity and country) via PxWebApi v2.
# Verified 2026-10-20: base https://data.ssb.no/api/pxwebapi/v2, no key,
# max 800,000 cells per call, 40 calls per minute (header X-Ratelimit-Limit).
# Q1 (Mengde1) is kg for all lines used here; Q2 (Mengde2) is pieces (stk) only
# where the line label says M2=stk. Value (Verdi) is NOK. Land codes are ISO2,
# exceptions in data/country_map.csv.

ssb_base_url <- "https://data.ssb.no/api/pxwebapi/v2/tables/08801"
ssb_chunk_size <- 25  # CN8 lines per call: 25 x 262 countries x 3 contents x 10 years < 800,000

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
  entry <- cached("ssb", "metadata", function() {
    m <- jsonlite::fromJSON(ssb_get("metadata", list(lang = "no")), simplifyVector = FALSE)
    labels <- unlist(m$dimension$Varekoder$category$label)
    list(
      cn8 = tibble::tibble(cn8 = names(labels), label = unname(labels),
                           q1_unit = sub(".*M1=([^,]*),.*", "\\1", labels),
                           q2_unit = sub(".*M2=([^)]*)\\).*", "\\1", labels)),
      years = sort(as.integer(unlist(m$dimension$Tid$category$label))),
      countries = unlist(m$dimension$Land$category$label),
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

# Unit check (Flow step 7): Q1 must be kg; Q2 is used only when it is stk.
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
    `valueCodes[ContentsCode]` = "Mengde1,Verdi,Mengde2",
    `valueCodes[Tid]` = paste(years, collapse = ","),
    outputFormat = "json-stat2"
  ))
  d <- rjstat::fromJSONstat(body, naming = "id")
  d <- d[!is.na(d$value) & d$value != 0, ]
  wide <- tidyr::pivot_wider(d[c("Varekoder", "Land", "Tid", "ContentsCode", "value")],
                             names_from = "ContentsCode", values_from = "value", values_fill = 0)
  for (col in setdiff(c("Mengde1", "Verdi", "Mengde2"), names(wide))) wide[[col]] <- numeric()
  tibble::tibble(cn8 = wide$Varekoder, country = wide$Land, year = as.integer(wide$Tid),
                 kg = as.numeric(wide$Mengde1), value_nok = as.numeric(wide$Verdi),
                 pc = as.numeric(wide$Mengde2))
}

# Imports for the given CN8 lines over `years`, chunked and cached per chunk.
# `pc` is kept only for lines whose Q2 unit is stk (otherwise NA).
ssb_imports <- function(lines, years, settings, meta = ssb_metadata(settings),
                        fetch_chunk = ssb_fetch_chunk) {
  ssb_check_units(lines, meta)
  codes <- sort(unique(lines$cn8))
  chunks <- split(codes, ceiling(seq_along(codes) / ssb_chunk_size))
  entries <- lapply(chunks, function(ch) {
    cached("ssb", list("08801", ch, years), function() fetch_chunk(ch, years), settings)
  })
  out <- dplyr::bind_rows(lapply(entries, `[[`, "value"))
  stk <- meta$cn8$cn8[meta$cn8$q2_unit == "stk"]
  out$pc[!out$cn8 %in% stk] <- NA
  structure(out, fetched_at = min(do.call(c, lapply(entries, `[[`, "fetched_at"))))
}

read_commodity_cn8 <- function(file = "data/commodity_cn8.csv") {
  utils::read.csv(file, colClasses = "character", encoding = "UTF-8", na.strings = "")
}

# CN8 lines for a pathway and a set of groups ("both" lines belong to every group).
# "monocot" lines (0601 bulbs, tubers) join non_conifer only when monocot = TRUE,
# i.e. at least one host is a monocot.
cn8_lines <- function(cn8_table, pathway, groups, monocot = FALSE) {
  g <- cn8_table$commodity_group
  keep <- g %in% groups | (g == "both" & length(groups) > 0) |
    (g == "monocot" & monocot & "non_conifer" %in% groups)
  cn8_table[cn8_table$pathway == pathway & keep, ]
}
