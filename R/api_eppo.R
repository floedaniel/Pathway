# EPPO Global Database API v2. Verified 2026-10-20 against the live API and the
# OpenAPI spec https://api.eppo.int/gd/v2/eppo_api_gd_v2.yml:
# - base https://api.eppo.int/gd/v2, key in header X-Api-Key
# - v1 (data.eppo.int/api/rest/1.0 with ?authtoken=) is marked legacy, not used
# - rate limit 2000 requests / 10 s per IP; 429 is retried
# - unknown or malformed codes return 400, not 404
# Not to be confused with eppo.cloud (an unrelated A/B-testing company).

eppo_base_url <- "https://api.eppo.int/gd/v2"

# Raw GET. Returns parsed JSON, or NULL when EPPO says the code does not exist.
eppo_fetch <- function(path, query = list()) {
  resp <- httr2::request(eppo_base_url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_headers(`X-Api-Key` = Sys.getenv("EPPO_KEY"), .redact = "X-Api-Key") |>
    httr2::req_retry(max_tries = 4, is_transient = function(r) httr2::resp_status(r) %in% c(429, 503)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status %in% c(400, 404)) return(NULL)
  if (status >= 400) {
    rlang::abort(sprintf("EPPO API error %d for %s", status, path), class = "pathwise_api_error")
  }
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# Cached GET. Value carries the fetch time as attribute "fetched_at".
eppo_get <- function(path, settings, query = list(), source = "eppo") {
  entry <- cached(source, list(path, query), function() eppo_fetch(path, query), settings)
  structure(entry$value %||% list(), fetched_at = entry$fetched_at)
}

# Turn a JSON array (data.frame or empty list) into a tibble with the given columns.
as_eppo_tibble <- function(x, cols) {
  out <- if (is.data.frame(x) && nrow(x) > 0) tibble::as_tibble(x) else tibble::tibble()
  for (col in setdiff(names(cols), names(out))) out[[col]] <- cols[[col]][0]
  out <- out[names(cols)]
  attr(out, "fetched_at") <- attr(x, "fetched_at")
  out
}

eppo_overview <- function(code, settings) {
  x <- eppo_get(file.path("taxons/taxon", code, "overview"), settings)
  if (length(x) == 0) NULL else x
}

eppo_names <- function(code, settings) {
  as_eppo_tibble(eppo_get(file.path("taxons/taxon", code, "names"), settings),
                 list(fullname = character(), lang_iso = character(), preferred = logical()))
}

eppo_hosts <- function(code, settings) {
  as_eppo_tibble(eppo_get(file.path("taxons/taxon", code, "hosts"), settings),
                 list(eppocode = character(), prefname = character(),
                      class_id = integer(), class_label = character()))
}

# Taxonomy never changes in practice: cached permanently (source eppo_taxonomy).
eppo_taxonomy <- function(code, settings) {
  as_eppo_tibble(eppo_get(file.path("taxons/taxon", code, "taxonomy"), settings, source = "eppo_taxonomy"),
                 list(eppocode = character(), prefname = character(),
                      level = integer(), type = character()))
}

eppo_distribution <- function(code, settings) {
  as_eppo_tibble(eppo_get(file.path("taxons/taxon", code, "distribution"), settings),
                 list(country_iso = character(), state_id = character(), peststatus = character(),
                      yr_situation = character()))
}

eppo_name2codes <- function(name, settings) {
  as_eppo_tibble(eppo_get("tools/name2codes", settings, query = list(name = name)),
                 list(eppocode = character(), preferred = logical()))
}

eppo_distribution_status <- function(settings) {
  as_eppo_tibble(eppo_get("references/distributionStatus", settings),
                 list(peststatus = character(), peststatus_label = character()))
}

# ---- Host commodities (EPPO GD web page) -------------------------------------
# EPPO lists host commodities (category + host genus) for EPPO/EU-listed pests only
# (GD user guide p. 13-14, docs/reference). There is no API endpoint (API v2 only
# gives a count, infos$pathwayhost), so the server-rendered table on
# https://gd.eppo.int/taxon/{code}/pathwayshosts is read, once per pest, cached
# like other EPPO data. Agreed with the user 2026-10-20.

eppo_gd_url <- "https://gd.eppo.int"

eppo_gd_fetch <- function(code) {
  resp <- httr2::request(eppo_gd_url) |>
    httr2::req_url_path_append("taxon", code, "pathwayshosts") |>
    httr2::req_user_agent("Pathwise (FinnPRIO ENT3 tool; one request per pest, cached)") |>
    httr2::req_retry(max_tries = 3, is_transient = function(r) httr2::resp_status(r) %in% c(429, 503)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) != 200) return(NULL)
  httr2::resp_body_string(resp)
}

# Parse the host-commodity table. Returns tibble(category, host, host_code);
# empty when the pest has no list (or the page layout changed).
parse_host_commodities <- function(html) {
  empty <- tibble::tibble(category = character(), host = character(), host_code = character())
  if (is.null(html) || !nzchar(html)) return(empty)
  doc <- xml2::read_html(html)
  rows <- xml2::xml_find_all(doc, "//table[@id='dttable']/tbody/tr")
  if (length(rows) == 0) return(empty)
  cells <- lapply(rows, function(r) trimws(xml2::xml_text(xml2::xml_find_all(r, "./td"))))
  cells <- cells[lengths(cells) >= 2]
  if (length(cells) == 0) return(empty)
  raw <- vapply(cells, `[`, "", 2)  # e.g. "Betula (1BETG)"
  tibble::tibble(
    category = tolower(vapply(cells, `[`, "", 1)),
    host = trimws(sub("\\s*\\([0-9A-Z]{5,6}\\)$", "", raw)),
    host_code = ifelse(grepl("\\([0-9A-Z]{5,6}\\)$", raw), sub("^.*\\(([0-9A-Z]{5,6})\\)$", "\\1", raw), NA_character_)
  )
}

eppo_host_commodities <- function(code, settings, fetch = eppo_gd_fetch) {
  entry <- cached("eppo", list("gd_pathwayshosts", code), function() fetch(code), settings)
  structure(parse_host_commodities(entry$value), fetched_at = entry$fetched_at)
}

# ---- Flow steps 1, 2 and 5 ---------------------------------------------------

# Step 1: the code must exist and be active.
eppo_validate_code <- function(code, settings) {
  code <- toupper(trimws(code))
  if (!grepl("^[0-9A-Z]{5,6}$", code)) stop_app("invalid_code")
  ov <- eppo_overview(code, settings)
  if (is.null(ov) || !isTRUE(ov$is_active)) stop_app("invalid_code")
  ov
}

# Single input: an EPPO code or a scientific name. Something that looks like a
# code and exists is used as is; otherwise the text is looked up as a name
# (preferred names first, then synonyms). Returns the EPPO code or stops.
resolve_input <- function(x, settings) {
  x <- trimws(x)
  if (grepl("^[0-9A-Za-z]{5,6}$", x)) {
    ov <- eppo_overview(toupper(x), settings)
    if (!is.null(ov)) {
      # deactivated code: follow EPPO's replacement (e.g. LITYMC -> LITYCR)
      rep <- ov$replacedby
      if (!isTRUE(ov$is_active) && !is.null(rep) && length(rep) == 1 && !is.na(rep) && nzchar(rep)) return(toupper(rep))
      return(toupper(x))
    }
  }
  hits <- eppo_name2codes(x, settings)
  if (nrow(hits) == 0) {
    hits <- as_eppo_tibble(eppo_get("tools/name2codes", settings, query = list(name = x, onlyPreferred = "false")),
                           list(eppocode = character(), preferred = logical()))
  }
  if (nrow(hits) == 0) stop_app("invalid_code")
  hits$eppocode[order(!hits$preferred)][1]
}

# Step 2: hosts filtered to the configured EPPO host classes (config host_classes).
eppo_selected_hosts <- function(code, settings) {
  hosts <- eppo_hosts(code, settings)
  if (nrow(hosts) == 0) stop_app("no_hosts", code = code)
  keep <- hosts[hosts$class_label %in% settings$host_classes, ]
  if (nrow(keep) == 0) stop_app("no_main_minor")
  keep
}

# Step 5: countries where the pest is present. "Present" = any status whose EPPO
# label starts with "Present" (absent, eradicated, intercepted and transient are out).
eppo_present_countries <- function(code, settings) {
  ref <- eppo_distribution_status(settings)
  present <- ref$peststatus[startsWith(ref$peststatus_label, "Present")]
  dist <- eppo_distribution(code, settings)
  countries <- sort(unique(dist$country_iso[dist$peststatus %in% present]))
  if (length(countries) == 0) stop_app("no_distribution")
  structure(countries, fetched_at = attr(dist, "fetched_at"))
}
