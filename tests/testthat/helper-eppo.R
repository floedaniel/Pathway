# Serve recorded EPPO API v2 responses (tests/testthat/fixtures/eppo, made by
# dev/record_eppo_fixtures.R). `overrides` maps an API path to a JSON string,
# for crafted edge cases. Unknown paths return 400, like the real API.
local_eppo_mock <- function(overrides = list(), env = parent.frame()) {
  fixture_dir <- normalizePath(testthat::test_path("fixtures", "eppo"))
  gd_dir <- normalizePath(testthat::test_path("fixtures", "eppo_gd"))
  httr2::local_mocked_responses(function(req) {
    # gd.eppo.int host-commodity pages: fixtures/eppo_gd/<code>_pathwayshosts.html
    if (grepl("^https://gd\\.eppo\\.int/taxon/", req$url)) {
      code <- sub("^.*/taxon/([0-9A-Z]+)/pathwayshosts.*$", "\\1", req$url)
      f <- file.path(gd_dir, paste0(code, "_pathwayshosts.html"))
      body <- overrides[[paste0("gd:", code)]]
      if (is.null(body)) {
        if (!file.exists(f)) return(httr2::response(status_code = 404))
        body <- readChar(f, file.size(f), useBytes = TRUE)
      }
      return(httr2::response(status_code = 200, headers = list(`Content-Type` = "text/html"),
                             body = charToRaw(enc2utf8(body))))
    }
    path <- sub("\\?.*$", "", sub("^.*/gd/v2/", "", req$url))
    body <- overrides[[path]]
    if (is.null(body)) {
      f <- file.path(fixture_dir, paste0(gsub("/", "__", path), ".json"))
      if (!file.exists(f)) return(httr2::response(status_code = 400))
      body <- readChar(f, file.size(f), useBytes = TRUE)
    }
    httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(enc2utf8(body)))
  }, env = env)
}
