test_that("cached() fetches once, then serves from disk", {
  s <- test_settings()
  n <- 0
  fetch <- function() { n <<- n + 1; "value" }
  t0 <- as.POSIXct("2026-01-01", tz = "UTC")

  e1 <- cached("eppo", list("hosts", "XYLEFA"), fetch, s, now = t0)
  e2 <- cached("eppo", list("hosts", "XYLEFA"), fetch, s, now = t0 + 3600)
  expect_equal(n, 1)
  expect_equal(e2$value, "value")
  expect_equal(e2$fetched_at, t0)
})

test_that("entries expire after the source TTL", {
  s <- test_settings()
  s$cache$ttl_days$eppo <- 30
  t0 <- as.POSIXct("2026-01-01", tz = "UTC")
  cache_set("eppo", "q", "old", s, now = t0)

  expect_equal(cache_get("eppo", "q", s, now = t0 + 29 * 86400)$value, "old")
  expect_null(cache_get("eppo", "q", s, now = t0 + 31 * 86400))
})

test_that("taxonomy (null TTL) never expires", {
  s <- test_settings()
  t0 <- as.POSIXct("2026-01-01", tz = "UTC")
  cache_set("eppo_taxonomy", "q", "tax", s, now = t0)
  expect_equal(cache_get("eppo_taxonomy", "q", s, now = t0 + 3650 * 86400)$value, "tax")
})

test_that("different queries get different keys; unknown source fails", {
  expect_false(cache_key(list("a", 1)) == cache_key(list("a", 2)))
  s <- test_settings()
  expect_error(cache_get("nosuch", "q", s), "No cache TTL configured")
})
