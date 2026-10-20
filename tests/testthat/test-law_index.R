law_fixture <- testthat::test_path("fixtures", "lovdata", "sf-20001201-1333.xml")
aliases <- utils::read.csv(file.path(root, "data", "law_taxon_aliases.csv"), colClasses = "character")
idx <- law_parse(law_fixture)

test_that("regulation splits into § articles and annex rows with deep links", {
  expect_true(all(c("§ 1", "Vedlegg 2, nr. 10", "Vedlegg 1, nr. 2.1") %in% idx$id))
  expect_false(anyDuplicated(idx$id) > 0)
  expect_true(all(startsWith(idx$url, "https://lovdata.no/dokument/SF/forskrift/2000-12-01-1333/")))
})

test_that("Ips calligraphus on conifers finds the Scolytidae row (via alias)", {
  t <- law_search_terms(c("Ips calligraphus", "Ips exesus"), c("Curculionidae", "Scolytinae", "Ips"),
                        "Pinus", "conifer", aliases = aliases)
  r <- law_search(idx, t)
  expect_equal(r$id[1], "Vedlegg 2, nr. 10")
  expect_true(r$pest_match[1])
})

test_that("Bursaphelenchus xylophilus finds its annex 2 row", {
  r <- law_search(idx, law_search_terms("Bursaphelenchus xylophilus", aliases = aliases))
  expect_true("Vedlegg 2, nr. 2" %in% r$id[r$pest_match])
})

test_that("Phytophthora ramorum is found in several units", {
  r <- law_search(idx, law_search_terms("Phytophthora ramorum", aliases = aliases))
  expect_gte(sum(r$pest_match), 2)
})

test_that("a pest absent from the regulation gives no pest match ('not found')", {
  r <- law_search(idx, law_search_terms("Xylella fastidiosa", aliases = aliases))
  expect_equal(sum(r$pest_match), 0)
})

test_that("country terms alone do not select units", {
  r <- law_search(idx, list(pest = character(), host = character(), country = "USA"))
  expect_equal(nrow(r), 0)
})

test_that("index is cached: second call does not download", {
  s <- test_settings()
  n <- 0
  dl <- function() { n <<- n + 1; law_fixture }
  law_index(s, download = dl)
  i2 <- law_index(s, download = dl)
  expect_equal(n, 1)
  expect_false(is.null(attr(i2, "fetched_at")))
})
