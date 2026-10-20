woody_file <- file.path(root, "data", "woody_taxa.csv")

classify_pest <- function(code, s, woody = read_woody_taxa(woody_file)) {
  classify_hosts(eppo_selected_hosts(code, s), s, woody = woody)
}

test_that("conifer-only pest (Ips calligraphus) gives conifer", {
  local_eppo_mock()
  s <- test_settings()
  ch <- classify_pest("IPSXCA", s)
  expect_true(all(ch$host_class == "conifer"))
  expect_equal(run_groups(ch), "conifer")
})

test_that("broadleaf-only pest (Aromia bungii) gives broadleaf", {
  local_eppo_mock()
  s <- test_settings()
  expect_equal(run_groups(classify_pest("AROMBU", s)), "broadleaf")
})

test_that("mixed pest (Phytophthora ramorum) gives both groups", {
  local_eppo_mock()
  s <- test_settings()
  expect_setequal(run_groups(classify_pest("PHYTRA", s)), c("conifer", "broadleaf"))
})

test_that("herbaceous-only pest (Phytophthora infestans) stops: no covered groups", {
  local_eppo_mock()
  s <- test_settings()
  ch <- classify_pest("PHYTIN", s)
  expect_equal(ch$host_class, "other")
  expect_error(run_groups(ch), class = "pathwise_stop_no_groups")
})

test_that("unlisted angiosperm hosts are unreviewed and trigger their own stop", {
  local_eppo_mock()
  s <- test_settings()
  woody <- read_woody_taxa(woody_file)
  woody <- woody[woody$taxon != "Solanaceae", ]
  ch <- classify_pest("PHYTIN", s, woody)
  expect_equal(ch$host_class, "unreviewed")
  expect_error(run_groups(ch), "Unreviewed: Solanum tuberosum", class = "pathwise_stop_unreviewed_hosts")
})

test_that("genus rows override family rows", {
  woody <- data.frame(rank = c("family", "genus"), taxon = c("Rosaceae", "Fragaria"),
                      woody = c(TRUE, FALSE), notes = "", reviewed = "")
  expect_true(woody_lookup("Rosaceae", "Prunus", woody))
  expect_false(woody_lookup("Rosaceae", "Fragaria", woody))
  expect_true(is.na(woody_lookup("Fabaceae", "Robinia", woody)))
})

test_that("woody_use_unreviewed = FALSE ignores rows without a review date", {
  woody <- data.frame(rank = "family", taxon = "Fagaceae", woody = TRUE, notes = "", reviewed = "")
  expect_true(is.na(woody_lookup("Fagaceae", "Quercus", woody, use_unreviewed = FALSE)))
  woody$reviewed <- "2026-10-20"
  expect_true(woody_lookup("Fagaceae", "Quercus", woody, use_unreviewed = FALSE))
})

test_that("conifer family fallback works when the class rank is missing", {
  tax <- tibble::tibble(eppocode = "X", prefname = "Pinaceae", level = 5L, type = "Family")
  expect_equal(classify_host(tax, read_woody_taxa(woody_file))$host_class, "conifer")
})
