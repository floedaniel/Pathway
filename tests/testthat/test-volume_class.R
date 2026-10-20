thr <- read_thresholds(file.path(root, "data", "finnprio_thresholds.csv"))
star <- threshold_for(thr, "conifer")
yrs <- 2016:2025
cfg <- list(volume_stat = "median", volume_n = 10)
info_of <- function(groups) list(groups = groups, monocot = FALSE, host_genera = NULL, categories = NULL)

imp <- function(cn8, country, year, kg) {
  tibble::tibble(cn8 = cn8, country = country, year = as.integer(year), kg = kg, value_nok = 0)
}
lines_of <- function(cn8, group = "conifer") data.frame(pathway = "wood", cn8 = cn8, commodity_group = group)

test_that("boundaries follow small_lt / medium_le", {
  expect_equal(volume_label(999999.6, star), "small")
  expect_equal(volume_label(1000000, star), "medium")
  expect_equal(volume_label(10000000, star), "medium")
  expect_equal(volume_label(10000000.1, star), "large")
})

test_that("class labels are FinnPRIO option names", {
  expect_true(all(c("non_existent", "small", "medium", "large") %in% finnprio_classes))
})

test_that("only kg thresholds exist; other units give 'not classified'", {
  expect_setequal(thr$unit, "kg")
  expect_equal(volume_label(5, threshold_for(thr, "conifer", "pc")), "not classified")
})

test_that("no imports give 'none recorded', not non_existent", {
  r <- classify_volume(imp(character(), character(), integer(), numeric()), lines_of("A"),
                       "conifer", "median", yrs, thresholds = thr)
  expect_equal(r$class, "none recorded")
})

test_that("countries are summed before classifying (ENT3 = total into the PRA area)", {
  d <- rbind(imp("A", "DE", yrs, 600000), imp("A", "SE", yrs, 600000))
  r <- classify_volume(d, lines_of("A"), "conifer", "median", yrs, thresholds = thr)
  expect_equal(r$kg, 1200000)
  expect_equal(r$class, "medium")
})

test_that("pest-present origins are shown as kg and share, without a class", {
  d <- rbind(imp("A", "DE", yrs, 5e6), imp("A", "US", yrs, 2e5))
  r <- classify_volume(d, lines_of("A"), "conifer", "median", yrs, present_countries = "US", thresholds = thr)
  expect_equal(r$kg, 5.2e6)
  expect_equal(r$class, "medium")
  expect_equal(r$kg_pest_present, 2e5)
  expect_equal(r$share_pest_present, 2e5 / 5.2e6)
  expect_false("class_pest_present" %in% names(r))
})

test_that("median over the last volume_n years", {
  d <- imp("A", "DE", yrs, c(rep(1, 7), 2e6, 3e6, 4e7))
  r <- classify_volume(d, lines_of("A"), "conifer", "median", yrs, volume_n = 3, thresholds = thr)
  expect_equal(r$kg, 3e6)
  expect_equal(r$years, "2023-2025")
})

test_that("years without imports count as zero", {
  r <- classify_volume(imp("A", "DE", 2025, 9e6), lines_of("A"), "conifer", "median", yrs, thresholds = thr)
  expect_equal(r$kg, 0)
})

test_that("output is tidy: one row per group, one value per column", {
  cn8 <- data.frame(pathway = "wood", cn8 = c("C", "B"), commodity_group = c("conifer", "both"))
  d <- rbind(imp("C", "DE", yrs, 1e5), imp("B", "DE", yrs, 1e5))
  res <- classify_pathway(d, cn8, "wood", info_of(c("conifer", "non_conifer")), character(), cfg, yrs, thr)
  expect_equal(names(res), c("pathway", "group", "role", "stat", "years", "kg", "class",
                             "kg_pest_present", "share_pest_present"))
  expect_equal(nrow(res), 3)
  expect_true(all(vapply(res, function(x) is.atomic(x) && length(x) == 3, logical(1))))
})

test_that("'both' lines count once per group; all_hosts is the union", {
  cn8 <- data.frame(pathway = "wood", cn8 = c("C", "B"), commodity_group = c("conifer", "both"))
  d <- rbind(imp("C", "DE", yrs, 1e5), imp("B", "DE", yrs, 1e5))
  res <- classify_pathway(d, cn8, "wood", info_of(c("conifer", "non_conifer")), character(), cfg, yrs, thr)
  expect_equal(res$kg[res$group == "conifer"], 2e5)
  expect_equal(res$kg[res$group == "non_conifer"], 1e5)
  expect_equal(res$kg[res$group == "all_hosts"], 2e5)
  expect_equal(res$role[res$group == "all_hosts"], "ent3")
  expect_true(all(res$role[res$group != "all_hosts"] == "info"))
})
