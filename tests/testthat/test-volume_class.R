thr <- read_thresholds(file.path(root, "data", "finnprio_thresholds.csv"))
star <- threshold_for(thr, "conifer", "kg")
yrs <- 2016:2025

imp <- function(cn8, country, year, kg, pc = NA_real_) {
  tibble::tibble(cn8 = cn8, country = country, year = as.integer(year), kg = kg, value_nok = 0, pc = pc)
}
lines_of <- function(cn8, group = "conifer", q2 = "stk") {
  data.frame(pathway = "wood", cn8 = cn8, commodity_group = group, q2_unit = q2)
}

test_that("boundaries follow small_lt / medium_le", {
  expect_equal(volume_label(999999.6, star), "small")
  expect_equal(volume_label(1000000, star), "medium")
  expect_equal(volume_label(10000000, star), "medium")
  expect_equal(volume_label(10000000.1, star), "large")
})

test_that("class labels are FinnPRIO option names", {
  expect_true(all(c("small", "medium", "large") %in% finnprio_classes))
  expect_true("non_existent" %in% finnprio_classes)
})

test_that("missing threshold gives 'not classified'; no imports give 'none recorded'", {
  expect_equal(volume_label(5, threshold_for(thr, "conifer", "m3")), "not classified")
  r <- classify_volume(imp(character(), character(), integer(), numeric()), lines_of("A"),
                       "conifer", "kg", "median", yrs, thresholds = thr)
  expect_equal(r$class, "none recorded")
  expect_false(r$class == "non_existent")
})

test_that("countries are summed before classifying (ENT3 = total into the PRA area)", {
  # 600,000 kg from each of two countries every year: each is small, the total is medium
  d <- rbind(imp("A", "DE", yrs, 600000), imp("A", "SE", yrs, 600000))
  r <- classify_volume(d, lines_of("A"), "conifer", "kg", "median", yrs, thresholds = thr)
  expect_equal(r$value, 1200000)
  expect_equal(r$class, "medium")
})

test_that("Class A uses all origins, Class B only pest-present origins", {
  d <- rbind(imp("A", "DE", yrs, 5e6), imp("A", "US", yrs, 2e5))
  a <- classify_volume(d, lines_of("A"), "conifer", "kg", "median", yrs, subset = "all",
                       present_countries = "US", thresholds = thr)
  b <- classify_volume(d, lines_of("A"), "conifer", "kg", "median", yrs, subset = "pest_present",
                       present_countries = "US", thresholds = thr)
  expect_equal(a$class, "medium")
  expect_equal(b$class, "small")
})

test_that("median over the last volume_n years; latest and max reported", {
  d <- imp("A", "DE", yrs, c(rep(1, 7), 2e6, 3e6, 4e7))
  r <- classify_volume(d, lines_of("A"), "conifer", "kg", "median", yrs, volume_n = 3, thresholds = thr)
  expect_equal(r$value, 3e6)
  expect_equal(r$years, "2023-2025")
  expect_equal(r$latest_class, "large")
  expect_true(r$stats_disagree)
})

test_that("years without imports count as zero", {
  d <- imp("A", "DE", 2025, 9e6)
  r <- classify_volume(d, lines_of("A"), "conifer", "kg", "median", yrs, thresholds = thr)
  expect_equal(r$value, 0)
  expect_equal(r$latest, 9e6)
})

test_that("'both' lines count once per group, never twice", {
  cn8 <- data.frame(pathway = "wood", cn8 = c("C", "B"), commodity_group = c("conifer", "both"), q2_unit = "nei")
  d <- rbind(imp("C", "DE", yrs, 1e5), imp("B", "DE", yrs, 1e5))
  res <- classify_pathway(d, cn8, "wood", c("conifer", "non_conifer"), character(),
                          list(volume_stat = "median", volume_n = 10), yrs, thr)
  a <- res[res$subset == "all" & res$unit == "kg", ]
  expect_equal(a$value[a$group == "conifer"], 2e5)
  expect_equal(a$value[a$group == "non_conifer"], 1e5)
  expect_equal(a$value[a$group == "all_hosts"], 2e5)  # union: the both-line counted once
})

test_that("kg and pc are classified separately and never summed", {
  cn8 <- data.frame(pathway = "plants_for_planting", cn8 = c("S", "N"),
                    commodity_group = "non_conifer", q2_unit = c("stk", "nei"))
  d <- rbind(imp("S", "DK", yrs, kg = 10, pc = 2e6), imp("N", "DK", yrs, kg = 5, pc = NA))
  res <- classify_pathway(d, cn8, "plants_for_planting", "non_conifer", character(),
                          list(volume_stat = "median", volume_n = 10), yrs, thr)
  a <- res[res$subset == "all" & res$role == "ent3", ]
  expect_equal(a$value[a$unit == "kg"], 15)
  expect_equal(a$value[a$unit == "pc"], 2e6)
  expect_equal(a$class[a$unit == "pc"], "medium")
})

test_that("wood is classified in kg only, even when lines report pieces", {
  cn8 <- data.frame(pathway = "wood", cn8 = "P", commodity_group = "both", q2_unit = "stk")
  d <- imp("P", "DE", yrs, kg = 10, pc = 5)
  res <- classify_pathway(d, cn8, "wood", "conifer", character(),
                          list(volume_stat = "median", volume_n = 10), yrs, thr)
  expect_equal(unique(res$unit), "kg")
})

test_that("A/B difference is flagged", {
  cn8 <- data.frame(pathway = "wood", cn8 = "A", commodity_group = "conifer", q2_unit = "nei")
  d <- rbind(imp("A", "DE", yrs, 5e6), imp("A", "US", yrs, 2e5))
  res <- classify_pathway(d, cn8, "wood", "conifer", "US", list(volume_stat = "median", volume_n = 10), yrs, thr)
  expect_true(all(res$a_b_differ[res$subset == "pest_present"]))
})

test_that("Class A (all_hosts, role ent3) is always present, group rows are info", {
  cn8 <- data.frame(pathway = "wood", cn8 = "A", commodity_group = "conifer", q2_unit = "nei")
  res <- classify_pathway(imp("A", "DE", yrs, 5e6), cn8, "wood", "conifer", character(),
                          list(volume_stat = "median", volume_n = 10), yrs, thr)
  expect_setequal(res$role, c("ent3", "info"))
  expect_equal(unique(res$group[res$role == "ent3"]), "all_hosts")
})
