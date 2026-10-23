# The assessor's selection (decision 59): excluded customs codes and hidden
# pathways change the ENT3 amount and class, and every panel follows.

cn8_table <- read_commodity_cn8(file.path(root, "data", "commodity_cn8.csv"))
wood_info <- list(groups = "conifer", monocot = FALSE, host_genera = NULL, categories = NULL)
pallets <- "44152000_2001"
thr <- read_thresholds(file.path(root, "data", "finnprio_thresholds.csv"))

sel_result <- function() {
  lines <- pathway_lines(cn8_table, "wood", wood_info)
  other <- setdiff(lines$cn8, pallets)[1]
  imports <- tibble::tibble(cn8 = rep(c(pallets, other), each = 10), country = "US",
                            year = rep(2016:2025, 2), kg = rep(c(1e7, 1e3), each = 10), value_nok = 0)
  s <- test_settings()
  list(code = "TEST", pest = "Test pest", pathways = c("wood", "plants_for_planting"),
       pathway_info = list(wood = wood_info), imports = imports, years = 2016:2025,
       classes = classify_pathway(imports, cn8_table, "wood", wood_info, s, 2016:2025, thr),
       country_names = c(US = "United States"), present_countries = "US", origin_countries = "US",
       commodities = tibble::tibble(category = character(), pathway = character()),
       other = other, settings = s)
}

test_that("pathway_lines() drops the excluded codes", {
  info <- c(wood_info, list(excluded = pallets))
  expect_true(pallets %in% pathway_lines(cn8_table, "wood", wood_info)$cn8)
  expect_false(pallets %in% pathway_lines(cn8_table, "wood", info)$cn8)
})

test_that("excluding a code recomputes the ENT3 amount and class", {
  r <- sel_result()
  ent3 <- function(x) x$classes[x$classes$pathway == "wood" & x$classes$role == "ent3", ]
  expect_equal(ent3(r)$class, "large")
  s <- apply_selection(r, list(wood = pallets), character(), cn8_table, r$settings, thr)
  expect_equal(ent3(s)$kg, 1e3)
  expect_equal(ent3(s)$class, "small")
  expect_equal(nrow(s$classes), nrow(r$classes))
  expect_equal(s$excluded, list(wood = pallets))
})

test_that("no selection leaves the result unchanged; hidden pathways are dropped", {
  r <- sel_result()
  expect_identical(apply_selection(r, list(), character(), cn8_table, r$settings, thr)$classes, r$classes)
  s <- apply_selection(r, list(), "plants_for_planting", cn8_table, r$settings, thr)
  expect_equal(s$pathways, "wood")
  expect_equal(s$hidden, "plants_for_planting")
})

test_that("the write-up lists the excluded codes", {
  r <- sel_result()
  s <- apply_selection(r, list(wood = pallets), character(), cn8_table, r$settings, thr)
  expect_match(excluded_text(s, "wood", cn8_table), "Excluded by the assessor: .*\\(44152000\\)")
  expect_length(excluded_text(r, "wood", cn8_table), 0)
})

test_that("host-specific codes: those naming a host taxon without other species", {
  expect_equal(host_specific(c("Betula", NA, "Allium", "Rosaceae"), c("", "", "Allium cepa", "")),
               c(TRUE, FALSE, FALSE, FALSE))
})

test_that("Panel 1 events: one code, all, none, host-specific, pathway switch", {
  s0 <- list(run_id = 1, excluded = list(), hidden = character())
  lines <- tibble::tibble(cn8 = c("a", "b", "c"), genus = c("Betula", NA, "Rosaceae"), excludes_genera = "")
  ev <- function(action, code = "", on = TRUE) list(action = action, pathway = "wood", code = code, on = on)
  s <- update_selection(s0, ev("code", "b", FALSE), lines)
  expect_equal(s$excluded$wood, "b")
  expect_equal(update_selection(s, ev("code", "b", TRUE), lines)$excluded$wood, character())
  expect_equal(update_selection(s0, ev("none"), lines)$excluded$wood, c("a", "b", "c"))
  expect_equal(update_selection(s, ev("all"), lines)$excluded$wood, character())
  expect_equal(update_selection(s0, ev("host"), lines)$excluded$wood, c("b", "c"))
  h <- update_selection(s0, ev("pathway", on = FALSE), lines)
  expect_equal(h$hidden, "wood")
  expect_equal(update_selection(h, ev("pathway", on = TRUE), lines)$hidden, character())
})

test_that("Panel 1 keeps excluded codes in the code table, out of the totals", {
  r <- sel_result()
  s <- apply_selection(r, list(wood = pallets), character(), cn8_table, r$settings, thr)
  t <- pathway_tables("wood", s, cn8_table, r$settings)
  expect_equal(t$total_kg, 1e4)
  expect_false(t$code$kept[t$code$key == pallets])
  expect_true(is.na(t$code$share[t$code$key == pallets]))
  expect_equal(t$country$kg_total, 1e4)
  ui <- as.character(panel1_ui(s, s$pathways, cn8_table, r$settings, "table-sel"))
  expect_match(ui, "text-decoration-line-through")
  expect_match(ui, "excluded 1 customs code")
})
