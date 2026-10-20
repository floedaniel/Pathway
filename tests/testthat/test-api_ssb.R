cn8_table <- read_commodity_cn8(file.path(root, "data", "commodity_cn8.csv"))

fake_meta <- function(q1 = "kg") {
  list(cn8 = tibble::tibble(cn8 = c("06029098_2016", "44071221_2022", "44151000_2001"),
                            label = "", q1_unit = c("kg", q1, "kg"), q2_unit = c("stk", "m³", "stk")),
       years = 1988:2025, countries = c(DE = "Tyskland"), updated = "2026-05-15")
}

test_that("CN8 filtering by group and pathway; 'both' lines in every group", {
  con <- cn8_lines(cn8_table, "wood", "conifer")
  non <- cn8_lines(cn8_table, "wood", "non_conifer")
  expect_true(all(con$commodity_group %in% c("conifer", "both")))
  expect_true(all(non$commodity_group %in% c("non_conifer", "both")))
  expect_true("44151000_2001" %in% con$cn8 && "44151000_2001" %in% non$cn8)
  expect_false(any(startsWith(con$cn8, "0602")))
  pfp <- cn8_lines(cn8_table, "plants_for_planting", "conifer")
  expect_setequal(pfp$cn8, c("06021029_2001", "06029010_2011", "06029021_2011", "06029059_2011", "06029098_2016"))
})

test_that("CN8 table has no excluded wood (tropical, treated, agglomerated)", {
  w <- cn8_table[cn8_table$pathway == "wood", ]
  expect_false(any(grepl("tropisk|meranti|teak|^Impregnert|pelleter", w$description) &
                   !grepl("unnt|ikke|i form av|også i briketter", w$description)))
  expect_true(all(nzchar(cn8_table$source)))
})

test_that("window is ssb_years back from the latest complete year", {
  s <- test_settings()
  expect_equal(ssb_window(s, fake_meta()), 2016:2025)
})

test_that("unit other than kg stops", {
  lines <- data.frame(cn8 = "44071221_2022", commodity_group = "conifer")
  expect_error(ssb_check_units(lines, fake_meta(q1 = "m³")),
               "Unit mismatch for conifer: SSB m³, expected kg or pc.", class = "pathwise_stop_unit_mismatch")
})

test_that("imports are chunked, cached, and pc is kept only for stk lines", {
  s <- test_settings()
  calls <- 0
  fake_fetch <- function(cn8, years) {
    calls <<- calls + 1
    tibble::tibble(cn8 = cn8, country = "DE", year = 2025L, kg = 10, value_nok = 100, pc = 5)
  }
  lines <- data.frame(cn8 = c("06029098_2016", "44071221_2022"), commodity_group = "conifer")
  x <- ssb_imports(lines, 2016:2025, s, meta = fake_meta(), fetch_chunk = fake_fetch)
  expect_equal(x$pc[x$cn8 == "06029098_2016"], 5)
  expect_true(is.na(x$pc[x$cn8 == "44071221_2022"]))
  ssb_imports(lines, 2016:2025, s, meta = fake_meta(), fetch_chunk = fake_fetch)
  expect_equal(calls, 1)
})
