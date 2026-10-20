cn8_table <- read_commodity_cn8(file.path(root, "data", "commodity_cn8.csv"))
law_fixture <- normalizePath(testthat::test_path("fixtures", "lovdata", "sf-20001201-1333.xml"))
law_fixture_idx <- function(settings) law_parse(law_fixture)

test_that("pathways: FinnPRIO ids, without hitchhiking / natural spread / intentional introduction", {
  expect_setequal(finnprio_pathways$id, c("plants_for_planting", "wood", "seeds", "food_fodder", "other_plant_parts"))
  expect_setequal(computed_pathways, c("plants_for_planting", "wood"))
})

test_that("an empty selection is blocked", {
  expect_error(validate_pathways(character()), class = "pathwise_stop_no_pathway")
  expect_silent(validate_pathways(finnprio_pathways$id))
})

test_that("pre-selection: plants for planting always, wood with a conifer host", {
  expect_equal(default_pathways("non_conifer"), "plants_for_planting")
  expect_equal(default_pathways(c("conifer", "non_conifer")), c("plants_for_planting", "wood"))
})

test_that("a monocot host brings in 0601; a dicot-only pest does not", {
  mono <- tibble::tibble(eppocode = c("1PLAK", "1MAGP", "1ANGC", "X", "1LILF"),
                         prefname = c("Plantae", "Magnoliophyta", "Angiospermae", "Basal monocotyledons", "Liliaceae"),
                         level = 1:5, type = c("Kingdom", "Phylum", "Class", "Category", "Family"))
  expect_true(classify_host(mono)$monocot)
  dicot <- mono
  dicot$prefname[4] <- "Fabids"
  expect_false(classify_host(dicot)$monocot)

  with_mono <- cn8_lines(cn8_table, "plants_for_planting", "non_conifer", monocot = TRUE)
  without <- cn8_lines(cn8_table, "plants_for_planting", "non_conifer", monocot = FALSE)
  expect_true(all(c("06011000_2015", "06012000_2001") %in% with_mono$cn8))
  expect_false(any(startsWith(without$cn8, "0601")))
  expect_false(any(startsWith(cn8_lines(cn8_table, "plants_for_planting", "conifer", monocot = TRUE)$cn8, "0601")))
})

test_that("manual pathways only: no SSB call, no classes, empty draft", {
  local_eppo_mock()  # SSB requests would hit the mock and fail with 400
  s <- test_settings()
  withr::local_dir(root)  # pipeline reads data/ relative to the project root
  r <- run_assessment("IPSXCA", s, pathways = "seeds", cn8_table = cn8_table, law_index_fn = law_fixture_idx)
  expect_null(r$imports)
  expect_null(r$classes)
  expect_equal(writeup_draft(r, "seeds", s), "")
})
