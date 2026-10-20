cn8_table <- read_commodity_cn8(file.path(root, "data", "commodity_cn8.csv"))
law_fixture <- normalizePath(testthat::test_path("fixtures", "lovdata", "sf-20001201-1333.xml"))
law_fixture_idx <- function(settings) law_parse(law_fixture)

test_that("pathway ids are exactly FinnPRIO's", {
  expect_setequal(finnprio_pathways$id, c("seeds", "plants_for_planting", "wood", "food_fodder",
                                          "other_plant_parts", "hitchhiking", "natural_spread",
                                          "intentional_introduction"))
})

test_that("more than five pathways is blocked, none is blocked", {
  expect_error(validate_pathways(finnprio_pathways$id[1:6]), "6 pathways selected; FinnPRIO allows at most 5",
               class = "pathwise_stop_too_many_pathways")
  expect_error(validate_pathways(character()), class = "pathwise_stop_no_pathway")
  expect_silent(validate_pathways(finnprio_pathways$id[1:5]))
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

test_that("a fixed-answer pathway returns non_existent without any SSB call", {
  local_eppo_mock()  # SSB requests would hit the mock and fail with 400
  s <- test_settings()
  withr::local_dir(root)  # pipeline reads data/ relative to the project root
  r <- run_assessment("IPSXCA", s, pathways = "hitchhiking", cn8_table = cn8_table,
                      law_index_fn = law_fixture_idx)
  expect_null(r$imports)
  expect_equal(r$classes$class, "non_existent")
  expect_equal(r$classes$pathway, "hitchhiking")
  expect_equal(writeup_draft(r, "hitchhiking", s), "ENT3: non_existent. Pathway Hitchhiking (FinnPRIO rule).")
})

test_that("manual pathways get no computed class and an empty draft", {
  expect_equal(pathway_type("seeds"), "manual")
  expect_equal(writeup_draft(list(), "seeds", test_settings()), "")
})
