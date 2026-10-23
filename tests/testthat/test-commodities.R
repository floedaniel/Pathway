# Pathways from EPPO host commodities (gd.eppo.int/taxon/{code}/pathwayshosts).

cn8_table <- read_commodity_cn8(file.path(root, "data", "commodity_cn8.csv"))
cmap <- utils::read.csv(file.path(root, "data", "eppo_commodity_map.csv"), colClasses = "character", na.strings = "")
gd_dir <- normalizePath(testthat::test_path("fixtures", "eppo_gd"))
gd_page <- function(code) {
  f <- file.path(gd_dir, paste0(code, "_pathwayshosts.html"))
  readChar(f, file.size(f), useBytes = TRUE)
}

test_that("host-commodity table is parsed into category, host and EPPO code", {
  x <- parse_host_commodities(gd_page("AGRLAX"))
  expect_equal(nrow(x), 6)
  expect_setequal(x$category, c("plants for planting", "squared wood", "non-squared wood", "bark",
                                "manufactured articles", "packaging material"))
  expect_true(all(x$host == "Betula" & x$host_code == "1BETG"))
})

test_that("a page without the table (pest not EPPO/EU-listed) gives an empty list", {
  expect_equal(nrow(parse_host_commodities("<html><body>No data</body></html>")), 0)
  expect_equal(nrow(parse_host_commodities(NULL)), 0)
})

test_that("every EPPO category in the user guide is in the mapping", {
  guide <- c("agricultural machinery", "all commodities", "bark", "bulbs or tubers", "cut flowers or branches",
             "fruits or vegetables", "manufactured articles", "non-squared wood", "packaging material",
             "plants for planting", "plant waste", "pollen", "seeds", "soil/growing medium", "squared wood",
             "stored products")
  expect_setequal(cmap$eppo_category, guide)
  expect_true(all(cmap$finnprio_pathway[cmap$status != "info"] %in% finnprio_pathways$id))
  # every category of a computed pathway has CN8 lines
  cats <- unique(unlist(strsplit(cn8_table$eppo_category, ";")))
  expect_true(all(cmap$eppo_category[cmap$status == "computed"] %in% cats))
})

test_that("AGRLAX: plants for planting and wood, from Betula only", {
  local_eppo_mock()
  s <- test_settings()
  withr::local_dir(root)
  pc <- pathways_from_commodities(parse_host_commodities(gd_page("AGRLAX")), s)
  expect_equal(names(pc$info), c("plants_for_planting", "wood"))
  expect_setequal(pc$info$wood$categories, c("squared wood", "non-squared wood", "bark",
                                             "manufactured articles", "packaging material"))
  expect_equal(pc$info$wood$host_genera$non_conifer, list(list(taxa = c("Betulaceae", "Betula"), rank = "Genus")))
  expect_equal(pc$info$wood$groups, "non_conifer")
  expect_length(pc$manual, 0)
})

test_that("IPSXCA: wood is non-squared only; cut flowers or branches are computed (other plant parts)", {
  local_eppo_mock()
  s <- test_settings()
  withr::local_dir(root)
  pc <- pathways_from_commodities(parse_host_commodities(gd_page("IPSXCA")), s)
  expect_equal(pc$info$wood$categories, "non-squared wood")
  expect_length(pc$manual, 0)
  expect_equal(pc$info$other_plant_parts$categories, "cut flowers or branches")
  o <- pathway_lines(cn8_table, "other_plant_parts", pc$info$other_plant_parts)
  expect_true("06042092_2012" %in% o$cn8)                # Christmas trees (conifer host)
  expect_true("06042099_2015" %in% o$cn8)                # foliage and branches n.e.s.
  expect_false(any(startsWith(o$cn8, "060311")))         # no roses
  w <- pathway_lines(cn8_table, "wood", pc$info$wood)
  expect_true("44032111_2017" %in% w$cn8)                # pine round wood
  expect_false(any(startsWith(w$cn8, "4407")))           # no sawn timber (squared wood)
  expect_false(any(startsWith(w$cn8, "4415")))           # no packaging
  expect_false(any(startsWith(w$cn8, "4403231")))        # no spruce round wood
})

test_that("PHYTRA: 'plants' (2PLAN) means all plants; soil and machinery are information only", {
  local_eppo_mock()
  s <- test_settings()
  withr::local_dir(root)
  pc <- pathways_from_commodities(parse_host_commodities(gd_page("PHYTRA")), s)
  expect_equal(names(pc$info), "plants_for_planting")
  expect_setequal(pc$info$plants_for_planting$groups, c("conifer", "non_conifer"))
  expect_null(pc$info$plants_for_planting$host_genera$non_conifer)  # no genus filter
  expect_setequal(pc$other, c("soil/growing medium", "agricultural machinery"))
})

test_that("bulbs or tubers bring in 0601 only when EPPO lists them", {
  info <- list(groups = "non_conifer", monocot = FALSE, host_genera = NULL, categories = "plants for planting")
  expect_false(any(startsWith(pathway_lines(cn8_table, "plants_for_planting", info)$cn8, "0601")))
  info$categories <- c("plants for planting", "bulbs or tubers")
  info$monocot <- TRUE
  expect_true(any(startsWith(pathway_lines(cn8_table, "plants_for_planting", info)$cn8, "0601")))
})

test_that("single input: an EPPO code or a scientific name", {
  local_eppo_mock(list("tools/name2codes" = '[{"eppocode":"IPSXCA","preferred":true}]'))
  s <- test_settings()
  expect_equal(resolve_input("ipsxca", s), "IPSXCA")
  expect_equal(resolve_input("Ips calligraphus", s), "IPSXCA")
})

test_that("unknown name stops", {
  local_eppo_mock(list("tools/name2codes" = "[]"))
  expect_error(resolve_input("Nonexistus fictus", test_settings()), class = "pathwise_stop_invalid_code")
})

test_that("fallback note when EPPO lists no host commodities", {
  r <- list(pathway_info = list(wood = list(groups = "conifer", categories = NULL, monocot = FALSE,
                                            hosts = tibble::tibble(eppocode = "PIUSY", host_class = "conifer", genus = "Pinus"))))
  expect_match(pathway_notes("wood", r)[1], "EPPO lists no host commodities")
  expect_match(pathway_basis("wood", r), "^Basis: host plants")
})

# Open question 15 (decision 61): CN 4415 is wooden packaging traded as goods; it
# says nothing about packaging that carries a non-wood pest (e.g. ToBRFV on crates).
test_that("packaging material counts as wood only when EPPO also lists a wood commodity", {
  local_eppo_mock()
  s <- test_settings()
  withr::local_dir(root)
  tomato <- tibble::tibble(category = c("plants for planting", "packaging material"), host = "Betula", host_code = "1BETG")
  pc <- pathways_from_commodities(tomato, s)
  expect_false("wood" %in% names(pc$info))
  expect_true("packaging material" %in% pc$other)
  expect_equal(pc$table$status[pc$table$category == "packaging material"], "info")
  wood <- tibble::tibble(category = c("squared wood", "packaging material"), host = "Betula", host_code = "1BETG")
  expect_setequal(pathways_from_commodities(wood, s)$info$wood$categories, c("squared wood", "packaging material"))
})
