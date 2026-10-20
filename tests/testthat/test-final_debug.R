# Regression tests for bugs found in the final debugging sweep (2026-10-20).

lineage <- function(...) {
  n <- c(...)
  tibble::tibble(eppocode = paste0("X", seq_along(n)), prefname = n, level = seq_along(n), type = "Not Specified")
}

test_that("EPPO commodity groups other than 'plants' are plants, not 'not_plant'", {
  expect_equal(classify_host(lineage("Commodity groups", "woody plants"))$host_class, "all_plants")
  expect_equal(classify_host(lineage("Commodity groups", "plants"))$host_class, "all_plants")
  for (g in c("vegetable plants", "fruit plants", "herbaceous plants", "herbaceous ornamental plants")) {
    expect_equal(classify_host(lineage("Commodity groups", g))$host_class, "non_conifer")
  }
  expect_equal(classify_host(lineage("Commodity groups", "some new group"))$host_class, "all_plants")
})

test_that("generic non-conifer groups are not genus-filtered", {
  ch <- tibble::tibble(eppocode = "2VEGP", prefname = "vegetable plants", class_label = "x",
                       family = NA_character_, genus = NA_character_, host_class = "non_conifer", monocot = FALSE)
  expect_equal(run_groups(ch), "non_conifer")
  expect_null(host_genera_by_group(ch)$non_conifer)
})

test_that("an EPPO list without any pathway category (soil only) falls back to host plants", {
  local_eppo_mock()
  s <- test_settings()
  withr::local_dir(root)
  hosts <- classify_hosts(eppo_selected_hosts("AGRLAX", s), s)
  soil <- tibble::tibble(category = "soil/growing medium", host = "plants", host_code = "2PLAN")
  ch <- choose_pathways(soil, hosts, s)
  expect_equal(ch$source, "host_plants")
  expect_setequal(names(ch$pc$info), fallback_pathways)
  expect_equal(ch$pc$other, "soil/growing medium")
})

test_that("a deactivated EPPO code follows EPPO's replacement", {
  local_eppo_mock(list("taxons/taxon/OLDCOD/overview" =
    '{"eppocode":"OLDCOD","prefname":"Old","is_active":false,"replacedby":"IPSXCA","datatype":"GAI"}'))
  expect_equal(resolve_input("OLDCOD", test_settings()), "IPSXCA")
})

test_that("short tariff labels: leaf text, with its parent when the leaf is generic", {
  expect_equal(tariff_short("A > stiklinger til bruk i gartnerier: > andre"), "stiklinger til bruk i gartnerier, andre")
  expect_equal(tariff_short("A > andre: > av bjørk (Betula spp.)"), "av bjørk (Betula spp.)")
  expect_equal(tariff_short("A > med klump av jord: > ellers: > andre"), "med klump av jord, ellers, andre")
  # item text that starts with a generic word also gets its parent (06029049 vs 06029039)
  expect_equal(tariff_short("A > potteplanter, i blomst: > ellers, også i samplantinger"),
               "potteplanter, i blomst, ellers, også i samplantinger")
})

test_that("spruce lines are fir AND spruce in the tariff (Abies, Picea); residual conifer lines exclude both", {
  cn8 <- read_commodity_cn8(file.path(root, "data", "commodity_cn8.csv"))
  w <- cn8_lines(cn8, "wood", "conifer", host_genera = list(conifer = c("Abies", "Pinaceae")))
  expect_true(all(c("44032311_2017", "44071221_2022") %in% w$cn8))  # fir/spruce round and sawn wood
  expect_false("44032501_2017" %in% w$cn8)                          # other conifers excl. pine, fir, spruce
  expect_false("44032111_2017" %in% w$cn8)                          # pine
})

test_that("ssb_imports works even when a global variable named c exists (do.call(c, ...) bug)", {
  s <- test_settings()
  assign("c", "AGRLAX", envir = globalenv())
  on.exit(rm("c", envir = globalenv()), add = TRUE)
  meta <- list(cn8 = tibble::tibble(cn8 = "A_2017", label = "", q1_unit = "kg"))
  fake <- function(cn8, years) tibble::tibble(cn8 = cn8, country = "DE", year = 2025L, kg = 1, value_nok = 1)
  x <- ssb_imports(data.frame(cn8 = "A_2017", commodity_group = "conifer"), 2016:2025, s, meta = meta, fetch_chunk = fake)
  expect_s3_class(attr(x, "fetched_at"), "POSIXct")
})
