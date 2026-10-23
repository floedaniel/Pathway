cn8_table <- read_commodity_cn8(file.path(root, "data", "commodity_cn8.csv"))
law_idx <- law_parse(testthat::test_path("fixtures", "lovdata", "sf-20001201-1333.xml"))
aliases <- utils::read.csv(file.path(root, "data", "law_taxon_aliases.csv"), colClasses = "character")

test_that("birch host: birch wood lines in, oak/beech/ash lines out, mixed lines in", {
  hg <- list(non_conifer = "Betula")
  w <- cn8_lines(cn8_table, "wood", "non_conifer", host_genera = hg)
  expect_true(all(c("44039500_2017", "44079600_2017") %in% w$cn8))           # birch round / sawn wood
  expect_false(any(c("44079100_1988", "44039101_2017", "44079500_2007") %in% w$cn8))  # oak, oak, ash
  expect_false("44079900_2017" %in% w$cn8)   # other broadleaved excludes Betula
  expect_true("44011200_2017" %in% w$cn8)    # broadleaved firewood (mixed)
  expect_true("44152000_2001" %in% w$cn8)    # pallets (both)
})

test_that("host families do not switch on residual lines (Betula + Betulaceae)", {
  w <- cn8_lines(cn8_table, "wood", "non_conifer", host_genera = list(non_conifer = c("Betula", "Betulaceae")))
  expect_false("44079900_2017" %in% w$cn8)
})

test_that("06029021 is all trees and shrubs with soil n.e.s. (customs tariff), so it counts for birch", {
  p <- cn8_lines(cn8_table, "plants_for_planting", "non_conifer", host_genera = list(non_conifer = c("Betula", "Betulaceae")))
  expect_true("06029021_2011" %in% p$cn8)
  c <- cn8_lines(cn8_table, "plants_for_planting", "conifer", host_genera = list(conifer = c("Pinus", "Pinaceae")))
  expect_true("06029021_2011" %in% c$cn8)
})

test_that("lines that list genera in the tariff (06021024 Pelargonium) need a host in the list", {
  p <- cn8_lines(cn8_table, "plants_for_planting", "non_conifer", host_genera = list(non_conifer = c("Betula", "Betulaceae")))
  expect_false("06021024_2001" %in% p$cn8)
})

test_that("a host outside the genus-specific lines brings in the residual line", {
  w <- cn8_lines(cn8_table, "wood", "non_conifer", host_genera = list(non_conifer = c("Betula", "Ulmus")))
  expect_true("44079900_2017" %in% w$cn8)
})

test_that("conifer residual lines are judged on conifer hosts only", {
  hg <- list(conifer = "Pinus", non_conifer = "Betula")
  w <- cn8_lines(cn8_table, "wood", c("conifer", "non_conifer"), host_genera = hg)
  expect_true("44032111_2017" %in% w$cn8)    # pine round wood
  expect_false("44032311_2017" %in% w$cn8)   # spruce round wood
  expect_false("44032501_2017" %in% w$cn8)   # other conifers (excl. pine, spruce)
})

test_that("no genus filter when a host is known only above genus (NULL)", {
  all_lines <- cn8_lines(cn8_table, "wood", "non_conifer")
  expect_equal(nrow(cn8_lines(cn8_table, "wood", "non_conifer", host_genera = list(non_conifer = NULL))), nrow(all_lines))
  ch <- tibble::tibble(genus = c("Betula", NA), host_class = "non_conifer")
  expect_null(host_genera_by_group(ch)$non_conifer)
})

test_that("rose and rhododendron plant lines count only for those genera", {
  p <- cn8_lines(cn8_table, "plants_for_planting", "non_conifer", host_genera = list(non_conifer = "Betula"))
  expect_false(any(startsWith(p$cn8, "060240") | startsWith(p$cn8, "060230")))
  r <- cn8_lines(cn8_table, "plants_for_planting", "non_conifer", host_genera = list(non_conifer = "Rosa"))
  expect_true(any(startsWith(r$cn8, "060240")))
})

test_that("Agrilus anxius: genus alone is no hit; birch ban from North America is an origin rule", {
  t <- law_search_terms("Agrilus anxius", c("Agrilus", "Buprestidae"), "Betula", "non_conifer",
                        c("Canada", "USA"), aliases = aliases)
  r <- law_search(law_idx, t)
  expect_setequal(r$id[r$pest_match], c("Vedlegg 1, nr. 2.1", "Vedlegg 4A, nr. 45"))
  expect_true("Vedlegg 3, nr. 11" %in% r$id[r$origin_rule])
  first_rule <- which(r$origin_rule)[1]
  expect_true(all(r$pest_match[seq_len(first_rule - 1)]))  # origin rules right after pest hits
})

test_that("higher taxon counts as '<taxon> spp.'", {
  t <- law_search_terms("Ips calligraphus", c("Ips", "Scolytinae"), groups = "conifer", aliases = aliases)
  expect_true("Vedlegg 2, nr. 10" %in% law_search(law_idx, t)$id[law_search(law_idx, t)$pest_match])
})

test_that("pest-present table lists every present country, also without imports", {
  imp <- tibble::tibble(cn8 = "A", country = c("US", "US", "DE"), year = c(2024L, 2025L, 2025L),
                        kg = c(100, 300, 5000), value_nok = 1)
  lines <- data.frame(cn8 = "A", description = "Trelast av bjørk")
  po <- present_origins(imp, lines, 2024:2025, c("US", "CA"), total_kg = 2700, country_names = c(US = "USA"))
  expect_equal(po$country, c("US", "CA"))
  expect_equal(po$kg, c(200, 0))
  expect_equal(po$years_with_imports, c(2L, 0L))
  expect_equal(po$main_cn8[1], "A")
  expect_equal(po$main_description[1], "Trelast av bjørk")
  expect_equal(po$name, c("USA", "CA"))
})

test_that("graph stacks the largest origin at the bottom (last factor level), Other on top", {
  imp <- tibble::tibble(cn8 = "06029098_2016", country = c("NL", "DK", "SE", "NL"), year = 2025L,
                        kg = c(1000, 10, 100, 1000), value_nok = 0)
  d <- graph_data(imp, cn8_table, "plants_for_planting", list(groups = "conifer"), 2025L, top_n = 2)
  lv <- levels(d$origin)
  expect_equal(lv[1], "Other countries")
  expect_equal(lv[length(lv)], "NL (06029098)")
})

test_that("legend names each country with its largest CN8 codes in the pathway", {
  imp <- tibble::tibble(cn8 = c("06029098_2016", "06029010_2011", "06021029_2001", "06029059_2011", "06029098_2016"),
                        country = c("NL", "NL", "NL", "NL", "DK"), year = 2025L,
                        kg = c(500, 300, 100, 50, 10), value_nok = 0)
  d <- graph_data(imp, cn8_table, "plants_for_planting", list(groups = "conifer"), 2025L,
                  country_names = c(NL = "Netherlands", DK = "Denmark"))
  expect_true("Netherlands (06029098, 06029010, 06021029)" %in% levels(d$origin))  # top 3 codes, largest first
  expect_true("Denmark (06029098)" %in% levels(d$origin))
})

test_that("graph can stack by customs code (largest code at the bottom, rest as 'Other codes')", {
  imp <- tibble::tibble(cn8 = c("06029098_2016", "06029010_2011", "06021029_2001"), country = "NL", year = 2025L,
                        kg = c(500, 300, 100), value_nok = 0)
  d <- graph_data(imp, cn8_table, "plants_for_planting", list(groups = "conifer"), 2025L, top_n = 2, by = "code")
  lv <- levels(d$origin)
  expect_equal(lv[1], "Other codes")
  expect_match(lv[length(lv)], "^06029098 ")
})
