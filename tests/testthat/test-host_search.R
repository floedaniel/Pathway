# Searching all SSB 08801 labels for the host plants (Latin and Norwegian names).

names_no <- utils::read.csv(file.path(root, "data", "genus_names_no.csv"), colClasses = "character", encoding = "UTF-8")
labels <- tibble::tibble(
  cn8 = c("44079600_2017", "44123301_2017", "44123309_2017", "87149200_1988", "44032311_2017", "08092100_2012", "44089009_2016"),
  label = c("Trelast av bjørk, saget el kuttet i lengderetningen (2017-) (M1=kg,  M2=m³)",
            "Kryssfiner av trefinerplater, med minst ett utvendig lag av bjørk (2017-) (M1=kg,  M2=m³)",
            "Kryssfiner av trefinerplater, med minst ett utvendig lag av lauvtre, unntatt bjørk (2017-) (M1=kg,  M2=m³)",
            "Felger og eiker til sykler (1988-) (M1=kg,  M2=nei)",
            "Massevirke av gran, befridd for bark (2017-) (M1=kg,  M2=m³)",
            "Surkirsebær (Prunus cerasus), friske (2012-) (M1=kg,  M2=nei)",
            "Plater for finering av bjørk (2016-2016) (M1=kg,  M2=m³)"),
  q1_unit = "kg")
counted <- list(wood = c("44079600_2017"))

test_that("host terms are the Latin genus plus its Norwegian names", {
  expect_setequal(host_search_terms("Betula", names_no), c("Betula", "bjørk"))
  expect_true(all(c("Prunus", "kirsebær", "kirsebærtre") %in% host_search_terms("Prunus", names_no)))
})

test_that("search finds birch lines by Norwegian name, marks exclusions and what is counted", {
  h <- host_search(labels, "Betula", names_no, counted, first_year = 2016)
  expect_setequal(h$cn8, c("44079600_2017", "44123301_2017", "44123309_2017", "44089009_2016"))
  expect_equal(h$status[h$cn8 == "44079600_2017"], "counted: Wood")
  expect_equal(h$status[h$cn8 == "44123309_2017"], "label excludes the host")
  expect_equal(h$status[h$cn8 == "44123301_2017"], "not counted: heading 4412 is outside the app's CN8 scope")
  expect_equal(h$code[h$cn8 == "44123301_2017"], "44123301")
})

test_that("whole words only: 'eik' does not match 'eiker' (bicycle spokes)", {
  h <- host_search(labels, "Quercus", names_no, counted, first_year = 2016)
  expect_false("87149200_1988" %in% h$cn8)
})

test_that("Latin names in labels are found too (Prunus cerasus)", {
  h <- host_search(labels, "Prunus", names_no, counted, first_year = 2016)
  expect_true("08092100_2012" %in% h$cn8)
})

test_that("lines that ended before the table window are left out", {
  h <- host_search(labels, "Betula", names_no, counted, first_year = 2017)
  expect_false("44089009_2016" %in% h$cn8)
})

test_that("a line excludes the host only when every matched host term is excluded", {
  lab <- tibble::tibble(cn8 = "44071121_2022", label = "Uimpregnert skurlast av furu, unntatt blanding av gran, furu, edelgran (2022-) (M1=kg,  M2=m³)")
  h <- host_search(lab, c("Pinus", "Abies"), names_no, list(), first_year = 2016)
  expect_match(h$status, "^not counted")
})

test_that("'ikke dekkfiner furu/gran' is recognised as an exclusion", {
  lab <- tibble::tibble(cn8 = "44123909_2007", label = "Kryssfiner, ikke dekkfiner furu/gran el utvendig lag av lauvtre (2007-2016) (M1=kg,  M2=m³)")
  expect_equal(host_search(lab, "Picea", names_no, list(), first_year = 2016)$status, "label excludes the host")
})

test_that("two SSB versions of one code are told apart by their validity", {
  lab <- tibble::tibble(cn8 = c("44091091_1988", "44091091_2023"),
                        label = c("Profilert trelast av furu (1988-2022) (M1=kg,  M2=m³)", "Profilert trelast av furu (2023-) (M1=kg,  M2=nei)"))
  h <- host_search(lab, "Pinus", names_no, list(), first_year = 2016)
  expect_setequal(h$valid, c("1988-2022", "2023-"))
})
