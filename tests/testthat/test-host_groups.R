# More host groups (decisions 55-56): seed potatoes, food and fodder, other plant
# parts, seeds. Hosts are matched on their EPPO taxonomy (species, genus, family).

cn8_table <- read_commodity_cn8(file.path(root, "data", "commodity_cn8.csv"))
h <- function(taxa, rank = "Species") list(taxa = taxa, rank = rank)
tomato <- h(c("Solanaceae", "Solanum", "Solanum lycopersicum"))
potato <- h(c("Solanaceae", "Solanum", "Solanum tuberosum"))
solanum <- h(c("Solanaceae", "Solanum"), "Genus")
info_of <- function(hosts, categories, groups = "non_conifer") {
  list(groups = groups, monocot = FALSE, host_genera = stats::setNames(list(hosts), groups[1]), categories = categories)
}
food <- function(...) pathway_lines(cn8_table, "food_fodder", info_of(list(...), "fruits or vegetables"))$cn8
codes <- function(x) unique(substr(x, 1, 4))

test_that("species-level hosts: tomato gets tomatoes, not potatoes or aubergines", {
  x <- food(tomato)
  expect_true(all(startsWith(x[startsWith(x, "070")], "0702") | startsWith(x[startsWith(x, "070")], "07099990")) )
  expect_true(any(startsWith(x, "0702")))
  expect_false(any(startsWith(x, "0701")))
  expect_false(any(startsWith(x, "070930")))
  expect_false("07099990_2012" %in% x)                   # tomato has its own line, so not 'vegetables n.e.s.'
})

test_that("a genus-level host (Solanum) gets every Solanum line", {
  x <- food(solanum)
  expect_true(any(startsWith(x, "0701")) && any(startsWith(x, "0702")) && any(startsWith(x, "070930")))
})

test_that("seed potatoes (0701 10) are plants for planting, only for potato", {
  p <- pathway_lines(cn8_table, "plants_for_planting", info_of(list(potato), "plants for planting"))$cn8
  expect_true("07011000_1988" %in% p)
  b <- pathway_lines(cn8_table, "plants_for_planting", info_of(list(h(c("Betulaceae", "Betula"), "Genus")), "plants for planting"))$cn8
  expect_false("07011000_1988" %in% b)
  expect_false("07011000_1988" %in% food(potato))        # not food and fodder
  expect_true(any(startsWith(food(potato), "070190")))   # ware potatoes are
})

test_that("only fresh or chilled food lines: no frozen, dried or preserved products", {
  f <- cn8_table[cn8_table$pathway == "food_fodder", ]
  expect_false(any(grepl("^(0710|0711|0712|0713|0811|0812|0813)", f$cn8)))
  expect_false(any(c("08062000_1988", "08042090_1988", "08011100_1996") %in% f$cn8))   # raisins, dried figs, desiccated coconut
  expect_false(any(startsWith(f$cn8, "07095")))                                         # mushrooms are not plants
})

test_that("residual lines: 'other Allium' for a leek-free Allium host, '* n.e.s.' for hosts without a line", {
  onion <- h(c("Amaryllidaceae", "Allium", "Allium cepa"))
  chinese_chives <- h(c("Amaryllidaceae", "Allium", "Allium tuberosum"))
  expect_false(any(grepl("^0703900[89]", food(onion))))   # onion has its own line, not 'other Allium'
  expect_true(any(grepl("^0703900[89]", food(chinese_chives))))
  annona <- h(c("Annonaceae", "Annona", "Annona cherimola"))
  expect_true("08109090_2002" %in% food(annona))         # fruits n.e.s.
  expect_false("08109090_2002" %in% food(tomato))
})

test_that("citrus: EPPO variety names match, hybrid sign ignored; kumquat is 'other citrus'", {
  orange <- h(c("Rutaceae", "Aurantioideae", "Citrus", "Citrus x aurantium var. sinensis"))
  x <- food(orange)
  expect_true(any(startsWith(x, "080510")))
  expect_false(any(startsWith(x, "080550")))             # no lemons
  expect_false(any(startsWith(x, "080590")))             # orange is excluded from 'other citrus'
  kumquat <- h(c("Rutaceae", "Aurantioideae", "Fortunella"), "Genus")
  expect_true(any(startsWith(food(kumquat), "080590")))
})

test_that("family-level line: orchids count for an orchid host", {
  orchid <- h(c("Orchidaceae", "Phalaenopsis"), "Genus")
  o <- pathway_lines(cn8_table, "other_plant_parts", info_of(list(orchid), "cut flowers or branches"))$cn8
  expect_true(any(startsWith(o, "060313")))
  expect_false(any(startsWith(o, "060311")))
})

test_that("seeds: maize gets maize seed, tomato gets 'vegetable seeds n.e.s.'", {
  maize <- h(c("Poaceae", "Zea", "Zea mays"))
  s1 <- pathway_lines(cn8_table, "seeds", info_of(list(maize), "seeds"))$cn8
  expect_true("10051000_1988" %in% s1)
  expect_false("12099199_1995" %in% s1)
  s2 <- pathway_lines(cn8_table, "seeds", info_of(list(tomato), "seeds"))$cn8
  expect_true("12099199_1995" %in% s2)
  expect_false("10051000_1988" %in% s2)
})

test_that("seed residuals are scoped: tomato gets vegetable seeds n.e.s., not forage or flower seeds", {
  s <- pathway_lines(cn8_table, "seeds", info_of(list(tomato), "seeds"))$cn8
  expect_false(any(c("12092990_1988", "12093000_1988", "12099900_1988") %in% s))
  grass <- h(c("Poaceae", "Bromus"), "Genus")
  g <- pathway_lines(cn8_table, "seeds", info_of(list(grass), "seeds"))$cn8
  expect_true("12092990_1988" %in% g)                    # forage seeds n.e.s.
  expect_false("12099199_1995" %in% g)
})

test_that("overlapping residuals do not cancel out: groundnut seed counts forage seeds n.e.s.", {
  groundnut <- h(c("Fabaceae", "Arachis", "Arachis hypogaea"))
  s <- pathway_lines(cn8_table, "seeds", info_of(list(groundnut), "seeds"))$cn8
  expect_true("12092990_1988" %in% s)
  expect_false("12099900_1988" %in% s)                   # general 'seeds n.e.s.' yields to the scoped residuals
})

test_that("0601 bulbs and tubers do not count for potato (HS chapter 6 notes)", {
  info <- list(groups = "non_conifer", monocot = TRUE, host_genera = list(non_conifer = list(potato)),
               categories = c("plants for planting", "bulbs or tubers"))
  p <- pathway_lines(cn8_table, "plants_for_planting", info)$cn8
  expect_false(any(startsWith(p, "0601")))
  expect_true("07011000_1988" %in% p)
})

test_that("'Only the host?' text works for an empty code table (pest without imports)", {
  expect_length(host_only_text(character(0), character(0), character(0)), 0)
  expect_equal(host_only_text("Solanum lycopersicum", "", ""), "Yes (Solanum lycopersicum)")
  expect_equal(host_only_text("", "upper_bound", "*"), "Partly (not elsewhere specified)")
})

test_that("the hybrid sign is ignored when comparing names", {
  expect_equal(norm_taxon(c("Citrus x limon", "x Citrofortunella microcarpa", "Citrus limon")),
               c("Citrus limon", "Citrofortunella microcarpa", "Citrus limon"))
})

test_that("every food, cut-flower and seed line has an EPPO category of its pathway", {
  x <- cn8_table[cn8_table$pathway %in% c("food_fodder", "other_plant_parts", "seeds"), ]
  cat_of <- c(food_fodder = "fruits or vegetables", other_plant_parts = "cut flowers or branches", seeds = "seeds")
  expect_true(all(x$eppo_category == cat_of[x$pathway]))
})

# Open question 16 (decision 60): 0602 lines follow their customs-tariff wording.
pfp <- function(...) pathway_lines(cn8_table, "plants_for_planting", info_of(list(...), "plants for planting"))$cn8
test_that("potato: seed potatoes and fruit/vegetable plants, not ornamental catch-alls", {
  x <- pfp(potato)
  expect_true(all(c("07011000_1988", "06029092_2016") %in% x))
  expect_false(any(c("06029021_2011", "06029022_2011", "06029098_2016", "06029049_2011", "06022000_2001",
                     "06029043_2011", "06029031_2011") %in% x))
  expect_true("06029010_2011" %in% x)                     # plants without soil n.e.s. still count
})

test_that("named ornamental lines count only for their plants", {
  begonia <- h(c("Begoniaceae", "Begonia"), "Genus")
  x <- pfp(begonia)
  expect_true(all(c("06021021_2001", "06029043_2011", "06029051_2011", "06029032_2011") %in% x))
  expect_false("06029031_2011" %in% x)
  expect_true("06029021_2011" %in% x)                     # ornamental hosts keep the catch-alls
})

test_that("fruit trees, lawn grass and ornamental pepper lines", {
  malus <- h(c("Rosaceae", "Malus"), "Genus")
  expect_true(all(c("06022000_2001", "06029021_2011", "06029092_2016") %in% pfp(malus)))
  expect_false("06022000_2001" %in% pfp(h(c("Rosaceae", "Fragaria", "Fragaria x ananassa"))))
  expect_true("06029091_2011" %in% pfp(h(c("Poaceae", "Lolium", "Lolium perenne"))))
  expect_false("06029091_2011" %in% pfp(malus))
  expect_true("06029044_2011" %in% pfp(h(c("Solanaceae", "Capsicum", "Capsicum annuum"))))
})
