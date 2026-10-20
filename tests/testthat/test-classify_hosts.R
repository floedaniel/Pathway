classify_pest <- function(code, s) classify_hosts(eppo_selected_hosts(code, s), s)

test_that("conifer-only pest (Ips calligraphus) gives conifer", {
  local_eppo_mock()
  s <- test_settings()
  ch <- classify_pest("IPSXCA", s)
  expect_true(all(ch$host_class == "conifer"))
  expect_equal(run_groups(ch), "conifer")
})

test_that("non-conifer pest (Aromia bungii) gives non_conifer", {
  local_eppo_mock()
  expect_equal(run_groups(classify_pest("AROMBU", test_settings())), "non_conifer")
})

test_that("mixed pest (Phytophthora ramorum) gives both groups", {
  local_eppo_mock()
  expect_setequal(run_groups(classify_pest("PHYTRA", test_settings())), c("conifer", "non_conifer"))
})

test_that("herbaceous host (potato, P. infestans) is non_conifer: no woody test", {
  local_eppo_mock()
  ch <- classify_pest("PHYTIN", test_settings())
  expect_equal(ch$host_class, "non_conifer")
})

test_that("Pinales or Coniferales in the lineage also mean conifer", {
  tax <- tibble::tibble(eppocode = c("1PLAK", "1CONO"), prefname = c("Plantae", "Pinales"),
                        level = 1:2, type = c("Kingdom", "Order"))
  expect_equal(classify_host(tax)$host_class, "conifer")
})

test_that("Ginkgo is non_conifer", {
  tax <- tibble::tibble(eppocode = c("1PLAK", "1GINC"), prefname = c("Plantae", "Ginkgoopsida"),
                        level = 1:2, type = c("Kingdom", "Class"))
  expect_equal(classify_host(tax)$host_class, "non_conifer")
})

test_that("hosts that are not plants stop the run", {
  local_eppo_mock(list(
    "taxons/taxon/FUNGHO/hosts" = '[{"eppocode":"FUNGUS","prefname":"Some fungus","class_id":1,"class_label":"Major host"}]',
    "taxons/taxon/FUNGUS/taxonomy" = '[{"eppocode":"1FUNGK","prefname":"Fungi","level":1,"type":"Kingdom"}]'
  ))
  ch <- classify_pest("FUNGHO", test_settings())
  expect_equal(ch$host_class, "not_plant")
  expect_error(run_groups(ch), class = "pathwise_stop_no_plant_hosts")
})
