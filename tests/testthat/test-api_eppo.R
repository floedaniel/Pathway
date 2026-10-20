test_that("valid code passes, unknown and malformed codes stop", {
  local_eppo_mock()
  s <- test_settings()
  expect_equal(eppo_validate_code(" ipsxca ", s)$prefname, "Ips calligraphus")
  expect_error(eppo_validate_code("NOSUCH", s), class = "pathwise_stop_invalid_code")
  expect_error(eppo_validate_code("not a code", s), class = "pathwise_stop_invalid_code")
})

test_that("inactive code stops", {
  local_eppo_mock(list("taxons/taxon/OLDCOD/overview" =
    '{"eppocode":"OLDCOD","prefname":"Old","is_active":false,"datatype":"GAI"}'))
  expect_error(eppo_validate_code("OLDCOD", test_settings()), class = "pathwise_stop_invalid_code")
})

test_that("hosts are filtered to the configured classes", {
  local_eppo_mock()
  s <- test_settings()
  h <- eppo_selected_hosts("PHYTRA", s)
  expect_true(all(h$class_label %in% c("Major host", "Host")))
  expect_gt(nrow(h), 100)
  expect_false(any(c("Experimental", "Wild/Weed") %in% h$class_label))
})

test_that("no hosts and no major/host-class hosts stop", {
  local_eppo_mock(list(
    "taxons/taxon/NOHOST/hosts" = "[]",
    "taxons/taxon/EXPONL/hosts" = '[{"eppocode":"PIUSY","prefname":"Pinus sylvestris","class_id":10,"class_label":"Experimental"}]'
  ))
  s <- test_settings()
  expect_error(eppo_selected_hosts("NOHOST", s), "No hosts registered in EPPO for NOHOST",
               class = "pathwise_stop_no_hosts")
  expect_error(eppo_selected_hosts("EXPONL", s), class = "pathwise_stop_no_main_minor")
})

test_that("present countries keep 'Present' statuses only", {
  local_eppo_mock()
  s <- test_settings()
  cc <- eppo_present_countries("IPSXCA", s)
  expect_true("US" %in% cc)
  expect_false("ZA" %in% cc)  # status F: Absent, intercepted only
  expect_false(is.null(attr(cc, "fetched_at")))
})

test_that("no present records stop", {
  local_eppo_mock(list(
    "taxons/taxon/ABSENT/distribution" = '[{"country_iso":"NO","state_id":null,"peststatus":"E","yr_situation":null}]'
  ))
  expect_error(eppo_present_countries("ABSENT", test_settings()), class = "pathwise_stop_no_distribution")
})

test_that("responses are cached on disk", {
  s <- test_settings()
  local_eppo_mock()
  first <- eppo_overview("IPSXCA", s)
  local_eppo_mock(list("taxons/taxon/IPSXCA/overview" = '{"prefname":"changed","is_active":true}'))
  expect_equal(eppo_overview("IPSXCA", s)$prefname, first$prefname)
})
