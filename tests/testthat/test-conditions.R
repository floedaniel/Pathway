test_that("stop_app signals a typed, catchable condition", {
  err <- expect_error(stop_app("no_hosts", code = "XYLEFA"), class = "pathwise_stop_no_hosts")
  expect_s3_class(err, "pathwise_stop")
  expect_equal(err$stop_type, "no_hosts")
  expect_equal(conditionMessage(err), "No hosts registered in EPPO for XYLEFA. No assessment possible.")
})

test_that("every stop type has a message, and fields are filled", {
  expect_error(stop_app("unit_mismatch", group = "conifer", unit = "m3"),
               "Unit mismatch for conifer: SSB m3, expected kg or pc.", fixed = TRUE)
  expect_error(stop_app("unreviewed_hosts", hosts = "Fragaria, Solanum"),
               "Unreviewed: Fragaria, Solanum. Add to woody_taxa.csv.", fixed = TRUE)
})

test_that("unknown stop types and missing fields fail loudly", {
  expect_error(stop_app("nope"), "Unknown stop type")
  expect_error(stop_app("no_hosts"), "Missing field 'code'")
})

test_that("check_secrets names the missing env vars for the configured provider", {
  s <- test_settings()
  withr::local_envvar(EPPO_KEY = "", ANTHROPIC_API_KEY = "", HF_TOKEN = "x")
  expect_error(check_secrets(s), "EPPO_KEY, ANTHROPIC_API_KEY", class = "pathwise_stop_missing_secret")

  s$llm$provider <- "hf"
  withr::local_envvar(EPPO_KEY = "x")
  expect_silent(check_secrets(s))

  s$llm$provider <- "other"
  expect_error(check_secrets(s), class = "pathwise_stop_bad_config")
})
