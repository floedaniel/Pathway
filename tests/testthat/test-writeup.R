# Write-up checks (Step 9), with a fake LLM: no network.

fake_result <- function(law_ids = c("Vedlegg 2, nr. 10", "§ 23"), pest_match = c(TRUE, FALSE),
                        origin_rule = c(FALSE, FALSE)) {
  list(
    code = "IPSXCA", pest = "Ips calligraphus", groups = "conifer", present_countries = c("US", "MX"),
    classes = tibble::tibble(pathway = "wood", group = c("all_hosts", "conifer"), role = c("ent3", "info"),
                             stat = "median", years = "2016-2025", kg = 1.26e9, class = "large",
                             kg_pest_present = 4.3e5, share_pest_present = 3e-4),
    law = tibble::tibble(id = law_ids, text = "x", pest_match = pest_match, origin_rule = origin_rule)
  )
}
fake_present <- tibble::tibble(country = c("US", "MX"), name = c("USA (US)", "Mexico (MX)"), kg = c(4.3e5, 0),
                               share = c(3e-4, 0), value_nok = 0, years_with_imports = c(10L, 0L),
                               main_commodity = c("44071121 Trelast av furu", NA))
fake_inputs <- list(
  lines = tibble::tibble(cn8 = c("44071121_2017", "44152000_2001"), code = c("44071121", "44152000"),
                         description = c("Uimpregnert skurlast av furu", "Laste-, kassepaller"),
                         flags = c("", "upper_bound"), kg = c(1.2e9, 6e7), kg_pest_present = c(4.3e5, 0),
                         share = c(0.95, 0.05)),
  present = fake_present,
  present_lines = tibble::tibble(country = "US", cn8 = "44071121_2017", kg_total = 4.3e6, years_with_imports = 10L)
)

fixed_ask <- function(...) {
  replies <- list(...)
  i <- 0
  function(settings, system, prompt) { i <<- i + 1; replies[[min(i, length(replies))]] }
}

ok_text <- paste("ENT3: large. Wood imports of all host commodities were 1 260 000 000 kg per year (median 2016-2025),",
                 "of which 430 000 kg per year from the USA, a small share. Regulated: Vedlegg 2, nr. 10.")

test_that("prompt contains labelled blocks, the numbers and pest-present countries", {
  p <- writeup_prompt(fake_result(), "wood", fake_inputs)
  for (k in c("EPPO\n", "SSB (pathway: Wood, source SSB table 08801)", "LINES", "PRESENT",
              "LAW (units naming the pest)", "ORIGIN RULES")) {
    expect_match(p, k, fixed = TRUE)
  }
  expect_match(p, "ENT3 class: large", fixed = TRUE)
  expect_match(p, "1 260 000 000 kg per year (median 2016-2025)", fixed = TRUE)
  expect_match(p, "USA (US): 430 000 kg per year; by CN8 line, total over 2016-2025: 44071121 4 300 000 kg in 10 of 10 years", fixed = TRUE)
  expect_match(p, "44152000 Laste-, kassepaller: 60 000 000 kg per year (5 % of total), mixed line", fixed = TRUE)
  expect_match(p, "No recorded imports from: Mexico (MX)", fixed = TRUE)
  expect_match(p, "Vedlegg 2, nr. 10:", fixed = TRUE)
})

test_that("LAW block says NOT FOUND when no unit names the pest; origin rules are listed", {
  r <- fake_result(law_ids = c("Vedlegg 3, nr. 11", "§ 23"), pest_match = c(FALSE, FALSE), origin_rule = c(TRUE, FALSE))
  p <- writeup_prompt(r, "wood", fake_inputs)
  expect_match(p, "NOT FOUND", fixed = TRUE)
  expect_match(p, "ORIGIN RULES (units naming a host genus and a pest-present country)\nVedlegg 3, nr. 11:", fixed = TRUE)
})

test_that("a valid reply is accepted and labelled with the model", {
  s <- test_settings()
  w <- generate_writeup(fake_result(), "wood", s, ask = fixed_ask(ok_text), inputs = fake_inputs)
  expect_equal(w$text, ok_text)
  expect_equal(w$source, paste0(s$llm$provider, ":", s$llm$model))
})

test_that("write-up contains no markdown characters", {
  bad <- "**ENT3: large.** 1 260 000 000 kg (median 2016-2025)."
  expect_true("contains markdown characters" %in% validate_writeup(bad, fake_result(), "wood", test_settings()))
})

test_that("every cited paragraph id exists in Panel 3", {
  s <- test_settings()
  txt <- "ENT3: large. 1 260 000 000 kg/yr (median 2016-2025). Regulated: Vedlegg 4A, nr. 99."
  expect_match(paste(validate_writeup(txt, fake_result(), "wood", s), collapse = " "), "not in Panel 3")
  ok <- "ENT3: large. 1 260 000 000 kg/yr (median 2016-2025). See § 23."
  expect_length(validate_writeup(ok, fake_result(), "wood", s), 0)
})

test_that("ENT3 class in the write-up equals Panel 1 Class A; 'a small share' elsewhere is fine", {
  s <- test_settings()
  expect_true(any(grepl("does not state", validate_writeup("Imports are 5 kg.", fake_result(), "wood", s))))
  expect_true(any(grepl("different ENT3 class", validate_writeup("ENT3 is medium. 5 kg.", fake_result(), "wood", s))))
  expect_length(validate_writeup(ok_text, fake_result(), "wood", s), 0)
})

test_that("a rejected reply is retried once, then the deterministic draft is used", {
  s <- test_settings()
  w <- generate_writeup(fake_result(), "wood", s, ask = fixed_ask("# bad", "# still bad"), inputs = fake_inputs)
  expect_equal(w$source, "draft")
  expect_equal(w$text, writeup_draft(fake_result(), "wood", s, fake_inputs))
  w2 <- generate_writeup(fake_result(), "wood", s, ask = fixed_ask("# bad", ok_text), nonce = 1, inputs = fake_inputs)
  expect_false(w2$source == "draft")
})

test_that("replies are cached per prompt and model; Regenerate (nonce) bypasses", {
  s <- test_settings()
  n <- 0
  ask <- function(...) { n <<- n + 1; ok_text }
  generate_writeup(fake_result(), "wood", s, ask = ask, inputs = fake_inputs)
  generate_writeup(fake_result(), "wood", s, ask = ask, inputs = fake_inputs)
  expect_equal(n, 1)
  generate_writeup(fake_result(), "wood", s, ask = ask, nonce = 2, inputs = fake_inputs)
  expect_equal(n, 2)
})

test_that("copy button targets the text box, so copied text equals the box content", {
  b <- copy_button("writeup-text_wood")
  expect_match(as.character(b), 'data-target="writeup-text_wood"', fixed = TRUE)
})

test_that("cited CN8 codes must belong to the pathway", {
  s <- test_settings()
  txt <- "Wood: ENT3 is large. Main line 44071121 and 06029021."
  expect_match(paste(validate_writeup(txt, fake_result(), "wood", s, codes = fake_inputs$lines$code), collapse = " "),
               "CN8 codes not in this pathway: 06029021")
  expect_length(validate_writeup("Wood: ENT3 is large, mainly 44071121.", fake_result(), "wood", s,
                                 codes = fake_inputs$lines$code), 0)
})

test_that("the draft names the pathway, CN8 codes and pest-present countries", {
  d <- writeup_draft(fake_result(), "wood", test_settings(), fake_inputs)
  expect_match(d, "^Wood: ENT3 is large")
  expect_match(d, "44071121", fixed = TRUE)
  expect_match(d, "USA (US) 430 000 kg", fixed = TRUE)
})
