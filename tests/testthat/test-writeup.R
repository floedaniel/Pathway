# Write-up checks (Step 9), with a fake LLM: no network.

fake_result <- function(law_ids = c("Vedlegg 2, nr. 10", "§ 23"), pest_match = c(TRUE, FALSE)) {
  list(
    code = "IPSXCA", pest = "Ips calligraphus", groups = "conifer", present_countries = c("US", "MX"),
    classes = tibble::tibble(pathway = "wood", group = c("all_hosts", "conifer"), role = c("ent3", "info"),
                             stat = "median", years = "2016-2025", kg = 1.26e9, class = "large",
                             kg_pest_present = 4.3e5, share_pest_present = 3e-4),
    law = tibble::tibble(id = law_ids, text = "x", pest_match = pest_match)
  )
}

fixed_ask <- function(...) {
  replies <- list(...)
  i <- 0
  function(settings, system, prompt) { i <<- i + 1; replies[[min(i, length(replies))]] }
}

test_that("prompt contains labelled blocks and the deterministic numbers", {
  p <- writeup_prompt(fake_result(), "wood")
  expect_true(all(c("EPPO\n", "SSB (pathway: Wood)", "LAW\n") %in% sapply(c("EPPO\n", "SSB (pathway: Wood)", "LAW\n"), function(k) if (grepl(k, p, fixed = TRUE)) k else "")))
  expect_match(p, "ENT3 class: large", fixed = TRUE)
  expect_match(p, "1 260 000 000 kg per year (median 2016-2025)", fixed = TRUE)
  expect_match(p, "Vedlegg 2, nr. 10:", fixed = TRUE)
})

test_that("LAW block says NOT FOUND when no unit names the pest", {
  p <- writeup_prompt(fake_result(pest_match = c(FALSE, FALSE)), "wood")
  expect_match(p, "NOT FOUND", fixed = TRUE)
})

test_that("a valid reply is accepted and labelled with the model", {
  s <- test_settings()
  ok <- "ENT3: large. Wood imports of all host commodities were 1 260 000 000 kg per year (median 2016-2025). Regulated: Vedlegg 2, nr. 10."
  w <- generate_writeup(fake_result(), "wood", s, ask = fixed_ask(ok))
  expect_equal(w$text, ok)
  expect_equal(w$source, paste0(s$llm$provider, ":", s$llm$model))
})

test_that("write-up contains no markdown characters", {
  s <- test_settings()
  bad <- "**ENT3: large.** 1 260 000 000 kg (median 2016-2025)."
  expect_true("contains markdown characters" %in% validate_writeup(bad, fake_result(), "wood", s))
})

test_that("every cited paragraph id exists in Panel 3", {
  s <- test_settings()
  txt <- "ENT3: large. 1 260 000 000 kg/yr (median 2016-2025). Regulated: Vedlegg 4A, nr. 99."
  expect_match(paste(validate_writeup(txt, fake_result(), "wood", s), collapse = " "), "not in Panel 3")
  ok <- "ENT3: large. 1 260 000 000 kg/yr (median 2016-2025). See § 23."
  expect_length(validate_writeup(ok, fake_result(), "wood", s), 0)
})

test_that("ENT3 class in the write-up equals Panel 1 Class A", {
  s <- test_settings()
  expect_true(any(grepl("does not state", validate_writeup("ENT3: medium. 5 kg.", fake_result(), "wood", s))))
  expect_true(any(grepl("different ENT3 class", validate_writeup("ENT3: large, not small.", fake_result(), "wood", s))))
})

test_that("a rejected reply is retried once, then the deterministic draft is used", {
  s <- test_settings()
  w <- generate_writeup(fake_result(), "wood", s, ask = fixed_ask("# bad", "# still bad"))
  expect_equal(w$source, "draft")
  expect_equal(w$text, writeup_draft(fake_result(), "wood", s))
  w2 <- generate_writeup(fake_result(), "wood", s, ask = fixed_ask("# bad", "ENT3: large. 1 260 000 000 kg/yr (median 2016-2025)."), nonce = 1)
  expect_false(w2$source == "draft")
})

test_that("replies are cached per prompt and model; Regenerate (nonce) bypasses", {
  s <- test_settings()
  n <- 0
  ask <- function(...) { n <<- n + 1; "ENT3: large. 1 260 000 000 kg/yr (median 2016-2025)." }
  generate_writeup(fake_result(), "wood", s, ask = ask)
  generate_writeup(fake_result(), "wood", s, ask = ask)
  expect_equal(n, 1)
  generate_writeup(fake_result(), "wood", s, ask = ask, nonce = 2)
  expect_equal(n, 2)
})

test_that("copy button targets the text box, so copied text equals the box content", {
  b <- copy_button("writeup-text_wood")
  expect_match(as.character(b), 'data-target="writeup-text_wood"', fixed = TRUE)
})
