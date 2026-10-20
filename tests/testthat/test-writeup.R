# Write-up checks (Step 9): fixed layout filled by R, LLM only for regulation lines.
# Fake LLM: no network.

fake_result <- function(law_ids = c("Vedlegg 2, nr. 10", "Vedlegg 3, nr. 1.2", "§ 23"),
                        pest_match = c(TRUE, FALSE, FALSE), origin_rule = c(FALSE, TRUE, FALSE)) {
  list(
    code = "IPSXCA", pest = "Ips calligraphus", groups = "conifer", present_countries = c("US", "MX"),
    classes = tibble::tibble(pathway = "wood", group = c("all_hosts", "conifer"), role = c("ent3", "info"),
                             stat = "median", years = "2016-2025", kg = 1.26e9, class = "large",
                             kg_pest_present = 4.3e5, share_pest_present = 3e-4),
    law = tibble::tibble(id = law_ids, text = paste0("1 | tekst ", seq_along(law_ids)),
                         pest_match = pest_match, origin_rule = origin_rule)
  )
}
fake_present <- tibble::tibble(country = c("US", "MX"), name = c("United States of America (US)", "Mexico (MX)"),
                               kg = c(4.3e5, 0), share = c(3e-4, 0), value_nok = 0,
                               years_with_imports = c(10L, 0L), main_commodity = c("44071121 Trelast av furu", NA))
fake_inputs <- list(
  lines_used = data.frame(cn8 = c("44071121_2017", "44152000_2001")),
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
good_reply <- "Vedlegg 2, nr. 10: Non-European Scolytidae are listed.\nVedlegg 3, nr. 1.2: Conifer chips from named countries are banned."

test_that("text follows the fixed layout with R's numbers, codes and class", {
  s <- test_settings()
  w <- generate_writeup(fake_result(), "wood", s, ask = fixed_ask(good_reply), inputs = fake_inputs)
  lines <- strsplit(w$text, "\n")[[1]]
  expect_equal(lines[1], "ENT3: Large")
  expect_match(w$text, "Annual import into Norway of wood (CN 44): 1260.0 million kg (median 2016-2025).", fixed = TRUE)
  expect_true("Largest CN8 lines (kg per year, share of total):" %in% lines)
  expect_true("44071121: 1200.0 million kg (95%)" %in% lines)
  expect_true("44152000: 60.0 million kg (5%, mixed line)" %in% lines)
  expect_true("Imports from countries where the pest is present (information only, not ENT3):" %in% lines)
  expect_true("United States of America: 430 000 kg per year (median 2016-2025); CN8 lines, total 2016-2025: 44071121 4.3 million kg" %in% lines)
  expect_true("Mexico: no recorded imports" %in% lines)
  expect_true("Regulation:" %in% lines)
  expect_true("Vedlegg 3, nr. 1.2: Conifer chips from named countries are banned." %in% lines)
  expect_equal(w$source, paste0(s$llm$provider, ":", s$llm$model))
})

test_that("ENT3 class in the write-up equals Panel 1 Class A", {
  r <- fake_result()
  r$classes$class <- "medium"
  txt <- writeup_text(r, "wood", fake_inputs, character())
  expect_equal(strsplit(txt, "\n")[[1]][1], "ENT3: Medium")
})

test_that("write-up contains no markdown characters", {
  txt <- writeup_text(fake_result(), "wood", fake_inputs, regulation_fallback(regulation_units(fake_result())))
  expect_false(grepl(markdown_chars, txt, perl = TRUE))
})

test_that("regulation lines must start with an id shown in Panel 3", {
  u <- regulation_units(fake_result())
  expect_length(validate_regulation(strsplit(good_reply, "\n")[[1]], u), 0)
  expect_match(validate_regulation("Vedlegg 9, nr. 99: something.", u), "not given: Vedlegg 9, nr. 99")
  expect_match(paste(validate_regulation("**Vedlegg 2, nr. 10**: x", u), collapse = " "), "markdown")
})

test_that("regulation section covers pest units then origin rules, not host-only units", {
  u <- regulation_units(fake_result())
  expect_equal(u$id, c("Vedlegg 2, nr. 10", "Vedlegg 3, nr. 1.2"))
})

test_that("a rejected reply is retried once, then the Lovdata text is used", {
  s <- test_settings()
  w <- generate_writeup(fake_result(), "wood", s, ask = fixed_ask("# bad", "# still bad"), inputs = fake_inputs)
  expect_equal(w$source, "fallback")
  expect_match(w$text, "Vedlegg 2, nr. 10: tekst 1", fixed = TRUE)
})

test_that("no units: text says not found and no LLM call is made", {
  s <- test_settings()
  r <- fake_result(pest_match = c(FALSE, FALSE, FALSE), origin_rule = c(FALSE, FALSE, FALSE))
  w <- generate_writeup(r, "wood", s, ask = function(...) stop("no call expected"), inputs = fake_inputs)
  expect_equal(w$source, "template")
  expect_match(w$text, "Regulation:\nNot found in FOR-2000-12-01-1333.", fixed = TRUE)
})

test_that("regulation lines are cached per units and model; Regenerate (nonce) bypasses", {
  s <- test_settings()
  n <- 0
  ask <- function(...) { n <<- n + 1; good_reply }
  generate_writeup(fake_result(), "wood", s, ask = ask, inputs = fake_inputs)
  generate_writeup(fake_result(), "wood", s, ask = ask, inputs = fake_inputs)
  expect_equal(n, 1)
  generate_writeup(fake_result(), "wood", s, ask = ask, nonce = 2, inputs = fake_inputs)
  expect_equal(n, 2)
})

test_that("mass formatting: millions with one decimal, smaller numbers in kg", {
  expect_equal(fmt_mass(19119489), "19.1 million kg")
  expect_equal(fmt_mass(99), "99 kg")
  expect_equal(fmt_mass(47234), "47 234 kg")
})

test_that("copy button targets the text box, so copied text equals the box content", {
  b <- copy_button("writeup-text_wood")
  expect_match(as.character(b), 'data-target="writeup-text_wood"', fixed = TRUE)
})
