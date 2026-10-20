# ENT3 write-up prompt (Step 9). The LLM gets labelled EPPO, SSB, LINES, PRESENT
# and LAW blocks built from deterministic results and must not recompute or
# reclassify. Every commodity is referred to by its CN8 code, so the text can be
# traced back to SSB table 08801. The reply is checked by validate_writeup(); if it
# fails twice, the deterministic draft (writeup_draft) is used instead.

writeup_system_prompt <- function(settings) {
  paste0(
    "You write the justification for FinnPRIO question ENT3 (volume of host plant commodity ",
    "traded into the PRA area per year) for one pathway. Rules:\n",
    "- Use only the facts in the blocks. Do not add knowledge. Never recompute or round differently.\n",
    "- Start with the pathway name and the ENT3 class exactly as given (one of: small, medium, large, ",
    "none recorded), e.g. 'Wood: ENT3 is large.'\n",
    "- Give the total import with unit kg and the statistic with its years exactly as given.\n",
    "- Name the main CN8 lines behind the total with their 8-digit codes, kg per year and share, as given in LINES.\n",
    "- Say how much comes from countries where the pest is present: per country, the median kg per year, then ",
    "the CN8 line(s) with codes and their totals over the period as given in PRESENT (say that these are totals); ",
    "name countries without recorded imports. If a line is marked 'mixed line', say that it covers many plants ",
    "or products, not only the host.\n",
    "- Mention the regulation: cite LAW and ORIGIN RULE ids exactly as written (e.g. Vedlegg 3, nr. 11), at most ",
    "three, and say what they do (listed pest, ban, requirement). If LAW says NOT FOUND and there is no origin rule, ",
    "write 'not found in FOR-2000-12-01-1333'.\n",
    "- End with one short caveat from NOTES if relevant.\n",
    "- Cite only CN8 codes that appear in LINES or PRESENT.\n",
    "- Plain text in ", if (identical(settings$language, "no")) "Norwegian" else "English",
    ", one paragraph, at most ", settings$writeup_max_chars, " characters. ",
    "No markdown, no bullet points, no quotes, no line breaks."
  )
}

# Deterministic inputs for one pathway: CN8 line summary, pest-present table and
# pest-present imports per country and line.
writeup_inputs <- function(r, pathway, cn8_table = read_commodity_cn8()) {
  ent3 <- r$classes[r$classes$pathway == pathway & r$classes$role == "ent3", ]
  lines <- cn8_lines(cn8_table, pathway, r$groups, r$monocot, r$host_genera)
  span <- as.integer(strsplit(ent3$years, "-")[[1]])
  yrs <- seq(span[1], span[2])
  list(
    lines = line_summary(r$imports, lines, yrs, r$present_countries, ent3$kg),
    present = present_origins(r$imports, lines, yrs, r$present_countries, ent3$kg, r$country_names),
    present_lines = present_by_line(r$imports, lines, yrs, r$present_countries)
  )
}

mixed_line <- function(flags) grepl("upper_bound", flags %||% "")

writeup_prompt <- function(r, pathway, inputs = NULL, max_law = 4, max_lines = 5, max_countries = 6) {
  cl <- r$classes[r$classes$pathway == pathway, ]
  ent3 <- cl[cl$role == "ent3", ]
  groups <- cl[cl$role == "info", ]
  if (is.null(inputs)) inputs <- writeup_inputs(r, pathway)
  ls <- inputs$lines
  present <- inputs$present
  pl <- inputs$present_lines

  lines_block <- if (is.null(ls) || nrow(ls) == 0 || all(ls$kg == 0)) "no imports recorded" else {
    top <- utils::head(ls[ls$kg > 0, ], max_lines)
    paste0(top$code, " ", substr(top$description, 1, 90), ": ", fmt_num(top$kg), " kg per year (",
           fmt_pct(top$share), " of total)", ifelse(mixed_line(top$flags), ", mixed line", ""), collapse = "\n")
  }
  with_imp <- present[present$kg > 0, ]
  n_years <- length(seq(as.integer(substr(ent3$years, 1, 4)), as.integer(substr(ent3$years, 6, 9))))
  country_lines <- function(cc) {
    x <- utils::head(pl[pl$country == cc, ], 3)
    if (nrow(x) == 0) return("")
    fl <- if (!is.null(ls)) ls$flags[match(x$cn8, ls$cn8)] else NA
    paste0("; by CN8 line, total over ", ent3$years, ": ",
           paste0(cn8_code(x$cn8), " ", fmt_num(x$kg_total), " kg in ", x$years_with_imports, " of ", n_years, " years",
                  ifelse(mixed_line(fl), ", mixed line", ""), collapse = "; "))
  }
  present_block <- if (nrow(present) == 0) "no countries" else paste0(
    "Total: ", fmt_num(sum(present$kg)), " kg per year (", fmt_pct(sum(present$share, na.rm = TRUE)),
    " of all imports) from ", nrow(with_imp), " of ", nrow(present), " pest-present countries",
    if (nrow(with_imp) > 0) paste0("\n", paste0(utils::head(paste0(with_imp$name, ": ", fmt_num(with_imp$kg), " kg per year",
      vapply(with_imp$country, country_lines, "")), max_countries), collapse = "\n")),
    if (nrow(present) > nrow(with_imp)) paste0("\nNo recorded imports from: ", paste(present$name[present$kg == 0], collapse = ", ")))

  law <- r$law[r$law$pest_match, ]
  law_block <- if (nrow(law) == 0) "NOT FOUND (the pest is not named in FOR-2000-12-01-1333)" else
    paste0(utils::head(law$id, max_law), ": ", substr(utils::head(law$text, max_law), 1, 300), collapse = "\n")
  rules <- if ("origin_rule" %in% names(r$law)) r$law[r$law$origin_rule, ] else r$law[0, ]
  rules_block <- if (nrow(rules) == 0) "none" else
    paste0(utils::head(rules$id, max_law), ": ", substr(utils::head(rules$text, max_law), 1, 300), collapse = "\n")
  notes <- if (!is.null(r$hosts)) pathway_notes(pathway, r) else character()

  paste0(
    "EPPO\n",
    "Pest: ", r$pest, " (", r$code, ")\n",
    "Host groups: ", paste(r$groups, collapse = ", "), "\n",
    "Present in ", length(r$present_countries), " countries\n\n",
    "SSB (pathway: ", pathway_label(pathway), ", source SSB table 08801)\n",
    "ENT3 class: ", ent3$class, "\n",
    "Imports, all host commodities: ", fmt_num(ent3$kg), " kg per year (", ent3$stat, " ", ent3$years, ")\n",
    if (nrow(groups) > 1) paste0("By host group: ", paste0(groups$group, " ", fmt_num(groups$kg), " kg", collapse = "; "), "\n"),
    "\nLINES (main CN8 lines in the total, median kg per year)\n", lines_block, "\n",
    "\nPRESENT (imports from countries where the pest is present, median kg per year ", ent3$years, ", with CN8 lines)\n",
    present_block, "\n",
    "\nLAW (units naming the pest)\n", law_block, "\n",
    "\nORIGIN RULES (units naming a host genus and a pest-present country)\n", rules_block, "\n",
    if (length(notes)) paste0("\nNOTES\n", paste(notes, collapse = "\n"), "\n"),
    "\nWrite the ENT3 justification."
  )
}

markdown_chars <- "[*#_`|>\\[\\]]"

# Returns character() if the text passes, else the problems found. `codes`: the
# 8-digit CN8 codes the text may cite (NULL = not checked).
validate_writeup <- function(text, r, pathway, settings, codes = NULL) {
  ent3 <- r$classes[r$classes$pathway == pathway & r$classes$role == "ent3", ]
  problems <- character()
  if (grepl(markdown_chars, text, perl = TRUE)) problems <- c(problems, "contains markdown characters")
  if (grepl("\n", text)) problems <- c(problems, "contains line breaks")
  if (nchar(text) > settings$writeup_max_chars) problems <- c(problems, paste0("longer than ", settings$writeup_max_chars, " characters"))
  # The class word right after "ENT3" must be Panel 1's class ("a small share"
  # elsewhere in the text is fine).
  stated <- regmatches(text, regexpr("ENT3[^.;]{0,30}?\\b(small|medium|large|none recorded)\\b", text, perl = TRUE))
  stated <- sub(".*\\b(small|medium|large|none recorded)$", "\\1", stated)
  if (length(stated) == 0) {
    problems <- c(problems, paste0("does not state the ENT3 class '", ent3$class, "'"))
  } else if (stated != ent3$class) {
    problems <- c(problems, paste0("names a different ENT3 class ('", stated, "' instead of '", ent3$class, "')"))
  }
  cited <- regmatches(text, gregexpr("(Vedlegg [0-9]+[A-Z]?, (nr\\. [0-9.]+[a-z]?|rad [0-9]+|avsnitt [0-9]+)|§ ?[0-9]+[a-z]?)", text))[[1]]
  cited <- sub("\\.$", "", cited)
  unknown <- setdiff(cited, r$law$id)
  if (length(unknown) > 0) problems <- c(problems, paste0("cites ids not in Panel 3: ", paste(unknown, collapse = ", ")))
  if (!is.null(codes)) {
    cn <- regmatches(text, gregexpr("(?<![0-9])[0-9]{8}(?![0-9])", text, perl = TRUE))[[1]]
    bad <- setdiff(cn, codes)
    if (length(bad) > 0) problems <- c(problems, paste0("cites CN8 codes not in this pathway: ", paste(bad, collapse = ", ")))
  }
  problems
}

# Generate the write-up for one pathway. Cached per prompt + model; `nonce`
# changes the cache key (Regenerate). One retry with the problems listed;
# then the deterministic draft. Returns list(text, source, problems).
generate_writeup <- function(r, pathway, settings, ask = llm_ask, nonce = 0, inputs = NULL) {
  if (pathway_type(pathway) == "manual") return(list(text = "", source = "manual", problems = character()))
  if (is.null(inputs)) inputs <- writeup_inputs(r, pathway)
  codes <- if (!is.null(inputs$lines)) inputs$lines$code else NULL
  system <- writeup_system_prompt(settings)
  prompt <- writeup_prompt(r, pathway, inputs)
  entry <- cached("llm", list(system, prompt, settings$llm$provider, settings$llm$model, nonce), function() {
    text <- trimws(ask(settings, system, prompt))
    problems <- validate_writeup(text, r, pathway, settings, codes)
    if (length(problems) > 0) {
      retry <- paste0(prompt, "\n\nYour previous answer was rejected: ", paste(problems, collapse = "; "),
                      ". Previous answer: ", text, "\nWrite it again following the rules.")
      text <- trimws(ask(settings, system, retry))
      problems <- validate_writeup(text, r, pathway, settings, codes)
    }
    if (length(problems) > 0) {
      list(text = writeup_draft(r, pathway, settings, inputs), source = "draft", problems = problems)
    } else {
      list(text = text, source = paste0(settings$llm$provider, ":", settings$llm$model), problems = character())
    }
  }, settings)
  entry$value
}
