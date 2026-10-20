# ENT3 write-up prompt (Step 9). The LLM gets labelled EPPO, SSB and LAW blocks
# built from deterministic results and must not recompute or reclassify.
# The reply is checked by validate_writeup(); if it fails twice, the
# deterministic draft (writeup_draft) is used instead.

writeup_system_prompt <- function(settings) {
  paste0(
    "You write the justification for FinnPRIO question ENT3 (volume of host plant commodity ",
    "traded into the PRA area per year). Rules:\n",
    "- Use only the facts in the EPPO, SSB and LAW blocks. Do not add knowledge.\n",
    "- State the ENT3 class exactly as given in SSB (one of: small, medium, large, none recorded).\n",
    "- Give the import number with unit kg and the statistic with its years exactly as given. Never recompute.\n",
    "- Say how much comes from countries where the pest is present, naming the main ones with kg as given ",
    "in PRESENT, or that none is recorded.\n",
    "- Mention the regulation: cite LAW ids exactly as written (e.g. Vedlegg 2, nr. 10), at most two. ",
    "If an ORIGIN RULE bans or restricts the host from pest-present countries, say so. If LAW says NOT FOUND ",
    "and there is no origin rule, write 'not found in FOR-2000-12-01-1333'.\n",
    "- Plain text in ", if (identical(settings$language, "no")) "Norwegian" else "English",
    ", two to four sentences, at most ", settings$writeup_max_chars, " characters. ",
    "No markdown, no bullet points, no quotes, no line breaks."
  )
}

# Labelled input blocks for one computed pathway. `present` is the table from
# present_origins() (Panel 1); NULL computes it from r.
writeup_prompt <- function(r, pathway, present = NULL, max_law = 4, max_countries = 5) {
  cl <- r$classes[r$classes$pathway == pathway, ]
  ent3 <- cl[cl$role == "ent3", ]
  groups <- cl[cl$role == "info", ]
  if (is.null(present)) present <- writeup_present(r, pathway)
  law <- r$law[r$law$pest_match, ]
  law_block <- if (nrow(law) == 0) "NOT FOUND (the pest is not named in FOR-2000-12-01-1333)" else
    paste0(utils::head(law$id, max_law), ": ", substr(utils::head(law$text, max_law), 1, 300), collapse = "\n")
  rules <- if ("origin_rule" %in% names(r$law)) r$law[r$law$origin_rule, ] else r$law[0, ]
  rules_block <- if (nrow(rules) == 0) "none" else
    paste0(utils::head(rules$id, max_law), ": ", substr(utils::head(rules$text, max_law), 1, 300), collapse = "\n")
  with_imp <- present[present$kg > 0, ]
  present_block <- if (nrow(present) == 0) "no countries" else paste0(
    "Total: ", fmt_num(sum(present$kg)), " kg per year (", fmt_pct(sum(present$share, na.rm = TRUE)), " of all imports) from ",
    nrow(with_imp), " of ", nrow(present), " pest-present countries\n",
    if (nrow(with_imp) > 0) paste0(utils::head(paste0(with_imp$name, ": ", fmt_num(with_imp$kg), " kg per year"), max_countries),
                                   collapse = "\n"),
    if (nrow(present) > nrow(with_imp)) paste0("\nNo recorded imports from: ",
                                               paste(present$name[present$kg == 0], collapse = ", ")))
  paste0(
    "EPPO\n",
    "Pest: ", r$pest, " (", r$code, ")\n",
    "Host groups: ", paste(r$groups, collapse = ", "), "\n",
    "Present in ", length(r$present_countries), " countries\n\n",
    "SSB (pathway: ", pathway_label(pathway), ")\n",
    "ENT3 class: ", ent3$class, "\n",
    "Imports, all host commodities: ", fmt_num(ent3$kg), " kg per year (", ent3$stat, " ", ent3$years, ")\n",
    if (nrow(groups) > 1) paste0("By host group: ", paste0(groups$group, " ", fmt_num(groups$kg), " kg", collapse = "; "), "\n"),
    "\nPRESENT (imports from countries where the pest is present, median kg per year ", ent3$years, ")\n",
    present_block, "\n",
    "\nLAW (units naming the pest)\n", law_block, "\n",
    "\nORIGIN RULES (units naming a host genus and a pest-present country)\n", rules_block, "\n\n",
    "Write the ENT3 justification."
  )
}

writeup_present <- function(r, pathway, cn8_table = read_commodity_cn8()) {
  ent3 <- r$classes[r$classes$pathway == pathway & r$classes$role == "ent3", ]
  lines <- cn8_lines(cn8_table, pathway, r$groups, r$monocot, r$host_genera)
  span <- as.integer(strsplit(ent3$years, "-")[[1]])
  present_origins(r$imports, lines, seq(span[1], span[2]), r$present_countries, ent3$kg, r$country_names)
}

markdown_chars <- "[*#_`|>\\[\\]]"

# Returns character() if the text passes, else the problems found.
validate_writeup <- function(text, r, pathway, settings) {
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
  problems
}

# Generate the write-up for one pathway. Cached per prompt + model; `nonce`
# changes the cache key (Regenerate). One retry with the problems listed;
# then the deterministic draft. Returns list(text, source, problems).
generate_writeup <- function(r, pathway, settings, ask = llm_ask, nonce = 0, present = NULL) {
  if (pathway_type(pathway) == "manual") return(list(text = "", source = "manual", problems = character()))
  system <- writeup_system_prompt(settings)
  prompt <- writeup_prompt(r, pathway, present)
  entry <- cached("llm", list(system, prompt, settings$llm$provider, settings$llm$model, nonce), function() {
    text <- trimws(ask(settings, system, prompt))
    problems <- validate_writeup(text, r, pathway, settings)
    if (length(problems) > 0) {
      retry <- paste0(prompt, "\n\nYour previous answer was rejected: ", paste(problems, collapse = "; "),
                      ". Previous answer: ", text, "\nWrite it again following the rules.")
      text <- trimws(ask(settings, system, retry))
      problems <- validate_writeup(text, r, pathway, settings)
    }
    if (length(problems) > 0) {
      list(text = writeup_draft(r, pathway, settings), source = "draft", problems = problems)
    } else {
      list(text = text, source = paste0(settings$llm$provider, ":", settings$llm$model), problems = character())
    }
  }, settings)
  entry$value
}
