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
    "- Give the number with unit kg and the statistic with its years exactly as given. Never recompute.\n",
    "- Cite at most one LAW id exactly as written (e.g. Vedlegg 2, nr. 10). If LAW says NOT FOUND, write ",
    "'not found in FOR-2000-12-01-1333'.\n",
    "- Plain text in ", if (identical(settings$language, "no")) "Norwegian" else "English",
    ", one or two sentences, at most ", settings$writeup_max_chars, " characters. ",
    "No markdown, no bullet points, no quotes, no line breaks."
  )
}

# Labelled input blocks for one computed pathway.
writeup_prompt <- function(r, pathway, max_law = 5) {
  cl <- r$classes[r$classes$pathway == pathway, ]
  ent3 <- cl[cl$role == "ent3", ]
  groups <- cl[cl$role == "info", ]
  law <- r$law[r$law$pest_match, ]
  law_block <- if (nrow(law) == 0) "NOT FOUND (the pest is not named in FOR-2000-12-01-1333)" else
    paste0(utils::head(law$id, max_law), ": ", substr(utils::head(law$text, max_law), 1, 300), collapse = "\n")
  paste0(
    "EPPO\n",
    "Pest: ", r$pest, " (", r$code, ")\n",
    "Host groups: ", paste(r$groups, collapse = ", "), "\n",
    "Present in ", length(r$present_countries), " countries\n\n",
    "SSB (pathway: ", pathway_label(pathway), ")\n",
    "ENT3 class: ", ent3$class, "\n",
    "Imports, all host commodities: ", fmt_num(ent3$kg), " kg per year (", ent3$stat, " ", ent3$years, ")\n",
    "From pest-present origins: ", fmt_num(ent3$kg_pest_present), " kg per year (", fmt_pct(ent3$share_pest_present), ")\n",
    if (nrow(groups) > 1) paste0("By host group: ", paste0(groups$group, " ", fmt_num(groups$kg), " kg", collapse = "; "), "\n"),
    "\nLAW\n", law_block, "\n\n",
    "Write the ENT3 justification."
  )
}

markdown_chars <- "[*#_`|>\\[\\]]"

# Returns character() if the text passes, else the problems found.
validate_writeup <- function(text, r, pathway, settings) {
  ent3 <- r$classes[r$classes$pathway == pathway & r$classes$role == "ent3", ]
  problems <- character()
  if (grepl(markdown_chars, text, perl = TRUE)) problems <- c(problems, "contains markdown characters")
  if (grepl("\n", text)) problems <- c(problems, "contains line breaks")
  if (nchar(text) > settings$writeup_max_chars) problems <- c(problems, paste0("longer than ", settings$writeup_max_chars, " characters"))
  if (!grepl(ent3$class, text, fixed = TRUE)) problems <- c(problems, paste0("does not state the ENT3 class '", ent3$class, "'"))
  other <- setdiff(c("small", "medium", "large"), ent3$class)
  if (any(vapply(other, function(k) grepl(paste0("\\b", k, "\\b"), text), logical(1)))) {
    problems <- c(problems, "names a different ENT3 class")
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
generate_writeup <- function(r, pathway, settings, ask = llm_ask, nonce = 0) {
  if (pathway_type(pathway) == "manual") return(list(text = "", source = "manual", problems = character()))
  system <- writeup_system_prompt(settings)
  prompt <- writeup_prompt(r, pathway)
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
