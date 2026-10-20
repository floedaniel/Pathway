# ENT3 write-up (Step 9). The text follows a fixed layout (decided 2026-10-20,
# user's example):
#
#   ENT3: Small
#
#   Annual import into Norway of wood (CN 44, 9406.10) from the countries where
#   the pest is present (Canada, United States of America): 49 145 kg (median
#   2016-2025). Imports from other countries are not counted. The figure covers
#   ... so it is an upper bound.
#
#   Largest CN8 lines (median kg per year, share of total, kg in total):
#   paller ... (44152000): 32 526 kg per year (66%), 327 671 kg in total 2016-2025
#   ...
#   Imports by country:
#   United States of America: 44 932 kg per year (median 2016-2025)
#     paller ... (44152000): 306 069 kg in total 2016-2025
#   No recorded imports from: Mexico
#
# Only imports from countries where the pest is present are used (user's rule,
# 2026-10-20), so the ENT3 class is the class of that total.
#
#   Regulation:
#   Vedlegg 1, nr. 2.1: Agrilus anxius is listed.
#   ...
#   Note: ...
#
# All numbers, classes, codes and notes are filled in by R from the deterministic
# results. The LLM only writes the regulation lines: one short English sentence per
# Lovdata unit saying what it does, from the unit's text. Each line must start with
# an id shown in Panel 3; otherwise the unit text itself is used.

fmt_mass <- function(x) {
  ifelse(is.na(x), "-", ifelse(x >= 1e6, paste0(formatC(x / 1e6, format = "f", digits = 1), " million kg"),
                               paste0(fmt_num(x), " kg")))
}
fmt_share <- function(x) ifelse(is.na(x), "-", ifelse(x > 0 & x < 0.01, "<1%", paste0(round(100 * x), "%")))

pathway_scope <- function(pathway, lines) {
  chapters <- unique(substr(cn8_code(lines$cn8), 1, 4))
  if (pathway == "plants_for_planting") {
    paste0("plants for planting (CN ", paste(sort(intersect(chapters, c("0601", "0602"))), collapse = ", "), ")")
  } else {
    paste0("wood (CN 44", if ("9406" %in% chapters) ", 9406.10" else "", ")")
  }
}

# Deterministic inputs for one pathway: CN8 line summary, the country table (every
# pest-present country) and imports per country and line.
writeup_inputs <- function(r, pathway, cn8_table = read_commodity_cn8()) {
  ent3 <- r$classes[r$classes$pathway == pathway & r$classes$role == "ent3", ]
  lines <- pathway_lines(cn8_table, pathway, pinfo(r, pathway))
  span <- as.integer(strsplit(ent3$years, "-")[[1]])
  yrs <- seq(span[1], span[2])
  list(
    lines_used = lines,
    lines = line_summary(r$imports, lines, yrs, ent3$kg),
    present = present_origins(r$imports, lines, yrs, origin_countries(r), ent3$kg, r$country_names),
    present_lines = present_by_line(r$imports, lines, yrs, origin_countries(r))
  )
}

mixed_line <- function(flags) grepl("upper_bound", flags %||% "")

# Lovdata units the regulation section covers: units naming the pest, then
# origin rules (host genus + pest-present country).
regulation_units <- function(r, max_each = 4) {
  law <- r$law
  if (is.null(law) || nrow(law) == 0) return(law)
  rules <- if ("origin_rule" %in% names(law)) law$origin_rule else rep(FALSE, nrow(law))
  in_order <- function(x) {  # regulation order: vedlegg number, then id; § articles last
    n <- suppressWarnings(as.numeric(sub("^Vedlegg ([0-9]+).*$", "\\1", x$id)))
    x[order(is.na(n), n, x$id), ]
  }
  rbind(in_order(utils::head(law[law$pest_match, ], max_each)), in_order(utils::head(law[rules, ], max_each)))
}

# The full text for one pathway; `regulation` = character vector of lines.
writeup_text <- function(r, pathway, inputs, regulation) {
  ent3 <- r$classes[r$classes$pathway == pathway & r$classes$role == "ent3", ]
  cls <- if (ent3$class %in% finnprio_classes) paste0(toupper(substr(ent3$class, 1, 1)), substring(ent3$class, 2)) else ent3$class
  ls <- inputs$lines
  out <- c(paste0("ENT3: ", cls), "")

  pr <- inputs$present
  where <- if (nrow(pr) <= 5) paste0("the countries where the pest is present (", paste(sort(pr$name), collapse = ", "), ")")
           else paste0("the ", nrow(pr), " countries where the pest is present")
  amount <- if (is.na(ent3$kg)) paste0("no recorded imports (", ent3$years, ")")
            else paste0(fmt_mass(ent3$kg), " (", ent3$stat, " ", ent3$years, ")")
  out <- c(out, paste0("Annual import into Norway of ", pathway_scope(pathway, inputs$lines_used), " from ", where, ": ",
                       amount, ". Imports from other countries are not counted.",
                       if ("NO" %in% r$present_countries) " The pest is also present in Norway.", " The figure covers all ",
                       if (pathway == "wood") "products" else "plants", " in these CN lines, not only host ",
                       if (pathway == "wood") "commodities" else "plants", ", so it is an upper bound."))
  basis <- if (!is.null(r$pathway_info)) pathway_basis(pathway, r) else NULL
  out <- c(out, if (!is.null(basis)) basis, "")

  # "commodity (CN8 code): amount", commodity = short customs-tariff wording
  label_of <- function(cn8) {
    used <- inputs$lines_used
    lab <- if (!is.null(used) && "cn8" %in% names(used)) cn8_short(used)[match(cn8, used$cn8)] else NA
    lab <- ifelse(is.na(lab) & !is.null(ls), ls$description[match(cn8, ls$cn8)], lab)
    ifelse(is.na(lab), "", paste0(lab, " "))
  }
  top <- if (is.null(ls)) ls else utils::head(ls[ls$kg_total > 0, ], 5)
  if (!is.null(top) && nrow(top) > 0) {
    out <- c(out, "Largest CN8 lines (median kg per year, share of total, kg in total):",
             paste0(label_of(top$cn8), "(", top$code, "): ", fmt_mass(top$kg), " per year (", fmt_share(top$share),
                    ifelse(mixed_line(top$flags), ", mixed line", ""), "), ", fmt_mass(top$kg_total),
                    " in total ", ent3$years), "")
  }

  # Countries with imports (largest first, up to max_countries), then the rest in one line
  max_countries <- 10
  pl <- inputs$present_lines
  with_imp <- pr$kg > 0 | pr$country %in% pl$country
  out <- c(out, "Imports by country:")
  shown <- utils::head(which(with_imp), max_countries)
  for (i in shown) {
    out <- c(out, paste0(pr$name[i], ": ", fmt_mass(pr$kg[i]), " per year (median ", ent3$years, ")"))
    x <- utils::head(pl[pl$country == pr$country[i], ], 3)
    if (nrow(x) > 0) out <- c(out, paste0("  ", label_of(x$cn8), "(", cn8_code(x$cn8), "): ",
                                          fmt_mass(x$kg_total), " in total ", ent3$years))
  }
  if (sum(with_imp) > max_countries) out <- c(out, paste0(sum(with_imp) - max_countries,
                                                          " further countries with smaller imports (see Panel 1)."))
  none <- sort(pr$name[!with_imp])
  if (length(none) > 0) out <- c(out, paste0("No recorded imports from: ", if (length(none) <= 15)
    paste(none, collapse = ", ") else paste0(paste(utils::head(none, 15), collapse = ", "), " and ",
                                             length(none) - 15, " more countries")))
  out <- c(out, "", "Regulation:", if (length(regulation)) regulation else "Not found in FOR-2000-12-01-1333.")

  notes <- if (!is.null(r$hosts)) pathway_notes(pathway, r) else character()
  notes <- notes[!grepl("^Bulbs and tubers \\(CN 0601\\) are included", notes)]
  if (length(notes)) out <- c(out, "", paste0("Note: ", notes))
  paste(out, collapse = "\n")
}

regulation_system_prompt <- function() {
  paste0(
    "You summarise units of the Norwegian plant health regulation FOR-2000-12-01-1333 in English. ",
    "For each unit you get its id and Norwegian text. Write exactly one line per unit: the id exactly as given, ",
    "a colon, then one plain sentence (at most 25 words) saying what the unit does: lists the pest, bans ",
    "import of named plants or products from named origins, or sets import requirements. Use only the unit's text. ",
    "No markdown, no bullets, no numbering, no extra lines."
  )
}

regulation_prompt <- function(r, units) {
  paste0("Pest: ", r$pest, " (", r$code, ")\n\nUNITS\n",
         paste0(units$id, ": ", substr(units$text, 1, 400), collapse = "\n"))
}

markdown_chars <- "[*#_`|>\\[\\]]"

# Check the LLM's regulation lines. Returns character() if they pass.
validate_regulation <- function(lines, units) {
  problems <- character()
  if (length(lines) == 0) return("no lines")
  if (any(grepl(markdown_chars, lines, perl = TRUE))) problems <- c(problems, "contains markdown characters")
  ids <- sub(":.*$", "", lines)
  bad <- setdiff(ids, units$id)
  if (length(bad) > 0) problems <- c(problems, paste0("lines must start with a given id; not given: ", paste(bad, collapse = ", ")))
  if (any(nchar(lines) > 300)) problems <- c(problems, "a line is longer than 300 characters")
  problems
}

# Fallback regulation lines: id and the start of the unit text.
regulation_fallback <- function(units) {
  if (nrow(units) == 0) return(character())
  paste0(units$id, ": ", substr(gsub("^[0-9.a-z]+ \\| ", "", units$text), 1, 160),
         ifelse(nchar(units$text) > 160, "...", ""))
}

# Generate the write-up for one pathway. The regulation lines are cached per units
# + model; `nonce` changes the cache key (Regenerate). One retry, then the
# fallback. Returns list(text, source, problems).
generate_writeup <- function(r, pathway, settings, ask = llm_ask, nonce = 0, inputs = NULL) {
  if (pathway_type(pathway) == "manual") return(list(text = "", source = "manual", problems = character()))
  if (is.null(inputs) && is.null(pinfo(r, pathway))) return(list(text = "", source = "not listed", problems = character()))
  if (is.null(inputs)) inputs <- writeup_inputs(r, pathway)
  units <- regulation_units(r)
  if (is.null(units) || nrow(units) == 0) {
    return(list(text = writeup_text(r, pathway, inputs, character()), source = "template", problems = character()))
  }
  system <- regulation_system_prompt()
  prompt <- regulation_prompt(r, units)
  entry <- cached("llm", list(system, prompt, settings$llm$provider, settings$llm$model, nonce), function() {
    read_lines <- function(txt) { x <- trimws(strsplit(txt, "\n")[[1]]); x[nzchar(x)] }
    lines <- read_lines(ask(settings, system, prompt))
    problems <- validate_regulation(lines, units)
    if (length(problems) > 0) {
      lines <- read_lines(ask(settings, system, paste0(prompt, "\n\nYour previous answer was rejected: ",
                                                        paste(problems, collapse = "; "), ". Write it again.")))
      problems <- validate_regulation(lines, units)
    }
    if (length(problems) > 0) list(lines = regulation_fallback(units), source = "fallback", problems = problems)
    else list(lines = lines, source = paste0(settings$llm$provider, ":", settings$llm$model), problems = character())
  }, settings)
  reg <- entry$value
  list(text = writeup_text(r, pathway, inputs, reg$lines), source = reg$source, problems = reg$problems)
}
