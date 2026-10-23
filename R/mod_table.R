# Panel 1: SSB table. Summary first (one row per pathway), then one card per
# computed pathway with the same two tables (by country, by customs code; same
# number columns, total row), then where the host is named in the statistics.
# Imports are only those from countries where the pest is present (user's rule,
# 2026-10-20). Tidy tables, one value per column, kg.

fmt_num <- function(x) ifelse(is.na(x), "-", format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))
fmt_pct <- function(x) ifelse(is.na(x), "-", ifelse(x > 0 & x < 0.01, "<1 %", ifelse(x > 0.99 & x < 1, ">99 %",
                                                                                     paste0(round(100 * x), " %"))))

class_badge <- function(label) {
  colour <- switch(label, small = "success", medium = "warning", large = "danger", "secondary")
  shiny::span(class = paste0("badge text-bg-", colour), label)
}

# Origin countries of a result: where the pest is present, Norway excluded
# (results without the field, e.g. in tests, fall back to the present countries).
origin_countries <- function(r) r$origin_countries %||% setdiff(r$present_countries, "NO")

# Country name from SSB (English); the ISO2 code if SSB has no name.
country_name <- function(code, country_names) {
  nm <- unname(country_names[code])
  ifelse(is.na(nm), code, nm)
}

# Per-country median annual kg and NOK over the statistic years (zero years included).
country_medians <- function(imports, lines, stat_years) {
  x <- imports[imports$cn8 %in% lines$cn8 & imports$year %in% stat_years, ]
  if (nrow(x) == 0) return(tibble::tibble(country = character(), kg = numeric(), value_nok = numeric(),
                                          years_with_imports = integer()))
  per_year <- dplyr::summarise(dplyr::group_by(x, country, year),
                               q = sum(kg, na.rm = TRUE), v = sum(value_nok), .groups = "drop")
  full <- tidyr::complete(per_year, country, year = stat_years, fill = list(q = 0, v = 0))
  dplyr::summarise(dplyr::group_by(full, country), kg = stats::median(q), value_nok = stats::median(v),
                   years_with_imports = sum(q > 0), .groups = "drop")
}

# CN8 code without SSB's version suffix ("06029021_2011" -> "06029021").
cn8_code <- function(x) sub("_.*$", "", x)

# Text of CN8 lines: the customs tariff wording when available (current lines),
# else the SSB label.
cn8_text <- function(lines) {
  t <- lines$tariff_description %||% rep(NA_character_, nrow(lines))
  ifelse(!is.na(t) & nzchar(t), t, lines$description)
}

# Short, readable label from the tariff wording: the item's own text, with its
# parent (and grandparent) when the item text is generic ("andre", "ellers").
generic_tariff_text <- "^(andre|ellers|annet|andre slag|i\\.e\\.n\\.)(,|$)"
tariff_short <- function(x) {
  vapply(x, function(t) {
    if (is.na(t) || !nzchar(t)) return(NA_character_)
    seg <- trimws(sub(":$", "", strsplit(t, " > ", fixed = TRUE)[[1]]))
    out <- seg[length(seg)]
    i <- length(seg)
    while (grepl(generic_tariff_text, seg[i], ignore.case = TRUE) && i > 1) {
      i <- i - 1
      out <- paste0(seg[i], ", ", out)
    }
    out
  }, "", USE.NAMES = FALSE)
}

# Readable label per CN8 line: the unique tariff label from dev/build_commodity_cn8.R
# (`tariff_label`), else the short tariff text, else the SSB label.
cn8_short <- function(lines) {
  n <- nrow(lines)
  lab <- lines$tariff_label %||% rep(NA_character_, n)
  s <- tariff_short(lines$tariff_description %||% rep(NA_character_, n))
  s <- ifelse(!is.na(lab) & nzchar(lab), lab, s)
  ifelse(is.na(s), lines$description %||% rep(NA_character_, n), s)
}

# Description of a CN8 line (cut to `width` characters for display).
cn8_description <- function(cn8, lines, width = Inf) {
  d <- cn8_text(lines)[match(cn8, lines$cn8)]
  ifelse(is.na(d), NA, ifelse(nchar(d) > width, paste0(substr(d, 1, width), "..."), d))
}

# Largest CN8 line per country over the statistic years.
main_line_by_country <- function(imports, lines, stat_years) {
  x <- imports[imports$cn8 %in% lines$cn8 & imports$year %in% stat_years, ]
  top <- dplyr::summarise(dplyr::group_by(x, country, cn8), q = sum(kg, na.rm = TRUE), .groups = "drop")
  top <- top[order(top$country, -top$q), ]
  top <- top[!duplicated(top$country), ]
  stats::setNames(top$cn8, top$country)
}

# Per CN8 line: median annual kg, total kg over the statistic years (sparse lines
# often have a median of 0) and the share of the pathway total.
line_summary <- function(imports, lines, stat_years, total_kg) {
  rows <- lapply(seq_len(nrow(lines)), function(i) {
    l <- lines[i, ]
    q <- annual_totals(imports, l, stat_years)$quantity
    tibble::tibble(cn8 = l$cn8, code = cn8_code(l$cn8), description = cn8_text(l), flags = l$flags,
                   genus = l$genus %||% NA_character_, excludes = l$excludes_genera %||% NA_character_,
                   kg = stats::median(q), kg_total = sum(q))
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(out)
  out$share <- if (is.na(total_kg) || total_kg == 0) NA_real_ else out$kg / total_kg
  out[order(-out$kg, -out$kg_total), ]
}

# Per pest-present country and CN8 line: total kg over the statistic years and the
# number of years with imports. Totals (not medians) so that sparse lines, whose
# median is often 0, still show and add up.
present_by_line <- function(imports, lines, stat_years, present) {
  x <- imports[imports$cn8 %in% lines$cn8 & imports$year %in% stat_years & imports$country %in% present, ]
  if (nrow(x) == 0) return(tibble::tibble(country = character(), cn8 = character(), kg_total = numeric(),
                                          years_with_imports = integer()))
  out <- dplyr::summarise(dplyr::group_by(x, country, cn8), kg_total = sum(kg, na.rm = TRUE),
                          years_with_imports = dplyr::n_distinct(year[kg > 0]), .groups = "drop")
  out <- out[out$kg_total > 0, ]
  out[order(out$country, -out$kg_total), ]
}

# Every pest-present country: median kg/year, share of the pathway total, years with
# imports, and the largest commodity line. Countries without imports get kg = 0.
present_origins <- function(imports, lines, stat_years, present, total_kg, country_names = character()) {
  m <- country_medians(imports, lines, stat_years)
  out <- tibble::tibble(country = present)
  out <- dplyr::left_join(out, m, by = "country")
  out$kg[is.na(out$kg)] <- 0
  out$value_nok[is.na(out$value_nok)] <- 0
  out$years_with_imports[is.na(out$years_with_imports)] <- 0L
  out$share <- if (is.na(total_kg) || total_kg == 0) NA_real_ else out$kg / total_kg
  main <- unname(main_line_by_country(imports, lines, stat_years)[out$country])
  out$main_cn8 <- cn8_code(main)
  out$main_description <- cn8_description(main, lines)
  out$name <- country_name(out$country, country_names)
  out[order(-out$kg, out$name), c("name", "country", "kg", "share", "value_nok", "years_with_imports",
                                  "main_cn8", "main_description")]
}

# Warnings shown under a pathway's tables.
pathway_notes <- function(pathway, r) {
  info <- pinfo(r, pathway)
  if (is.null(info)) return(character())
  cats <- info$categories
  from_eppo <- !is.null(cats)
  n <- character()
  if (!from_eppo) {
    other <- r$other_categories %||% character()
    n <- c(n, if (length(other) > 0) paste0("EPPO lists only ", paste(other, collapse = ", "),
                                           " for this pest (no traded commodity with a CN line): CN8 lines are chosen from its host plants (Major host, Host).")
              else "EPPO lists no host commodities for this pest: CN8 lines are chosen from its host plants (Major host, Host).")
  }
  ignored <- ignored_groups(info$hosts)
  if (length(ignored)) n <- c(n, paste0("EPPO also lists the group '", paste(ignored, collapse = "', '"),
                                        "'. Codes are chosen from the named hosts only; other hosts in the group are not counted, so the amount may be too low."))
  if (pathway == "plants_for_planting") {
    if ("conifer" %in% info$groups) n <- c(n, "Conifer plants for planting only appear in catch-all lines. Class is a loose upper bound.")
    if (from_eppo) {
      if ("bulbs or tubers" %in% cats) n <- c(n, "Bulbs and tubers (CN 0601) are included: EPPO lists them as host commodity.")
    } else {
      n <- c(n, if (isTRUE(info$monocot)) "Bulbs, tubers, corms and rhizomes (CN 0601) are included because at least one host is a monocot."
                else "Bulbs and tubers (CN 0601) are not included: no monocot host. Dicot bulbs and tubers (e.g. Dahlia, Paeonia) are missed.")
    }
    if (any(grepl("^070110", pathway_lines(read_commodity_cn8(), pathway, info)$cn8)))
      n <- c(n, "Seed potatoes (CN 0701 10) are included because a host is potato.")
    n <- c(n, "Soil associated with host plants belongs to this pathway but has no CN line.")
  }
  if (pathway == "food_fodder") {
    n <- c(n, "Only fresh or chilled products are counted; frozen, dried and processed products are not.",
           "Lines for products 'not elsewhere specified' (e.g. other vegetables, other fruit) count for hosts without a line of their own; they are an upper bound.")
    if ("stored products" %in% (r$commodities$category %||% character()))
      n <- c(n, "EPPO also lists stored products for this pest; they are not counted (not fresh or chilled).")
  }
  if (pathway == "other_plant_parts") {
    n <- c(n, "Only fresh cut flowers, foliage and branches (CN 0603, 0604 20) are counted; dried or treated ones are not.",
           "Fresh foliage and branches n.e.s. (0604 20 99) count for any host; Christmas trees (0604 20 92) for fir, spruce, pine and Douglas fir.")
    if ("pollen" %in% (r$commodities$category %||% character()))
      n <- c(n, "EPPO also lists pollen for this pest; it has no customs code.")
  }
  if (pathway == "seeds") {
    n <- c(n, "Seeds are counted in kg (FinnPRIO assesses seeds in pieces; kg by decision).",
           "Seed lines 'not elsewhere specified' (e.g. other vegetable seeds, which include tomato and pepper seed) count for hosts without a line of their own.")
  }
  if (pathway == "wood") {
    n <- c(n, "Genus-specific wood lines (e.g. birch, oak, pine) count only for host genera; mixed lines (firewood, chips, waste, packaging) are an upper bound.")
    if (!from_eppo || "packaging material" %in% cats) n <- c(n, "Wood packaging: CN 4415 only captures packaging traded as goods. Most packaging travels with other products and has no SSB line.")
    if (!from_eppo || "bark" %in% cats) n <- c(n, "Bark is part of this pathway; isolated bark is counted in the wood waste lines (4401 40-49).")
    if ("manufactured articles" %in% cats) n <- c(n, "Manufactured articles: only casks (4416) and prefabricated wooden buildings (9406.10) are counted; other manufactured wood is not.")
  }
  n
}

# Host names of a pathway as EPPO lists them (e.g. "Betula", "Solanum lycopersicum").
host_names <- function(info) {
  sort(unique(stats::na.omit(ifelse(info$hosts$host_class == "all_plants", "all plants",
                                    info$hosts$prefname %||% info$hosts$genus))))
}

# One line under the pathway heading: what the CN8 lines are based on.
pathway_basis <- function(pathway, r) {
  info <- pinfo(r, pathway)
  if (is.null(info)) return(NULL)
  genera <- sort(unique(stats::na.omit(ifelse(info$hosts$host_class == "all_plants", "all plants",
                                              info$hosts$genus %||% info$hosts$prefname))))
  if (is.null(info$categories)) {
    paste0("Basis: host plants (", length(unique(info$hosts$eppocode)), " Major host / Host records).")
  } else {
    paste0("Basis: EPPO host commodities: ", paste(info$categories, collapse = ", "), "; hosts: ",
           paste(host_names(info), collapse = ", "), ".")
  }
}

# Origin countries in words: their names (up to 5) or their number, saying they are
# where the pest is present.
origins_text <- function(r) {
  o <- origin_countries(r)
  if (length(o) <= 5) paste0(paste(sort(country_name(o, r$country_names)), collapse = ", "), " (where the pest is present)")
  else paste0("the ", length(o), " countries where the pest is present")
}

# Codes that name host taxa and nothing else: residual codes ("other Allium") and
# family-level codes cover other species too. "Host-specific codes only" in Panel 1.
host_specific <- function(genus, excludes = "") {
  !is.na(genus) & nzchar(genus %||% "") & (is.na(excludes) | !nzchar(excludes %||% "")) &
    !grepl("aceae(;|$)", genus %||% "")
}

# Does a customs code cover only the host? Yes when the code names a host genus
# (column `genus`, e.g. sawn birch); otherwise it also covers other species
# (pallets, broadleaved firewood, all trees and shrubs ...). Bark status is added.
host_only_text <- function(genus, flags, excludes = "") {
  g <- !is.na(genus) & nzchar(genus %||% "")
  mixed <- g & !host_specific(genus, excludes)
  short <- vapply(strsplit(as.character(ifelse(g, genus, "")), ";"), function(t)
    if (length(t) > 3) paste0(paste(t[1:3], collapse = ", "), " ...") else paste(t, collapse = ", "), "")
  star <- !is.na(excludes) & excludes %in% "*"
  out <- ifelse(g & !mixed, paste0("Yes (", short, ")"),
                ifelse(star, "Partly (not elsewhere specified)",
                       ifelse(g, paste0("Partly (", short, ", also other species)"), "No, also other species")))
  extra <- ifelse(grepl("with_bark", flags %||% ""), ", with bark", ifelse(grepl("debarked", flags %||% ""), ", debarked", ""))
  paste0(out, extra)
}

# ---- Panel 1 layout ----------------------------------------------------------
# 1. Summary: one row per pathway, ENT3 answer first.
# 2. One card per computed pathway with the same two tables, "By country" and
#    "By customs code": same number columns, a total row at the bottom. Shares are
#    of the 10-year total, so they add up to 100 %.
# 3. Where the host is named in the import statistics (own card).

simple_table <- function(header, rows, right = integer(), foot = NULL, row_class = NULL) {
  cell <- function(tag, x, j) tag(class = if (j %in% right) "text-end", x)
  th <- lapply(seq_along(header), function(j) cell(shiny::tags$th, header[j], j))
  shiny::tags$table(class = "table table-sm align-middle mb-1",
    shiny::tags$thead(shiny::tags$tr(th)),
    shiny::tags$tbody(lapply(seq_along(rows), function(i) shiny::tags$tr(class = row_class[i],
      lapply(seq_along(rows[[i]]), function(j) cell(shiny::tags$td, rows[[i]][[j]], j))))),
    if (!is.null(foot)) shiny::tags$tfoot(shiny::tags$tr(class = "fw-semibold table-light",
      lapply(seq_along(foot), function(j) cell(shiny::tags$td, foot[[j]], j)))))
}

bark_subtotal <- function(imports, lines, stat_years) {
  wb <- lines[grepl("with_bark", lines$flags), ]
  if (nrow(wb) == 0) return(NA_real_)
  stats::median(annual_totals(imports, wb, stat_years)$quantity)
}

# What a computed pathway is based on, in words.
based_on <- function(p, r) {
  info <- pinfo(r, p)
  genera <- sort(unique(stats::na.omit(ifelse(info$hosts$host_class == "all_plants", "all plants", info$hosts$genus))))
  if (is.null(info$categories)) paste0("host plants (EPPO lists no host commodities): ", paste(genera, collapse = ", "))
  else paste0("EPPO host commodities: ", paste(info$categories, collapse = ", "), " of ", paste(host_names(info), collapse = ", "))
}

# All numbers for one computed pathway: the ENT3 row, per-country and per-code
# tables over the statistic years, and their totals. Codes the assessor excluded
# stay in the per-code table (kept = FALSE, no share) but not in any total.
pathway_tables <- function(p, r, cn8_table, settings) {
  stat_years <- utils::tail(r$years, settings$volume_n)
  info_all <- pinfo(r, p)
  info_all$excluded <- NULL
  all_lines <- pathway_lines(cn8_table, p, info_all)
  lines <- pathway_lines(cn8_table, p, pinfo(r, p))
  x_all <- r$imports[r$imports$cn8 %in% all_lines$cn8 & r$imports$year %in% stat_years, ]
  x <- x_all[x_all$cn8 %in% lines$cn8, ]
  total_kg <- sum(x$kg, na.rm = TRUE)
  per <- function(by, x) {
    if (nrow(x) == 0) return(tibble::tibble(key = character(), kg = numeric(), kg_total = numeric(),
                                            nok_total = numeric(), years = integer()))
    y <- dplyr::summarise(dplyr::group_by(x, key = .data[[by]], year), q = sum(kg, na.rm = TRUE),
                          v = sum(value_nok, na.rm = TRUE), .groups = "drop")
    y <- tidyr::complete(y, key, year = stat_years, fill = list(q = 0, v = 0))
    out <- dplyr::summarise(dplyr::group_by(y, key), kg = stats::median(q), kg_total = sum(q),
                            nok_total = sum(v), years = sum(q > 0), .groups = "drop")
    out[out$kg_total > 0, ]
  }
  share <- function(v) if (total_kg > 0) v / total_kg else rep(NA_real_, length(v))
  country <- per("country", x)
  country <- country[order(-country$kg_total), ]
  country$share <- share(country$kg_total)
  main <- main_line_by_country(r$imports, lines, stat_years)
  country$main_code <- cn8_code(unname(main[country$key]))
  country$name <- country_name(country$key, r$country_names)
  code <- per("cn8", x_all)
  code$kept <- code$key %in% lines$cn8
  code <- code[order(!code$kept, -code$kg_total), ]
  code$share <- ifelse(code$kept, share(code$kg_total), NA_real_)
  m <- match(code$key, all_lines$cn8)
  code$label <- cn8_short(all_lines)[m]
  code$text <- cn8_text(all_lines)[m]
  code$host_only <- host_only_text(all_lines$genus[m] %||% rep(NA, length(m)), all_lines$flags[m],
                                   all_lines$excludes_genera[m] %||% rep("", length(m)))
  origins <- origin_countries(r)
  years_any <- sum(annual_totals(r$imports, lines, stat_years)$quantity > 0)
  list(ent3 = r$classes[r$classes$pathway == p & r$classes$role == "ent3", ],
       groups = r$classes[r$classes$pathway == p & r$classes$role == "info", ],
       stat_years = stat_years, lines = lines, all_lines = all_lines, country = country, code = code,
       no_imports = sort(country_name(setdiff(origins, country$key), r$country_names)),
       codes_without = sort(cn8_code(setdiff(lines$cn8, code$key))),
       total_kg = total_kg, total_nok = sum(x$value_nok, na.rm = TRUE), years_any = years_any,
       bark = if (p == "wood") bark_subtotal(r$imports, lines, stat_years) else NA)
}

ent3_text <- function(t) {
  if (is.na(t$ent3$kg)) "no recorded imports" else paste0(fmt_num(t$ent3$kg), " kg per year")
}

# Summary: one row per pathway (computed first, then manual ones).
summary_ui <- function(r, tabs, manual, hidden = character()) {
  yrs <- if (length(tabs)) tabs[[1]]$stat_years else r$years
  rows <- lapply(names(tabs), function(p) {
    t <- tabs[[p]]
    b <- based_on(p, r)
    list(shiny::tags$b(pathway_label(p)), paste0(toupper(substr(b, 1, 1)), substring(b, 2)), fmt_num(t$ent3$kg), class_badge(t$ent3$class),
         if (nrow(t$country)) t$country$name[1] else "-",
         if (nrow(t$code)) shiny::span(shiny::tags$code(cn8_code(t$code$key[1])), " ", t$code$label[1]) else "-")
  })
  rows <- c(rows, lapply(manual, function(p) {
    listed <- r$commodities[r$commodities$pathway %in% p, ]
    list(shiny::tags$b(pathway_label(p)),
         if (nrow(listed)) paste0("EPPO host commodities: ", paste(unique(listed$category), collapse = ", ")) else "-",
         "-", shiny::span(class = "badge text-bg-secondary", "assess manually"), "-", "-")
  }))
  rows <- c(rows, lapply(hidden, function(p) list(shiny::tags$b(pathway_label(p)), "-", "-",
    shiny::span(class = "badge text-bg-light border", "left out by the assessor"), "-", "-")))
  n_ex <- sum(lengths(r$excluded))
  shiny::tagList(
    shiny::h4("Summary"),
    shiny::p(class = "mb-2", paste0("Imports into Norway from ", origins_text(r), ", ",
                                    min(yrs), "-", max(yrs), ". Imports from other countries are not used.",
                                    if ("NO" %in% r$present_countries) " The pest is also present in Norway.")),
    simple_table(c("Pathway", "Based on", paste0("kg per year (median ", min(yrs), "-", max(yrs), ")"), "ENT3 class",
                   "Largest source country", "Largest customs code"), rows, right = 3),
    shiny::p(class = "small text-muted", "One ENT3 answer per pathway. The pathways are assessed separately, so their amounts are not added up.",
             if (n_ex > 0) paste0(" The assessor excluded ", n_ex, " customs code", if (n_ex > 1) "s",
                                  "; the amounts and classes above are without them."),
             if (identical(r$pathway_source, "eppo_commodities") && "packaging material" %in% r$other_categories)
               paste0(" EPPO also lists packaging material. It is not counted: CN 4415 covers wooden pallets and crates ",
                      "traded as goods, and EPPO lists no wood commodity for this pest; assess packaging manually."))
  )
}

# Checkbox, switch or button that reports a selection change (JS in app.R).
sel_control <- function(input_id, action, pathway, code = "", checked = TRUE, label = NULL) {
  if (is.null(label)) {
    shiny::tags$input(type = "checkbox", class = "form-check-input pathwise-sel", checked = if (checked) NA,
                      `data-input` = input_id, `data-action` = action, `data-pathway` = pathway, `data-code` = code,
                      title = if (action == "code") "Count this code" else "Include this pathway")
  } else {
    shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary pathwise-sel",
                       `data-input` = input_id, `data-action` = action, `data-pathway` = pathway, label)
  }
}

# One card per computed pathway; both tables have the same number columns.
pathway_card_ui <- function(i, p, t, r, input_id = "") {
  yrs <- paste0(min(t$stat_years), "-", max(t$stat_years))
  n <- length(t$stat_years)
  num_head <- c("kg per year (median)", paste0("kg in total ", yrs), "Share of total", paste0("NOK in total ", yrs),
                paste0("Years with imports (of ", n, ")"))
  num_cells <- function(d, k) list(fmt_num(d$kg[k]), fmt_num(d$kg_total[k]), fmt_pct(d$share[k]),
                                   fmt_num(d$nok_total[k]), d$years[k])
  num_total <- list(shiny::span(fmt_num(t$ent3$kg), "*"), fmt_num(t$total_kg), if (t$total_kg > 0) "100 %" else "-",
                    fmt_num(t$total_nok), t$years_any)
  star <- shiny::p(class = "small text-muted", paste0(
    "* Total per year = the ENT3 amount: the median of the yearly totals, not the sum of the medians above."))
  groups <- if (nrow(t$groups) >= 2) shiny::p(class = "small", paste0(
    "Of which codes for conifer hosts: ", fmt_num(t$groups$kg[t$groups$group == "conifer"]),
    " kg per year; for non-conifer hosts: ", fmt_num(t$groups$kg[t$groups$group == "non_conifer"]), " kg per year."))
  notes <- c(pathway_notes(p, r),
             if (!is.na(t$bark)) paste0("Of which round wood with bark: ", fmt_num(t$bark), " kg per year (median)."))
  notes <- notes[!grepl("^EPPO lists (no host commodities|only)", notes)]  # said in 'Based on'
  bslib::card(class = "mt-3",
    bslib::card_header(class = "d-flex flex-wrap justify-content-between align-items-center",
      shiny::tags$h5(class = "mb-0", paste0(i, ". ", pathway_label(p))),
      shiny::span("ENT3: ", class_badge(t$ent3$class), " ", ent3_text(t), " (median ", yrs, ")"),
      include_switch(input_id, p, TRUE)),
    bslib::card_body(
      shiny::p(class = "small text-muted mb-2", paste0("Based on ", based_on(p, r), ". Imports from ", origins_text(r), ".")),
      groups,
      shiny::h6("By country"),
      if (nrow(t$country)) simple_table(c("Country", "ISO", num_head, "Largest customs code"),
        lapply(seq_len(nrow(t$country)), function(k) c(list(t$country$name[k], t$country$key[k]), num_cells(t$country, k),
                                                       list(shiny::tags$code(t$country$main_code[k])))),
        right = 3:7, foot = c(list("Total", ""), num_total, list(""))) else shiny::p("No recorded imports."),
      if (length(t$no_imports)) shiny::p(class = "small mb-1", shiny::tags$b("No recorded imports from: "),
                                         paste(t$no_imports, collapse = ", ")),
      if (nrow(t$country)) star,
      shiny::h6(class = "mt-3", "By customs code"),
      shiny::p(class = "small text-muted mb-1", paste0(
        "The customs (CN8) codes whose imports make up the ENT3 amount. 'No, also other species' means the code covers ",
        "more than the host (e.g. pallets of any wood), so the amount is an upper bound. Hover over a name for the full tariff text. ",
        "Untick a code to leave it out: every panel and the ENT3 class then use the remaining codes.")),
      if (nrow(t$code)) shiny::div(class = "d-flex gap-1 mb-1",
        sel_control(input_id, "all", p, label = "All codes"), sel_control(input_id, "none", p, label = "No codes"),
        sel_control(input_id, "host", p, label = "Host-specific codes only")),
      if (nrow(t$code)) simple_table(c("", "Customs code", "What the code covers", "Only the host?", num_head),
        lapply(seq_len(nrow(t$code)), function(k) c(list(sel_control(input_id, "code", p, t$code$key[k], t$code$kept[k]),
                                                         shiny::tags$code(cn8_code(t$code$key[k])),
                                                         shiny::span(title = t$code$text[k], t$code$label[k]),
                                                         t$code$host_only[k]), num_cells(t$code, k))),
        right = 5:9, foot = c(list("", "Total", "", ""), num_total),
        row_class = ifelse(t$code$kept, "", "text-muted text-decoration-line-through")) else shiny::p("No recorded imports."),
      if (length(t$codes_without)) shiny::p(class = "small mb-1", shiny::tags$b("Also counted, no recorded imports: "),
                                            paste(t$codes_without, collapse = ", ")),
      if (nrow(t$code)) star,
      if (length(notes)) shiny::div(class = "small text-muted mt-2", shiny::tags$b("Notes"),
                                    shiny::tags$ul(class = "mb-0", lapply(notes, shiny::tags$li)))
    )
  )
}

include_switch <- function(input_id, p, on) {
  shiny::div(class = "form-check form-switch mb-0 small",
    sel_control(input_id, "pathway", p, checked = on), shiny::tags$label(class = "form-check-label", "Include"))
}

# A pathway the assessor left out: header and switch only.
hidden_card_ui <- function(i, p, input_id) {
  bslib::card(class = "mt-3",
    bslib::card_header(class = "d-flex flex-wrap justify-content-between align-items-center text-muted",
      shiny::tags$h5(class = "mb-0", paste0(i, ". ", pathway_label(p))),
      shiny::span("Left out by the assessor: not in the graph or the write-up."),
      include_switch(input_id, p, FALSE)))
}

# All SSB 08801 codes whose name mentions a host genus (R/host_search.R).
host_search_ui <- function(r) {
  h <- r$host_search
  if (is.null(h)) return(NULL)
  bslib::card(class = "mt-3",
    bslib::card_header(shiny::tags$h5(class = "mb-0", "Where the host is named in the import statistics")),
    bslib::card_body(
      shiny::p(class = "small text-muted mb-1", paste0(
        "Every customs code in SSB table 08801 whose name mentions a host genus (",
        paste(unique(paste0(h$host, " = ", h$term)), collapse = ", "),
        "). 'counted' codes are part of the ENT3 amounts above; for the others the reason is given.")),
      if (nrow(h) == 0) shiny::p("No customs code in SSB table 08801 names the host genera.") else simple_table(
        c("Customs code", "Valid years", "Name in SSB", "Host", "Matched word", "Counted?"),
        lapply(seq_len(nrow(h)), function(i) list(shiny::tags$code(h$code[i]), h$valid[i], h$label[i], h$host[i], h$term[i],
          if (startsWith(h$status[i], "counted")) shiny::span(class = "text-success", h$status[i]) else h$status[i])))
    )
  )
}

# r: the result with the assessor's selection applied (apply_selection()); sel: all
# pathways of the run, also those left out; input_id: the selection input.
panel1_ui <- function(r, sel, cn8_table, settings, input_id = "") {
  all_computed <- intersect(sel, names(r$pathway_info))
  computed <- setdiff(all_computed, r$hidden)
  manual <- setdiff(sel, all_computed)  # EPPO category without CN lines (stored products, pollen)
  tabs <- stats::setNames(lapply(computed, pathway_tables, r = r, cn8_table = cn8_table, settings = settings), computed)
  shiny::tagList(
    summary_ui(r, tabs, manual, intersect(all_computed, r$hidden)),
    lapply(seq_along(all_computed), function(i) {
      p <- all_computed[i]
      if (p %in% computed) pathway_card_ui(i, p, tabs[[p]], r, input_id) else hidden_card_ui(i, p, input_id)
    }),
    host_search_ui(r)
  )
}

mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "d-flex justify-content-end",
      shiny::actionButton(ns("reset"), "Reset selection (all codes and pathways)", class = "btn-sm btn-outline-secondary")),
    shiny::uiOutput(ns("summary")),
    shiny::p(class = "small text-muted mt-2", "All quantities in kg. 'none recorded' means no imports from these countries in the data; you decide."),
    shiny::downloadButton(ns("csv"), "Download CSV", class = "btn-sm")
  )
}

# New selection after one event from Panel 1 (sel_control()). s: list(run_id,
# excluded, hidden); lines: all CN8 lines of the pathway (none excluded).
update_selection <- function(s, e, lines) {
  p <- e$pathway
  ex <- s$excluded[[p]] %||% character()
  s$excluded[[p]] <- switch(e$action,
    code = if (isTRUE(e$on)) setdiff(ex, e$code) else union(ex, e$code),
    all = character(),
    none = lines$cn8,
    host = lines$cn8[!host_specific(lines$genus %||% rep(NA, nrow(lines)), lines$excludes_genera %||% rep("", nrow(lines)))],
    ex)
  if (e$action == "pathway") s$hidden <- if (isTRUE(e$on)) setdiff(s$hidden, p) else union(s$hidden, p)
  s
}

# result: the run; selected: the run with the selection applied; selection: the
# shared reactiveVal; pathways: all pathways of the run.
mod_table_server <- function(id, result, selected, selection, pathways, cn8_table, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$sel, {
      r <- result()
      s <- selection()
      if (!identical(s$run_id, r$run_id)) s <- list(run_id = r$run_id, excluded = list(), hidden = character())
      info <- pinfo(r, input$sel$pathway)
      lines <- if (is.null(info)) cn8_table[0, ] else pathway_lines(cn8_table, input$sel$pathway, info)
      selection(update_selection(s, input$sel, lines))
    })
    shiny::observeEvent(input$reset, selection(list(run_id = NULL, excluded = list(), hidden = character())))
    output$summary <- shiny::renderUI({
      sel <- pathways()
      shiny::validate(shiny::need(length(sel) > 0, "No pathway for this pest."))
      panel1_ui(selected(), sel, cn8_table, settings, session$ns("sel"))
    })
    output$csv <- shiny::downloadHandler(
      filename = function() paste0("pathwise_", result()$code, ".csv"),
      content = function(file) {
        r <- selected()
        cl <- r$classes[r$classes$pathway %in% r$pathways, ]
        cl$excluded_codes <- vapply(cl$pathway, function(p) paste(cn8_code(r$excluded[[p]]), collapse = ";"), "")
        utils::write.csv(cl, file, row.names = FALSE)
      }
    )
  })
}
