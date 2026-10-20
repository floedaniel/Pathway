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
                   genus = l$genus %||% NA_character_, kg = stats::median(q), kg_total = sum(q))
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
  if (pathway == "plants_for_planting") {
    if ("conifer" %in% info$groups) n <- c(n, "Conifer plants for planting only appear in catch-all lines. Class is a loose upper bound.")
    if (from_eppo) {
      if ("bulbs or tubers" %in% cats) n <- c(n, "Bulbs and tubers (CN 0601) are included: EPPO lists them as host commodity.")
    } else {
      n <- c(n, if (isTRUE(info$monocot)) "Bulbs, tubers, corms and rhizomes (CN 0601) are included because at least one host is a monocot."
                else "Bulbs and tubers (CN 0601) are not included: no monocot host. Dicot bulbs and tubers (e.g. Dahlia, Paeonia) are missed.")
    }
    n <- c(n, "Soil associated with host plants belongs to this pathway but has no CN line.")
  }
  if (pathway == "wood") {
    n <- c(n, "Genus-specific wood lines (e.g. birch, oak, pine) count only for host genera; mixed lines (firewood, chips, waste, packaging) are an upper bound.")
    if (!from_eppo || "packaging material" %in% cats) n <- c(n, "Wood packaging: CN 4415 only captures packaging traded as goods. Most packaging travels with other products and has no SSB line.")
    if (!from_eppo || "bark" %in% cats) n <- c(n, "Bark is part of this pathway; isolated bark is counted in the wood waste lines (4401 40-49).")
    if ("manufactured articles" %in% cats) n <- c(n, "Manufactured articles: only casks (4416) and prefabricated wooden buildings (9406.10) are counted; other manufactured wood is not.")
  }
  n
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
    paste0("Basis: EPPO host commodities: ", paste(info$categories, collapse = ", "), "; host genera: ",
           paste(genera, collapse = ", "), ".")
  }
}

# Origin countries in words: their names (up to 5) or their number, saying they are
# where the pest is present.
origins_text <- function(r) {
  o <- origin_countries(r)
  if (length(o) <= 5) paste0(paste(sort(country_name(o, r$country_names)), collapse = ", "), " (where the pest is present)")
  else paste0("the ", length(o), " countries where the pest is present")
}

# Does a customs code cover only the host? Yes when the code names a host genus
# (column `genus`, e.g. sawn birch); otherwise it also covers other species
# (pallets, broadleaved firewood, all trees and shrubs ...). Bark status is added.
host_only_text <- function(genus, flags) {
  g <- !is.na(genus) & nzchar(genus %||% "")
  out <- ifelse(g, paste0("Yes (", gsub(";", ", ", genus), ")"), "No, also other species")
  extra <- ifelse(grepl("with_bark", flags %||% ""), ", with bark", ifelse(grepl("debarked", flags %||% ""), ", debarked", ""))
  paste0(out, extra)
}

# ---- Panel 1 layout ----------------------------------------------------------
# 1. Summary: one row per pathway, ENT3 answer first.
# 2. One card per computed pathway with the same two tables, "By country" and
#    "By customs code": same number columns, a total row at the bottom. Shares are
#    of the 10-year total, so they add up to 100 %.
# 3. Where the host is named in the import statistics (own card).

simple_table <- function(header, rows, right = integer(), foot = NULL) {
  cell <- function(tag, x, j) tag(class = if (j %in% right) "text-end", x)
  th <- lapply(seq_along(header), function(j) cell(shiny::tags$th, header[j], j))
  shiny::tags$table(class = "table table-sm align-middle mb-1",
    shiny::tags$thead(shiny::tags$tr(th)),
    shiny::tags$tbody(lapply(rows, function(cells) shiny::tags$tr(lapply(seq_along(cells), function(j)
      cell(shiny::tags$td, cells[[j]], j))))),
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
  else paste0("EPPO host commodities: ", paste(info$categories, collapse = ", "), " of ", paste(genera, collapse = ", "))
}

# All numbers for one computed pathway: the ENT3 row, per-country and per-code
# tables over the statistic years, and their totals.
pathway_tables <- function(p, r, cn8_table, settings) {
  stat_years <- utils::tail(r$years, settings$volume_n)
  lines <- pathway_lines(cn8_table, p, pinfo(r, p))
  x <- r$imports[r$imports$cn8 %in% lines$cn8 & r$imports$year %in% stat_years, ]
  total_kg <- sum(x$kg, na.rm = TRUE)
  per <- function(by) {
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
  country <- per("country")
  country <- country[order(-country$kg_total), ]
  country$share <- share(country$kg_total)
  main <- main_line_by_country(r$imports, lines, stat_years)
  country$main_code <- cn8_code(unname(main[country$key]))
  country$name <- country_name(country$key, r$country_names)
  code <- per("cn8")
  code <- code[order(-code$kg_total), ]
  code$share <- share(code$kg_total)
  m <- match(code$key, lines$cn8)
  code$label <- cn8_short(lines)[m]
  code$text <- cn8_text(lines)[m]
  code$host_only <- host_only_text(lines$genus[m] %||% rep(NA, length(m)), lines$flags[m])
  origins <- origin_countries(r)
  years_any <- sum(annual_totals(r$imports, lines, stat_years)$quantity > 0)
  list(ent3 = r$classes[r$classes$pathway == p & r$classes$role == "ent3", ],
       groups = r$classes[r$classes$pathway == p & r$classes$role == "info", ],
       stat_years = stat_years, lines = lines, country = country, code = code,
       no_imports = sort(country_name(setdiff(origins, country$key), r$country_names)),
       codes_without = sort(cn8_code(setdiff(lines$cn8, code$key))),
       total_kg = total_kg, total_nok = sum(x$value_nok, na.rm = TRUE), years_any = years_any,
       bark = if (p == "wood") bark_subtotal(r$imports, lines, stat_years) else NA)
}

ent3_text <- function(t) {
  if (is.na(t$ent3$kg)) "no recorded imports" else paste0(fmt_num(t$ent3$kg), " kg per year")
}

# Summary: one row per pathway (computed first, then manual ones).
summary_ui <- function(r, tabs, manual) {
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
  shiny::tagList(
    shiny::h4("Summary"),
    shiny::p(class = "mb-2", paste0("Imports into Norway from ", origins_text(r), ", ",
                                    min(yrs), "-", max(yrs), ". Imports from other countries are not used.",
                                    if ("NO" %in% r$present_countries) " The pest is also present in Norway.")),
    simple_table(c("Pathway", "Based on", paste0("kg per year (median ", min(yrs), "-", max(yrs), ")"), "ENT3 class",
                   "Largest source country", "Largest customs code"), rows, right = 3),
    shiny::p(class = "small text-muted", "One ENT3 answer per pathway. The pathways are assessed separately, so their amounts are not added up.")
  )
}

# One card per computed pathway; both tables have the same number columns.
pathway_card_ui <- function(i, p, t, r) {
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
      shiny::span("ENT3: ", class_badge(t$ent3$class), " ", ent3_text(t), " (median ", yrs, ")")),
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
        "more than the host (e.g. pallets of any wood), so the amount is an upper bound. Hover over a name for the full tariff text.")),
      if (nrow(t$code)) simple_table(c("Customs code", "What the code covers", "Only the host?", num_head),
        lapply(seq_len(nrow(t$code)), function(k) c(list(shiny::tags$code(cn8_code(t$code$key[k])),
                                                         shiny::span(title = t$code$text[k], t$code$label[k]),
                                                         t$code$host_only[k]), num_cells(t$code, k))),
        right = 4:8, foot = c(list("Total", "", ""), num_total)) else shiny::p("No recorded imports."),
      if (length(t$codes_without)) shiny::p(class = "small mb-1", shiny::tags$b("Also counted, no recorded imports: "),
                                            paste(t$codes_without, collapse = ", ")),
      if (nrow(t$code)) star,
      if (length(notes)) shiny::div(class = "small text-muted mt-2", shiny::tags$b("Notes"),
                                    shiny::tags$ul(class = "mb-0", lapply(notes, shiny::tags$li)))
    )
  )
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

panel1_ui <- function(r, sel, cn8_table, settings) {
  computed <- intersect(sel, names(r$pathway_info))
  manual <- setdiff(sel[pathway_type(sel) == "manual"], computed)
  tabs <- stats::setNames(lapply(computed, pathway_tables, r = r, cn8_table = cn8_table, settings = settings), computed)
  shiny::tagList(
    summary_ui(r, tabs, manual),
    lapply(seq_along(computed), function(i) pathway_card_ui(i, computed[i], tabs[[i]], r)),
    host_search_ui(r)
  )
}

mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    shiny::p(class = "small text-muted mt-2", "All quantities in kg. 'none recorded' means no imports from these countries in the data; you decide."),
    shiny::downloadButton(ns("csv"), "Download CSV", class = "btn-sm")
  )
}

mod_table_server <- function(id, result, pathways, cn8_table, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      r <- result()
      shiny::req(is.list(r), !inherits(r, "condition"))
      sel <- pathways()
      shiny::validate(shiny::need(length(sel) > 0, "No pathway for this pest."))
      panel1_ui(r, sel, cn8_table, settings)
    })
    output$csv <- shiny::downloadHandler(
      filename = function() paste0("pathwise_", result()$code, ".csv"),
      content = function(file) {
        cl <- result()$classes
        utils::write.csv(cl[cl$pathway %in% pathways(), ], file, row.names = FALSE)
      }
    )
  })
}
