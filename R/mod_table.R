# Panel 1: SSB table summary, one block per selected pathway. Tidy tables, one
# value per column, median over the statistic years, kg. Imports are only those
# from countries where the pest is present (user's rule, 2026-10-20).
# 1. ENT3: median annual kg summed over the pest-present countries and its class,
#    for all host commodities of the pathway (row "all_hosts"); group rows are
#    information.
# 2. Imports by country: every pest-present country, with or without imports.
# 3. CN8 lines of the pathway.

fmt_num <- function(x) ifelse(is.na(x), "-", format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))
fmt_pct <- function(x) ifelse(is.na(x), "-", ifelse(x > 0 & x < 0.01, "<1 %", paste0(round(100 * x), " %")))

group_label <- c(all_hosts = "All customs codes below (ENT3 answer)", conifer = "Of which codes for conifer hosts (information)",
                 non_conifer = "Of which codes for non-conifer hosts (information)")

class_badge <- function(label, official = TRUE) {
  colour <- switch(label, small = "success", medium = "warning", large = "danger", "secondary")
  shiny::span(class = paste0("badge text-bg-", colour, if (!official) " opacity-75"), label)
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

# Origin countries in words, for headings: their names (up to 5) or their number.
origins_text <- function(r) {
  o <- origin_countries(r)
  if (length(o) <= 5) paste(sort(country_name(o, r$country_names)), collapse = ", ")
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

simple_table <- function(header, rows, right = integer()) {
  th <- lapply(seq_along(header), function(j) shiny::tags$th(class = if (j %in% right) "text-end", header[j]))
  shiny::tags$table(class = "table table-sm align-middle",
    shiny::tags$thead(shiny::tags$tr(th)),
    shiny::tags$tbody(lapply(rows, function(cells) shiny::tags$tr(lapply(seq_along(cells), function(j)
      shiny::tags$td(class = if (j %in% right) "text-end", cells[[j]]))))))
}

class_rows_ui <- function(cl, r) {
  cl <- cl[order(match(cl$group, names(group_label))), ]
  if (sum(cl$role == "info") < 2) cl <- cl[cl$role == "ent3", ]  # one group: info row would repeat ENT3
  rows <- lapply(seq_len(nrow(cl)), function(i) list(
    group_label[[cl$group[i]]], fmt_num(cl$kg[i]), class_badge(cl$class[i], official = cl$role[i] == "ent3")))
  simple_table(c("What is counted", paste0("kg imported per year from ", origins_text(r), " (", cl$stat[1], " ", cl$years[1], ")"),
                 "ENT3 class"),
               rows, right = 2)
}

present_ui <- function(r, lines, stat_years, total_kg) {
  po <- present_origins(r$imports, lines, stat_years, origin_countries(r), total_kg, r$country_names)
  with_imp <- po[po$kg > 0, ]
  without <- po[po$kg == 0, ]
  shiny::tagList(
    shiny::h6(class = "mt-3", paste0("Where the imports come from (", min(stat_years), "-", max(stat_years), ")")),
    shiny::p(class = "small text-muted mb-1", paste0(
      "Imports per year from each country where the pest is present. ",
      nrow(with_imp), " of the ", nrow(po), " countries had recorded imports",
      if ("NO" %in% r$present_countries) " (Norway, where the pest is also present, is not counted as a source)", ". ",
      "Imports from other countries are not used.")),
    if (nrow(with_imp) > 0) simple_table(
      c("Country", "ISO", "kg per year (median)", "Share of the ENT3 amount", "NOK per year (median)",
        paste0("Years with imports (of ", length(stat_years), ")"), "Largest customs code", "What it covers"),
      lapply(seq_len(nrow(with_imp)), function(i) list(
        with_imp$name[i], with_imp$country[i], fmt_num(with_imp$kg[i]), fmt_pct(with_imp$share[i]),
        fmt_num(with_imp$value_nok[i]), with_imp$years_with_imports[i],
        shiny::tags$code(with_imp$main_cn8[i]), with_imp$main_description[i]
      )), right = 3:6),
    if (nrow(without) > 0) shiny::p(class = "small", shiny::tags$b("No recorded imports from: "),
                                    paste0(without$name, " (", without$country, ")", collapse = ", "))
  )
}

lines_ui <- function(r, lines, stat_years, total_kg) {
  ls <- line_summary(r$imports, lines, stat_years, total_kg)
  used <- ls[ls$kg_total > 0, ]
  shiny::tagList(
    if (nrow(used) > 0) simple_table(c("Customs code (CN8)", "What the code covers (customs tariff)", "Only the host?",
                                       "kg per year (median)", "Share of the ENT3 amount",
                                       paste0("kg in total ", min(stat_years), "-", max(stat_years))),
      lapply(seq_len(nrow(used)), function(i) list(
        shiny::tags$code(used$code[i]), used$description[i], host_only_text(used$genus[i], used$flags[i]),
        fmt_num(used$kg[i]), fmt_pct(used$share[i]), fmt_num(used$kg_total[i]))), right = 4:6),
    if (nrow(ls) > nrow(used)) shiny::p(class = "small text-muted", paste0(
      "Also counted, but no recorded imports from these countries: ",
      paste(ls$code[!(ls$cn8 %in% used$cn8)], collapse = ", "), "."))
  )
}

# Explanation above the customs-code table.
lines_intro <- function(p, r) {
  info <- pinfo(r, p)
  genera <- sort(unique(stats::na.omit(info$hosts$genus)))
  paste0("The ENT3 amount is the sum of these customs codes. ",
         if (!is.null(info$categories)) paste0("They are the codes for the commodities EPPO lists for this pest (",
                                                paste(info$categories, collapse = ", "), ")")
         else "They are the codes for this pathway",
         if (length(genera)) paste0(", limited to the host genera (", paste(genera, collapse = ", "), ") where a code names a species"),
         ". Codes marked 'No, also other species' cover more than the host (e.g. pallets of any wood), so the amount is an upper bound. ",
         "A median of 0 means imports in fewer than half of the years; the total shows them.")
}

bark_subtotal <- function(imports, lines, stat_years) {
  wb <- lines[grepl("with_bark", lines$flags), ]
  if (nrow(wb) == 0) return(NA_real_)
  stats::median(annual_totals(imports, wb, stat_years)$quantity)
}

pathway_block_ui <- function(p, r, cn8_table, settings) {
  head <- shiny::h5(class = "mt-3", pathway_label(p))
  listed <- if (nrow(r$commodities %||% data.frame()) > 0) r$commodities[r$commodities$pathway %in% p, ] else NULL
  listed_txt <- if (!is.null(listed) && nrow(listed) > 0) paste0("EPPO lists: ", paste(unique(paste0(listed$category, " (", listed$host, ")")),
                                                                  collapse = "; "), ". ") else ""
  if (pathway_type(p) == "manual") {
    return(shiny::tagList(head, shiny::div(class = "alert alert-secondary py-2", listed_txt, "Not computed in v1. Assess manually.")))
  }
  if (is.null(pinfo(r, p))) {
    return(shiny::tagList(head, shiny::div(class = "alert alert-secondary py-2",
      "EPPO lists no host commodity of this pathway for the pest. Not assessed.")))
  }
  cl <- r$classes[r$classes$pathway == p, ]
  stat_years <- utils::tail(r$years, settings$volume_n)
  lines <- pathway_lines(cn8_table, p, pinfo(r, p))
  total <- cl$kg[cl$role == "ent3"]
  bark <- if (p == "wood") bark_subtotal(r$imports, lines, stat_years) else NA
  shiny::tagList(
    head, shiny::p(class = "small text-muted", pathway_basis(p, r)), class_rows_ui(cl, r),
    present_ui(r, lines, stat_years, total),
    shiny::tags$details(open = NA,
      shiny::tags$summary(class = "h6", paste0("What is imported: the customs codes counted for ", tolower(pathway_label(p)),
                                               " (", min(stat_years), "-", max(stat_years), ")")),
      shiny::p(class = "small text-muted mb-1", lines_intro(p, r)),
      lines_ui(r, lines, stat_years, total)),
    shiny::div(class = "alert alert-warning py-2", lapply(pathway_notes(p, r), shiny::p, class = "mb-1")),
    if (!is.na(bark)) shiny::p(class = "small text-muted",
      paste0("Of which round wood with bark: ", fmt_num(bark), " kg per year (median; information only)."))
  )
}

# All SSB 08801 lines whose label names a host genus (R/host_search.R).
host_search_ui <- function(r) {
  h <- r$host_search
  if (is.null(h)) return(NULL)
  shiny::tagList(
    shiny::h5(class = "mt-4", paste0("Where the host is named in the import statistics (",
                                     paste(unique(paste0(h$host, " = ", h$term)), collapse = ", "), ")")),
    shiny::p(class = "small text-muted", paste0(
      "Every customs code in SSB table 08801 whose name mentions a host genus, by Latin or Norwegian name. ",
      "Codes marked 'counted' are part of the ENT3 amount above; for the others the reason is given.")),
    if (nrow(h) == 0) shiny::p("No customs code in SSB table 08801 names the host genera.") else simple_table(
      c("Customs code (CN8)", "Valid years", "Name in SSB", "Host", "Matched word", "Counted?"),
      lapply(seq_len(nrow(h)), function(i) list(shiny::tags$code(h$code[i]), h$valid[i], h$label[i], h$host[i], h$term[i],
        if (startsWith(h$status[i], "counted")) shiny::span(class = "text-success", h$status[i]) else h$status[i]))))
}

mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    shiny::p(class = "small text-muted", "Only imports from countries where the pest is present (EPPO) are used. 'none recorded' means no such imports in the data; you decide. All quantities in kg."),
    shiny::downloadButton(ns("csv"), "Download CSV", class = "btn-sm")
  )
}

mod_table_server <- function(id, result, pathways, cn8_table, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      r <- result()
      shiny::req(is.list(r), !inherits(r, "condition"))
      sel <- pathways()
      shiny::validate(shiny::need(length(sel) > 0, "Select at least one pathway."))
      shiny::tagList(lapply(sel, pathway_block_ui, r = r, cn8_table = cn8_table, settings = settings), host_search_ui(r))
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
