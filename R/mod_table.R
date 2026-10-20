# Panel 1: SSB table summary, one block per selected pathway. Tidy tables, one
# value per column, median over the statistic years, kg.
# 1. ENT3: median annual kg over all origins and its class, for all host
#    commodities of the pathway (row "all_hosts"); group rows are information.
# 2. Imports from countries where the pest is present (key context, not ENT3):
#    every pest-present country, with or without recorded imports.
# 3. Top origins overall.

fmt_num <- function(x) ifelse(is.na(x), "-", format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))
fmt_pct <- function(x) ifelse(is.na(x), "-", ifelse(x > 0 & x < 0.01, "<1 %", paste0(round(100 * x), " %")))

group_label <- c(all_hosts = "All host commodities (ENT3)", conifer = "Conifer (info)",
                 non_conifer = "Non-conifer (info)")

class_badge <- function(label, official = TRUE) {
  colour <- switch(label, small = "success", medium = "warning", large = "danger", "secondary")
  shiny::span(class = paste0("badge text-bg-", colour, if (!official) " opacity-75"), label)
}

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

# Per CN8 line: median annual kg (all origins), share of the pathway total, and
# median annual kg from pest-present origins.
line_summary <- function(imports, lines, stat_years, present, total_kg) {
  rows <- lapply(seq_len(nrow(lines)), function(i) {
    l <- lines[i, ]
    tibble::tibble(
      cn8 = l$cn8, code = cn8_code(l$cn8), description = cn8_text(l), flags = l$flags,
      kg = stats::median(annual_totals(imports, l, stat_years)$quantity),
      kg_pest_present = stats::median(annual_totals(imports, l, stat_years, "pest_present", present)$quantity)
    )
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(out)
  out$share <- if (is.na(total_kg) || total_kg == 0) NA_real_ else out$kg / total_kg
  out[order(-out$kg, -out$kg_pest_present), ]
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

# Top origins overall, with each origin's largest CN8 line.
origin_summary <- function(imports, lines, stat_years, present, country_names = character(), top_n = 10) {
  m <- country_medians(imports, lines, stat_years)
  m <- m[m$kg > 0 | m$value_nok > 0, ]
  m$name <- country_name(m$country, country_names)
  m$pest_present <- m$country %in% present
  main <- unname(main_line_by_country(imports, lines, stat_years)[m$country])
  m$main_cn8 <- cn8_code(main)
  m$main_description <- cn8_description(main, lines)
  utils::head(m[order(-m$kg), c("name", "country", "kg", "value_nok", "pest_present", "main_cn8", "main_description")], top_n)
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
  if (!from_eppo) n <- c(n, "EPPO lists no host commodities for this pest: CN8 lines are chosen from its host plants (Major host, Host).")
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

simple_table <- function(header, rows, right = integer()) {
  th <- lapply(seq_along(header), function(j) shiny::tags$th(class = if (j %in% right) "text-end", header[j]))
  shiny::tags$table(class = "table table-sm align-middle",
    shiny::tags$thead(shiny::tags$tr(th)),
    shiny::tags$tbody(lapply(rows, function(cells) shiny::tags$tr(lapply(seq_along(cells), function(j)
      shiny::tags$td(class = if (j %in% right) "text-end", cells[[j]]))))))
}

class_rows_ui <- function(cl) {
  cl <- cl[order(match(cl$group, names(group_label))), ]
  if (sum(cl$role == "info") < 2) cl <- cl[cl$role == "ent3", ]  # one group: info row would repeat ENT3
  rows <- lapply(seq_len(nrow(cl)), function(i) list(
    group_label[[cl$group[i]]], fmt_num(cl$kg[i]), class_badge(cl$class[i], official = cl$role[i] == "ent3")))
  simple_table(c("Host commodities", paste0(cl$stat[1], " kg/year, all origins, ", cl$years[1]), "ENT3 class"),
               rows, right = 2)
}

present_ui <- function(r, lines, stat_years, total_kg) {
  po <- present_origins(r$imports, lines, stat_years, r$present_countries, total_kg, r$country_names)
  with_imp <- po[po$kg > 0, ]
  without <- po[po$kg == 0, ]
  shiny::tagList(
    shiny::h6(class = "mt-3", paste0("Imports from countries where the pest is present (median kg/year ",
                                     min(stat_years), "-", max(stat_years), ")")),
    shiny::p(class = "small text-muted mb-1", paste0(
      fmt_num(sum(po$kg)), " kg/year from ", nrow(with_imp), " of ", nrow(po), " pest-present countries (",
      fmt_pct(if (is.na(total_kg) || total_kg == 0) NA else sum(po$kg) / total_kg), " of all imports). Context, not ENT3.")),
    if (nrow(with_imp) > 0) simple_table(
      c("Country", "ISO", "kg/year", "Share of all imports", "NOK/year",
        paste0("Years with imports (of ", length(stat_years), ")"), "Main CN8 code", "Main CN8 description"),
      lapply(seq_len(nrow(with_imp)), function(i) list(
        with_imp$name[i], with_imp$country[i], fmt_num(with_imp$kg[i]), fmt_pct(with_imp$share[i]),
        fmt_num(with_imp$value_nok[i]), with_imp$years_with_imports[i],
        shiny::tags$code(with_imp$main_cn8[i]), with_imp$main_description[i]
      )), right = 3:6),
    if (nrow(without) > 0) shiny::p(class = "small", shiny::tags$b("No recorded imports from: "),
                                    paste0(without$name, " (", without$country, ")", collapse = ", "))
  )
}

origins_ui <- function(r, lines, stat_years) {
  o <- origin_summary(r$imports, lines, stat_years, r$present_countries, r$country_names)
  if (nrow(o) == 0) return(shiny::p("No imports recorded."))
  simple_table(c("Country", "ISO", "kg/year", "NOK/year", "Pest present", "Main CN8 code", "Main CN8 description"),
    lapply(seq_len(nrow(o)), function(i) list(
      o$name[i], o$country[i], fmt_num(o$kg[i]), fmt_num(o$value_nok[i]),
      if (o$pest_present[i]) shiny::span(class = "text-danger", "yes") else "no",
      shiny::tags$code(o$main_cn8[i]), o$main_description[i])),
    right = 3:4)
}

lines_ui <- function(r, lines, stat_years, total_kg) {
  ls <- line_summary(r$imports, lines, stat_years, r$present_countries, total_kg)
  used <- ls[ls$kg > 0 | ls$kg_pest_present > 0, ]
  shiny::tagList(
    simple_table(c("CN8", "Description", "kg/year", "Share of total", "From pest-present origins (kg/year)", "Flags"),
      lapply(seq_len(nrow(used)), function(i) list(
        shiny::tags$code(used$code[i]), used$description[i], fmt_num(used$kg[i]),
        fmt_pct(used$share[i]), fmt_num(used$kg_pest_present[i]), gsub(";", ", ", used$flags[i]))), right = 3:5),
    if (nrow(ls) > nrow(used)) shiny::p(class = "small text-muted", paste0(
      "No recorded imports on ", nrow(ls) - nrow(used), " further line(s): ",
      paste(ls$code[!(ls$cn8 %in% used$cn8)], collapse = ", "), "."))
  )
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
    head, shiny::p(class = "small text-muted", pathway_basis(p, r)), class_rows_ui(cl),
    present_ui(r, lines, stat_years, total),
    shiny::tags$details(open = NA, shiny::tags$summary(paste0("CN8 lines in this pathway (median kg/year ",
                                                               min(stat_years), "-", max(stat_years), ")")),
                        lines_ui(r, lines, stat_years, total)),
    shiny::div(class = "alert alert-warning py-2", lapply(pathway_notes(p, r), shiny::p, class = "mb-1")),
    shiny::tags$details(open = NA, shiny::tags$summary("Top origins (all countries)"), origins_ui(r, lines, stat_years)),
    if (!is.na(bark)) shiny::p(class = "small text-muted",
      paste0("Information only: round wood with bark, median ", fmt_num(bark), " kg/year (not ENT3)."))
  )
}

mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    shiny::p(class = "small text-muted", "'none recorded' means no imports in the data; you decide. All quantities in kg."),
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
      shiny::tagList(lapply(sel, pathway_block_ui, r = r, cn8_table = cn8_table, settings = settings))
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
