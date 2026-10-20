# Panel 1: SSB table summary, one block per selected pathway. Tidy table, one
# value per column: median annual kg over all origins and its ENT3 class for all
# host commodities of the pathway (row "all_hosts", the ENT3 answer), plus kg from
# pest-present origins and its share (context only, no class). Per-group rows
# (conifer, non_conifer) are information.

fmt_num <- function(x) ifelse(is.na(x), "-", format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))
fmt_pct <- function(x) ifelse(is.na(x), "-", paste0(round(100 * x), " %"))

group_label <- c(all_hosts = "All host commodities (ENT3)", conifer = "Conifer (info)",
                 non_conifer = "Non-conifer (info)")

class_badge <- function(label, official = TRUE) {
  colour <- switch(label, small = "success", medium = "warning", large = "danger", "secondary")
  shiny::span(class = paste0("badge text-bg-", colour, if (!official) " opacity-75"), label)
}

# Median annual quantity and value per origin over the statistic years.
origin_summary <- function(imports, lines, stat_years, present, unit = "kg", country_names = character(),
                           top_n = 10) {
  x <- imports[imports$cn8 %in% lines$cn8 & imports$year %in% stat_years, ]
  if (nrow(x) == 0) return(tibble::tibble())
  per_year <- dplyr::summarise(dplyr::group_by(x, country, year),
                               q = sum(.data[[unit]], na.rm = TRUE), v = sum(value_nok), .groups = "drop")
  full <- tidyr::complete(per_year, country, year = stat_years, fill = list(q = 0, v = 0))
  out <- dplyr::summarise(dplyr::group_by(full, country),
                          quantity = stats::median(q), value_nok = stats::median(v), .groups = "drop")
  out$name <- unname(country_names[out$country])
  out$pest_present <- out$country %in% present
  out <- out[out$quantity > 0 | out$value_nok > 0, ]
  utils::head(out[order(-out$quantity), c("country", "name", "quantity", "value_nok", "pest_present")], top_n)
}

# Information only: median annual kg on round-wood lines that are not debarked.
bark_subtotal <- function(imports, lines, stat_years) {
  wb <- lines[grepl("with_bark", lines$flags), ]
  if (nrow(wb) == 0) return(NA_real_)
  stats::median(annual_totals(imports, wb, stat_years)$quantity)
}

# Warnings shown under a computed pathway's class table.
pathway_notes <- function(pathway, r) {
  n <- character()
  if (pathway == "plants_for_planting") {
    if ("conifer" %in% r$groups) n <- c(n, "Conifer plants for planting only appear in catch-all lines. Class is a loose upper bound.")
    n <- c(n, if (isTRUE(r$monocot)) "Bulbs, tubers, corms and rhizomes (CN 0601) are included because at least one host is a monocot."
              else "Bulbs and tubers (CN 0601) are not included: no monocot host. Dicot bulbs and tubers (e.g. Dahlia, Paeonia) are missed.",
           "Soil associated with host plants belongs to this pathway but has no CN line.")
  }
  if (pathway == "wood") {
    if ("non_conifer" %in% r$groups) n <- c(n, "Non-conifer wood lines are not matched to host genera. Class is an upper bound.")
    n <- c(n, "Wood packaging: CN 4415 only captures packaging traded as goods. Most packaging travels with other products and has no SSB line.",
           "Bark is part of this pathway; isolated bark is counted in the wood waste lines (4401 40-49).")
  }
  n
}

class_rows_ui <- function(cl) {
  cl <- cl[order(match(cl$group, names(group_label))), ]
  if (sum(cl$role == "info") < 2) cl <- cl[cl$role == "ent3", ]  # one group: info row would repeat ENT3
  stat_head <- paste0(cl$stat[1], " kg/year, ", cl$years[1])
  rows <- lapply(seq_len(nrow(cl)), function(i) {
    x <- cl[i, ]
    shiny::tags$tr(
      class = if (x$role == "ent3") "fw-semibold" else "text-muted",
      shiny::tags$td(group_label[[x$group]]),
      shiny::tags$td(class = "text-end", fmt_num(x$kg)),
      shiny::tags$td(class_badge(x$class, official = x$role == "ent3")),
      shiny::tags$td(class = "text-end", fmt_num(x$kg_pest_present)),
      shiny::tags$td(class = "text-end", fmt_pct(x$share_pest_present))
    )
  })
  shiny::tags$table(
    class = "table table-sm align-middle",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("Host commodities"),
      shiny::tags$th(class = "text-end", paste0("All origins (", stat_head, ")")),
      shiny::tags$th("ENT3 class"),
      shiny::tags$th(class = "text-end", "From pest-present origins (kg/year)"),
      shiny::tags$th(class = "text-end", "Share from pest-present origins")
    )),
    shiny::tags$tbody(rows)
  )
}

origins_ui <- function(r, cn8_table, pathway, stat_years) {
  lapply(r$groups, function(g) {
    lines <- cn8_lines(cn8_table, pathway, g, r$monocot)
    o <- origin_summary(r$imports, lines, stat_years, r$present_countries, "kg", r$country_names)
    bark <- if (pathway == "wood") bark_subtotal(r$imports, lines, stat_years) else NA
    shiny::div(
      shiny::h6(paste0("Top origins, ", g, " (median kg/year ", min(stat_years), "-", max(stat_years), ")")),
      if (nrow(o) == 0) shiny::p("No imports recorded.") else shiny::tags$table(
        class = "table table-sm",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Origin"), shiny::tags$th("kg"),
                                         shiny::tags$th("NOK"), shiny::tags$th("Pest present"))),
        shiny::tags$tbody(lapply(seq_len(nrow(o)), function(i) shiny::tags$tr(
          shiny::tags$td(paste0(o$name[i], " (", o$country[i], ")")),
          shiny::tags$td(fmt_num(o$quantity[i])), shiny::tags$td(fmt_num(o$value_nok[i])),
          shiny::tags$td(if (o$pest_present[i]) shiny::span(class = "text-danger", "● present") else "")
        )))
      ),
      if (!is.na(bark)) shiny::p(class = "small text-muted",
        paste0("Information only: round wood with bark, median ", fmt_num(bark), " kg/year (not ENT3)."))
    )
  })
}

pathway_block_ui <- function(p, r, cn8_table, settings) {
  head <- shiny::h5(class = "mt-3", pathway_label(p))
  if (pathway_type(p) == "manual") {
    return(shiny::tagList(head, shiny::div(class = "alert alert-secondary py-2",
      "Not computed in v1. Assess manually.")))
  }
  cl <- r$classes[r$classes$pathway == p, ]
  stat_years <- utils::tail(r$years, settings$volume_n)
  shiny::tagList(
    head, class_rows_ui(cl),
    shiny::div(class = "alert alert-warning py-2", lapply(pathway_notes(p, r), shiny::p, class = "mb-1")),
    shiny::tags$details(open = NA, shiny::tags$summary("Top origins"), origins_ui(r, cn8_table, p, stat_years))
  )
}

mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    shiny::p(class = "small text-muted",
      "'none recorded' means no imports in the data; you decide. All quantities in kg."),
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
