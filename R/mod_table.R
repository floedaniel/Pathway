# Panel 1: SSB table summary, one block per selected FinnPRIO pathway.
# Class A = total over all origins for all host commodities of the pathway
# (row "all_hosts", the ENT3 answer). Class B = pest-present origins only
# (informational, not ENT3). Per-group rows (conifer, non_conifer) are information.

fmt_num <- function(x) ifelse(is.na(x), "-", format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))

group_label <- c(all_hosts = "All host commodities (ENT3)", conifer = "conifer (info)",
                 non_conifer = "non-conifer (info)")

class_badge <- function(label, official = TRUE) {
  colour <- switch(label, small = "success", medium = "warning", large = "danger", non_existent = "info", "secondary")
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
  stats::median(annual_totals(imports, wb, "kg", stat_years)$quantity)
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
  rows <- lapply(unique(paste(cl$group, cl$unit)), function(k) {
    x <- cl[paste(cl$group, cl$unit) == k, ]
    a <- x[x$subset == "all", ]
    b <- x[x$subset == "pest_present", ]
    shiny::tags$tr(
      class = if (a$role == "ent3") "fw-semibold" else "text-muted",
      shiny::tags$td(group_label[[a$group]]), shiny::tags$td(a$unit),
      shiny::tags$td(class_badge(a$class, official = a$role == "ent3"), " ", fmt_num(a$value)),
      shiny::tags$td(class_badge(b$class, official = FALSE), " ", fmt_num(b$value),
                     if (isTRUE(b$a_b_differ)) shiny::span(class = "text-danger ms-1", title = "Class B differs from Class A", "≠ A")),
      shiny::tags$td(paste0(a$stat, " ", a$years)),
      shiny::tags$td(paste0(fmt_num(a$latest), " / ", fmt_num(b$latest), " (", a$latest_year, ")")),
      shiny::tags$td(paste0(fmt_num(a$max), " / ", fmt_num(b$max))),
      shiny::tags$td(if (isTRUE(a$stats_disagree) || isTRUE(b$stats_disagree))
        shiny::span(class = "text-warning", "statistics land in different classes"))
    )
  })
  shiny::tags$table(
    class = "table table-sm align-middle",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("Host commodities"), shiny::tags$th("Unit"), shiny::tags$th("Class A"),
      shiny::tags$th("Class B (informational, not ENT3)"), shiny::tags$th("Statistic"),
      shiny::tags$th("Latest A / B"), shiny::tags$th("Max A / B"), shiny::tags$th("")
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
  type <- pathway_type(p)
  if (type == "fixed") {
    return(shiny::tagList(head, shiny::p("ENT3: ", class_badge("non_existent"),
      " FinnPRIO rule for hitchhiking, natural spread and intentional introduction. No trade data needed.")))
  }
  if (type == "manual") {
    return(shiny::tagList(head, shiny::div(class = "alert alert-secondary py-2",
      "Not computed in v1. Assess manually.")))
  }
  cl <- r$classes[r$classes$pathway == p, ]
  stat_years <- utils::tail(r$years, settings$volume_n)
  notes <- pathway_notes(p, r)
  if (any(cl$a_b_differ, na.rm = TRUE)) notes <- c(notes, "Class A and Class B differ.")
  shiny::tagList(
    head, class_rows_ui(cl),
    shiny::div(class = "alert alert-warning py-2", lapply(notes, shiny::p, class = "mb-1")),
    shiny::tags$details(shiny::tags$summary("Top origins"), origins_ui(r, cn8_table, p, stat_years))
  )
}

mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    shiny::p(class = "small text-muted",
      "'none recorded' means no imports in the data; you decide. Units kg and pc are never summed."),
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
