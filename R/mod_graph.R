# Panel 2: SSB graph. 10 years, one facet per host group, stacked bars of the top N
# origins (rest as "Other"), largest origin at the bottom of the stack. Origins are
# only the countries where the pest is present (r$imports holds nothing else).
# ggplot2 + viridis.

# info: the pathway's host selection (pinfo(r, pathway)).
# Legend: each origin is named with its largest CN8 codes in the pathway (up to
# n_codes, largest first, over the shown years), computed once over the whole
# pathway so a country keeps one colour and one legend entry across facets.
# by = "code": stacked by customs code instead (legend: code and short tariff text).
graph_data <- function(imports, cn8_table, pathway, info, years, country_names = character(),
                       top_n = 6, n_codes = 3, by = "country") {
  all_lines <- pathway_lines(cn8_table, pathway, info)
  xa <- imports[imports$cn8 %in% all_lines$cn8 & imports$year %in% years, ]
  per_line <- dplyr::summarise(dplyr::group_by(xa, country, cn8), q = sum(kg, na.rm = TRUE), .groups = "drop")
  per_line <- per_line[per_line$q > 0, ]
  per_line <- per_line[order(per_line$country, -per_line$q), ]
  codes <- tapply(cn8_code(per_line$cn8), per_line$country, function(v) paste(utils::head(unique(v), n_codes), collapse = ", "))
  label <- if (by == "code") function(k) {
    s <- cn8_short(all_lines)[match(k, all_lines$cn8)]
    paste0(cn8_code(k), " ", ifelse(nchar(s) > 40, paste0(substr(s, 1, 40), "..."), s))
  } else function(cc) {
    nm <- ifelse(is.na(country_names[cc]), cc, country_names[cc])
    ifelse(is.na(codes[cc]), nm, paste0(nm, " (", codes[cc], ")"))
  }
  rows <- lapply(info$groups, function(g) {
    lines <- pathway_lines(cn8_table, pathway, info, g)
    x <- imports[imports$cn8 %in% lines$cn8 & imports$year %in% years, ]
    if (nrow(x) == 0) return(NULL)
    by_c <- dplyr::summarise(dplyr::group_by(x, key = .data[[if (by == "code") "cn8" else "country"]], year),
                             q = sum(kg, na.rm = TRUE), .groups = "drop")
    totals <- sort(tapply(by_c$q, by_c$key, sum), decreasing = TRUE)
    top <- utils::head(names(totals), top_n)
    by_c$origin <- ifelse(by_c$key %in% top, unname(label(by_c$key)),
                          if (by == "code") "Other codes" else "Other countries")
    out <- dplyr::summarise(dplyr::group_by(by_c, origin, year), quantity = sum(q), .groups = "drop")
    out$group <- g
    out$rank <- match(out$origin, unname(label(top)))  # 1 = largest
    out
  })
  d <- dplyr::bind_rows(rows)
  if (nrow(d) == 0) return(d)
  # Stack order: ggplot draws the first level on top, so "Other" first and the
  # largest origin last (bottom of the stack).
  ord <- unique(d[order(-ifelse(is.na(d$rank), Inf, d$rank)), "origin", drop = TRUE])
  d$origin <- factor(d$origin, levels = ord)
  d
}

graph_plot <- function(d, title, by = "country") {
  ggplot2::ggplot(d, ggplot2::aes(factor(year), quantity / 1e6, fill = origin)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~group, scales = "free_y", ncol = 1) +
    ggplot2::scale_fill_viridis_d(option = "D") +
    ggplot2::labs(x = NULL, y = "million kg", title = title,
                  fill = if (by == "code") "Customs code" else "Pest-present country\n(largest CN8 lines)",
                  caption = paste0("Bars: imports per year from countries where the pest is present, stacked by ",
                                   if (by == "code") "customs code" else "country", " (largest at the bottom).\n",
                                   if (by == "code") "Only the codes counted in Panel 1 (codes the assessor left out are not shown)."
                                   else "Codes in the legend: each country's largest CN8 lines in this pathway. See Panel 1 for all lines.",
                                   " Source: SSB table 08801.")) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "right", plot.caption = ggplot2::element_text(hjust = 0, size = 10))
}

mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "d-flex flex-wrap gap-4",
      shiny::uiOutput(ns("choose")),
      shiny::radioButtons(ns("by"), NULL, inline = TRUE,
                          choices = c("Stack by country" = "country", "Stack by customs code" = "code"))),
    shiny::plotOutput(ns("plot"), height = "520px"),
    shiny::downloadButton(ns("png"), "Download PNG", class = "btn-sm"),
    shiny::downloadButton(ns("csv"), "Download CSV", class = "btn-sm")
  )
}

# result: the run with the assessor's selection applied; pathways: the pathways kept.
mod_graph_server <- function(id, result, pathways, cn8_table) {
  shiny::moduleServer(id, function(input, output, session) {
    shown <- shiny::reactive(intersect(pathways(), names(result()$pathway_info %||% list())))
    output$choose <- shiny::renderUI({
      p <- shown()
      if (length(p) > 1) shiny::radioButtons(session$ns("pathway"), NULL, inline = TRUE,
                                             choices = stats::setNames(p, pathway_label(p)),
                                             selected = shiny::isolate(input$pathway))
    })
    pathway <- shiny::reactive({
      p <- shown()
      shiny::validate(shiny::need(length(p) > 0, "No computed pathway selected."))
      if (!is.null(input$pathway) && input$pathway %in% p) input$pathway else p[1]
    })
    by <- shiny::reactive(input$by %||% "country")
    plot_data <- shiny::reactive({
      r <- result()
      shiny::req(is.list(r), !inherits(r, "condition"))
      graph_data(r$imports, cn8_table, pathway(), pinfo(r, pathway()), r$years, r$country_names, by = by())
    })
    plot_obj <- shiny::reactive({
      d <- plot_data()
      shiny::validate(shiny::need(nrow(d) > 0, "No recorded imports from countries where the pest is present."))
      r <- result()
      graph_plot(d, paste0(r$pest, ": imports from pest-present countries, ", pathway_label(pathway())), by())
    })
    output$plot <- shiny::renderPlot(plot_obj())
    output$png <- shiny::downloadHandler(
      filename = function() paste0("pathwise_", result()$code, "_", pathway(), ".png"),
      content = function(file) ggplot2::ggsave(file, plot_obj(), width = 10, height = 7, dpi = 150)
    )
    output$csv <- shiny::downloadHandler(
      filename = function() paste0("pathwise_", result()$code, "_", pathway(), "_graph.csv"),
      content = function(file) {
        d <- plot_data()
        utils::write.csv(data.frame(group = d$group, stacked_by = by(), origin = as.character(d$origin),
                                    year = d$year, kg = d$quantity), file, row.names = FALSE)
      }
    )
  })
}
