# Panel 2: SSB graph. 10 years, one facet per host group, stacked bars of the top N
# origins (rest as "Other"), largest origin at the bottom of the stack. Origins:
# all countries or pest-present countries only.
# ggplot2 + viridis.

# info: the pathway's host selection (pinfo(r, pathway)).
# Legend: each origin is named with its largest CN8 codes in the pathway (up to
# n_codes, largest first, over the shown years), computed once over the whole
# pathway so a country keeps one colour and one legend entry across facets.
graph_data <- function(imports, cn8_table, pathway, info, years, present, country_names = character(),
                       top_n = 6, origins = c("all", "present"), n_codes = 3) {
  origins <- match.arg(origins)
  all_lines <- pathway_lines(cn8_table, pathway, info)
  xa <- imports[imports$cn8 %in% all_lines$cn8 & imports$year %in% years, ]
  per_line <- dplyr::summarise(dplyr::group_by(xa, country, cn8), q = sum(kg, na.rm = TRUE), .groups = "drop")
  per_line <- per_line[per_line$q > 0, ]
  per_line <- per_line[order(per_line$country, -per_line$q), ]
  codes <- tapply(cn8_code(per_line$cn8), per_line$country, function(v) paste(utils::head(unique(v), n_codes), collapse = ", "))
  label <- function(cc) {
    nm <- ifelse(is.na(country_names[cc]), cc, country_names[cc])
    ifelse(is.na(codes[cc]), nm, paste0(nm, " (", codes[cc], ")"))
  }
  rows <- lapply(info$groups, function(g) {
    lines <- pathway_lines(cn8_table, pathway, info, g)
    x <- imports[imports$cn8 %in% lines$cn8 & imports$year %in% years, ]
    if (origins == "present") x <- x[x$country %in% present, ]
    if (nrow(x) == 0) return(NULL)
    by_c <- dplyr::summarise(dplyr::group_by(x, country, year), q = sum(kg, na.rm = TRUE), .groups = "drop")
    totals <- sort(tapply(by_c$q, by_c$country, sum), decreasing = TRUE)
    top <- utils::head(names(totals), top_n)
    by_c$origin <- ifelse(by_c$country %in% top, unname(label(by_c$country)), "Other countries")
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

graph_plot <- function(d, title, origins = "all") {
  ggplot2::ggplot(d, ggplot2::aes(factor(year), quantity / 1e6, fill = origin)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~group, scales = "free_y", ncol = 1) +
    ggplot2::scale_fill_viridis_d(option = "D") +
    ggplot2::labs(x = NULL, y = "million kg", title = title,
                  fill = paste0(if (origins == "all") "Country of origin" else "Pest-present country",
                                "\n(largest CN8 lines)"),
                  caption = paste0("Bars: imports per year, stacked by country of origin (largest at the bottom). ",
                                   "Codes in the legend: each country's largest CN8 lines in this pathway.
",
                                   "See Panel 1 for all lines. Source: SSB table 08801.")) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "right", plot.caption = ggplot2::element_text(hjust = 0, size = 10))
}

mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "d-flex gap-4",
      shiny::uiOutput(ns("choose")),
      shiny::radioButtons(ns("origins"), NULL, inline = TRUE,
                          choices = c("All origins" = "all", "Pest-present origins only" = "present"))),
    shiny::plotOutput(ns("plot"), height = "520px"),
    shiny::downloadButton(ns("png"), "Download PNG", class = "btn-sm")
  )
}

mod_graph_server <- function(id, result, pathways, cn8_table) {
  shiny::moduleServer(id, function(input, output, session) {
    shown <- shiny::reactive(intersect(pathways(), names(result()$pathway_info %||% list())))
    output$choose <- shiny::renderUI({
      p <- shown()
      if (length(p) > 1) shiny::radioButtons(session$ns("pathway"), NULL, inline = TRUE,
                                             choices = stats::setNames(p, pathway_label(p)))
    })
    pathway <- shiny::reactive({
      p <- shown()
      shiny::validate(shiny::need(length(p) > 0, "No computed pathway selected (plants for planting or wood)."))
      if (!is.null(input$pathway) && input$pathway %in% p) input$pathway else p[1]
    })
    plot_obj <- shiny::reactive({
      r <- result()
      shiny::req(is.list(r), !inherits(r, "condition"))
      origins <- input$origins %||% "all"
      d <- graph_data(r$imports, cn8_table, pathway(), pinfo(r, pathway()), r$years, r$present_countries,
                      r$country_names, origins = origins)
      shiny::validate(shiny::need(nrow(d) > 0, if (origins == "present")
        "No recorded imports from countries where the pest is present." else "No imports recorded for this pathway."))
      graph_plot(d, paste0(r$pest, ": imports, ", pathway_label(pathway()),
                           if (origins == "present") ", pest-present origins"), origins)
    })
    output$plot <- shiny::renderPlot(plot_obj())
    output$png <- shiny::downloadHandler(
      filename = function() paste0("pathwise_", result()$code, "_", pathway(), "_", input$origins, ".png"),
      content = function(file) ggplot2::ggsave(file, plot_obj(), width = 10, height = 7, dpi = 150)
    )
  })
}
