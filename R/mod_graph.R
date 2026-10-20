# Panel 2: SSB graph. 10 years, one facet per commodity group, top N origins
# (rest as "Other"), pest-present origins marked with "*". ggplot2 + viridis.

graph_data <- function(imports, cn8_table, pathway, groups, years, present, country_names = character(),
                       top_n = 6, unit = "kg", monocot = FALSE) {
  rows <- lapply(groups, function(g) {
    lines <- cn8_lines(cn8_table, pathway, g, monocot)
    x <- imports[imports$cn8 %in% lines$cn8 & imports$year %in% years, ]
    if (nrow(x) == 0) return(NULL)
    by_c <- dplyr::summarise(dplyr::group_by(x, country, year), q = sum(.data[[unit]], na.rm = TRUE), .groups = "drop")
    top <- utils::head(names(sort(tapply(by_c$q, by_c$country, sum), decreasing = TRUE)), top_n)
    nm <- ifelse(is.na(country_names[by_c$country]), by_c$country, country_names[by_c$country])
    by_c$origin <- ifelse(by_c$country %in% top,
                          paste0(nm, ifelse(by_c$country %in% present, " *", "")), "Other")
    out <- dplyr::summarise(dplyr::group_by(by_c, origin, year), quantity = sum(q), .groups = "drop")
    out$group <- g
    out
  })
  dplyr::bind_rows(rows)
}

graph_plot <- function(d, title) {
  ggplot2::ggplot(d, ggplot2::aes(factor(year), quantity / 1e6, fill = origin)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~group, scales = "free_y", ncol = 1) +
    ggplot2::scale_fill_viridis_d(option = "D") +
    ggplot2::labs(x = NULL, y = "million kg", fill = "Origin (* = pest present)", title = title) +
    ggplot2::theme_minimal(base_size = 13)
}

mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("choose")),
    shiny::plotOutput(ns("plot"), height = "520px"),
    shiny::downloadButton(ns("png"), "Download PNG", class = "btn-sm")
  )
}

mod_graph_server <- function(id, result, pathways, cn8_table) {
  shiny::moduleServer(id, function(input, output, session) {
    shown <- shiny::reactive(intersect(pathways(), computed_pathways))
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
      d <- graph_data(r$imports, cn8_table, pathway(), r$groups, r$years, r$present_countries,
                      r$country_names, monocot = r$monocot)
      shiny::validate(shiny::need(nrow(d) > 0, "No imports recorded for this pathway."))
      graph_plot(d, paste0(r$pest, ": imports, ", pathway_label(pathway())))
    })
    output$plot <- shiny::renderPlot(plot_obj())
    output$png <- shiny::downloadHandler(
      filename = function() paste0("pathwise_", result()$code, "_", pathway(), ".png"),
      content = function(file) ggplot2::ggsave(file, plot_obj(), width = 10, height = 7, dpi = 150)
    )
  })
}
