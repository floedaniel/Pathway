# Pathwise: pest import risk (FinnPRIO ENT3) from a single EPPO code.
library(shiny)
library(config)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

load_local_keys()
settings <- load_settings()
check_secrets(settings)
cn8_table <- read_commodity_cn8()

copy_js <- shiny::tags$script(shiny::HTML("
document.addEventListener('click', function (e) {
  var b = e.target.closest('.pathwise-copy');
  if (!b) return;
  var el = document.getElementById(b.dataset.target);
  var text = el.value !== undefined ? el.value : el.innerText;
  navigator.clipboard.writeText(text).then(function () {
    var old = b.innerText; b.innerText = 'Copied'; setTimeout(function () { b.innerText = old; }, 1200);
  });
});"))

ui <- bslib::page_fluid(
  title = "Pathwise: FinnPRIO ENT3 import volume",
  theme = bslib::bs_theme(version = 5),
  copy_js,
  mod_input_ui("input"),
  uiOutput("header"),
  uiOutput("stop"),
  conditionalPanel(
    "output.has_result",
    bslib::navset_card_tab(
      bslib::nav_panel("1. SSB table", mod_table_ui("table")),
      bslib::nav_panel("2. SSB graph", mod_graph_ui("graph")),
      bslib::nav_panel("3. Lovdata", mod_law_ui("law")),
      bslib::nav_panel("4. FinnPRIO write-up", mod_writeup_ui("writeup"))
    ),
    bslib::accordion(open = FALSE,
      bslib::accordion_panel("Derived inputs (EPPO hosts and present countries)", uiOutput("derived")))
  ),
  tags$footer(class = "small text-muted mt-3", textOutput("fetch_dates"))
)

server <- function(input, output, session) {
  inp <- mod_input_server("input", settings)

  # One input (EPPO code or scientific name). Pathways come from the EPPO
  # host-commodity list (or host plants when EPPO has none).
  result <- eventReactive(inp$run(), {
    input_text <- inp$code()
    req(nzchar(input_text))
    withProgress(message = paste("Running", input_text), value = 0.3, {
      tryCatch(run_assessment(input_text, settings, cn8_table = cn8_table),
               pathwise_stop = function(e) e, pathwise_api_error = function(e) e)
    })
  })

  shown_pathways <- reactive({
    r <- result()
    if (is.list(r) && !inherits(r, "condition")) r$pathways else character()
  })

  output$has_result <- reactive(is.list(result()) && !inherits(result(), "condition"))
  outputOptions(output, "has_result", suspendWhenHidden = FALSE)

  output$stop <- renderUI({
    r <- result()
    if (inherits(r, "condition")) div(class = "alert alert-danger", conditionMessage(r))
  })

  output$header <- renderUI({
    r <- result()
    req(is.list(r), !inherits(r, "condition"))
    n <- table(factor(r$hosts$host_class, c("conifer", "non_conifer", "not_plant")))
    div(class = "d-flex flex-wrap gap-3 border rounded p-2 mb-2 small",
      span(tags$b(r$pest), " (", r$code, ")"),
      span("Pathways from: ", if (r$pathway_source == "eppo_commodities")
        paste0("EPPO host commodities (", nrow(r$commodities), " entries)") else if (length(r$other_categories))
        paste0("host plants (EPPO lists only ", paste(r$other_categories, collapse = ", "), ")") else "host plants (EPPO lists no host commodities)"),
      span("Hosts: ", n[["conifer"]], " conifer, ", n[["non_conifer"]], " non-conifer",
           if (n[["not_plant"]] > 0) paste0(", ", n[["not_plant"]], " not plants")),
      span("Present in ", length(r$present_countries), " countries"),
      if (!is.null(r$years)) span("Years ", min(r$years), "-", max(r$years)))
  })

  output$derived <- renderUI({
    r <- result()
    req(is.list(r), !inherits(r, "condition"))
    h <- r$hosts
    cm <- r$commodities
    tagList(
      p(tags$b("Present countries: "), paste(sort(r$present_countries), collapse = ", ")),
      if (nrow(cm) > 0) tagList(
        h6("EPPO host commodities (gd.eppo.int)"),
        tags$table(class = "table table-sm",
          tags$thead(tags$tr(tags$th("EPPO category"), tags$th("Host"), tags$th("EPPO"), tags$th("FinnPRIO pathway"), tags$th("Status"))),
          tags$tbody(lapply(seq_len(nrow(cm)), function(i) tags$tr(
            tags$td(cm$category[i]), tags$td(cm$host[i]), tags$td(cm$host_code[i]),
            tags$td(cm$pathway[i] %||% ""), tags$td(cm$status[i])))))),
      h6("Host plants (Major host, Host)"),
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th("Host"), tags$th("EPPO"), tags$th("Class"), tags$th("Family"), tags$th("Group"))),
        tags$tbody(lapply(seq_len(nrow(h)), function(i) tags$tr(
          tags$td(h$prefname[i]), tags$td(h$eppocode[i]), tags$td(h$class_label[i]),
          tags$td(h$family[i]), tags$td(h$host_class[i])))))
    )
  })

  output$fetch_dates <- renderText({
    r <- result()
    req(is.list(r), !inherits(r, "condition"))
    f <- function(x) format(x, "%Y-%m-%d")
    paste0("Fetched: EPPO ", f(r$fetched$eppo),
           if (!is.null(r$fetched$ssb)) paste0(", SSB ", f(r$fetched$ssb), " (table 08801 updated ",
                                              substr(r$fetched$ssb_table_updated, 1, 10), ")"),
           ", Lovdata ", f(r$fetched$lovdata),
           ". Thresholds reviewed ", read_thresholds()$reviewed[1], ".")
  })

  mod_table_server("table", result, shown_pathways, cn8_table, settings)
  mod_graph_server("graph", result, shown_pathways, cn8_table)
  mod_law_server("law", result)
  mod_writeup_server("writeup", result, shown_pathways, settings)
}

shinyApp(ui, server)
