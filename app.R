# Pathwise: pest import risk (FinnPRIO ENT3) from a single EPPO code.
library(shiny)

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

ui <- bslib::page_sidebar(
  title = "Pathwise: FinnPRIO ENT3 import volume",
  theme = bslib::bs_theme(version = 5),
  sidebar = bslib::sidebar(mod_input_ui("input"), width = 280),
  copy_js,
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
  tags$footer(class = "small text-muted mt-3",
    p("Import statistics show recorded trade, not pest pressure or entry. CN8 lines cover whole commodity groups, ",
      "so classes are an upper bound for host-specific volume. Classes assume the selected pathway."),
    textOutput("fetch_dates"))
)

server <- function(input, output, session) {
  inp <- mod_input_server("input", settings)

  # Run computes both data pathways (cached) plus the selected fixed ones, so the
  # pathway selection can change afterwards without a new run.
  result <- eventReactive(inp$run(), {
    code <- inp$code()
    req(nzchar(code))
    withProgress(message = paste("Running", code), value = 0.3, {
      tryCatch({
        validate_pathways(inp$pathways())
        fixed <- inp$pathways()[pathway_type(inp$pathways()) == "fixed"]
        run_assessment(code, settings, pathways = c(computed_pathways, fixed), cn8_table = cn8_table)
      }, pathwise_stop = function(e) e, pathwise_api_error = function(e) e)
    })
  })

  # Pre-select plants_for_planting, and wood when a conifer host is present
  observeEvent(result(), {
    r <- result()
    if (is.list(r) && !inherits(r, "condition")) inp$preselect(r$groups)
  })

  # Pathways shown: the selection, capped at FinnPRIO's limit
  shown_pathways <- reactive(utils::head(inp$pathways(), max_pathways))

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
      span("Groups: ", paste(r$groups, collapse = ", ")),
      span("Hosts: ", n[["conifer"]], " conifer, ", n[["non_conifer"]], " non-conifer",
           if (n[["not_plant"]] > 0) paste0(", ", n[["not_plant"]], " not plants")),
      span("Present in ", length(r$present_countries), " countries"),
      span("Years ", min(r$years), "-", max(r$years)))
  })

  output$derived <- renderUI({
    r <- result()
    req(is.list(r), !inherits(r, "condition"))
    h <- r$hosts
    tagList(
      p(tags$b("Present countries: "), paste(sort(r$present_countries), collapse = ", ")),
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
    paste0("Fetched: EPPO ", f(r$fetched$eppo), ", SSB ", f(r$fetched$ssb),
           " (table 08801 updated ", substr(r$fetched$ssb_table_updated, 1, 10), "), Lovdata ", f(r$fetched$lovdata),
           ". Thresholds reviewed ", read_thresholds()$reviewed[1], ".")
  })

  mod_table_server("table", result, shown_pathways, cn8_table, settings)
  mod_graph_server("graph", result, shown_pathways, cn8_table)
  mod_law_server("law", result)
  mod_writeup_server("writeup", result, shown_pathways, settings)
}

shinyApp(ui, server)
