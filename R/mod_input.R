# Input: EPPO code + Run, FinnPRIO pathway selector (max 5), name -> code helper.

pathway_choice_list <- function() {
  stats::setNames(finnprio_pathways$id,
                  paste0(finnprio_pathways$label,
                         c(computed = "", fixed = " (non_existent)", manual = " (assess manually)")[finnprio_pathways$type]))
}

mod_input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(ns("code"), "EPPO code", placeholder = "e.g. IPSXCA"),
    shiny::actionButton(ns("run"), "Run", class = "btn-primary w-100"),
    shiny::hr(),
    shiny::checkboxGroupInput(ns("pathways"), paste0("Pathways (max ", max_pathways, ")"),
                              choices = pathway_choice_list(), selected = "plants_for_planting"),
    shiny::uiOutput(ns("pathway_msg")),
    shiny::hr(),
    shiny::textInput(ns("name"), "Find code by name", placeholder = "e.g. Ips typographus"),
    shiny::actionButton(ns("lookup"), "Look up", class = "btn-sm btn-outline-secondary"),
    shiny::uiOutput(ns("lookup_result"))
  )
}

mod_input_server <- function(id, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    output$lookup_result <- shiny::renderUI({
      shiny::req(input$lookup > 0)
      name <- shiny::isolate(trimws(input$name))
      shiny::req(nzchar(name))
      codes <- tryCatch(eppo_name2codes(name, settings), error = function(e) NULL)
      if (is.null(codes) || nrow(codes) == 0) return(shiny::p(class = "small text-muted", "No EPPO code found."))
      shiny::p(class = "small", "EPPO code: ", shiny::tags$b(paste(codes$eppocode, collapse = ", ")))
    })
    output$pathway_msg <- shiny::renderUI({
      msg <- tryCatch({ validate_pathways(input$pathways); NULL }, pathwise_stop = conditionMessage)
      if (!is.null(msg)) shiny::div(class = "text-danger small", msg)
    })
    list(
      run = shiny::reactive(input$run),
      code = shiny::reactive(toupper(trimws(input$code))),
      pathways = shiny::reactive(input$pathways %||% character()),
      # Pre-select defaults after a run, keeping the user's picks within the limit
      preselect = function(groups) {
        sel <- union(input$pathways, default_pathways(groups))
        if (length(sel) <= max_pathways) shiny::updateCheckboxGroupInput(session, "pathways", selected = sel)
      }
    )
  })
}
