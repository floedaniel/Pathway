# Input: one field, EPPO code or scientific name, and Run. Pathways are set from
# the EPPO host-commodity list after each run (or from host plants when EPPO has
# no list); the checkboxes are an override.

pathway_choice_list <- function() {
  stats::setNames(finnprio_pathways$id,
                  paste0(finnprio_pathways$label,
                         c(computed = "", manual = " (assess manually)")[finnprio_pathways$type]))
}

mod_input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(ns("code"), "EPPO code or scientific name", placeholder = "e.g. AGRLAX or Agrilus anxius"),
    shiny::actionButton(ns("run"), "Run", class = "btn-primary w-100"),
    shiny::hr(),
    shiny::checkboxGroupInput(ns("pathways"), "Pathways (set from EPPO after Run)",
                              choices = pathway_choice_list(), selected = character())
  )
}

mod_input_server <- function(id, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      run = shiny::reactive(input$run),
      code = shiny::reactive(trimws(input$code)),
      pathways = shiny::reactive(input$pathways %||% character()),
      # After a run: select the pathways derived for the pest
      preselect = function(pathways) shiny::updateCheckboxGroupInput(session, "pathways", selected = pathways)
    )
  })
}
