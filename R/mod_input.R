# Input: one field, EPPO code or scientific name, and Run, centred on the page.
# Pathways are not chosen by the user: they come from the EPPO host-commodity
# list (or from host plants when EPPO has none).

mod_input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "mx-auto my-4 text-center", style = "max-width: 560px;",
    shiny::h4("Pathwise: FinnPRIO ENT3 import volume"),
    shiny::div(class = "d-flex gap-2 justify-content-center align-items-end",
      shiny::div(class = "flex-grow-1 text-start",
        shiny::textInput(ns("code"), "EPPO code or scientific name", width = "100%",
                         placeholder = "e.g. AGRLAX or Agrilus anxius")),
      shiny::div(class = "mb-3", shiny::actionButton(ns("run"), "Run", class = "btn-primary")))
  )
}

mod_input_server <- function(id, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      run = shiny::reactive(input$run),
      code = shiny::reactive(trimws(input$code))
    )
  })
}
