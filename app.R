# Pathwise: pest import risk (FinnPRIO ENT3) from a single EPPO code.
library(shiny)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

settings <- load_settings()
check_secrets(settings)

ui <- fluidPage(
  titlePanel("Pathwise"),
  p("Scaffold. Panels are added in Step 8.")
)

server <- function(input, output, session) {}

shinyApp(ui, server)
