# Headless check of the selection (decision 59) on ANSTFR through the app's server.
app <- shiny::shinyAppDir(".")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
load_local_keys(); settings <- load_settings()
cn8_table <- read_commodity_cn8()
shiny::testServer(app, {
  session$setInputs(`input-code` = "ANSTFR", `input-run` = 1)
  r <- selected()
  ent3 <- function(r) r$classes[r$classes$role == "ent3", c("pathway", "kg", "class")]
  cat("pathways:", r$pathways, "\n"); print(ent3(r))
  ff <- pathway_lines(cn8_table, "food_fodder", pinfo(result(), "food_fodder"))
  lime <- ff$cn8[startsWith(ff$cn8, "08055030")]
  session$setInputs(`table-sel` = list(action = "code", pathway = "food_fodder", code = lime, on = FALSE))
  r <- selected(); cat("\nafter excluding lime", lime, ":\n"); print(ent3(r))
  ui <- as.character(output$`table-summary`$html)
  cat("struck rows:", lengths(regmatches(ui, gregexpr("line-through", ui))), " note:", grepl("excluded 1 customs code", ui), "\n")
  other <- setdiff(r$pathways, "food_fodder")[1]
  session$setInputs(`table-sel` = list(action = "pathway", pathway = other, code = "", on = FALSE))
  r <- selected(); cat("\nhidden:", r$hidden, " kept:", r$pathways, "\n")
  session$setInputs(`graph-pathway` = "food_fodder", `graph-by` = "code")
  d <- plot_data <- NULL
  cat("graph rendered:", !is.null(output$`graph-plot`), "\n")
  session$setInputs(`table-sel` = list(action = "host", pathway = "food_fodder", code = "", on = TRUE))
  r <- selected(); cat("\nhost-specific only:\n"); print(ent3(r)); cat("excluded:", length(r$excluded$food_fodder), "codes\n")
  w <- generate_writeup(r, "food_fodder", settings, ask = function(...) "x")$text
  cat("\nwrite-up excluded line:", grep("Excluded by the assessor", strsplit(w, "\n")[[1]], value = TRUE), "\n")
  session$setInputs(`table-reset` = 1)
  r <- selected(); cat("\nafter reset:\n"); print(ent3(r)); cat("hidden:", length(r$hidden), "\n")
})
