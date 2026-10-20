# Panel 4: FinnPRIO ENT3 write-up, one per selected pathway. Editable plain text
# with a Copy button; Regenerate rebuilds the texts. The LLM step is Step 9;
# until then each box holds a deterministic draft built only from Panel 1 numbers.

writeup_draft <- function(r, pathway, settings) {
  type <- pathway_type(pathway)
  if (type == "fixed") {
    return(paste0("ENT3: non_existent. Pathway ", pathway_label(pathway), " (FinnPRIO rule)."))
  }
  if (type == "manual") return("")
  cl <- r$classes[r$classes$pathway == pathway & r$classes$role == "ent3" &
                    r$classes$subset == "all" & r$classes$unit == "kg", ]
  if (nrow(cl) == 0) return("")
  law <- r$law[r$law$pest_match, ]
  cite <- if (nrow(law) > 0) paste0(" Regulated: ", law$id[1], ".") else " Not found in FOR-2000-12-01-1333."
  txt <- paste0("ENT3: ", cl$class, ". ", pathway_label(pathway), ", all host commodities ",
                fmt_num(cl$value), " kg/yr (", cl$stat, " ", cl$years, ").", cite)
  substr(txt, 1, settings$writeup_max_chars)
}

mod_writeup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(class = "small text-muted",
      "Draft built from Panel 1 numbers (LLM write-up comes in Step 9). Edit before pasting into FinnPRIO."),
    shiny::uiOutput(ns("boxes")),
    shiny::actionButton(ns("regenerate"), "Regenerate", class = "btn-sm btn-outline-secondary")
  )
}

mod_writeup_server <- function(id, result, pathways, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    output$boxes <- shiny::renderUI({
      input$regenerate
      r <- result()
      shiny::req(is.list(r), !inherits(r, "condition"))
      lapply(pathways(), function(p) {
        box_id <- paste0("text_", p)
        txt <- writeup_draft(r, p, settings)
        shiny::div(class = "mb-3",
          shiny::tags$label(class = "fw-semibold", `for` = session$ns(box_id), pathway_label(p)),
          if (pathway_type(p) == "manual") shiny::p(class = "text-muted", "Assess manually (not computed in v1).")
          else shiny::tagList(
            shiny::textAreaInput(session$ns(box_id), NULL, value = txt, width = "100%", rows = 3, resize = "vertical"),
            shiny::div(class = "d-flex gap-2 align-items-center",
              copy_button(session$ns(box_id), "Copy"),
              shiny::span(class = "small text-muted", paste0(nchar(txt), " / ", settings$writeup_max_chars, " characters")))
          ))
      })
    })
  })
}
