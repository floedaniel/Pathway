# Panel 4: FinnPRIO ENT3 write-up, one per selected pathway, in a fixed layout
# (R/prompts.R). Numbers, class, CN8 codes and notes come from the deterministic
# results; only the regulation lines are written by the LLM. Editable plain text
# with a Copy button; Regenerate asks the LLM again.

writeup_label <- function(w) {
  if (w$source == "template") {
    "All text from the computed results (no Lovdata units to summarise)."
  } else if (w$source == "fallback") {
    paste0("Numbers from the computed results; regulation lines are the Lovdata text (LLM output rejected: ",
           paste(w$problems, collapse = "; "), ").")
  } else if (startsWith(w$source, "error")) {
    paste0("Numbers from the computed results; regulation lines are the Lovdata text (LLM call failed: ",
           sub("^error: ", "", w$source), ").")
  } else {
    paste0("Numbers from the computed results; regulation lines summarised by LLM (", w$source,
           "). Check before pasting into FinnPRIO.")
  }
}

mod_writeup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("boxes")),
    shiny::actionButton(ns("regenerate"), "Regenerate regulation lines", class = "btn-sm btn-outline-secondary")
  )
}

mod_writeup_server <- function(id, result, pathways, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    texts <- shiny::reactive({
      r <- result()
      shiny::req(is.list(r), !inherits(r, "condition"))
      nonce <- input$regenerate
      p <- intersect(pathways(), names(r$pathway_info))
      shiny::withProgress(message = "Writing ENT3 text", value = 0.5, {
        stats::setNames(lapply(p, function(x) {
          tryCatch(generate_writeup(r, x, settings, nonce = nonce), error = function(e) {
            list(text = writeup_text(r, x, writeup_inputs(r, x), regulation_fallback(regulation_units(r))),
                 source = paste0("error: ", conditionMessage(e)), problems = character())
          })
        }), p)
      })
    })

    output$boxes <- shiny::renderUI({
      w <- texts()
      hidden <- result()$hidden
      left_out <- if (length(hidden)) shiny::p(class = "small text-muted", paste0(
        "Left out by the assessor (Panel 1): ", paste(pathway_label(hidden), collapse = ", "), "."))
      shiny::tagList(left_out, lapply(pathways(), function(p) {
        box_id <- session$ns(paste0("text_", p))
        head <- shiny::tags$label(class = "fw-semibold", `for` = box_id, pathway_label(p))
        if (is.null(pinfo(result(), p))) {
          return(shiny::div(class = "mb-3", head, shiny::p(class = "text-muted",
            "Assess manually: EPPO lists a commodity of this pathway that has no customs code in the app (e.g. stored products, pollen).")))
        }
        x <- w[[p]]
        if (is.null(x)) {
          return(shiny::div(class = "mb-3", head, shiny::p(class = "text-muted",
            "EPPO lists no host commodity of this pathway for the pest. Not assessed.")))
        }
        n_rows <- min(30, length(strsplit(x$text, "\n")[[1]]) + 1)
        shiny::div(class = "mb-3", head,
          shiny::p(class = "small text-muted mb-1", writeup_label(x)),
          shiny::textAreaInput(box_id, NULL, value = x$text, width = "100%", rows = n_rows, resize = "vertical"),
          copy_button(box_id, "Copy"))
      }))
    })
  })
}
