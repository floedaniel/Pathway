# Panel 3: Lovdata snippets from FOR-2000-12-01-1333, each with id, link,
# matched terms and its own copy button. Deterministic, never LLM-generated.

copy_button <- function(target_id, label = "Copy") {
  shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary pathwise-copy",
                     `data-target` = target_id, label)
}

mod_law_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id)("snippets"))
}

mod_law_server <- function(id, result, max_snippets = 15) {
  shiny::moduleServer(id, function(input, output, session) {
    output$snippets <- shiny::renderUI({
      r <- result()
      shiny::req(is.list(r), !inherits(r, "condition"))
      law <- r$law
      head_note <- shiny::p(class = "small text-muted",
        paste0("Source: FOR-2000-12-01-1333, Lovdata (fetched ", format(r$fetched$lovdata, "%Y-%m-%d"), "). ",
               sum(law$pest_match), " unit(s) name the pest or its higher taxon; ",
               sum(!law$pest_match), " match host genera only."))
      if (nrow(law) == 0) return(shiny::tagList(head_note, shiny::p("Not found in FOR-2000-12-01-1333.")))
      law <- utils::head(law, max_snippets)
      cards <- lapply(seq_len(nrow(law)), function(i) {
        tid <- session$ns(paste0("law_", i))
        shiny::div(class = "border rounded p-2 mb-2",
          shiny::div(class = "d-flex justify-content-between",
            shiny::tags$b(law$id[i], if (law$pest_match[i]) shiny::span(class = "badge text-bg-danger ms-1", "pest")),
            copy_button(tid)),
          shiny::p(class = "small text-muted mb-1", "Matched: ", law$matched[i]),
          shiny::p(id = tid, class = "mb-1", paste0(law$id[i], ": ", law$text[i])),
          shiny::tags$a(href = law$url[i], target = "_blank", class = "small", law$url[i]))
      })
      shiny::tagList(head_note,
        if (!any(law$pest_match)) shiny::div(class = "alert alert-info py-2", "The pest is not named in FOR-2000-12-01-1333 (not found)."),
        cards)
    })
  })
}
