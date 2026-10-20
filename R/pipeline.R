# Flow steps 1-8 for one EPPO code. Deterministic: no LLM here.
# Any stop condition aborts with class "pathwise_stop" before SSB or Lovdata are called.

run_assessment <- function(code, settings, cn8_table = read_commodity_cn8(),
                           thresholds = read_thresholds()) {
  code <- toupper(trimws(code))
  overview <- eppo_validate_code(code, settings)                     # 1
  hosts <- eppo_selected_hosts(code, settings)                       # 2
  classified <- classify_hosts(hosts, settings)                      # 3
  groups <- run_groups(classified)                                   # 4
  present <- eppo_present_countries(code, settings)                  # 5

  meta <- ssb_metadata(settings)                                     # 6-7
  years <- ssb_window(settings, meta)
  lines <- cn8_table[cn8_table$commodity_group %in% c(groups, "both"), ]
  imports <- ssb_imports(lines, years, settings, meta)
  classes <- dplyr::bind_rows(lapply(c("plants_for_planting", "wood"), function(p) {
    classify_pathway(imports, cn8_table, p, groups, present, settings, years, thresholds)
  }))

  pest_tax <- eppo_taxonomy(code, settings)                          # 8
  names <- eppo_names(code, settings)
  terms <- law_search_terms(
    pest_names = names$fullname[names$lang_iso == "la"],
    pest_lineage = pest_tax$prefname[pest_tax$type %in% c("Genus", "Subfamily", "Family")],
    host_genera = unique(stats::na.omit(classified$genus)),
    groups = groups,
    countries = unname(meta$countries[present])
  )
  law <- law_search(law_index(settings), terms)

  list(
    code = code, pest = overview$prefname, groups = groups,
    hosts = classified, present_countries = present,
    years = years, imports = imports, classes = classes, law = law,
    fetched = list(
      eppo = attr(present, "fetched_at"),
      ssb = attr(imports, "fetched_at"),
      ssb_table_updated = meta$updated,
      lovdata = attr(law, "fetched_at")
    )
  )
}
