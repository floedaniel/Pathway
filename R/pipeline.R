# Flow steps 1-8 for one EPPO code. Deterministic: no LLM here.
# Any stop condition aborts with class "pathwise_stop" before SSB or Lovdata are called.
# SSB is queried only when a computed pathway (plants_for_planting, wood) is requested;
# fixed pathways get ENT3 = non_existent and manual ones are listed for the user.

run_assessment <- function(code, settings, pathways = computed_pathways,
                           cn8_table = read_commodity_cn8(), thresholds = read_thresholds(),
                           law_index_fn = law_index) {
  validate_pathways(pathways)
  code <- toupper(trimws(code))
  overview <- eppo_validate_code(code, settings)                     # 1
  hosts <- eppo_selected_hosts(code, settings)                       # 2
  classified <- classify_hosts(hosts, settings)                      # 3
  groups <- run_groups(classified)                                   # 4
  monocot <- any(classified$monocot)
  present <- eppo_present_countries(code, settings)                  # 5

  computed <- intersect(pathways, computed_pathways)                 # 6-7
  imports <- NULL
  years <- NULL
  meta <- NULL
  classes <- dplyr::bind_rows(lapply(pathways[pathway_type(pathways) == "fixed"], fixed_ent3))
  if (length(computed) > 0) {
    meta <- ssb_metadata(settings)
    years <- ssb_window(settings, meta)
    lines <- dplyr::bind_rows(lapply(computed, function(p) cn8_lines(cn8_table, p, groups, monocot)))
    imports <- ssb_imports(lines, years, settings, meta)
    classes <- dplyr::bind_rows(classes, dplyr::bind_rows(lapply(computed, function(p) {
      classify_pathway(imports, cn8_table, p, groups, present, settings, years, thresholds, monocot)
    })))
  }

  pest_tax <- eppo_taxonomy(code, settings)                          # 8
  names <- eppo_names(code, settings)
  country_names <- if (is.null(meta)) character() else meta$countries
  terms <- law_search_terms(
    pest_names = names$fullname[names$lang_iso == "la"],
    pest_lineage = pest_tax$prefname[pest_tax$type %in% c("Genus", "Subfamily", "Family")],
    host_genera = unique(stats::na.omit(classified$genus)),
    groups = groups,
    countries = unname(country_names[present])
  )
  law <- law_search(law_index_fn(settings), terms)

  list(
    code = code, pest = overview$prefname, groups = groups, monocot = monocot,
    pathways = pathways, hosts = classified, present_countries = present,
    country_names = country_names,
    years = years, imports = imports, classes = classes, law = law,
    fetched = list(
      eppo = attr(present, "fetched_at"),
      ssb = if (is.null(imports)) NULL else attr(imports, "fetched_at"),
      ssb_table_updated = if (is.null(meta)) NULL else meta$updated,
      lovdata = attr(law, "fetched_at")
    )
  )
}
