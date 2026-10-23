# Flow for one input (EPPO code or scientific name). Deterministic: no LLM here.
# Any stop condition aborts with class "pathwise_stop" before SSB or Lovdata are called.
#
# Pathways (decided 2026-10-20):
# - If EPPO lists host commodities for the pest (EPPO/EU-listed pests), the
#   pathways, CN8 lines and host genera come from that list, via
#   data/eppo_commodity_map.csv (category -> FinnPRIO pathway).
# - Otherwise (no list), plants for planting and wood are assessed from the host
#   plants (Major host / Host), labelled in the UI; food and fodder, other plant
#   parts and seeds need an EPPO list (user's decision 2026-10-20).
# SSB is queried only for computed pathways, and only imports from countries where
# the pest is present are kept (user's rule, 2026-10-20); ENT3 is their total.

read_commodity_map <- function(file = "data/eppo_commodity_map.csv") {
  utils::read.csv(file, colClasses = "character", encoding = "UTF-8", na.strings = "")
}

# Host selection for one computed pathway. hosts: classified hosts (classify_hosts()).
# categories: EPPO categories (NULL = host-plant fallback, no category filter).
pathway_info <- function(hosts, categories = NULL, monocot = NULL) {
  use <- usable_hosts(hosts)
  list(
    groups = run_groups(use),
    monocot = monocot %||% any(use$monocot),
    host_genera = host_genera_by_group(use),
    categories = categories,
    hosts = hosts
  )
}

# Pathways from the EPPO host-commodity list. Returns list(info = per computed
# pathway, manual = manual pathway ids, other = info-only categories, table =
# commodity rows with pathway and status).
pathways_from_commodities <- function(commodities, settings, map = read_commodity_map()) {
  commodities$pathway <- map$finnprio_pathway[match(commodities$category, map$eppo_category)]
  commodities$status <- map$status[match(commodities$category, map$eppo_category)]
  commodities$status[is.na(commodities$status)] <- "info"  # category unknown to the map
  computed <- intersect(computed_pathways, commodities$pathway[commodities$status == "computed"])
  info <- lapply(stats::setNames(computed, computed), function(p) {
    rows <- commodities[commodities$pathway %in% p & commodities$status == "computed", ]
    hosts <- unique(rows[c("host_code", "host")])
    ch <- classify_hosts(tibble::tibble(eppocode = hosts$host_code, prefname = hosts$host,
                                        class_label = "host commodity"), settings)
    pathway_info(ch, categories = unique(rows$category), monocot = "bulbs or tubers" %in% rows$category)
  })
  list(info = info,
       manual = setdiff(unique(stats::na.omit(commodities$pathway[commodities$status == "manual"])), computed),
       other = unique(commodities$category[commodities$status == "info"]),
       table = commodities)
}

# Pathways for a run: from the EPPO host-commodity list when it yields at least one
# pathway; otherwise (no list, or only information categories such as soil) from
# the host plants. Information categories are kept for the UI either way.
choose_pathways <- function(commodities, classified, settings) {
  pc <- if (nrow(commodities) > 0) pathways_from_commodities(commodities, settings) else NULL
  if (!is.null(pc) && (length(pc$info) > 0 || length(pc$manual) > 0)) {
    return(list(pc = pc, source = "eppo_commodities"))
  }
  list(pc = list(info = lapply(stats::setNames(fallback_pathways, fallback_pathways), function(p) pathway_info(classified)),
                 manual = character(), other = if (is.null(pc)) character() else pc$other,
                 table = if (is.null(pc)) commodities else pc$table),
       source = "host_plants")
}

run_assessment <- function(input, settings, cn8_table = read_commodity_cn8(), thresholds = read_thresholds(),
                           law_index_fn = law_index, commodities_fn = eppo_host_commodities,
                           pathways = NULL) {
  code <- resolve_input(input, settings)                             # 1
  overview <- eppo_validate_code(code, settings)
  hosts <- eppo_selected_hosts(code, settings)                       # 2
  classified <- classify_hosts(hosts, settings)                      # 3
  groups <- run_groups(classified)                                   # 4
  present <- eppo_present_countries(code, settings)                  # 5
  # Origin countries for SSB and for Lovdata origin rules: where the pest is present,
  # Norway (the PRA area itself) excluded.
  origins <- setdiff(present, "NO")

  chosen <- choose_pathways(commodities_fn(code, settings), classified, settings)
  pc <- chosen$pc
  source <- chosen$source
  if (!is.null(pathways)) {  # explicit request (tests, override)
    validate_pathways(pathways)
    pc$info <- pc$info[intersect(names(pc$info), pathways)]
    pc$manual <- intersect(pc$manual, pathways)
  }
  computed <- names(pc$info)

  imports <- NULL                                                    # 6-7
  years <- NULL
  meta <- NULL
  classes <- NULL
  if (length(computed) > 0) {
    meta <- ssb_metadata(settings)
    years <- ssb_window(settings, meta)
    lines <- dplyr::bind_rows(lapply(computed, function(p) pathway_lines(cn8_table, p, pc$info[[p]])))
    # Only imports from countries where the pest is present (user's rule, 2026-10-20)
    imports <- ssb_imports(lines, years, settings, meta, countries = origins)
    classes <- dplyr::bind_rows(lapply(computed, function(p) {
      classify_pathway(imports, cn8_table, p, pc$info[[p]], settings, years, thresholds)
    }))
  }

  # Host search: every 08801 line whose label names a host genus (Latin or
  # Norwegian), with whether it is counted. Genera from the EPPO host-commodity
  # list when used, else from the host plants.
  if (is.null(meta)) meta <- ssb_metadata(settings)
  search_genera <- if (source == "eppo_commodities") unlist(lapply(pc$info, function(i) i$hosts$genus)) else classified$genus
  counted <- lapply(pc$info, function(i) NULL)
  for (p in computed) counted[[p]] <- pathway_lines(cn8_table, p, pc$info[[p]])$cn8
  host_hits <- host_search(meta$cn8, search_genera, read_genus_names_no(), counted,
                           first_year = min(years %||% ssb_window(settings, meta)))

  pest_tax <- eppo_taxonomy(code, settings)                          # 8
  names <- eppo_names(code, settings)
  country_names <- if (is.null(meta)) character() else meta$countries_en   # English, for UI and write-up
  country_names_no <- if (is.null(meta)) character() else meta$countries  # Norwegian, for Lovdata
  host_genera <- unique(stats::na.omit(c(classified$genus,
                                         unlist(lapply(pc$info, function(i) i$hosts$genus)))))
  terms <- law_search_terms(
    pest_names = names$fullname[names$lang_iso == "la"],
    pest_lineage = pest_tax$prefname[pest_tax$type %in% c("Genus", "Subfamily", "Family")],
    host_genera = host_genera,
    groups = groups,
    countries = unname(country_names_no[origins])
  )
  law <- law_search(law_index_fn(settings), terms)

  list(
    code = code, input = input, pest = overview$prefname, groups = groups,
    pathways = c(computed, pc$manual), pathway_info = pc$info, pathway_source = source,
    commodities = pc$table, other_categories = pc$other,
    hosts = classified, present_countries = present, origin_countries = origins, country_names = country_names,
    years = years, imports = imports, classes = classes, law = law, host_search = host_hits,
    fetched = list(
      eppo = attr(present, "fetched_at"),
      ssb = if (is.null(imports)) NULL else attr(imports, "fetched_at"),
      ssb_table_updated = if (is.null(meta)) NULL else meta$updated,
      lovdata = attr(law, "fetched_at")
    )
  )
}

# Host selection of a computed pathway in a result.
pinfo <- function(r, pathway) r$pathway_info[[pathway]]

# The assessor's selection (decision 59). excluded: list(pathway = CN8 keys left
# out); hidden: pathways left out. The excluded codes go into the pathway info, so
# pathway_lines() drops them in every panel, and the pathway's ENT3 rows are
# recomputed from the yearly totals of the kept codes (not by subtracting medians).
apply_selection <- function(r, excluded, hidden, cn8_table, settings, thresholds = read_thresholds()) {
  excluded <- Filter(length, excluded[intersect(names(excluded), names(r$pathway_info))])
  r$hidden <- intersect(hidden, r$pathways)
  r$pathways <- setdiff(r$pathways, hidden)
  r$excluded <- excluded
  for (p in names(excluded)) {
    r$pathway_info[[p]]$excluded <- excluded[[p]]
    rows <- classify_pathway(r$imports, cn8_table, p, r$pathway_info[[p]], settings, r$years, thresholds)
    r$classes <- dplyr::bind_rows(lapply(unique(r$classes$pathway), function(q)
      if (q == p) rows else r$classes[r$classes$pathway == q, ]))
  }
  r
}
