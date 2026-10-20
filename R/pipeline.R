# Flow for one input (EPPO code or scientific name). Deterministic: no LLM here.
# Any stop condition aborts with class "pathwise_stop" before SSB or Lovdata are called.
#
# Pathways (decided 2026-10-20):
# - If EPPO lists host commodities for the pest (EPPO/EU-listed pests), the
#   pathways, CN8 lines and host genera come from that list, via
#   data/eppo_commodity_map.csv (category -> FinnPRIO pathway).
# - Otherwise (no list), both computed pathways are assessed from the host plants
#   (Major host / Host), as before; labelled in the UI.
# SSB is queried only for computed pathways.

read_commodity_map <- function(file = "data/eppo_commodity_map.csv") {
  utils::read.csv(file, colClasses = "character", encoding = "UTF-8", na.strings = "")
}

# Host selection for one computed pathway. hosts: classified hosts (classify_hosts()).
# categories: EPPO categories (NULL = host-plant fallback, no category filter).
pathway_info <- function(hosts, categories = NULL, monocot = NULL) {
  list(
    groups = run_groups(hosts),
    monocot = monocot %||% any(hosts$monocot),
    host_genera = host_genera_by_group(hosts),
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
       manual = unique(stats::na.omit(commodities$pathway[commodities$status == "manual"])),
       other = unique(commodities$category[commodities$status == "info"]),
       table = commodities)
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

  commodities <- commodities_fn(code, settings)
  if (nrow(commodities) > 0) {
    pc <- pathways_from_commodities(commodities, settings)
    source <- "eppo_commodities"
  } else {
    pc <- list(info = lapply(stats::setNames(computed_pathways, computed_pathways), function(p) pathway_info(classified)),
               manual = character(), other = character(), table = commodities)
    source <- "host_plants"
  }
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
    imports <- ssb_imports(lines, years, settings, meta)
    classes <- dplyr::bind_rows(lapply(computed, function(p) {
      classify_pathway(imports, cn8_table, p, pc$info[[p]], present, settings, years, thresholds)
    }))
  }

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
    countries = unname(country_names_no[present])
  )
  law <- law_search(law_index_fn(settings), terms)

  list(
    code = code, input = input, pest = overview$prefname, groups = groups,
    pathways = c(computed, pc$manual), pathway_info = pc$info, pathway_source = source,
    commodities = pc$table, other_categories = pc$other,
    hosts = classified, present_countries = present, country_names = country_names,
    years = years, imports = imports, classes = classes, law = law,
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
