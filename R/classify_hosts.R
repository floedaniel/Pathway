# Host classification (Flow step 3), programmatic, no reviewed list
# (decided 2026-10-20). Per host, walk the EPPO taxonomy lineage:
# - lineage contains Pinopsida, Pinales or Coniferales -> conifer
#   (FOR-2000-12-01-1333 calls this group Coniferales)
# - any other plant (kingdom Plantae)                   -> non_conifer
#   (cycads, Ginkgo, ferns included; no woody test)
# - not a plant (e.g. a fungus listed as host)          -> not_plant
# Taxonomy is cached permanently per host code (source eppo_taxonomy).

conifer_taxa <- c("Pinopsida", "Pinales", "Coniferales")
plant_kingdoms <- c("Plantae")
# Monocots: EPPO v2 puts them under Category "Basal monocotyledons" or
# "Commelinids" (verified 2026-10-20 on Tulipa, Lilium, Narcissus, Allium, Zea);
# Liliopsida / Monocotyledons kept as aliases. Used for the 0601 (bulbs) rule.
monocot_taxa <- c("Basal monocotyledons", "Commelinids", "Liliopsida", "Monocotyledons")

lineage_rank <- function(tax, type) {
  x <- tax$prefname[tax$type == type]
  if (length(x) == 0) NA_character_ else x[1]
}

# EPPO's non-taxonomic commodity groups in host-commodity lists (lineage
# "Commodity groups" > group): "plants" (2PLAN) and "woody plants" (2WOOP) cover
# conifers too; the herbaceous / vegetable / fruit groups are non-conifer. Unknown
# groups are treated as all plants (upper bound). No genus filter for any group.
commodity_group_class <- c("plants" = "all_plants", "woody plants" = "all_plants",
                           "herbaceous plants" = "non_conifer", "herbaceous ornamental plants" = "non_conifer",
                           "vegetable plants" = "non_conifer", "fruit plants" = "non_conifer")

classify_host <- function(tax) {
  group <- if (any(tax$prefname == "Commodity groups")) tolower(utils::tail(tax$prefname, 1)) else NA
  host_class <- if (!is.na(group)) {
    if (group %in% names(commodity_group_class)) commodity_group_class[[group]] else "all_plants"
  } else if (any(tax$prefname %in% conifer_taxa)) {
    "conifer"
  } else if (lineage_rank(tax, "Kingdom") %in% plant_kingdoms) {
    "non_conifer"
  } else {
    "not_plant"
  }
  # Taxa for matching CN8 lines (decided 2026-10-20, user: use the EPPO taxonomy):
  # the host's own EPPO name and every lineage name from family down
  # (family, subfamily, genus, species), ";"-separated; rank = the host's own rank.
  fam <- match("Family", tax$type)
  taxa <- if (is.na(group) && nrow(tax) > 0) tax$prefname[if (is.na(fam)) nrow(tax) else seq(fam, nrow(tax))] else character()
  tibble::tibble(family = lineage_rank(tax, "Family"), genus = lineage_rank(tax, "Genus"),
                 host_class = host_class,
                 monocot = host_class == "non_conifer" && any(tax$prefname %in% monocot_taxa),
                 taxa = paste(taxa, collapse = ";"),
                 rank = if (nrow(tax) > 0 && is.na(group)) utils::tail(tax$type, 1) else NA_character_)
}

# hosts: tibble from eppo_selected_hosts(). Returns hosts with lineage and host_class.
classify_hosts <- function(hosts, settings, taxonomy_fn = eppo_taxonomy) {
  rows <- lapply(hosts$eppocode, function(code) classify_host(taxonomy_fn(code, settings)))
  dplyr::bind_cols(hosts[c("eppocode", "prefname", "class_label")], dplyr::bind_rows(rows))
}
