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

lineage_rank <- function(tax, type) {
  x <- tax$prefname[tax$type == type]
  if (length(x) == 0) NA_character_ else x[1]
}

classify_host <- function(tax) {
  host_class <- if (any(tax$prefname %in% conifer_taxa)) {
    "conifer"
  } else if (lineage_rank(tax, "Kingdom") %in% plant_kingdoms) {
    "non_conifer"
  } else {
    "not_plant"
  }
  tibble::tibble(family = lineage_rank(tax, "Family"), genus = lineage_rank(tax, "Genus"),
                 host_class = host_class)
}

# hosts: tibble from eppo_selected_hosts(). Returns hosts with lineage and host_class.
classify_hosts <- function(hosts, settings, taxonomy_fn = eppo_taxonomy) {
  rows <- lapply(hosts$eppocode, function(code) classify_host(taxonomy_fn(code, settings)))
  dplyr::bind_cols(hosts[c("eppocode", "prefname", "class_label")], dplyr::bind_rows(rows))
}
