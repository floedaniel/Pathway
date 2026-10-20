# Host classification (Flow step 3). Per host, walk the EPPO taxonomy lineage:
# - class Pinopsida                                  -> conifer
# - class Angiospermae AND woody in woody_taxa.csv   -> broadleaf
# - class Angiospermae, listed woody = FALSE         -> other
# - class Angiospermae, not listed                   -> unreviewed (shown, not counted)
# - anything else (ferns, Ginkgo, ...)               -> other
# EPPO API v2 uses class "Angiospermae" for flowering plants (verified 2026-10-20);
# "Magnoliopsida" is kept as an alias in case EPPO changes it.

conifer_classes <- c("Pinopsida")
angiosperm_classes <- c("Angiospermae", "Magnoliopsida")
# Fallback when a lineage lacks the class rank
conifer_families <- c("Pinaceae", "Cupressaceae", "Taxaceae", "Araucariaceae",
                      "Podocarpaceae", "Sciadopityaceae")

read_woody_taxa <- function(file = "data/woody_taxa.csv") {
  w <- utils::read.csv(file, colClasses = "character", encoding = "UTF-8")
  w$woody <- as.logical(w$woody)
  if (anyNA(w$woody) || !all(w$rank %in% c("family", "genus"))) {
    rlang::abort("woody_taxa.csv: rank must be family|genus and woody TRUE|FALSE")
  }
  w
}

# TRUE / FALSE from the list (genus rows override family rows), NA if not listed.
# With use_unreviewed = FALSE, rows without a `reviewed` date are ignored.
woody_lookup <- function(family, genus, woody, use_unreviewed = TRUE) {
  if (!use_unreviewed) woody <- woody[nzchar(woody$reviewed), ]
  g <- woody$woody[woody$rank == "genus" & woody$taxon %in% genus]
  if (length(g) > 0) return(g[1])
  f <- woody$woody[woody$rank == "family" & woody$taxon %in% family]
  if (length(f) > 0) return(f[1])
  NA
}

lineage_rank <- function(tax, type) {
  x <- tax$prefname[tax$type == type]
  if (length(x) == 0) NA_character_ else x[1]
}

classify_host <- function(tax, woody, use_unreviewed = TRUE) {
  cls <- lineage_rank(tax, "Class")
  family <- lineage_rank(tax, "Family")
  genus <- lineage_rank(tax, "Genus")
  host_class <- if (cls %in% conifer_classes || (is.na(cls) && family %in% conifer_families)) {
    "conifer"
  } else if (cls %in% angiosperm_classes) {
    w <- woody_lookup(family, genus, woody, use_unreviewed)
    if (is.na(w)) "unreviewed" else if (w) "broadleaf" else "other"
  } else {
    "other"
  }
  tibble::tibble(class = cls, family = family, genus = genus, host_class = host_class)
}

# hosts: tibble from eppo_selected_hosts(). Returns hosts with lineage and host_class.
classify_hosts <- function(hosts, settings, woody = read_woody_taxa(),
                           taxonomy_fn = eppo_taxonomy) {
  use_unreviewed <- isTRUE(settings$woody_use_unreviewed)
  rows <- lapply(hosts$eppocode, function(code) {
    classify_host(taxonomy_fn(code, settings), woody, use_unreviewed)
  })
  dplyr::bind_cols(hosts[c("eppocode", "prefname", "class_label")], dplyr::bind_rows(rows))
}
