# FinnPRIO entry pathways (ids as in FinnPRIO). computed: ENT3 from SSB data.
# manual: not computed in v1, assess manually. Hitchhiking, natural spread and
# intentional introduction are left out of the app (decided 2026-10-20).

finnprio_pathways <- tibble::tribble(
  ~id,                   ~label,                ~type,
  "plants_for_planting", "Plants for planting", "computed",
  "wood",                "Wood",                "computed",
  "seeds",               "Seeds",               "manual",
  "food_fodder",         "Food and fodder",     "manual",
  "other_plant_parts",   "Other plant parts",   "manual"
)

pathway_type <- function(id) finnprio_pathways$type[match(id, finnprio_pathways$id)]
pathway_label <- function(id) finnprio_pathways$label[match(id, finnprio_pathways$id)]
computed_pathways <- finnprio_pathways$id[finnprio_pathways$type == "computed"]

validate_pathways <- function(selected) {
  if (length(selected) == 0) stop_app("no_pathway")
  unknown <- setdiff(selected, finnprio_pathways$id)
  if (length(unknown) > 0) stop_app("bad_config", field = "pathway", value = unknown[1])
  invisible(selected)
}

# Pre-selection: plants_for_planting always; wood when a conifer host is present.
default_pathways <- function(groups) {
  c("plants_for_planting", if ("conifer" %in% groups) "wood")
}
