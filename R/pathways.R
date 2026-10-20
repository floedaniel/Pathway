# FinnPRIO entry pathways (ids exactly as in FinnPRIO, decided 2026-10-20).
# computed: ENT3 from SSB data. fixed: ENT3 = non_existent by FinnPRIO's rule,
# no data needed. manual: not computed in v1, assess manually.

finnprio_pathways <- tibble::tribble(
  ~id,                        ~label,                         ~type,
  "plants_for_planting",      "Plants for planting",          "computed",
  "wood",                     "Wood",                         "computed",
  "seeds",                    "Seeds",                        "manual",
  "food_fodder",              "Food and fodder",              "manual",
  "other_plant_parts",        "Other plant parts",            "manual",
  "hitchhiking",              "Hitchhiking",                  "fixed",
  "natural_spread",           "Natural spread",               "fixed",
  "intentional_introduction", "Intentional introduction",     "fixed"
)
max_pathways <- 5  # FinnPRIO's limit

pathway_type <- function(id) finnprio_pathways$type[match(id, finnprio_pathways$id)]
pathway_label <- function(id) finnprio_pathways$label[match(id, finnprio_pathways$id)]
computed_pathways <- finnprio_pathways$id[finnprio_pathways$type == "computed"]

validate_pathways <- function(selected) {
  if (length(selected) == 0) stop_app("no_pathway")
  if (length(selected) > max_pathways) stop_app("too_many_pathways", n = length(selected), max = max_pathways)
  unknown <- setdiff(selected, finnprio_pathways$id)
  if (length(unknown) > 0) stop_app("bad_config", field = "pathway", value = unknown[1])
  invisible(selected)
}

# Pre-selection: plants_for_planting always; wood when a conifer host is present.
default_pathways <- function(groups) {
  c("plants_for_planting", if ("conifer" %in% groups) "wood")
}

# ENT3 for a fixed-answer pathway: non_existent, no data.
fixed_ent3 <- function(pathway) {
  tibble::tibble(pathway = pathway, group = "all_hosts", role = "ent3", unit = NA_character_,
                 subset = "all", class = "non_existent",
                 note = "FinnPRIO rule: non_existent for hitchhiking, natural spread and intentional introduction.")
}
