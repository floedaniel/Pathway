# Commodity groups for a run (Flow step 4). Groups come from the host lineage
# (conifer, non_conifer); the same groups exist in both pathways
# (plants_for_planting, wood) in data/commodity_cn8.csv.

commodity_groups <- c("conifer", "non_conifer")

host_to_groups <- function(host_class) {
  intersect(commodity_groups, host_class)
}

# classified: output of classify_hosts(). Returns the union of groups, or stops.
run_groups <- function(classified) {
  groups <- host_to_groups(unique(classified$host_class))
  if (length(groups) == 0) stop_app("no_plant_hosts")
  groups
}
