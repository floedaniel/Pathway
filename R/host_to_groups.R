# Commodity groups for a run (Flow step 4). v1: groups come from the lineage
# class (conifer, broadleaf); the same groups exist in both pathways
# (plants for planting, wood) in data/commodity_cn8.csv.

commodity_groups <- c("conifer", "broadleaf")

host_to_groups <- function(host_class) {
  intersect(commodity_groups, host_class)
}

# classified: output of classify_hosts(). Returns the union of groups, or stops.
run_groups <- function(classified) {
  groups <- host_to_groups(unique(classified$host_class))
  if (length(groups) > 0) return(groups)
  unreviewed <- classified$prefname[classified$host_class == "unreviewed"]
  if (length(unreviewed) > 0) {
    stop_app("unreviewed_hosts", hosts = paste(sort(unique(unreviewed)), collapse = ", "))
  }
  stop_app("no_groups")
}
