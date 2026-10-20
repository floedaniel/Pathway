# FinnPRIO ENT3 volume classes (Step 7). Pure functions, no API calls. kg only.
# Rule (data/finnprio_thresholds.csv): x < small_lt -> small; x <= medium_le ->
# medium; else large. Group-specific rows override "*". No imports at all ->
# "none recorded" (not non_existent; the user decides). No threshold row ->
# "not classified". Labels small/medium/large/non_existent are the exact
# FinnPRIO option names.

finnprio_classes <- c("non_existent", "small", "medium", "large")

read_thresholds <- function(file = "data/finnprio_thresholds.csv") {
  utils::read.csv(file, colClasses = c(small_lt = "numeric", medium_le = "numeric"),
                  encoding = "UTF-8")
}

threshold_for <- function(thresholds, group, unit = "kg") {
  t <- thresholds[thresholds$unit == unit & thresholds$commodity_group == group, ]
  if (nrow(t) == 0) t <- thresholds[thresholds$unit == unit & thresholds$commodity_group == "*", ]
  if (nrow(t) == 0) NULL else t[1, ]
}

volume_label <- function(x, threshold) {
  if (is.null(threshold)) return("not classified")
  if (is.na(x)) return("none recorded")
  if (x < threshold$small_lt) "small" else if (x <= threshold$medium_le) "medium" else "large"
}

# Annual kg, summed over CN8 lines and origins. imports: cn8, country, year, kg;
# in the app only origins where the pest is present (ssb_imports(countries =)).
# Years without imports count as 0. Returns tibble(year, quantity).
annual_totals <- function(imports, lines, years) {
  x <- imports[imports$cn8 %in% unique(lines$cn8) & imports$year %in% years, ]
  totals <- tapply(x$kg, factor(x$year, levels = years), function(v) sum(v, na.rm = TRUE))
  totals[is.na(totals)] <- 0
  tibble::tibble(year = as.integer(years), quantity = as.numeric(totals))
}

summary_stat <- function(x, stat) {
  switch(stat, median = stats::median(x), mean = mean(x), max = max(x),
         rlang::abort(paste0("Unknown volume_stat: ", stat)))
}

# One tidy row for a set of CN8 lines: the statistic (median) of annual kg over
# the last volume_n years, summed over the origins in `imports`, and its ENT3 class.
classify_volume <- function(imports, lines, group, stat = "median", years, volume_n = length(years),
                            thresholds = read_thresholds()) {
  stat_years <- utils::tail(sort(years), volume_n)
  total <- annual_totals(imports, lines, stat_years)
  kg <- if (all(total$quantity == 0)) NA_real_ else summary_stat(total$quantity, stat)
  tibble::tibble(
    group = group, stat = stat, years = paste0(min(stat_years), "-", max(stat_years)),
    kg = kg, class = volume_label(kg, threshold_for(thresholds, group))
  )
}

# All rows for one pathway. "all_hosts" = total over the union of the selected
# lines (each CN8 line once): the pathway's ENT3 answer (role "ent3"). Per-group
# rows (conifer, non_conifer) are information (role "info").
# info: the pathway's host selection, see pathway_info() in R/pipeline.R
# (groups, monocot, host_genera, categories).
classify_pathway <- function(imports, cn8_table, pathway, info, settings, years, thresholds = read_thresholds()) {
  rows <- lapply(c("all_hosts", info$groups), function(g) {
    lines <- pathway_lines(cn8_table, pathway, info, if (g == "all_hosts") NULL else g)
    classify_volume(imports, lines, g, settings$volume_stat, years, settings$volume_n, thresholds)
  })
  out <- dplyr::bind_rows(rows)
  out$pathway <- pathway
  out$role <- ifelse(out$group == "all_hosts", "ent3", "info")
  out[c("pathway", "group", "role", "stat", "years", "kg", "class")]
}
