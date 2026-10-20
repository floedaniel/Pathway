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

# Annual kg, summed over CN8 lines and origins. imports: cn8, country, year, kg.
# subset "all" = every origin; "pest_present" = origins in present_countries.
# Years without imports count as 0. Returns tibble(year, quantity).
annual_totals <- function(imports, lines, years, subset = c("all", "pest_present"),
                          present_countries = character()) {
  subset <- match.arg(subset)
  x <- imports[imports$cn8 %in% unique(lines$cn8) & imports$year %in% years, ]
  if (subset == "pest_present") x <- x[x$country %in% present_countries, ]
  totals <- tapply(x$kg, factor(x$year, levels = years), function(v) sum(v, na.rm = TRUE))
  totals[is.na(totals)] <- 0
  tibble::tibble(year = as.integer(years), quantity = as.numeric(totals))
}

summary_stat <- function(x, stat) {
  switch(stat, median = stats::median(x), mean = mean(x), max = max(x),
         rlang::abort(paste0("Unknown volume_stat: ", stat)))
}

# One tidy row for a set of CN8 lines: the statistic (median) of annual kg over
# the last volume_n years, its ENT3 class, and the same statistic for imports from
# pest-present origins (information, no class) with its share of the total.
classify_volume <- function(imports, lines, group, stat = "median", years, volume_n = length(years),
                            present_countries = character(), thresholds = read_thresholds()) {
  stat_years <- utils::tail(sort(years), volume_n)
  total <- annual_totals(imports, lines, stat_years, "all")
  present <- annual_totals(imports, lines, stat_years, "pest_present", present_countries)
  no_data <- all(total$quantity == 0)
  kg <- if (no_data) NA_real_ else summary_stat(total$quantity, stat)
  kg_present <- if (no_data) NA_real_ else summary_stat(present$quantity, stat)
  tibble::tibble(
    group = group, stat = stat, years = paste0(min(stat_years), "-", max(stat_years)),
    kg = kg, class = volume_label(kg, threshold_for(thresholds, group)),
    kg_pest_present = kg_present,
    share_pest_present = if (is.na(kg) || kg == 0) NA_real_ else kg_present / kg
  )
}

# All rows for one pathway. "all_hosts" = total over the union of the selected
# lines (each CN8 line once): the pathway's ENT3 answer (role "ent3"). Per-group
# rows (conifer, non_conifer) are information (role "info").
# info: the pathway's host selection, see pathway_info() in R/pipeline.R
# (groups, monocot, host_genera, categories).
classify_pathway <- function(imports, cn8_table, pathway, info, present_countries, settings,
                             years, thresholds = read_thresholds()) {
  rows <- lapply(c("all_hosts", info$groups), function(g) {
    lines <- pathway_lines(cn8_table, pathway, info, if (g == "all_hosts") NULL else g)
    classify_volume(imports, lines, g, settings$volume_stat, years, settings$volume_n,
                    present_countries, thresholds)
  })
  out <- dplyr::bind_rows(rows)
  out$pathway <- pathway
  out$role <- ifelse(out$group == "all_hosts", "ent3", "info")
  out[c("pathway", "group", "role", "stat", "years", "kg", "class", "kg_pest_present", "share_pest_present")]
}
