# FinnPRIO ENT3 volume classes (Step 7). Pure functions, no API calls.
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

threshold_for <- function(thresholds, group, unit) {
  t <- thresholds[thresholds$unit == unit & thresholds$commodity_group == group, ]
  if (nrow(t) == 0) t <- thresholds[thresholds$unit == unit & thresholds$commodity_group == "*", ]
  if (nrow(t) == 0) NULL else t[1, ]
}

volume_label <- function(x, threshold) {
  if (is.null(threshold)) return("not classified")
  if (is.na(x)) return("none recorded")
  if (x < threshold$small_lt) "small" else if (x <= threshold$medium_le) "medium" else "large"
}

# Annual totals for one group and unit, summed over CN8 lines and origins.
# imports: cn8, country, year, kg, pc. lines: CN8 rows of the group (each cn8 once).
# subset "all" = every origin (Class A); "pest_present" = origins in present_countries (Class B).
# Years without imports count as 0. Returns tibble(year, quantity).
annual_totals <- function(imports, lines, unit = c("kg", "pc"), years,
                          subset = c("all", "pest_present"), present_countries = character()) {
  unit <- match.arg(unit)
  subset <- match.arg(subset)
  x <- imports[imports$cn8 %in% unique(lines$cn8) & imports$year %in% years, ]
  if (subset == "pest_present") x <- x[x$country %in% present_countries, ]
  q <- x[[unit]]
  totals <- tapply(q, factor(x$year, levels = years), function(v) sum(v, na.rm = TRUE))
  totals[is.na(totals)] <- 0
  tibble::tibble(year = as.integer(years), quantity = as.numeric(totals))
}

summary_stat <- function(x, stat) {
  switch(stat, median = stats::median(x), mean = mean(x), max = max(x),
         rlang::abort(paste0("Unknown volume_stat: ", stat)))
}

# Classify one group/unit/subset. Statistic over the last `volume_n` years of the
# window; also the latest year and the max. All three get a class.
classify_volume <- function(imports, lines, group, unit, stat = "median", years, volume_n = length(years),
                            subset = c("all", "pest_present"), present_countries = character(),
                            thresholds = read_thresholds()) {
  subset <- match.arg(subset)
  tot <- annual_totals(imports, lines, unit, years, subset, present_countries)
  stat_years <- utils::tail(sort(years), volume_n)
  thr <- threshold_for(thresholds, group, unit)
  no_data <- all(tot$quantity == 0)
  value <- if (no_data) NA_real_ else summary_stat(tot$quantity[tot$year %in% stat_years], stat)
  latest <- if (no_data) NA_real_ else tot$quantity[tot$year == max(years)]
  maxv <- if (no_data) NA_real_ else max(tot$quantity)
  classes <- c(volume_label(value, thr), volume_label(latest, thr), volume_label(maxv, thr))
  tibble::tibble(
    group = group, unit = unit, subset = subset, stat = stat,
    years = paste0(min(stat_years), "-", max(stat_years)),
    value = value, class = classes[1],
    latest_year = max(years), latest = latest, latest_class = classes[2],
    max = maxv, max_class = classes[3],
    stats_disagree = length(unique(classes)) > 1,
    threshold_reviewed = if (is.null(thr)) NA_character_ else thr$reviewed
  )
}

# All classes for one pathway: per group, per unit, Class A (all origins) and
# Class B (pest-present origins). kg always; pc only for plants for planting when
# a line reports stk (wood uses kg only, decided 2026-10-20: its Q2 pieces are
# pallets/crates). kg and pc are never summed together.
pc_pathways <- c("plants_for_planting")

classify_pathway <- function(imports, cn8_table, pathway, groups, present_countries, settings,
                             years, thresholds = read_thresholds()) {
  rows <- list()
  for (g in groups) {
    lines <- cn8_lines(cn8_table, pathway, g)
    units <- c("kg", if (pathway %in% pc_pathways && any(lines$q2_unit == "stk")) "pc")
    for (u in units) {
      unit_lines <- if (u == "pc") lines[lines$q2_unit == "stk", ] else lines
      for (s in c("all", "pest_present")) {
        rows[[length(rows) + 1]] <- classify_volume(
          imports, unit_lines, g, u, settings$volume_stat, years, settings$volume_n,
          s, present_countries, thresholds
        )
      }
    }
  }
  out <- dplyr::bind_rows(rows)
  out$pathway <- pathway
  # Flag when Class A (official) and Class B (informational) differ
  key <- paste(out$group, out$unit)
  a <- stats::setNames(out$class[out$subset == "all"], key[out$subset == "all"])
  out$a_b_differ <- out$class != a[key]
  out
}
