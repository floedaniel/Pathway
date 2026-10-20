# Disk cache. One RDS file per entry: cache/<source>/<hash>.rds holding
# list(value, fetched_at, query). Key = hash of the query; TTL per source from
# config.yml (null = permanent, e.g. taxonomy).

cache_key <- function(query) {
  digest::digest(query, algo = "xxhash64")
}

cache_path <- function(source, query, dir) {
  file.path(dir, source, paste0(cache_key(query), ".rds"))
}

cache_ttl <- function(source, settings) {
  if (!source %in% names(settings$cache$ttl_days)) {
    rlang::abort(paste0("No cache TTL configured for source: ", source))
  }
  ttl <- settings$cache$ttl_days[[source]]
  if (is.null(ttl)) Inf else ttl
}

# Returns the stored entry, or NULL if missing or expired.
cache_get <- function(source, query, settings, now = Sys.time()) {
  ttl <- cache_ttl(source, settings)
  path <- cache_path(source, query, settings$cache$dir)
  if (!file.exists(path)) return(NULL)
  entry <- readRDS(path)
  age_days <- as.numeric(difftime(now, entry$fetched_at, units = "days"))
  if (age_days > ttl) return(NULL)
  entry
}

cache_set <- function(source, query, value, settings, now = Sys.time()) {
  path <- cache_path(source, query, settings$cache$dir)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  entry <- list(value = value, fetched_at = now, query = query)
  saveRDS(entry, path)
  entry
}

# Return the cached entry for `query`, or call fetch() and cache its result.
# The entry carries fetched_at so the UI can show fetch dates.
cached <- function(source, query, fetch, settings, now = Sys.time()) {
  hit <- cache_get(source, query, settings, now = now)
  if (!is.null(hit)) return(hit)
  cache_set(source, query, fetch(), settings, now = now)
}
