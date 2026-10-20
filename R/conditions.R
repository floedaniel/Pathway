# Typed stop conditions. stop_app("no_hosts", code = "XYLEFA") signals a
# condition of class c("pathwise_stop_no_hosts", "pathwise_stop", "error", "condition").
# The server catches "pathwise_stop" and shows one alert replacing all panels.

stop_messages <- c(
  invalid_code      = "Unknown or non-pest EPPO code.",
  no_hosts          = "No hosts registered in EPPO for {code}. No assessment possible.",
  no_main_minor     = "No main or minor hosts registered.",
  unreviewed_hosts  = "No reviewed woody hosts. Unreviewed: {hosts}. Add to woody_taxa.csv.",
  no_groups         = "No hosts covered by current commodity groups (v1: woody plants).",
  no_distribution   = "No present-country records in EPPO.",
  unit_mismatch     = "Unit mismatch for {group}: SSB {unit}, expected kg or pc.",
  missing_secret    = "Missing environment variable(s): {vars}. Set them in ~/.Renviron and restart R.",
  bad_config        = "Invalid setting {field}: {value}."
)

stop_app <- function(type, ..., call = rlang::caller_env()) {
  if (!type %in% names(stop_messages)) {
    rlang::abort(paste0("Unknown stop type: ", type))
  }
  data <- list(...)
  msg <- glue_fields(stop_messages[[type]], data)
  rlang::abort(
    msg,
    class = c(paste0("pathwise_stop_", type), "pathwise_stop"),
    stop_type = type,
    stop_data = data,
    call = call
  )
}

# Replace {name} placeholders with values from `data`. Missing fields are an error,
# so a stop never shows a half-filled message.
glue_fields <- function(template, data) {
  fields <- regmatches(template, gregexpr("\\{[a-z_]+\\}", template))[[1]]
  for (f in unique(fields)) {
    name <- gsub("[{}]", "", f)
    if (is.null(data[[name]])) {
      rlang::abort(paste0("Missing field '", name, "' for message: ", template))
    }
    template <- gsub(f, as.character(data[[name]]), template, fixed = TRUE)
  }
  template
}
