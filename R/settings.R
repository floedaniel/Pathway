# Settings from config.yml and secrets from environment variables.
# Secrets are never read from files: set them in ~/.Renviron.

load_settings <- function(file = "config.yml", profile = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  config::get(file = file, config = profile)
}

# Env vars needed for a given settings list. EPPO is always needed;
# the LLM key depends on the configured provider.
required_secrets <- function(settings) {
  llm_key <- switch(settings$llm$provider,
    claude = "ANTHROPIC_API_KEY",
    hf = "HF_TOKEN",
    stop_app("bad_config", field = "llm.provider", value = settings$llm$provider)
  )
  c("EPPO_KEY", llm_key)
}

check_secrets <- function(settings) {
  needed <- required_secrets(settings)
  missing <- needed[!nzchar(Sys.getenv(needed))]
  if (length(missing) > 0) {
    stop_app("missing_secret", vars = paste(missing, collapse = ", "))
  }
  invisible(needed)
}
