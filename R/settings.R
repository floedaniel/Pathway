# Settings from config.yml and secrets from environment variables.
# Env vars (e.g. from ~/.Renviron) win. If one is unset, it is read from the
# git-ignored local key folder API_kays/ (first line of the file).

local_key_files <- c(EPPO_KEY = "EPPO_beta.txt", ANTHROPIC_API_KEY = "anthropic_key.txt",
                     HF_TOKEN = "hugging_face.txt")

load_local_keys <- function(dir = "API_kays") {
  for (var in names(local_key_files)) {
    f <- file.path(dir, local_key_files[[var]])
    if (!nzchar(Sys.getenv(var)) && file.exists(f)) {
      key <- trimws(readLines(f, n = 1, warn = FALSE))
      if (nzchar(key)) do.call(Sys.setenv, stats::setNames(list(key), var))
    }
  }
  invisible(NULL)
}

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
