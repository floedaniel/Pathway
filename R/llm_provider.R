# LLM provider for the ENT3 write-up (Step 9), via ellmer. Provider and model
# come from config.yml (llm.provider: openai | claude | hf). The LLM only sees
# the labelled blocks built in R/prompts.R; its output is validated and labelled.

llm_chat <- function(settings, system_prompt) {
  p <- settings$llm
  switch(p$provider,
    openai = ellmer::chat_openai(system_prompt = system_prompt, model = p$model, echo = "none"),
    claude = ellmer::chat_anthropic(system_prompt = system_prompt, model = p$model, echo = "none"),
    hf = ellmer::chat_openai_compatible(base_url = p$hf_base_url, system_prompt = system_prompt,
                                        model = p$model, credentials = function() Sys.getenv("HF_TOKEN"),
                                        echo = "none"),
    stop_app("bad_config", field = "llm.provider", value = p$provider)
  )
}

# Send one prompt; returns the reply text.
llm_ask <- function(settings, system_prompt, prompt) {
  chat <- llm_chat(settings, system_prompt)
  as.character(chat$chat(prompt, echo = "none"))
}
