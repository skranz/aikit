set_ai_opts = function(model = c("gemini-2.0-flash", "gemini-1.5-flash-001")[1], temperature=0, cache_context = model == "gemini-1.5-flash-001") {
  ai_opts = list(model = model, temperature=0, cache_context=cache_context)
  options(repbox_ai_opts = ai_opts)
  ai_opts
}

get_ai_opts = function() {
  ai_opts = getOption("repbox_ai_opts")
  if (is.null(ai_opts)) return(set_ai_opts())
  ai_opts
}

ai_model_short = function(model) {
  case_when(
    model == "gemini-2.0-flash" ~ "g2f",
    model == "gemini-1.5-flash-001" ~ "g15f",
    model == "gemini-2.0-flash-lite-preview-02-05" ~ "g2flp",
    model == "gemini-2.0-flash-lite" ~ "g2fl",
    model == "gemini-2.0-flash-thinking-exp" ~ "g2fte",
    TRUE ~ model    
  )
}
