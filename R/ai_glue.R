# glue as convenient for ai

ai_glue = function(ai, values=ai$values) {
  restore.point("ai_glue")
  if (length(values)>0) {
    ai$prompt = ai_replace_whisker(ai$tpl, values)
  } else {
    ai$prompt = ai$tpl
  }
  ai
}

example = function() {
  tpl = "Hi {{name}}!"
}

ai_tpl_vars = function(txt) {
  pattern <- "\\{\\{\\s*(.*?)\\s*\\}\\}"
  vars <- unique(stri_match_all_regex(txt, pattern)[[1]][,2])
  vars
}

ai_replace_whisker = function(txt, values) {
  restore.point("ai_replace_whisker")
  # Define regex with a capturing group for content inside {{ }}
  pattern <- "\\{\\{\\s*(.*?)\\s*\\}\\}"

  # Locate positions of all matches in the string
  matches <- stri_match_all_regex(txt, pattern)[[1]]
  if (NROW(matches)==0) return(txt)

  symbols = unique(matches[,2])

  vars = names(values)
  missing = setdiff(symbols, vars)
  if (length(missing)>0) {
    stop("The whisker symbols ", paste0(missing, collapse=", "), " are not in values.")
  }
  vals = unlist(values[symbols])
  whiskers =  paste0("{{", symbols,"}}")
  # Replace all occurrences simultaneously using fixed replacement
  result <- stri_replace_all_fixed(txt, whiskers, vals, vectorize_all = FALSE)
  result
}
