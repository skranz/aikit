ai_content = function(ai, err_val = NULL) {
  if (is(ai$content, "try-error")) return(err_val)
  if (is.null(ai$content)) return(err_val)
  ai$content
}

ai_combine_content_df = function(ai_li, ind_var=NULL, content_var=NULL) {
  
}

ai_combine_content_str = function(ai_li, ind_var=NULL, content_var=NULL, err_val=NA_character_) {
  sapply(ai_li, ai_content, err_val = err_val)
}
