ai_content = function(ai, err_val = NULL) {
  if (is(ai$content, "try-error")) return(err_val)
  if (is.null(ai$content)) return(err_val)
  ai$content
}

#' Combine content of multiple AI calls with similar content
#'
#' ai_li list of response object from ai call
#' add_df optonal a data frame with as many rows as elements of ai_li. Columns in add_df will be added on the left of ai_li
ai_combine_content_df = function(ai_li, add_df=NULL) {
  li = lapply(seq_along(ai_li), function(i) {
    cont = ai_content(ai_li[[i]])
    if (is.null(cont)) return(NULL)
    if (!is.data.frame(cont)) {
      cont = as_tibble(cont)
    }
    if (!is.null(add_df)) {
      cont = bind_cols(add_df[i,], cont)
    }
    cont
  })
  bind_rows(li)
}

ai_combine_content_str = function(ai_li, ind_var=NULL, content_var=NULL, err_val=NA_character_) {
  sapply(ai_li, ai_content, err_val = err_val)
}
