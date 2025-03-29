ai_content = function(ai, err_val = NULL) {
  if (is(ai$content, "try-error")) return(err_val)
  if (is.null(ai$content)) return(err_val)
  ai$content
}

#' Combine content of multiple AI calls with similar content
#'
#' ai_li list of response object from ai call
#' add_df optonal a data frame with as many rows as elements of ai_li. Columns in add_df will be added on the left of ai_li
ai_combine_content_df = function(ai_li, add_df=NULL, atom_null_to_na = TRUE, rclasses=NULL) {
  restore.point("ai_combine_content_df")
  cont_li = lapply(ai_li, ai_content)
  na_obj = ai_li_na_obj(cont_li, rclasses)

  i = 1
  li = lapply(seq_along(ai_li), function(i) {
    #cat("\n",i)
    cont = cont_li[[i]]
    if (is.null(cont)) return(NULL)
    if (!is.data.frame(cont)) {
      obj = na_obj
      non_null_fields = names(cont)[!sapply(cont, is.null)]
      obj[non_null_fields] = cont[non_null_fields]
      cont = as_tibble(obj)
    }
    if (!is.null(add_df)) {
      cont = bind_left_or_set_col(cont, add_df[i,])
    }
    cont
  })
  bind_rows(li)
}

bind_left_or_set_col = function(df, lhs=NULL) {
  if (NROW(lhs)==0 | NCOL(lhs)==0) return(df)
  set_names = intersect(names(df), names(lhs))
  df[set_names] = lhs[set_names]
  rem_names = setdiff(names(lhs), set_names)
  if (length(rem_names)>0) {
    df = bind_cols(lhs[rem_names], df)
  }
  df

}

ai_combine_content_str = function(ai_li, ind_var=NULL, content_var=NULL, err_val=NA_character_) {
  sapply(ai_li, ai_content, err_val = err_val)
}

ai_li_na_obj = function(cont_li, rclasses=NULL) {
  restore.point("ai_li_na_obj")
  # Get the union of all names from non-null elements in ai_li
  all_names <- unique(unlist(lapply(cont_li, function(x) {
    if (!is.null(x)) names(x) else NULL
  })))

  all_names = union(names(rclasses), all_names)
  # Determine the type for each field by checking the first occurrence in ai_li

  na_vals <- lapply(all_names, function(x) NULL)
  names(na_vals) = all_names
  if (length(rclasses)>0)
    na_vals[names(rclasses)] = class_to_na_or_null(rclasses)


  rem_fields = setdiff(all_names, names(rclasses))
  my_class = function(x) class(x)[1]

  item = cont_li[[1]]
  for (item in cont_li) {
    names = intersect(rem_fields, names(item))
    if (length(names)==0) next
    class_vec = lapply(item[names], my_class)
    na_vals[names] = class_to_na_or_null(class_vec)
    rem_fields = setdiff(rem_fields, names)
    if (length(rem_fields)==0) break
  }

  return(na_vals)
}


class_to_na_or_null <- function(class_vec) {
  # Create NA values for each class in the input vector
  lapply(class_vec, function(cls) {
    switch(cls,
           "numeric" = NA_real_,
           "integer" = NA_integer_,
           "character" = NA_character_,
           "logical" = NA,
           "complex" = NA_complex_,
           "Date" = as.Date(NA),
           "POSIXct" = as.POSIXct(NA),
           "POSIXt" = as.POSIXlt(NA),
           # Default case for unknown classes
           NULL)
  })
}

