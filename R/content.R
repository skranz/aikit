# Sometimes the returned content has the structure of the schema object...
ai_correct_schema_content = function(content, schema=NULL, rclasses = schema_fields_to_rclasses(schema)) {
  restore.point("ai_correct_schema_response_content")
  if (!"type" %in% names(content)) return(content)
  if (all(c("properties","allOf") %in% names(content))) {
    # only works for flat tables
    li = lapply(content$allOf$properties, function(df) unlist(df, use.names=FALSE))
    return(li)
  }

  remove_from_content = function(field) {
    if (field %in% names(content) & !field %in% names(rclasses))
    content = content[setdiff(name(content), field)]
    content
  }

  if (!is.null(rclasses)) {
    content = remove_from_content("properties")
    content = remove_from_content("type")
  }
  return(content)
}

ai_parse_content = function(content, schema=NULL, err_val = NULL, as_tibble = TRUE, correct_schema_content=TRUE, rclasses=NULL, na_obj=NULL) {
  restore.point("ai_parse_content")

  if (is(content, "try-error")) return(err_val)
  if (is.null(content)) return(err_val)

  if (!is.null(schema)) {
    new_rclasses = DataSchema::schema_fields_to_rclasses(schema)
    rclasses = union_by_name(new_rclasses, rclasses)
  }

  if (correct_schema_content) {
    content = ai_correct_schema_content(content,  rclasses=rclasses)
  }

  if (is.null(na_obj)) {
    na_obj = rclasses_to_na_list(rclasses)
  } else {
    na_obj = as.list(na_obj)
  }
  obj = na_obj
  non_null_fields = names(content)[!sapply(content, is.null)]
  obj[non_null_fields] = content[non_null_fields]
  if (as_tibble) return(as_tibble(obj))
  obj
}

ai_content = function(ai, err_val = NULL) {
  if (is(ai$content, "try-error")) return(err_val)
  if (is.null(ai$content)) return(err_val)
  ai$content
}



#' Combine content of multiple AI calls with similar content
#'
#' ai_li list of response object from ai call
#' add_df optonal a data frame with as many rows as elements of ai_li. Columns in add_df will be added on the left of ai_li
ai_combine_content_df = function(ai_li, add_df=NULL, atom_null_to_na = TRUE,schema=NULL, rclasses=NULL) {
  restore.point("ai_combine_content_df")
  cont_li = lapply(ai_li, ai_content)
  if (is.null(schema) & is.null(rclasses)) {
    rclasses = obj_li_rclasses(cont_li, rclasses)
  } else if (!is.null(schema)) {
    rclasses = schema_fields_to_rclasses(schema)
  }
  na_obj = rclasses_to_na_list(rclasses)

  li = lapply(seq_along(cont_li), function(i) {
    cont = cont_li[[i]]
    if (is.null(cont)) return(NULL)
    df = ai_parse_content(cont,na_obj=na_obj)
    if (!is.null(add_df)) {
      df = bind_left_or_set_col(df, add_df[i,])
    }
    df
  })
  bind_rows(li)
}

ai_combine_content_str = function(ai_li, ind_var=NULL, content_var=NULL, err_val=NA_character_) {
  sapply(ai_li, ai_content, err_val = err_val)
}

