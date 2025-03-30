union_by_name = function(x,y) {
  nx = names(x)
  ny = names(y)
  dupl_y = ny %in% nx
  c(x,ny[!dupl_y])
}



example = function() {
  rclasses_to_na_list(c(a="numeric",b="logical",c="list"))
  rclasses_to_na_tibble(c(a="numeric",b="logical",c="list"))

}

first_class = function(x) {
  class(x)[1]
}

obj_li_rclasses = function(obj_li, rclasses=NULL) {
  #item = obj_li[[1]]
  for (item in obj_li) {
    new_names = setdiff(names(item),names(rclasses))
    if (length(new_names)==0) next
    new_classes = sapply(item[new_names], first_class)
    rclasses = c(rclasses, new_classes)
  }
  rclasses
}

rclasses_to_na_list <- function(class_vec) {
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
           "list" = list(NULL),
           # Default case for unknown classes
           list(NULL))
  })
}

rclasses_to_na_tibble <- function(class_vec) {
  as_tibble(rclasses_to_na_list(class_vec))
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
