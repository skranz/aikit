# Create a NA-like structure that conforms to a JSON schema
.ai_na_from_schema <- function(schema) {
  restore.point(".ai_na_from_schema")
  if (is.null(schema) || is.null(schema$type)) return(NULL)
  type <- schema$type
  switch(type,
    "object" = {
      props <- schema$properties
      if (is.null(props)) return(tibble::tibble())
      na_list <- lapply(names(props), function(pname) {
        prop_schema <- props[[pname]]
        na_val <- .ai_na_from_schema(prop_schema)
        # Nested structures must be wrapped in a list to form a list-column
        if (prop_schema$type %in% c("object", "array")) {
          return(list(na_val))
        }
        na_val
      })
      names(na_list) <- names(props)
      return(tibble::as_tibble(na_list))
    },
    "array" = {
      # If array of objects, the NA form is an empty tibble with correct columns
      if (!is.null(schema$items) && schema$items$type == "object") {
        na_item_row <- .ai_na_from_schema(schema$items)
        if (is.data.frame(na_item_row)) return(na_item_row[0, , drop = FALSE])
      }
      # For an array of primitives, the NA form is an empty list.
      return(list())
    },
    "string" = NA_character_,
    "number" = NA_real_,
    "integer" = NA_integer_,
    "boolean" = NA,
    NA
  )
}

# Recursively parse content based on a JSON schema
.ai_parse_recursive <- function(content, schema) {
  restore.point(".ai_parse_recursive")
  if (is.null(schema)) return(content)
  if (is.null(content)) return(.ai_na_from_schema(schema))
  type <- schema$type
  if (is.null(type)) return(content)

  switch(type,
    "object" = {
      if (!is.list(content) || is.data.frame(content)) {
         if (is.data.frame(content)) return(content)
         return(.ai_na_from_schema(schema))
      }
      props <- schema$properties
      if (is.null(props)) return(tibble::as_tibble(content))
      parsed_list <- lapply(names(props), function(pname) {
        prop_schema <- props[[pname]]
        prop_content <- content[[pname]]
        parsed_val <- .ai_parse_recursive(prop_content, prop_schema)
        # Wrap nested results in a list to create list-columns
        if (prop_schema$type %in% c("object", "array")) {
          return(list(parsed_val))
        }
        parsed_val
      })
      names(parsed_list) <- names(props)
      return(tibble::as_tibble(parsed_list))
    },
    "array" = {
      if (!is.list(content) && !is.vector(content)) return(.ai_na_from_schema(schema))
      item_schema <- schema$items
      if (is.null(item_schema) || length(content) == 0) return(.ai_na_from_schema(schema))

      parsed_items <- lapply(content, .ai_parse_recursive, schema = item_schema)

      if (item_schema$type == "object") {
        return(robust_bind_rows(parsed_items))
      } else {
        return(unlist(parsed_items, recursive = FALSE))
      }
    },
    "string" = as.character(content),
    "number" = as.numeric(content),
    "integer" = as.integer(content),
    "boolean" = as.logical(content),
    content
  )
}

#' Parse content from an AI call based on a JSON schema
#' @param content The content list from an AI response.
#' @param schema The JSON schema for the content.
#' @param err_val Value to return on error or if content is NULL.
#' @return A tibble, list, or vector, depending on the schema.
ai_parse_content = function(content, schema = NULL, err_val = NULL) {
  restore.point("ai_parse_content")
  if (is(content, "try-error")) return(err_val)
  if (is.null(content)) {
    if (is.null(schema)) return(err_val)
    return(.ai_na_from_schema(schema))
  }
  if (is.null(schema)) {
    if (is.list(content) && !is.data.frame(content)) return(tibble::as_tibble(content))
    return(content)
  }
  .ai_parse_recursive(content, schema)
}

#' Extract content from an AI response object
#' @param ai The AI response object from `ai_run`.
#' @param err_val Value to return on error or if content is NULL.
#' @return The content, typically a list or tibble.
ai_content = function(ai, err_val = NULL) {
  if (is(ai, "try-error")) return(err_val)
  if (is.null(ai)) return(err_val)
  if (is(ai$content, "try-error")) return(err_val)
  if (is.null(ai$content)) return(err_val)
  ai$content
}

#' Combine content of multiple AI calls with similar content
#' @param ai_li List of response objects from AI calls.
#' @param add_df Optional data frame to prepend columns to the result.
#' @param schema The JSON schema that all responses should conform to.
#' @return A single tibble with combined results.
ai_combine_content_df = function(ai_li, add_df = NULL, schema = NULL) {
  restore.point("ai_combine_content_df")
  if (is.null(schema)) {
    ok_ai <- Filter(function(ai) !is.null(ai$content) && !is(ai$content, "try-error"), ai_li)
    if (length(ok_ai) > 0) schema <- ok_ai[[1]]$schema
  }
  if (is.null(schema)) warning("Could not determine schema from AI responses.")

  # Parse each AI call's content into a data frame
  li = lapply(seq_along(ai_li), function(i) {
    cont = ai_content(ai_li[[i]])
    df = ai_parse_content(cont, schema = schema)

    if (!is.null(add_df)) {
      if (is.null(df) || NROW(df) == 0) df <- add_df[i, ]
      else df = bind_left_or_set_col(df, add_df[i, ])
    }
    # We expect a single-row data frame here, where list-columns are correctly formatted
    df
  })

  #res = bind_rows(li)
  res = robust_bind_rows(li)
  res
}

#' Combine content of multiple AI calls into a character vector
#' @param ai_li List of response objects.
#' @param err_val Value for failed calls.
#' @return A character vector.
ai_combine_content_str = function(ai_li, err_val = NA_character_) {
  sapply(ai_li, function(ai) {
    res <- ai_content(ai, err_val = err_val)
    if (is.null(res)) return(err_val)
    if (is.list(res) || is.data.frame(res)) {
      if (length(res) > 0 && !is.list(res[[1]])) return(as.character(res[[1]]))
      return("[structured content]")
    }
    as.character(res)
  })
}

#' Examples for ai_combine_content_df
#'
#' Demonstrates how the function handles various schemas.
examples = function() {
  mock_ai <- function(content, schema) list(content = content, schema = schema)

  cat("\n--- Example 1: Flat Schema ---\n")
  schema_flat <- DataSchema::schema_obj(list(
    name = DataSchema::schema_str(), age = DataSchema::schema_int()
  ))
  ai_li_flat <- list(
    mock_ai(list(name = "Alice", age = 30), schema_flat),
    mock_ai(list(name = "Bob"), schema_flat),
    mock_ai(NULL, schema_flat)
  )
  print(ai_combine_content_df(ai_li_flat, schema = schema_flat))

  cat("\n--- Example 2: Nested Object (creates list-column with 1-row data frame) ---\n")
  schema_nested_obj <- DataSchema::schema_obj(list(
    name = DataSchema::schema_str(),
    address = DataSchema::schema_obj(list(city = DataSchema::schema_str(), zip = DataSchema::schema_int()))
  ))
  ai_li_nested_obj <- list(
    mock_ai(list(name = "Alice", address = list(city = "New York", zip = 10001)), schema_nested_obj),
    mock_ai(list(name = "Bob", address = NULL), schema_nested_obj)
  )
  df_nested_obj <- ai_combine_content_df(ai_li_nested_obj, schema = schema_nested_obj)
  print(str(df_nested_obj))
  class(df_nested_obj$address)

  cat("Structure of the 'address' list-column:\n")
  print(str(df_nested_obj$address, width = 60, strict.width = "cut"))

  cat("\n--- Example 3: Nested Array of Objects (creates list-column with multi-row data frame) ---\n")
  schema_nested_arr <- DataSchema::schema_obj(list(
    project = DataSchema::schema_str("Project Name"),
    tasks = DataSchema::schema_arr(
      items = DataSchema::schema_obj(list(task_name = DataSchema::schema_str(), done = DataSchema::schema_bool()))
    )
  ))
  ai_li_nested_arr <- list(
    mock_ai(list(project = "Project A", tasks = list(
      list(task_name = "Design", done = TRUE),
      list(task_name = "Implement", done = FALSE),
      list(task_name = "Test", done = FALSE)
    )), schema_nested_arr),
    mock_ai(list(project = "Project B", tasks = list(list(task_name = "Deploy", done = TRUE))), schema_nested_arr),
    mock_ai(list(project = "Project C", tasks = NULL), schema_nested_arr)
  )
  df_nested_arr <- ai_combine_content_df(ai_li_nested_arr, schema = schema_nested_arr)
  print(df_nested_arr)
  class(df_nested_arr$tasks)
  cat("Structure of the 'tasks' list-column:\n")
  print(str(df_nested_arr$tasks, width = 60, strict.width = "cut"))
  cat("First element of the 'tasks' list-column is a 3-row data frame:\n")
  print(df_nested_arr$tasks[[1]])

  cat("\n--- Example 4: Top-level Array of Objects ---\n")
  schema_top_array <- DataSchema::schema_arr(list(
    user = DataSchema::schema_str(), score = DataSchema::schema_int()
  ))
  ai_li_top_array <- list(
    mock_ai(list(list(user="Zoe", score=99), list(user="Yara", score=80)), schema_top_array),
    mock_ai(list(list(user="Xeno", score=75)), schema_top_array)
  )
  print(ai_combine_content_df(ai_li_top_array))
}
