You are absolutely right. My previous attempts incorrectly flattened the list-columns during the final `bind_rows` step. The core issue is that `dplyr::bind_rows` is "too helpful" and tries to row-bind list-columns if they contain data frames.

I've corrected this by replacing `dplyr::bind_rows` in `ai_combine_content_df` with a manual combining logic. This new logic iterates through each column name, gathers the corresponding values from each intermediate data frame, and correctly constructs the final columns—preserving lists of data frames as intended.

The other functions in the file (`.ai_parse_recursive`, etc.) were correctly creating the intermediate single-row tibbles with the right structure. The bug was confined to how they were combined at the end.

Here is the corrected `ai_combine_content_df` function. The rest of `content.R`, including the examples, remains correct as it was designed to test for the behavior we are now implementing.

!MODIFICATION ai_combine_content_df in R/content.R
scope = "function"
file = "R/content.R"
function_name = "ai_combine_content_df"
description = '''Rewrite `ai_combine_content_df` to manually combine rows. This prevents `dplyr::bind_rows` from incorrectly flattening list-columns, ensuring nested objects and arrays correctly result in list-columns of data frames.'''
---
```r
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

  # Manually combine the list of single-row data frames to correctly handle list-columns
  li <- li[!sapply(li, is.null)]
  if (length(li) == 0) return(tibble::tibble())

  # Get all unique column names
  all_names <- unique(unlist(lapply(li, names)))
  if (is.null(all_names)) return(tibble::tibble())

  # Build each final column by collecting elements from the list of data frames
  final_cols <- lapply(all_names, function(name) {
    # Get a list of the column 'name' from each data frame in 'li'
    col_list <- lapply(li, function(df) {
      if (name %in% names(df)) df[[name]] else NULL
    })

    # Check if it's a list-column by inspecting the first non-null element
    first_val <- Find(Negate(is.null), col_list)

    # A column is a list-column if its elements are themselves lists.
    # This correctly identifies columns where each entry is list(data.frame) or list(vector).
    if (is.list(first_val)) {
      # The elements are already wrapped in a list, e.g., list(df1), list(c(1,2,3)).
      # We extract the inner element to form the final list for the column.
      lapply(col_list, function(item) {
        if (is.null(item)) return(NULL)
        item[[1]]
      })
    } else {
      # It's a simple vector column. Combine into a single vector.
      unlist(col_list, recursive = FALSE)
    }
  })
  names(final_cols) <- all_names

  tibble::as_tibble(final_cols)
}
```
!END_MODIFICATION ai_combine_content_df in R/content.R
