examples = function()  {
  # Sample Dataframes
  df1 <- data.frame(
    id = 1:3,
    value = c("a", "b", "c"),
    data_year_start = c(2020L, 2021L, 2022L),
    extra_col = c(1.1, 2.2, 3.3)
  )

  df2 <- data.frame(
    id = 4:6,
    value = c("d", "e", "f"),
    data_year_start = c("2023", "2024", "2025"), # Character type conflict
    notes = c("note1", "note2", "note3")
  )

  df3 <- data.frame(
    id = 7:9,
    value = 7:9, # Numeric type conflict
    data_year_start = c(2026L, 2027L, 2028L)
  )

  df4 <- data.frame(
    id = 10:11,
    all_na_col = c(NA, NA) # Column with only NAs
  )


  df_list <- list(df1, df2, df3, df4)
  robust_bind_rows(df_list)
}

robust_bind_rows <- function(df_list, col_classes = NULL) {
  # Attempt to bind rows using dplyr::bind_rows
  bound_df <- try(dplyr::bind_rows(df_list), silent = TRUE)

  # If dplyr::bind_rows fails, proceed with robust binding
  if (inherits(bound_df, "try-error")) {
    cat("dplyr::bind_rows failed, initiating robust binding process...\n")

    # Get all column names across all dataframes
    all_names <- unique(unlist(lapply(df_list, names)))

    # Coerce all dataframes to have the same columns, filling with NA
    df_list <- lapply(df_list, function(df) {
      missing_cols <- setdiff(all_names, names(df))
      if (length(missing_cols) > 0) {
        df[missing_cols] <- NA
      }
      df[all_names]
    })

    # Initialize a list to store the processed columns
    processed_cols <- list()

    # Iterate over each column to resolve types
    for (col in all_names) {
      # Check if a specific class is provided by the user for this column
      if (!is.null(col_classes) && col %in% names(col_classes)) {
        target_type <- col_classes[[col]]
        cat(paste0("Column '", col, "': User-defined type '", target_type, "' specified.\n"))
        # Apply the user-defined type
        all_values <- unlist(lapply(df_list, function(df) df[[col]]))
        storage.mode(all_values) <- target_type
        processed_cols[[col]] <- all_values
        next # Move to the next column
      }

      # Get the types of the current column from all dataframes
      types <- sapply(df_list, function(df) class(df[[col]]))
      unique_types <- unique(types)

      # If there's only one type, no conflict to resolve
      if (length(unique_types) == 1) {
        processed_cols[[col]] <- unlist(lapply(df_list, `[[`, col))
        next
      }

      cat(paste0("Column '", col, "': Mismatched types found: ", paste(unique_types, collapse = ", "), ". Resolving...\n"))

      # Extract all values for the column
      all_values_list <- lapply(df_list, `[[`, col)

      # Identify dataframes where the column is all NA
      non_na_indices <- which(sapply(all_values_list, function(v) !all(is.na(v))))

      # If all are NA or no non-NA columns exist, just combine them
      if (length(non_na_indices) == 0) {
        processed_cols[[col]] <- unlist(all_values_list)
        next
      }

      # Consider only the types of columns that are not all NA
      active_types <- sapply(df_list[non_na_indices], function(df) class(df[[col]]))

      # Determine the most frequent type among the non-NA columns
      type_counts <- table(active_types)
      most_frequent_type <- names(type_counts)[which.max(type_counts)]
      cat(paste0("Column '", col, "': Most frequent type is '", most_frequent_type, "'. Attempting conversion.\n"))

      # Try converting all values to the most frequent type
      converted_values <- try({
        temp_list <- lapply(all_values_list, function(v) {
          storage.mode(v) <- most_frequent_type
          v
        })
        unlist(temp_list)
      }, silent = TRUE)

      # If conversion to the most frequent type fails, convert to character
      if (inherits(converted_values, "try-error")) {
        cat(paste0("Column '", col, "': Conversion to '", most_frequent_type, "' failed. Converting to character.\n"))
        processed_cols[[col]] <- as.character(unlist(all_values_list))
      } else {
        cat(paste0("Column '", col, "': Successfully converted to '", most_frequent_type, "'.\n"))
        processed_cols[[col]] <- converted_values
      }
    }

    # Combine the processed columns into a final dataframe
    bound_df <- as.data.frame(processed_cols, stringsAsFactors = FALSE)
  }

  return(bound_df)
}
