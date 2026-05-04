#' @importFrom dplyr tbl select filter mutate left_join bind_rows arrange distinct group_by summarise ungroup pull count n rename
#' @importFrom DBI dbWriteTable dbExecute
#' @importFrom stringr str_detect str_length str_sub str_split str_c
#' @importFrom stringi stri_sub
#' @importFrom tidyr separate_rows
#' @importFrom lubridate as_date as_datetime year month day
#' @importFrom openssl sha256
#' @importFrom digest digest
#' @importFrom data.table data.table setDT setorder rbindlist

#' Map Codes to Standard Concepts
#'
#' This function maps a vector of codes to standard concepts using a provided
#' concepts and relationships data frame. It attempts to map directly and then
#' by shortening the codes if necessary. The function supports vocabulary
#' ordering, dot handling, and truncation options for flexible code mapping.
#'
#' @param codes A character vector of codes to be mapped.
#' @param concepts A data frame containing concept information with columns:
#'   `concept_id`, `concept_name`, `domain_id`, `vocabulary_id`,
#'   `concept_class_id`, `standard_concept`, `concept_code`, `valid_start_date`,
#'   `valid_end_date`, `invalid_reason`.
#' @param relationships A data frame containing relationships between concepts
#'   with columns: `concept_id_1`, `concept_id_2`, `relationship_id`,
#'   `valid_start_date`, `valid_end_date`, `invalid_reason`.
#' @param connection A database connection object for temporary table operations.
#' @param vocabulary A data frame containing vocabulary information with columns:
#'   `vocabulary_id`, `vocabulary_name`, `vocabulary_reference`, `vocabulary_version`,
#'   `vocabulary_concept_id`.
#' @param vocab_order A character vector specifying the order of vocabularies
#'   by preference (highest to lowest). If NULL or NA, all vocabularies are
#'   treated equally.
#' @param expect_dots A logical value indicating whether to expect dots in the
#'   codes. If FALSE, dots will be removed from the codes. Default is TRUE.
#' @param allow_truncation A logical value indicating whether to allow
#'   truncation of codes for mapping. Default is TRUE.
#' @param min_length An integer specifying the minimum length for truncated
#'   codes. Default is 1.
#'
#' @return A data frame with columns `original_code`, `standard_concept_id`,
#'   `mapped_concept_id`, `direct_mapping`, `vocabulary_id`, `rank`,
#'   `source_vocabulary_id`, and `mapping_to_use`, mapping each input code
#'   to concept information.
#' @export
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' library(DBI)
#'
#' # Create sample data
#' concepts <- tibble(
#'   concept_id = c(1, 2, 3),
#'   concept_name = c("A01.1", "A01", "B02"),
#'   domain_id = c("D1", "D1", "D2"),
#'   vocabulary_id = c("ICD10CM", "ICD10CM", "SNOMED"),
#'   concept_class_id = c("C1", "C1", "C2"),
#'   standard_concept = c("S", "C", "S"),
#'   concept_code = c("A01.1", "A01", "B02"),
#'   valid_start_date = as.Date("2020-01-01"),
#'   valid_end_date = as.Date("2099-12-31"),
#'   invalid_reason = NA
#' )
#'
#' relationships <- tibble(
#'   concept_id_1 = c(2),
#'   concept_id_2 = c(1),
#'   relationship_id = c("Maps to"),
#'   valid_start_date = as.Date("2020-01-01"),
#'   valid_end_date = as.Date("2099-12-31"),
#'   invalid_reason = NA
#' )
#'
#' vocabulary <- tibble(
#'   vocabulary_id = c("ICD10CM", "SNOMED"),
#'   vocabulary_name = c("ICD10CM", "SNOMED CT"),
#'   vocabulary_reference = c("", ""),
#'   vocabulary_version = c("", ""),
#'   vocabulary_concept_id = c(44819096, 44819097)
#' )
#'
#' codes <- c("A01.1", "A01.2", "B02", "C03")
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' result <- map_codes(codes, concepts, relationships, con, vocabulary,
#'   vocab_order = c("SNOMED", "ICD10CM")
#' )
#' print(result)
#'
#' DBI::dbDisconnect(con)
#' }
map_codes <- function(
  codes,
  concepts,
  relationships,
  connection,
  vocabulary,
  vocab_order = NULL,
  expect_dots = TRUE,
  allow_truncation = TRUE,
  min_length = 1
) {
  # Create code map tibble
  code_map <- tibble(raw_codes = codes) %>%
    filter(!is.na(raw_codes)) %>%
    mutate(
      formatted_codes = stringr::str_replace(raw_codes, "^[^[:alnum:]]+", "")
    )

  # Assert that vocab_order is NA, NULL, or a character vector
  if (
    !is.null(vocab_order) &&
      !is.character(vocab_order) &&
      !all(is.na(vocab_order))
  ) {
    stop("vocab_order must be NA, NULL, or a character vector.")
  }

  if (is.vector(vocab_order) && is.character(vocab_order)) {
    print(
      "Use vocab order. Verify that they are sorted from highest to lowest preference."
    )
    vocab_tibble <- tibble(
      vocabulary_id = vocab_order
    ) %>%
      mutate(
        rank = row_number()
      )

    joined_vocab <- vocab_tibble %>%
      left_join(vocabulary, copy = TRUE)

    if (any(is.na(joined_vocab$vocabulary_name))) {
      missing <- joined_vocab %>%
        filter(is.na(vocabulary_name)) %>%
        pull(vocabulary_id)
      stop(sprintf(
        "Vocabularies %s are not present in the vocabulary table.",
        paste(missing, collapse = ", ")
      ))
    }

    concepts_to_use <- concepts %>%
      inner_join(vocab_tibble, by = "vocabulary_id", copy = TRUE)
  } else {
    # Set rank to 1 for all concepts
    concepts_to_use <- concepts %>%
      mutate(rank = 1)
  }

  # Fix dots
  if (!expect_dots) {
    code_map <- code_map %>%
      mutate(formatted_codes = gsub("\\.", "", raw_codes))
    codes <- unique(code_map$formatted_codes)
    concepts_to_use <- concepts_to_use %>%
      mutate(concept_code = sql("replace(concept_code, '.', '')"))
  }

  # Initial mapping
  initial_mapping <- concepts_to_use %>%
    inner_join(
      code_map,
      by = c("concept_code" = "formatted_codes"),
      copy = TRUE
    ) %>%
    mutate(original_code = concept_code) %>%
    collect()

  n_total <- length(codes)
  n_mapped <- length(unique(initial_mapping$concept_code))
  print(sprintf("%s/%s concepts were mapped directly", n_mapped, n_total))
  print(
    initial_mapping %>%
      group_by(vocabulary_id) %>%
      count() %>%
      arrange(desc(n))
  )

  # Codes that cannot be mapped directly
  remaining_codes <- code_map %>%
    anti_join(initial_mapping, by = c("formatted_codes" = "concept_code")) %>%
    distinct() %>%
    filter(!is.na(formatted_codes))

  print(sprintf("%s codes remain to be mapped", nrow(remaining_codes)))

  if (nrow(remaining_codes) > 0 && allow_truncation) {
    match <- remaining_codes %>%
      mutate(
        id = row_number(),
        orig_length = stringr::str_length(formatted_codes)
      ) %>%
      rowwise() %>%
      mutate(lengths = list((orig_length - 1):1)) %>%
      unnest(lengths) %>%
      filter(lengths >= min_length) %>%
      mutate(truncated = stringr::str_sub(formatted_codes, 1, lengths)) %>%
      inner_join(
        concepts_to_use,
        by = c("truncated" = "concept_code"),
        copy = TRUE,
        relationship = "many-to-many"
      ) %>%
      group_by(id) %>%
      slice_max(lengths, n = 1, with_ties = TRUE) %>%
      ungroup()

    shortened_mapping <- remaining_codes %>%
      left_join(match) %>%
      select(-id, -orig_length, -lengths) %>%
      rename(
        original_code = formatted_codes,
        concept_code = truncated
      )

    if (!"concept_id" %in% colnames(shortened_mapping)) {
      shortened_mapping <- shortened_mapping %>%
        mutate(
          concept_id = NA,
          standard_concept = NA,
          vocabulary_id = NA,
          rank = NA
        )
    }
    print("Mapped the remaining codes.")

    # Combine both
    combined_mapping <- initial_mapping %>%
      select(
        raw_codes,
        original_code,
        concept_id,
        standard_concept,
        vocabulary_id,
        rank
      ) %>%
      bind_rows(
        shortened_mapping %>%
          select(
            raw_codes,
            original_code,
            concept_id,
            standard_concept,
            vocabulary_id,
            rank
          )
      )
  } else {
    combined_mapping <- initial_mapping %>%
      select(
        raw_codes,
        original_code,
        concept_id,
        standard_concept,
        vocabulary_id,
        rank
      )
  }

  dbWriteTable(
    connection,
    "combined_mapping",
    combined_mapping,
    temporary = TRUE,
    overwrite = TRUE
  )
  combined_mapping_tbl <- tbl(connection, "combined_mapping")

  print("Generated combined mapping")

  # Map standard concepts directly
  final_mapping <- combined_mapping_tbl %>%
    filter(standard_concept == "S") %>%
    mutate(
      standard_concept_id = concept_id,
      direct_mapping = TRUE,
      mapped_concept_id = concept_id,
      source_vocabulary_id = vocabulary_id
    ) %>%
    select(
      raw_codes,
      original_code,
      standard_concept_id,
      mapped_concept_id,
      direct_mapping,
      vocabulary_id,
      rank,
      source_vocabulary_id
    )

  # Non standard concepts
  non_standard <- combined_mapping_tbl %>%
    anti_join(
      final_mapping,
      by = c(
        "original_code" = "original_code",
        "concept_id" = "mapped_concept_id"
      )
    ) %>%
    group_by(original_code) %>%
    slice_min(order_by = rank, n = 1, with_ties = TRUE) %>%
    left_join(
      relationships %>% filter(relationship_id == "Maps to"),
      by = c("concept_id" = "concept_id_1"),
      copy = TRUE
    ) %>%
    rename(
      original_concept_id = concept_id,
      related_concept_id = concept_id_2,
      source_vocabulary_id = vocabulary_id
    ) %>%
    left_join(
      concepts,
      by = c("related_concept_id" = "concept_id"),
      copy = TRUE
    ) %>%
    mutate(direct_mapping = FALSE) %>%
    select(
      raw_codes,
      original_code,
      standard_concept_id = related_concept_id,
      mapped_concept_id = original_concept_id,
      vocabulary_id,
      rank,
      source_vocabulary_id,
      direct_mapping
    )

  # Combine the entire mapping
  final <- final_mapping %>%
    union(non_standard) %>%
    distinct() %>%
    group_by(original_code) %>%
    slice_min(order_by = rank, n = 1, with_ties = TRUE) %>%
    ungroup() %>%
    mutate(
      mapping_to_use = ifelse(
        is.na(standard_concept_id),
        mapped_concept_id,
        standard_concept_id
      )
    ) %>%
    collect()

  # Add the non-mappable codes
  all_mapped <- final %>%
    select(-original_code) %>%
    rename(original_code = raw_codes) %>%
    right_join(
      tibble(original_code = unique(code_map$raw_codes)),
      by = "original_code"
    ) %>%
    distinct()

  # Ratio of unmapped codes
  n_unmapped <- length(unique(all_mapped$original_code[is.na(
    all_mapped$mapping_to_use
  )]))
  n_nonstandard <- length(unique(all_mapped$original_code[
    is.na(all_mapped$standard_concept_id) & !is.na(all_mapped$mapping_to_use)
  ]))
  n_total <- nrow(all_mapped)
  print(sprintf("%s out of %s are not mappable", n_unmapped, n_total))
  print(sprintf(
    "%s out of %s are not standard",
    n_nonstandard,
    (n_total - n_unmapped)
  ))

  # Drop temporary tables
  if (dbExistsTable(connection, "combined_mapping")) {
    dbRemoveTable(connection, "combined_mapping")
  }

  # Check that each code is only mapped once
  if (any(duplicated(all_mapped$original_code))) {
    duplicated_codes <- all_mapped$original_code[duplicated(
      all_mapped$original_code
    )]
    print("Duplicated codes found:")
    print(
      all_mapped %>%
        filter(original_code %in% duplicated_codes) %>%
        glimpse()
    )

    # Check if there is more than one vocabulary used for the duplicated codes
    num_vocabs_larger_2 <- all_mapped %>%
      filter(original_code %in% duplicated_codes) %>%
      group_by(original_code) %>%
      summarise(num_vocabs = n_distinct(rank), n = n(), .groups = "drop") %>%
      filter(num_vocabs > 1) %>%
      nrow()

    if (num_vocabs_larger_2 > 0) {
      print(sprintf(
        "There are %s codes that are mapped to more than one vocabulary.",
        num_vocabs_larger_2
      ))
      stop("Please check the mapping.")
    } else {
      print("No codes are mapped to more than one vocabulary.")
    }
  }

  # Print count per vocabulary_id in final mapping
  print("Final mapping counts by vocabulary:")
  print(
    all_mapped %>%
      filter(!is.na(mapping_to_use)) %>%
      group_by(source_vocabulary_id) %>%
      count() %>%
      arrange(desc(n))
  )

  return(all_mapped)
}


#' convert a vector of dates to a vector of POSIXct datetimes
#'
#' Use a default time string of "00:00:00" and a default date format of "%Y-%m-%d"
#'
#' @param date_vector A vector of dates
#' @param time_string A string representing the time to be added to the date
#' @param date_format A string representing the format of the dates in date_vector
#'
#' @return A vector of POSIXct datetimes
#' @export
convert_to_datetime_from_date <- function(
  date_vector,
  time_string = "00:00:00",
  date_format = "%Y-%m-%d"
) {
  return(
    as.POSIXct(
      paste0(date_vector, " ", time_string),
      format = paste0(date_format, " %H:%M:%S"),
      tz = "UTC"
    )
  )
}


#' Convert a vector of dates to a vector of POSIXct datetimes
#'
#' Use a default time string of "00:00:00"
#'
#' @param year A vector of years
#' @param month A vector of months
#' @param day A vector of days
#' @param time_string A string representing the time to be added to the date
#'
#' @return A vector of POSIXct datetimes
#' @export
convert_to_datetime <- function(year, month, day, time_string = "00:00:00") {
  return(
    as.POSIXct(
      paste0(year, "-", month, "-", day, " ", time_string),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  )
}

# Function to generate unique 12-digit numerical IDs
#' Generate Unique ID
#'
#' This function generates a unique 12-digit numerical ID.
#'
#' @param n The input number.
#' @return An integer representation of the unique ID.
#' @examples
#' generate_unique_id(1)
#' generate_unique_id(10)
#' @export
generate_unique_id <- function(n) {
  return(as.integer(sprintf("%012d", n)))
}


#' Truncate a string to a specified limit
#'
#' This function truncates a given string to a specified limit. If the length of the string
#' is greater than the limit, it will be truncated to the specified limit. Otherwise, the
#' original string will be returned.
#'
#' @param x The string to be truncated.
#' @param limit The maximum length of the truncated string.
#'
#' @return The truncated string.
#'
#' @examples
#' truncate("Hello, world!", 5)
#' # Output: "Hello"
#'
#' truncate("Hello, world!", 20)
#' # Output: "Hello, world!"
#'
#' @export
truncate <- function(x, limit) {
  ifelse(nchar(x) > limit, substr(x, 1, limit), x)
}


#' Convert a string to a date object
#'
#' This function takes a string in the format "YYYYMMDD" and converts it to a date object.
#'
#' @param x A character string representing a date in the format "YYYYMMDD".
#' @return A date object representing the input date.
#' @examples
#' convert_date("20211231")
#' # Output: "2021-12-31"
#' @export
convert_date <- function(x) {
  year <- substr(x, 1, 4)
  month <- substr(x, 5, 6)
  day <- substr(x, 7, 8)
  return(as.Date(paste(year, month, day, sep = "-")))
}


#' Pseudonymize a number using SHA256 hash
#'
#' @param num The number to pseudonymize
#' @param salt Salt to add to the hash
#' @return A pseudonymized string
#' @export
pseudonymize <- function(num, salt = "real4reg") {
  num_with_salt <- paste(as.character(num), salt, sep = "_")
  hash <- openssl::sha256(num_with_salt)
  pseudonym <- stringi::stri_sub(hash, 1, 12)
  return(pseudonym)
}

#' Generate unique ID from ID and code combination
#'
#' @param id The ID value
#' @param code The code value
#' @return A unique hashed ID
#' @export
generate_unique_id <- function(id, code) {
  combined <- paste(id, code)
  hashed <- abs(digest::digest2int(as.character(combined)))
  return(hashed)
}


#' Merge overlapping date intervals
#'
#' @param starts Vector of start dates
#' @param ends Vector of end dates
#' @return Data frame with merged intervals
#' @export
merge_intervals <- function(starts, ends) {
  # Initialize empty date vectors so they remain Date objects
  merged_starts <- as.Date(character())
  merged_ends <- as.Date(character())

  # Start with the first interval
  current_start <- starts[1]
  current_end <- ends[1]

  # Loop over the remaining intervals
  if (length(starts) > 1) {
    for (i in 2:length(starts)) {
      # Check if intervals are overlapping or consecutive (next start within one day after current_end)
      if (
        !is.na(current_end) && !is.na(starts[i]) && starts[i] <= current_end + 1
      ) {
        # If the new interval is open-ended, the merged interval becomes open-ended
        if (is.na(ends[i])) {
          current_end <- NA
        } else {
          current_end <- max(current_end, ends[i])
        }
      } else {
        # Save the current merged interval and restart with the new one
        merged_starts <- c(merged_starts, current_start)
        merged_ends <- c(merged_ends, current_end)
        current_start <- starts[i]
        current_end <- ends[i]
      }
    }
  }
  # Append the final interval
  merged_starts <- c(merged_starts, current_start)
  merged_ends <- c(merged_ends, current_end)

  data.frame(start = merged_starts, end = merged_ends)
}

#' Merge intervals using data.table approach
#'
#' @param dt_person Data table with start and end columns
#' @return List of merged intervals
#' @export
merge_intervals_dt <- function(dt_person) {
  data.table::setorder(dt_person, start, end)
  merged <- list()
  current_start <- dt_person$start[1]
  current_end <- dt_person$end[1]

  for (i in 2:nrow(dt_person)) {
    if (
      !is.na(current_end) &&
        !is.na(dt_person$start[i]) &&
        dt_person$start[i] <= current_end + 1
    ) {
      if (is.na(dt_person$end[i])) {
        current_end <- NA
      } else {
        current_end <- max(current_end, dt_person$end[i])
      }
    } else {
      merged[[length(merged) + 1]] <- list(
        start = current_start,
        end = current_end
      )
      current_start <- dt_person$start[i]
      current_end <- dt_person$end[i]
    }
  }
  merged[[length(merged) + 1]] <- list(start = current_start, end = current_end)
  data.table::rbindlist(merged)
}
