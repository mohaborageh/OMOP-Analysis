#' @importFrom DBI dbConnect dbDisconnect dbExistsTable dbExecute dbListTables dbWriteTable
#' @importFrom readr read_delim read_delim_chunked spec_delim locale
#' @importFrom dplyr tbl pull filter count select all_of
#' @importFrom duckdb duckdb_read_csv
#' @importFrom haven read_sas
#' @importFrom readxl read_excel
#' @importFrom dbplyr sql_render

#' Get Available Vocabulary IDs
#'
#' This function retrieves a list of all available vocabulary IDs from the vocabulary table.
#' An optional pattern parameter can be used to filter the results.
#'
#' @param connection A database connection object.
#' @param pattern An optional regular expression pattern to filter vocabulary IDs. Default is NULL (no filtering).
#' @return A character vector containing all available vocabulary IDs that match the pattern (if provided).
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' vocabulary_ids <- get_vocabulary_ids(con)
#' print(vocabulary_ids)
#'
#' # Filter vocabularies containing "ICD"
#' icd_vocabulary_ids <- get_vocabulary_ids(con, pattern = "ICD")
#' print(icd_vocabulary_ids)
#'
#' DBI::dbDisconnect(con)
#' }
#' @export
get_vocabulary_ids <- function(connection, pattern = NULL) {
  # Check if vocabulary table exists
  if (!dbExistsTable(connection, "vocabulary")) {
    stop("The vocabulary table does not exist in the connected database.")
  }

  # Query vocabulary table
  vocab_ids <- tbl(connection, "vocabulary") %>%
    pull(vocabulary_id) %>%
    sort()

  # Filter by pattern if provided
  if (!is.null(pattern)) {
    vocab_ids <- vocab_ids[grepl(pattern, vocab_ids)]
  }

  return(vocab_ids)
}

#' Reduce table to shared columns
#' @param table Input table
#' @param name Table name
#' @param connection Database connection
#' @export
reduce_to_shared_columns <- function(table, name, connection) {
  selection <- intersect(colnames(table), colnames(tbl(connection, name)))
  return(table %>% select(all_of(selection)))
}

#' Check if table processing is already done
#' @param table_name Table name
#' @param connection Database connection
#' @export
is_already_done <- function(table_name, connection) {
  if (
    !table_name %in% duckdb::dbListTables(connection) ||
      tbl(connection, table_name) %>%
        count() %>%
        pull(n) ==
        0
  ) {
    print(sprintf("%s has not been processed yet. Preparing ...", table_name))
    return(FALSE)
  }
  print(sprintf("%s has already been processed.", table_name))
  return(TRUE)
}

#' Function to initialize omop database
#'
#' This function adds data to a specified database in the OMOP Common Data Model (CDM) format.
#'
#' @param db_type The type of the database ('sqlite', 'postgresql', or 'duckdb').
#' @param connection The DBI connection object to the database.
#'
#' @return None
#' @export
initOmopDb <- function(db_type, connection) {
  sql_paths <- list(
    sqlite = list(
      ddl = "data/omop_cdm_ddl/sqlite/omop_cdm_sqlite_ddl.sql",
      indices = "data/omop_cdm_ddl/sqlite/omop_cdm_sqlite_indices.sql"
    ),
    postgresql = list(
      ddl = "data/omop_cdm_ddl/postgres/omop_cdm_postgres_ddl.sql",
      indices = "data/omop_cdm_ddl/postgres/omop_cdm_postgres_indices.sql"
    ),
    duckdb = list(
      ddl = "data/omop_cdm_ddl/duckdb/omop_cdm_duckdb_ddl.sql",
      indices = "data/omop_cdm_ddl/duckdb/omop_cdm_duckdb_indices.sql"
    )
  )

  # Read and execute the CDM DDL SQL file
  cdm_ddl_sql <- readLines(sql_paths[[db_type]]$ddl)
  cdm_ddl_sql <- paste(cdm_ddl_sql, collapse = "\n")
  print("Read the ddl file.")
  # Split the SQL script into individual statements using ';' as a delimiter
  cdm_ddl_statements <- unlist(strsplit(cdm_ddl_sql, ";", fixed = TRUE))

  # Execute each statement separately
  for (statement in cdm_ddl_statements) {
    # Trim whitespace and check if the statement is not empty
    trimmed_statement <- trimws(statement)
    if (nchar(trimmed_statement) > 0) {
      dbExecute(connection, trimmed_statement)
    }
  }
  print("Created tables in the database.")
}

#' Create OMOP CDM indices
#' @param db_type Database type
#' @param connection Database connection
#' @export
createOmopIndices <- function(db_type, connection) {
  sql_paths <- list(
    sqlite = list(
      ddl = "data/omop_cdm_ddl/sqlite/omop_cdm_sqlite_ddl.sql",
      indices = "data/omop_cdm_ddl/sqlite/omop_cdm_sqlite_indices.sql"
    ),
    postgresql = list(
      ddl = "data/omop_cdm_ddl/postgres/omop_cdm_postgres_ddl.sql",
      indices = "data/omop_cdm_ddl/postgres/omop_cdm_postgres_indices.sql"
    ),
    duckdb = list(
      ddl = "data/omop_cdm_ddl/duckdb/omop_cdm_duckdb_ddl.sql",
      indices = "data/omop_cdm_ddl/duckdb/omop_cdm_duckdb_indices.sql"
    )
  )

  # Read and execute the CDM DDL SQL file
  cdm_idx_sql <- readLines(sql_paths[[db_type]]$indices)
  cdm_idx_sql <- paste(cdm_idx_sql, collapse = "\n")
  print("Read the idx file.")
  # Split the SQL script into individual statements using ';' as a delimiter
  cdm_idx_statements <- unlist(strsplit(cdm_idx_sql, ";", fixed = TRUE))

  # Execute each statement separately
  for (statement in cdm_idx_statements) {
    # Trim whitespace and check if the statement is not empty
    trimmed_statement <- trimws(statement)
    if (nchar(trimmed_statement) > 0) {
      dbExecute(connection, trimmed_statement)
    }
  }
  print("Created tables in the database.")
}

#' Helper function to load data from file into database
#' @param table Table name
#' @param file File path
#' @param fdelim Field delimiter
#' @param type File type
#' @param append_mode Whether to append to existing table
#' @param encoding Character encoding for reading files
#' @param connection Database connection
#' @keywords internal
load_file_to_table <- function(
  table,
  file,
  fdelim,
  type,
  append_mode,
  encoding,
  connection
) {
  if (type == "csv") {
    if (append_mode) {
      # For append, use read_delim to load into memory then append
      tmp <- read_delim(
        file,
        delim = fdelim,
        show_col_types = FALSE,
        locale = locale(encoding = encoding)
      )
      dbWriteTable(connection, table, tmp, append = TRUE)
    } else {
      # For new table, use efficient duckdb_read_csv
      duckdb::duckdb_read_csv(
        connection,
        table,
        file,
        delim = fdelim,
        nrow.check = 5000000
      )
    }
  } else if (type == "table") {
    tmp <- read_delim(
      file,
      delim = fdelim,
      show_col_types = FALSE,
      locale = locale(encoding = encoding)
    )
    dbWriteTable(connection, table, tmp, append = append_mode)
  } else if (type == "sas") {
    tmp <- haven::read_sas(file)
    dbWriteTable(connection, table, tmp, append = append_mode)
  } else if (type == "chunked") {
    if (append_mode) {
      # For append, read chunks and append each
      total <- 0
      process_chunk <- function(chunk, pos) {
        dbWriteTable(connection, table, chunk, append = TRUE)
        total <<- total + nrow(chunk)
        return(TRUE)
      }
      col_spec <- spec_delim(file, delim = fdelim)
      read_delim_chunked(
        file,
        callback = process_chunk,
        chunk_size = 10000000,
        delim = fdelim,
        progress = FALSE,
        col_types = col_spec,
        locale = locale(encoding = encoding)
      )
      print(sprintf("Appended %g rows", total))
    } else {
      # For new table, use existing helper function
      read_chunked_and_add(table, file, fdelim, connection, encoding)
    }
  } else if (type == "excel") {
    tmp <- readxl::read_excel(file)
    dbWriteTable(connection, table, tmp, append = append_mode)
  } else {
    stop("Unexpected type: ", type)
  }
}

#' Add table to database if missing
#' @param table Table name
#' @param file File path
#' @param fdelim Field delimiter
#' @param type File type (csv, table, sas, chunked, excel)
#' @param append Append data if table exists (default: FALSE)
#' @param encoding Character encoding for reading files (default: "UTF-8")
#' @param connection Database connection
#' @export
add_if_missing <- function(
  table,
  file,
  fdelim = ",",
  type = "csv",
  append = FALSE,
  encoding = "UTF-8",
  connection
) {
  # Check if table exists using more efficient method
  table_exists <- dbExistsTable(connection, table)

  if (!table_exists) {
    # Table doesn't exist - create it
    print(sprintf("Creating table '%s'", table))
    load_file_to_table(
      table,
      file,
      fdelim,
      type,
      append_mode = FALSE,
      encoding,
      connection
    )

    # Add uuid column for every row
    # TODO: verify that this works with different database types
    tryCatch(
      {
        dbExecute(
          connection,
          sprintf("ALTER TABLE %s ADD COLUMN uuid UUID DEFAULT uuid();", table)
        )
      },
      error = function(e) {
        warning(sprintf(
          "Could not add UUID column to table '%s': %s",
          table,
          e$message
        ))
      }
    )
  } else {
    # Table exists
    if (append) {
      print(sprintf("Table '%s' exists - appending data", table))
      load_file_to_table(
        table,
        file,
        fdelim,
        type,
        append_mode = TRUE,
        encoding,
        connection
      )
    } else {
      print(sprintf("Table '%s' already exists", table))
    }
  }

  return(tbl(connection, table))
}
read_chunked_and_add <- function(
  table,
  file,
  fdelim,
  connection,
  encoding = "UTF-8"
) {
  first <- TRUE
  process_chunk <- function(chunk, pos) {
    dbWriteTable(connection, table, chunk, append = !first)
    total <<- total + nrow(chunk)
    first <<- FALSE
    return(TRUE) # Continue reading
  }
  tables <- duckdb::dbListTables(connection)
  total <- 0
  if (!table %in% tables) {
    print("add table")
    # Read and process the file in chunks
    col_spec <- spec_delim(file, delim = fdelim)

    read_delim_chunked(
      file,
      callback = process_chunk,
      chunk_size = 10000000,
      delim = fdelim,
      progress = FALSE,
      col_types = col_spec,
      locale = locale(encoding = encoding)
    )
    print(sprintf("Added %g", total))
  }
}

#' Delete records from table
#' @param table_name Table name
#' @param column Column name
#' @param code_list List of codes to delete
#' @param connection Database connection
#' @export
delete_from_table <- function(table_name, column, code_list, connection) {
  code_string <- paste0("'", code_list, "'", collapse = ", ")
  sql_string <- sprintf(
    "DELETE FROM %s WHERE %s IN (%s);",
    table_name,
    column,
    code_string
  )
  dbExecute(connection, sql_string)
}


#' Append data to a database table.
#'
#' This function appends a data frame to an existing database table.
#'
#' @param conn The database connection object.
#' @param table_name The name of the table to append the data to.
#' @param data_frame The data frame containing the data to be appended.
#'
#' @return None
#'
#' @examples
#' # Connect to the database
#' conn <- dbConnect(RSQLite::SQLite(), dbname = "mydatabase.db")
#'
#' # Create a data frame
#' data <- data.frame(x = 1:5, y = letters[1:5])
#'
#' # Append the data to the table
#' append_data_to_table(conn, "mytable", data)
#' @export
append_data_to_table <- function(conn, table_name, data_frame) {
  dbWriteTable(conn, table_name, data_frame, append = TRUE, row.names = FALSE)
}


#' Append data to DuckDB table
#' @param conn Database connection
#' @param table_name Table name
#' @param data_frame Data frame to append
#' @param type Operation type
#' @export
append_data_to_table_duckdb <- function(
  conn,
  table_name,
  data_frame,
  type = "insert"
) {
  sql_string <- dbplyr::sql_render(data_frame)
  if (type == "insert") {
    insert_string <- sprintf(
      "INSERT INTO %s BY NAME %s",
      table_name,
      sql_string
    )
  } else if (type == "add") {
    insert_string <- sprintf("CREATE TABLE %s AS %s", table_name, sql_string)
  } else {
    stop("Wrong type")
  }
  dbExecute(conn, insert_string)
}

#' Add or get provider ID
#' @param name Provider name
#' @param connection Database connection
#' @export
add_or_get_provider <- function(name, connection) {
  provider_table <- tbl(connection, "provider")
  id_res <- provider_table %>%
    filter(provider_name == name) %>%
    pull(provider_id)
  if (length(id_res) == 0) {
    # Add provider
    n_provider <- (provider_table %>% count() %>% pull(n))
    insertion <- data.frame(
      provider_id = n_provider + 1,
      provider_name = name
    )
    append_data_to_table(connection, "provider", insertion)
    return(add_or_get_provider(name, connection))
  } else {
    return(id_res[1])
  }
}
