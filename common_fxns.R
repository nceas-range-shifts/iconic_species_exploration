# Get all exit pixels (pixel by pixel) using duckdb to filter first
extract_all_exit_pixels_df_db <- function(id, df, s) {
  spp_filter <- df %>% filter(aphiaid == id)
  
  if (nrow(spp_filter) == 0) {
    warning("No rows found for aphiaid: ", id)
    return(NULL)
  }
  
  # Create database connection
  con <- DBI::dbConnect(duckdb::duckdb())
  # Ensure connection is closed 
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # Write query to round x and y to 3 decimal places and filter for scenario pixels at or above cutoff threshold
  query_str <- paste0("SELECT ROUND(x, 3) AS x, ROUND(y, 3) AS y, ", paste(s, collapse = ", "), 
                      " FROM read_parquet('", spp_filter$f, "')",
                      " WHERE ", paste(s, collapse = ", "), " < cutoff AND Current >= cutoff")
  
  # Run query and filter for scenario pixels below current cutoff threshold and above cutoff threshold
  spp_exit <- DBI::dbGetQuery(con, query_str) 
  
  # Set option for if there are no exit pixels 
  if (nrow(spp_exit) == 0) {
    warning("No exit pixels for aphiaid: ", id)
    # Return NA to track no pixels above threshold
    return(data.frame(x = NA, y = NA, MRGID = NA, exit = 0))
  } else {
    
    # Flag the pixels for exit
    spp_exit_pxls <- spp_exit %>% 
      mutate(exit = 1) 
    
    return(spp_exit_pxls)
  }
}

# Get all entry pixels (pixel by pixel) using duckdb to filter first
extract_all_entry_pixels_df_db <- function(id, df, s) {
    spp_filter <- df %>% filter(aphiaid == id)
  
    if (nrow(spp_filter) == 0) {
      warning("No rows found for aphiaid: ", id)
      return(NULL)
  }

  # Create database connection
  con <- DBI::dbConnect(duckdb::duckdb())
  # Ensure connection is closed 
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # Write query to round x and y to 3 decimal places and filter for scenario pixels at or above cutoff threshold
   query_str <- paste0("SELECT ROUND(x, 3) AS x, ROUND(y, 3) AS y, ", paste(s, collapse = ", "), 
                      " FROM read_parquet('", spp_filter$f, "')",
                      " WHERE ", paste(s, collapse = ", "), " >= cutoff AND Current < cutoff")

 # Run query and filter for scenario pixels below current cutoff threshold and above cutoff threshold
    spp_entry <- DBI::dbGetQuery(con, query_str) 

  # Set option for if there are no pixels below current cutoff threshold and above the cutoff threshold
    if (nrow(spp_entry) == 0) {
        warning("No entry pixels for aphiaid: ", id)
        # Return NA to track no pixels above threshold
      return(data.frame(x = NA, y = NA, MRGID = NA, entry = 0))
    } else {
  
  # Flag the pixels for entry
    spp_entry_pxls <- spp_entry %>% 
      mutate(entry = 1) 
  
  return(spp_entry_pxls)
  }
}

species_count <- function(id, df, scenario, n_threads = 1) {
    spp_filter <- df %>% filter(aphiaid == id)
  
  if (nrow(spp_filter) == 0) {
    warning("No rows found for aphiaid: ", id)
    return(NULL)
  }

  # Create database connection
  con <- DBI::dbConnect(duckdb::duckdb())

  # Ensure connection is closed 
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Cap threads so that function doesn't overscribe and hang
  DBI::dbExecute(con, paste0("PRAGMA threads = ", n_threads, ";"))

  query_str <- paste0("SELECT ROUND(x, 3) AS x, ROUND(y, 3) AS y, ", paste(scenario, collapse = ", "), 
    " FROM read_parquet('", spp_filter$f, "')",
    " WHERE ", paste(scenario, collapse = ", "), " >= cutoff")

 # Run query and filter for scenario pixels above cutoff threshold

    spp_df <- DBI::dbGetQuery(con, query_str) 

  # Set option for if there are no pixels above the cutoff threshold
    if (nrow(spp_df) == 0) {
        warning("No pixels above cutoff threshold for aphiaid: ", id)
      # Return 0 to track loss species
        return(data.frame(x = NA, y = NA, binary = 0, s = scenario))
    } else {
  
  # Return the pixels above the cutoff to be summarized into LC count  
  spp_df_clean <- spp_df %>% 
    mutate(binary = 1) %>%  
    dplyr::select(x, y, binary) %>% 
    mutate(s = scenario)

  return(spp_df_clean)
}}