# Specify the database file path
db_file <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/VKM Data/26.08.2024_lopende_oppdrag_plantehelse/data/2_processed_data/EPPO_DB.db"

# Establish a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_file)
dbListTables(con)
data <- tbl(con, "EPPO") %>%  collect() %>% distinct(eppocode)

# Vector of EPPO codes (example, modify as needed)
eppo_codes <- data$eppocode  # Adjust as per your data
EPPO_token <- readLines("C:/Users/dafl/OneDrive - Folkehelseinstituttet/API keys/EPPO_API_KEY.txt", warn = FALSE)[1]

# Initialize an empty list to store results
hosts_results_list <- list()

# Loop over each EPPO code to retrieve host information
# Loop over each EPPO code to retrieve host information
total_codes <- length(eppo_codes)
for (i in seq_along(eppo_codes)) {
  code <- eppo_codes[i]
  
  # Construct the URL
  eppo_hosts_url <- paste0("https://data.eppo.int/api/rest/1.0/taxon/", code, "/hosts")
  
  # Make the GET request
  eppo_get_request <- httr::GET(eppo_hosts_url, query = list(authtoken = EPPO_token))
  
  # Parse the content
  hosts_content <- httr::content(eppo_get_request, as = 'text')
  
  # Check if content is not empty and handle output in a single line
  if (!nzchar(hosts_content) || hosts_content == "{}") {
    message(sprintf("\rRetrieving host information for EPPO code %s (%d of %d) - No data", code, i, total_codes), appendLF = FALSE)
  } else {
    message(sprintf("\rRetrieving host information for EPPO code %s (%d of %d) - Data retrieved", code, i, total_codes), appendLF = FALSE)
    
    # Convert the JSON content to a list
    hosts_content <- fromJSON(hosts_content, flatten = TRUE)
    
    # Add eppocode as a new column to the result if not empty
    if (length(hosts_content) > 0) {
      hosts_content$eppocode <- code
      hosts_results_list[[code]] <- hosts_content
    }
  }
}



# Convert the list to a data frame if there are any results ---------------
if (length(hosts_results_list) > 0) {
  # Initialize an empty list to store standardized results
  standardized_results <- list()
  
  for (entry in hosts_results_list) {
    # Get the primary eppocode
    main_eppocode <- if ("eppocode" %in% names(entry)) entry$eppocode[1] else NA
    
    # Create a data frame for each type of host, if available
    if ("Major host" %in% names(entry)) {
      major_host_df <- as.data.frame(entry$`Major host`, stringsAsFactors = FALSE)
      major_host_df$host_type <- "Major host"  # Add a column to indicate host type
      # Rename eppocode column if present
      if ("eppocode" %in% names(major_host_df)) {
        colnames(major_host_df)[colnames(major_host_df) == "eppocode"] <- "host_eppocode"
      }
      # Add main eppocode
      major_host_df$eppocode <- main_eppocode
      standardized_results[[length(standardized_results) + 1]] <- major_host_df
    }
    
    if ("Host" %in% names(entry)) {
      host_df <- as.data.frame(entry$Host, stringsAsFactors = FALSE)
      host_df$host_type <- "Host"  # Add a column to indicate host type
      # Rename eppocode column if present
      if ("eppocode" %in% names(host_df)) {
        colnames(host_df)[colnames(host_df) == "eppocode"] <- "host_eppocode"
      }
      # Add main eppocode
      host_df$eppocode <- main_eppocode
      standardized_results[[length(standardized_results) + 1]] <- host_df
    }
    
    if ("Experimental" %in% names(entry)) {
      experimental_df <- as.data.frame(entry$Experimental, stringsAsFactors = FALSE)
      experimental_df$host_type <- "Experimental"  # Add a column to indicate host type
      # Rename eppocode column if present
      if ("eppocode" %in% names(experimental_df)) {
        colnames(experimental_df)[colnames(experimental_df) == "eppocode"] <- "host_eppocode"
      }
      # Add main eppocode
      experimental_df$eppocode <- main_eppocode
      standardized_results[[length(standardized_results) + 1]] <- experimental_df
    }
  }
  
  # Combine all standardized results into one data frame
  if (length(standardized_results) > 0) {
    # Find all unique column names across the list of data frames
    all_columns <- unique(unlist(lapply(standardized_results, function(x) names(as.data.frame(x, stringsAsFactors = FALSE)))))
    
    # Standardize each data frame to have the same columns
    standardized_results <- lapply(standardized_results, function(df) {
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      missing_cols <- setdiff(all_columns, names(df))
      df[missing_cols] <- NA  # Add missing columns with NA values
      df <- df[, all_columns]  # Reorder columns to match
      return(df)
    })
    
    # Combine the list of data frames into a single data frame
    hosts_results_df <- do.call(rbind, standardized_results)
    
    # Ensure eppocode column is the first column
    hosts_results_df <- hosts_results_df[, c("eppocode", setdiff(names(hosts_results_df), "eppocode"))]
    
    # Remove row names
    rownames(hosts_results_df) <- NULL
    
    # Convert to tibble for better display (optional)
    hosts_results_df <- as_tibble(hosts_results_df)
  } else {
    cat("No valid host data was available for standardization.\n")
  }
} else {
  cat("No host data was retrieved for any EPPO codes.\n")
}

hosts_results_df

# -------------------------------------------------------------------------

# Specify the database file path
db_file <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/VKM Data/26.08.2024_lopende_oppdrag_plantehelse/data/2_processed_data/EPPO_DB.db"

# Establish a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_file)

# Push the categorization table to the database
# If the table exists, you can set overwrite=TRUE to replace it or append=TRUE to add new rows
# dbWriteTable(con, "EPPO hosts", hosts_results_df, overwrite = TRUE, row.names = FALSE)

# Confirm the new table
tables <- dbListTables(con)
print(tables)

tbl(con, "EPPO hosts") 

# Close the connection
dbDisconnect(con)
