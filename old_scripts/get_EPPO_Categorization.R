
library(tidyverse)
library(DBI)
library(RSQLite)
library(httr)
library(jsonlite)

dir("data/2_processed_data")
# Define the path to your SQLite database file
db_file <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/VKM Data/26.08.2024_lopende_oppdrag_plantehelse/data/2_processed_data/EPPO_DB.db"

# Establish a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_file)

# List all tables in the database
tables <- dbListTables(con)
print(tables)

# Load data from a specific table, e.g., "your_table_name"
data <- dbReadTable(con, "EPPO")

data <- as_tibble(data)

data <- data %>% filter(preferred==1) # preferred
data <- data %>% filter(codelang=="la") # latine 
data <- data %>% filter(status=="A") # active 

# -------------------------------------------------------------------------
# Retrieve EPPO codes from your existing table (assuming your table is named 'data' and column 'eppocode')
eppo_codes <- data$eppocode
EPPO_token <- readLines("C:/Users/dafl/OneDrive - Folkehelseinstituttet/API keys/EPPO_API_KEY.txt", warn = FALSE)[1]

# Initialize an empty list to store results
results_list <- list()

# Loop over each EPPO code
total_codes <- length(eppo_codes)

for (i in seq_along(eppo_codes)) {
  code <- eppo_codes[i]
  
  # Display progress
  cat(sprintf("Handling EPPO code %s (%d of %d)\n", code, i, total_codes))
  
  # Construct the URL
  eppo_categorization_url <- paste0("https://data.eppo.int/api/rest/1.0/taxon/", code, "/categorization")
  
  # Make the GET request
  eppo_get_request <- httr::GET(eppo_categorization_url, query = list(authtoken = EPPO_token))
  
  # Parse the content
  categorization_content <- httr::content(eppo_get_request, as = 'text')
  
  # Check if content is not empty
  if (!nzchar(categorization_content) || categorization_content == "{}") {
    cat(sprintf("No data returned for EPPO code %s\n", code))
    next
  }
  
  # Convert the JSON content to a list
  categorization_content <- fromJSON(categorization_content, flatten = TRUE)
  
  # Add eppocode as a new column to the result if not empty
  if (length(categorization_content) > 0) {
    categorization_content$eppocode <- code
    results_list[[code]] <- categorization_content
  } else {
    cat(sprintf("Empty content for EPPO code %s\n", code))
  }
}


# Convert the list to a data frame ----------------------------------------

if (length(results_list) > 0) {
  # Find all unique column names across the list of data frames
  all_columns <- unique(unlist(lapply(results_list, function(x) names(as.data.frame(x, stringsAsFactors = FALSE)))))
  
  # Standardize each data frame to have the same columns
  results_list <- lapply(results_list, function(df) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    missing_cols <- setdiff(all_columns, names(df))
    df[missing_cols] <- NA  # Add missing columns with NA values
    df <- df[, all_columns]  # Reorder columns to match
    return(df)
  })
  
  # Combine the list of data frames into a single data frame
  results_df <- do.call(rbind, results_list)
  
  # Ensure eppocode column is the first column
  results_df <- results_df[, c("eppocode", setdiff(names(results_df), "eppocode"))]
  
  # Remove row names
  rownames(results_df) <- NULL
  
  # Display the data frame
  print(results_df)
} else {
  cat("No data was retrieved for any EPPO codes.\n")
}


# -------------------------------------------------------------------------

# Specify the database file path
db_file <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/VKM Data/26.08.2024_lopende_oppdrag_plantehelse/data/2_processed_data/EPPO_DB.db"

# Establish a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_file)

# Push the categorization table to the database
# If the table exists, you can set overwrite=TRUE to replace it or append=TRUE to add new rows
dbWriteTable(con, "Categorization", results_df, overwrite = TRUE, row.names = FALSE)

# Confirm the new table
tables <- dbListTables(con)
print(tables)

# Close the connection
dbDisconnect(con)

