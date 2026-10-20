
# Important note about the taxonomy displayed in GD: the database is NOT a primary source for taxonomy (classification and nomenclature) and should not be considered as such. 
# The EPPO Secretariat follows different sources (literature, databases) to provide some elements of taxonomy to its users. However, 
# it does not attempt to provide all levels of the taxonomic tree or exhaustive lists of accepted taxa.

library(httr)
library(jsonlite)

# input is a list vector of eppocodes
# next script uses the taxonomy_results_list not the taxonomy_wide df

#eppo_codes<- c("PHMPOM", "ARCHCO")
# Vector of EPPO codes 
eppo_codes <- data$eppocode 
EPPO_token <- readLines("C:/Users/dafl/Desktop/API keys/EPPO_API_KEY.txt") 
EPPO_token <- EPPO_token[1]

eppo_codes <-eppo_not_norway$eppocode

# Initialize an empty list to store taxonomy results
taxonomy_results_list <- list()

# Loop over each EPPO code to retrieve taxonomy information
total_codes <- length(eppo_codes)
for (i in seq_along(eppo_codes)) {
  code <- eppo_codes[i]
  
  # Display progress
  cat(sprintf("Retrieving taxonomy information for EPPO code %s (%d of %d)\n", code, i, total_codes))
  
  # Construct the URL
  eppo_taxonomy_url <- paste0("https://data.eppo.int/api/rest/1.0/taxon/", code, "/taxonomy")
  
  # Make the GET request
  eppo_get_request <- httr::GET(eppo_taxonomy_url, query = list(authtoken = EPPO_token))
  
  # Parse the content
  taxonomy_content <- httr::content(eppo_get_request, as = 'text')
  
  # Check if content is not empty
  if (!nzchar(taxonomy_content) || taxonomy_content == "{}") {
    cat(sprintf("No data returned for EPPO code %s\n", code))
    next
  }
  
  # Convert the JSON content to a list
  taxonomy_content <- fromJSON(taxonomy_content, flatten = TRUE)
  
  # Add eppocode as a new column to the result if not empty
  if (length(taxonomy_content) > 0) {
    taxonomy_content$eppocode <- code
    taxonomy_results_list[[code]] <- taxonomy_content
  }
}

# Combine the list of data frames into one data frame
if (length(taxonomy_results_list) > 0) {
  taxonomy_combined <- bind_rows(taxonomy_results_list, .id = "eppocode")
  
  # Create a consistent column name using `level` values
  taxonomy_combined <- taxonomy_combined %>%
    arrange(eppocode, level) %>%  # Ensure ordering by eppocode and level
    mutate(level_col = paste0("level_", level))  # Create a new column name based on the level
  
  # Pivot data to wide format with one row per eppocode
  taxonomy_wide <- taxonomy_combined %>%
    select(eppocode, level_col, prefname) %>%  # Select relevant columns
    pivot_wider(names_from = level_col, values_from = prefname, values_fill = NA)  # Pivot to wide format
  
  # Display the result
  print(taxonomy_wide)
} else {
  cat("No taxonomy data was retrieved for any EPPO codes.\n")
}

# -------------------------------------------------------------------------

# Specify the database file path
db_file <- "./2_processed_data/EPPO_DB.db"

# Establish a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_file)

# Push the categorization table to the database
dbWriteTable(con, "EPPO taxonomy", taxonomy_wide, overwrite = TRUE, row.names = FALSE)

# Confirm the new table
tables <- dbListTables(con)
print(tables)

# Close the connection
dbDisconnect(con)

