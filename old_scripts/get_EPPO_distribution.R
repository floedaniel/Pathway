
# Function to fetch and process data
get_names_content <- function(eppo_code, eppo_token) {
  # Load required packages inside the function
  library(httr)
  library(XML)
  library(rlist)
  library(dplyr)
  
  eppo.distr.url <- paste0("https://gd.eppo.int/taxon/", eppo_code, "/distribution")
  eppo.get.request <- httr::GET(eppo.distr.url, query = list(apikey = eppo_token, details = "true"))
  table_content <- httr::content(eppo.get.request, as = 'text')
  tables <- XML::readHTMLTable(table_content)
  tables <- rlist::list.clean(tables, fun = is.null, recursive = FALSE)
  
  if (length(tables) == 0) {
    message(paste("No data found for EPPO code:", eppo_code))
    return(NULL)
  }
  
  distr_table <- tables$dttable
  if (!is.null(distr_table)) {
    distr_table <- distr_table %>% 
      select(Continent, Country, State, Status) %>%
      mutate(EPPO_code = eppo_code)  # Add the EPPO code as a column
    message(paste("Data successfully retrieved for EPPO code:", eppo_code))
  } else {
    message(paste("No distribution table found for EPPO code:", eppo_code))
    return(NULL)
  }
  return(distr_table)
}

# Your specific eppo_code vector
eppo_code <- data$eppocode  # Adjust as per your data
eppo_token <- readLines("C:/Users/dafl/OneDrive - Folkehelseinstituttet/API keys/EPPO_API_KEY.txt", warn = FALSE)[1]

# Set up a parallel cluster
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, varlist = c("get_names_content", "eppo_token", "eppo_code"))

# Use parLapply for parallel processing
output_list <- parLapply(cl, eppo_code, get_names_content, eppo_token = eppo_token)

# Stop the cluster
stopCluster(cl)

# Filter out NULL elements (for EPPO codes with no data returned)
output_list <- output_list[!sapply(output_list, is.null)]

# Combine the list elements into a single data frame if any data is found
if (length(output_list) > 0) {
  output_df <- bind_rows(output_list) %>% as_tibble()
  print(output_df)
  message("Data retrieval and processing completed successfully!")
} else {
  message("No data found for any of the EPPO codes.")
}

# -------------------------------------------------------------------------

# Specify the database file path
db_file <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/VKM Data/26.08.2024_lopende_oppdrag_plantehelse/data/2_processed_data/EPPO_DB.db"

# Establish a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_file)

# Push the categorization table to the database
# If the table exists, you can set overwrite=TRUE to replace it or append=TRUE to add new rows
dbWriteTable(con, "EPPO distribution", output_df, overwrite = TRUE, row.names = FALSE)

# Confirm the new table
tables <- dbListTables(con)
print(tables)

tbl(con, "EPPO distribution") 

# Close the connection
dbDisconnect(con)
