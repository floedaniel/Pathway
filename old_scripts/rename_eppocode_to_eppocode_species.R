library(readxl)

# Define the file paths
excel_file <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/VKM Data/26.08.2024_lopende_oppdrag_plantehelse/Species/master_species.xlsx"
base_directory <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/VKM Data/26.08.2024_lopende_oppdrag_plantehelse/Species"

# Read the Excel file
species_data <- read_excel(excel_file, col_types = c("text", "text"))

# Ensure column names are in lowercase for consistency
colnames(species_data) <- tolower(colnames(species_data))

# Iterate over the rows to rename folders
for (i in 1:nrow(species_data)) {
  eppocode <- species_data$eppocode[i]
  fullname <- gsub(" ", "_", tolower(species_data$fullname[i])) # Replace spaces with underscores and convert to lowercase
  
  # Define the old and new folder paths
  old_folder <- file.path(base_directory, eppocode)
  new_folder <- file.path(base_directory, paste0(eppocode, "_", fullname))
  
  # Rename the folder if it exists
  if (dir.exists(old_folder)) {
    file.rename(old_folder, new_folder)
    message(sprintf("Renamed: %s -> %s", old_folder, new_folder))
  } else {
    warning(sprintf("Folder not found: %s", old_folder))
  }
}