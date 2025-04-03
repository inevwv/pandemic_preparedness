install_and_load(
  "dplyr",
  "here"
)

# Function to clean and process the data
clean_and_process_data <- function(input_file, iso_map, country_num_of_interest) {
  # Extract the year (Y####) from the input filename
  year <- sub(".*Y(\\d{4}).*\\.csv$", "\\1", input_file)  # Extract year using regex
  
  # Load the dataset
  poultry_prod <- read.csv(input_file)
  
  # Convert codes to numeric for proper subsetting
  poultry_prod$i <- as.numeric(poultry_prod$i)
  
  # Subset poultry production data for specific countries
  poultry_prod_subset <- subset(poultry_prod, i %in% country_num_of_interest)
  
  # Subset iso_map based on country_num_of_interest
  iso_map_subset <- subset(iso_map, country_iso3 %in% country_num_of_interest)
  
  # Rename iso_map_subset columns for merging
  colnames(iso_map_subset) <- c("Country.Code", "iso3")
  
  # Merge and replace numeric codes with ISO-3 codes
  poultry_prod_cleaned <- poultry_prod_subset %>%
    left_join(iso_map_subset, by = c("i" = "Country.Code")) %>%
    mutate(i = ifelse(!is.na(iso3), iso3, i)) %>%
    select(-iso3)  # Drop extra column
  
  # Define the output file path in the "cleaned_poultry" folder under the root
  cleaned_poultry <- here::here("cleaned_poultry")  # Using here() to ensure the correct path
  
  # Create the directory if it doesn't exist
  if (!dir.exists(cleaned_poultry)) {
    dir.create(cleaned_poultry)
  }
  
  # Define the output filename based on the year
  output_file <- file.path(cleaned_poultry, paste0("cleaned_poultry_export_Y", year, ".csv"))
  
  # Save cleaned dataset with dynamic file name
  write.csv(poultry_prod_cleaned, output_file, row.names = FALSE)
  
  # Print message to confirm
  print(paste("DATA CLEANED AND SAVED AS:", output_file))
}

# Define country codes of interest (same for all files)
country_num_of_interest <- c(156, 704, 826, 842)

# Load iso_map (this only needs to be done once)
iso_map <- read.csv(here::here("data_sources", "BACI_HS02_V202501", "country_codes_V202501.csv"))[, c("country_code", "country_iso3")]
iso_map$country_code <- as.numeric(iso_map$country_code)

# List of files to clean (use here() for correct paths)
file_list <- c(here::here("data_sources", "BACI_HS02_V202501", "BACI_HS02_Y2004_V202501.csv"), 
               here::here("data_sources", "BACI_HS02_V202501", "BACI_HS02_Y2005_V202501.csv"), 
               here::here("data_sources", "BACI_HS02_V202501", "BACI_HS02_Y2006_V202501.csv"), 
               here::here("data_sources", "BACI_HS02_V202501", "BACI_HS02_Y2007_V202501.csv"), 
               here::here("data_sources", "BACI_HS02_V202501", "BACI_HS02_Y2008_V202501.csv"), 
               here::here("data_sources", "BACI_HS02_V202501", "BACI_HS02_Y2009_V202501.csv"), 
               here::here("data_sources", "BACI_HS02_V202501", "BACI_HS02_Y2010_V202501.csv"))

# Loop through files and clean each one
for (file in file_list) {
  clean_and_process_data(file, iso_map, country_num_of_interest)
}