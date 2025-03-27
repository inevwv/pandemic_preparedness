# REFINE THE POULTRY
library(readr)
library(purrr)
library(dplyr)

# Define valid product codes as character to preserve leading zeros
poultry_codes <- c(
  "010511", "010512", "010519", "010592", "010593", "010599",
  "020711", "020712", "020713", "020714", "020724", "020725",
  "020726", "020727", "020732", "020733", "020734", "020735",
  "020736", "160232", "160239"
)


# Function to clean and filter each file
clean_file <- function(file) {
  df <- read_csv(file, col_types = cols(.default = "c"))  # Read everything as character to avoid type issues
  df <- df %>% 
    filter(k %in% poultry_codes) %>%  # Filter only rows with matching product codes
    mutate(
      v = as.numeric(v),
      q = as.numeric(q),
      file_name = basename(file))  # Add file name as identifier
  return(df)
}

# List all CSV files in the directory
row_of_birds <- list.files(
  path = "cleaned_data/",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Apply cleaning and combine all files
df <- map_df(row_of_birds, clean_file)


# View a summary of the combined data
print(head(df))
print("DEFEATHERED BIRDS")