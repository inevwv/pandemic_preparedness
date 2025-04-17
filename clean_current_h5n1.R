# data_cleaning.R
source("install_and_load.R")
install_and_load(
  "dplyr",
  "countrycode"
)

# Load datasets
h5n1_current <- read.csv("data_sources/monthly-confirmed-cases-of-influenza-a-h5.csv")

# Standardize column name
colnames(h5n1_current)[colnames(h5n1_current) == "Entity"] <- "Country.Name"
colnames(h5n1_current)[colnames(h5n1_current) == "ah5combined"] <- "Human.Cases"

# Add ISO3 country code using countrycode package
h5n1_current$Country.Code <- countrycode(h5n1_current$Country.Name,
                                         origin = "country.name",
                                         destination = "iso3c")

# Define target countries by ISO3 code
countries_of_interest <- c("USA", "GBR", "VNM", "CHN")

# Clean and subset the data
H5N1_current_subset <- h5n1_current %>%
  filter(Country.Code %in% countries_of_interest) %>%
  group_by(Country.Name, Country.Code, Day) %>%
  summarise(Total.Cases = sum(Human.Cases, na.rm = TRUE), .groups = "drop")

# Create folder if needed
if (!dir.exists("working_data")) {
  dir.create("working_data")
}

# Save the cleaned data
write.csv(H5N1_current_subset, "working_data/h5n1_current.csv", row.names = FALSE)

# Print message to confirm
print("DATA CLEANED AND SAVED!")
