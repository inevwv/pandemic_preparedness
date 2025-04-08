# data_cleaning.R
source("install_and_load.R")
install_and_load(
  "dplyr"
)

# Load datasets
gdp_data <- read.csv("data_sources/gdp_by_year.csv")
health_expenditure <- read.csv("data_sources/health_expenditure_per_capita.csv")
hospital_beds <- read.csv("data_sources/hospital_bed_per_1000.csv")
exp_gdp_ratio <- read.csv("data_sources/health_exp_gdp_ratio.csv")
poultry_prod <- read.csv("data_sources/poultry-production-tonnes/poultry-production-tonnes.csv")
H5N1_incidence <- read.csv("data_sources/h5n1-flu-reported-cases/h5n1-flu-reported-cases.csv")

# Filter data for the countries you're interested in
countries_of_interest <- c("USA", "GBR", "VNM", "CHN")

# Rename columns for consistency
colnames(poultry_prod)[colnames(poultry_prod) == "Entity"] <- "Country.Name"
colnames(poultry_prod)[colnames(poultry_prod) == "Code"] <- "Country.Code"
colnames(poultry_prod)[colnames(poultry_prod) == "Meat..poultry...00001808....Production...005510....tonnes"] <- "Tonnes_Produced"


# Clean and subset the data
gdp_subset <- subset(gdp_data, Country.Code %in% countries_of_interest)[, c("Country.Name", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010")]
health_exp_subset <- subset(health_expenditure, Country.Code %in% countries_of_interest)[, c("Country.Name", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010")]
hosp_beds_subset <- subset(hospital_beds, Country.Code %in% countries_of_interest)[, c("Country.Name", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010")]
exp_gdp_ratio_subset <- subset(exp_gdp_ratio, Country.Code %in% countries_of_interest)[, c("Country.Name", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010")]
poultry_prod_subset <- subset(poultry_prod, Country.Code %in% countries_of_interest & Year %in% 2004:2010)
H5N1_incidence_subset <- H5N1_incidence %>%
  subset(Country.Code %in% countries_of_interest & Year %in% 2004:2010) %>%
  group_by(Country.Name, Year) %>%
  summarise(Total.Cases = sum(Human.Cases, na.rm = TRUE))

if (!dir.exists("working_data")) {
  dir.create("working_data")
}

# Save the cleaned data for use in the next scripts
write.csv(gdp_subset, "working_data/gdp_subset.csv", row.names = FALSE)
write.csv(health_exp_subset, "working_data/health_exp_subset.csv", row.names = FALSE)
write.csv(hosp_beds_subset, "working_data/hosp_beds_subset.csv", row.names = FALSE)
write.csv(exp_gdp_ratio_subset, "working_data/exp_gdp_ratio_subset.csv", row.names = FALSE)
write.csv(poultry_prod_subset, "working_data/poultry_prod_subset.csv", row.names = FALSE)
write.csv(H5N1_incidence_subset, "working_data/H5N1_incidence_subset.csv", row.names = FALSE)


# Clean up environment by removing raw datasets
rm(gdp_data, health_expenditure, hospital_beds, exp_gdp_ratio, poultry_prod, H5N1_incidence)


# Print message to confirm
print("DATA CLEANED AND SAVED!")