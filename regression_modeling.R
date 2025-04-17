# Load necessary libraries
source("install_and_load.R")
install_and_load(
  "dplyr",
  "readr",
  "tidyr"
)

# 1. Load the CSV files
exp_gdp_ratio <- read_csv("working_data/exp_gdp_ratio_subset.csv")
gdp_subset <- read_csv("working_data/gdp_subset.csv")
gov_type <- read_csv("working_data/gov_type.csv")
h5n1_incidence <- read_csv("working_data/H5N1_incidence_subset.csv")
health_exp <- read_csv("working_data/health_exp_subset.csv")
hosp_beds <- read_csv("working_data/hosp_beds_subset.csv")
physicians <- read_csv("working_data/physicians_subset.csv")
poultry_prod <- read_csv("working_data/poultry_prod_subset.csv")
reporting_scores <- read_csv("working_data/reporting_scores.csv")
testing_access <- read_csv("working_data/testing_access.csv")
print("CSVs ARE IN")

# Apply the function to your datasets
source("reshape_long.R")
gdp_subset_long <- reshape_long(gdp_subset, "gdp")
exp_gdp_ratio_long <- reshape_long(exp_gdp_ratio, "exp_gdp_ratio")
h5n1_incidence_long <- reshape_long(h5n1_incidence, "h5n1_incidence")
health_exp_long <- reshape_long(health_exp, "health_exp")
hosp_beds_long <- reshape_long(hosp_beds, "hosp_beds")
physicians_long <- reshape_long(physicians, "physicians")
poultry_prod_long <- reshape_long(poultry_prod, "poultry_prod")
reporting_scores_long <- reshape_long(reporting_scores, "reporting_scores")
testing_access_long <- reshape_long(testing_access, "testing_access")

# Rename the 'year' column to 'Year' in datasets where it's inconsistent
exp_gdp_ratio_long <- rename(exp_gdp_ratio_long, Year = "year")
gdp_subset_long <- rename(gdp_subset_long, Year = "year")
health_exp_long <- rename(health_exp_long, Year = "year")
hosp_beds_long <- rename(hosp_beds_long, Year = "year")
physicians_long <- rename(physicians_long, Year = "year")
poultry_prod_long <- rename(poultry_prod_long, Year = "Year")  # It's already fine
reporting_scores_long <- rename(reporting_scores_long, Year = "Year")  # It's already fine
testing_access_long <- rename(testing_access_long, Year = "Year")
print("ALL DATA FORMATTED")

# Clean up Country.Name columns before merging
exp_gdp_ratio_long <- exp_gdp_ratio_long %>% select(-starts_with("Country.Name"))
gdp_subset_long <- gdp_subset_long %>% select(-starts_with("Country.Name"))
h5n1_incidence_long <- h5n1_incidence_long %>% select(-starts_with("Country.Name"))
health_exp_long <- health_exp_long %>% select(-starts_with("Country.Name"))
hosp_beds_long <- hosp_beds_long %>% select(-starts_with("Country.Name"))
physicians_long <- physicians_long %>% select(-starts_with("Country.Name"))
poultry_prod_long <- poultry_prod_long %>% select(-starts_with("Country.Name"))
reporting_scores_long <- reporting_scores_long %>% select(-starts_with("Country.Name"))
testing_access_long <- testing_access_long %>% select(-starts_with("Country.Name"))

# 2. Merge all datasets by Country.Code and Year (using long-format data)
merged_data <- exp_gdp_ratio_long %>%
  full_join(gdp_subset_long, by = c("Country.Code", "Year")) %>%
  full_join(gov_type, by = c("Country.Code")) %>%
  full_join(h5n1_incidence_long, by = c("Country.Code", "Year")) %>%
  full_join(health_exp_long, by = c("Country.Code", "Year")) %>%
  full_join(hosp_beds_long, by = c("Country.Code", "Year")) %>%
  full_join(physicians_long, by = c("Country.Code", "Year")) %>%
  full_join(poultry_prod_long, by = c("Country.Code", "Year")) %>%
  full_join(reporting_scores_long, by = c("Country.Code", "Year")) %>%
  full_join(testing_access_long, by = c("Country.Code", "Year"))

# Check for missing values and handle them if needed
summary(merged_data)

# 3. Rename columns for predictors
merged_data <- merged_data %>%
  rename(
    poultry_prod = "Tonnes_Produced",
    reporting_scores = "diagnostic_lag_score",
    testing_access = "testing_access_score"
  )

# 4. Z-score all predictors (excluding outcome variable - H5N1 or SARS incidence)
predictor_columns <- c("exp_gdp_ratio", "gdp", "health_exp", "hosp_beds", "physicians", "poultry_prod", "reporting_scores", "testing_access")

# Z-score using base R's scale function
merged_data_z <- merged_data %>%
  mutate(across(all_of(predictor_columns), ~ scale(.)[, 1], .names = "{col}_z"))

# 5. Run correlation analysis to see relationships between predictors and H5N1 cases (or SARS)
correlation_results <- merged_data_z %>%
  select(c("Total.Cases", starts_with("exp_gdp_ratio_z"), starts_with("gdp_z"), starts_with("health_exp_z"), starts_with("hosp_beds_z"), starts_with("physicians_z"), starts_with("poultry_prod_z"), starts_with("reporting_scores_z"), starts_with("testing_access_z"))) %>%
  drop_na() %>%  # Remove rows with any missing values
  cor(use = "complete.obs")

# Print the correlation matrix
print(correlation_results)

# 6. Optional: Run linear regression to determine the most significant factors impacting H5N1 incidence
regression_model <- lm(Total.Cases ~ exp_gdp_ratio_z + gdp_z + health_exp_z + hosp_beds_z + physicians_z + poultry_prod_z + reporting_scores_z + testing_access_z, data = merged_data_z)

# Summary of the regression model
summary(regression_model) 

# 7. Create and export predictors_zscores.csv for use in optimization
predictors_zscores <- merged_data_z %>%
  select(Country.Code, Year, all_of(paste0(predictor_columns, "_z"))) %>%
  drop_na()

# Expand predictors to monthly for joining with monthly case data
predictors_zscores_monthly <- predictors_zscores %>%
  crossing(Month = 1:12)

# Save the monthly version
write_csv(predictors_zscores_monthly, "working_data/predictors_zscores.csv")

