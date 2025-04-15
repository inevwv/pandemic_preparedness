source("install_and_load.R")
install_and_load(
  "dplyr",
  "readr",
  "tidyr",
  "countrycode"  # This package helps convert country names to ISO codes
)
print("PACKAGES, CHECK")

# Load data
physicians <- read.csv("working_data/physicians_subset.csv")
exp_gdp <- read.csv("working_data/exp_gdp_ratio_subset.csv")
hosp_beds <- read.csv("working_data/hosp_beds_subset.csv")

# Use countrycode package to map country names to ISO 3166-1 alpha-3 codes
physicians <- physicians %>%
  mutate(Country.Code = countrycode(Country.Name, "country.name", "iso3c"))  # Use country names to get ISO3 codes
exp_gdp <- exp_gdp %>%
  mutate(Country.Code = countrycode(Country.Name, "country.name", "iso3c"))
hosp_beds <- hosp_beds %>%
  mutate(Country.Code = countrycode(Country.Name, "country.name", "iso3c"))

# Score function factory
score_variable <- function(df, value_col, thresholds, score_name) {
  df_long <- df %>%
    pivot_longer(cols = -c(Country.Name, Country.Code), names_to = "Year", values_to = value_col) %>%
    mutate(
      Year = as.integer(gsub("X", "", Year)),
      # Convert the value column to numeric to prevent issues
      !!sym(value_col) := as.numeric(!!sym(value_col))
    ) %>%
    group_by(Country.Name) %>%
    arrange(Country.Name, Year) %>%
    # Impute NAs by filling them with the closest available year's data
    fill(!!sym(value_col), .direction = "downup") %>%
    mutate(
      !!score_name := case_when(
        is.na(!!sym(value_col)) ~ 0,  # Assign 0 if still NA after imputation
        !!sym(value_col) >= thresholds[5] ~ 5,
        !!sym(value_col) >= thresholds[4] ~ 4,
        !!sym(value_col) >= thresholds[3] ~ 3,
        !!sym(value_col) >= thresholds[2] ~ 2,
        !!sym(value_col) >= thresholds[1] ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(Country.Name, Country.Code, Year, !!score_name)
  
  return(df_long)
}

# Define thresholds
hospital_thresholds  <- c(1, 2, 4, 6, Inf)
physician_thresholds <- c(0.5, 1, 2, 3, Inf)
expgdp_thresholds    <- c(3, 5, 7, 9, Inf)

# Score each dataset
physician_scores <- score_variable(physicians, "physicians", physician_thresholds, "physician_score")
hospbed_scores    <- score_variable(hosp_beds, "hosp_beds", hospital_thresholds, "hospital_score")
expgdp_scores     <- score_variable(exp_gdp, "exp_gdp", expgdp_thresholds, "expgdp_score")


# Ensure numeric columns are handled and join Country.Code
testing_access <- physician_scores %>%
  full_join(hospbed_scores, by = c("Country.Name", "Country.Code", "Year")) %>%
  full_join(expgdp_scores, by = c("Country.Name", "Country.Code", "Year")) %>%
  ungroup() %>%
  mutate(
    testing_access_score = round(rowMeans(select(., physician_score, hospital_score, expgdp_score), na.rm = TRUE), 2)
  )

# Preview result
print(testing_access)

rm(expgdp_scores, hospbed_scores, physician_scores)

# Write the result to a CSV
write.csv(testing_access, "working_data/testing_access.csv", row.names = FALSE)
print("TESTING ACCESS SCORED")