# cleaning up reporting scores
source("install_and_load.R")
install_and_load(
  "readr",
  "dplyr"
)

reporting_dates <- read.csv("data_sources/reporting_dates.csv")


T0 <- as.Date(reporting_dates$T0_First_Detection, format="%m/%d/%Y")
T1 <- as.Date(reporting_dates$T1_Internal_Conf, format="%m/%d/%Y")
T2 <- as.Date(reporting_dates$T2_WHO_Notif, format="%m/%d/%Y")

# Create a clean scoring table
reporting_scores <- reporting_dates %>%
  mutate(
    diagnostic_lag = as.numeric(T1 - T0),
    reporting_lag = as.numeric(T2 - T1),
    net_lag = as.numeric(T2 - T0),
    
    # Reporting Lag Score (T1 - T0)
    diagnostic_lag_score = case_when(
      diagnostic_lag >= 0 & reporting_lag <= 1 ~ 5,
      diagnostic_lag >= 2 & reporting_lag <= 3 ~ 4,
      diagnostic_lag >= 4 & reporting_lag <= 7 ~ 3,
      diagnostic_lag >= 8 & reporting_lag <= 14 ~ 2,
      diagnostic_lag > 14 | reporting_lag < 0 ~ 1,
      TRUE ~ 1  # catch unclear cases
    ),
    
    reporting_lag_score = case_when(
      reporting_lag >= 0 & reporting_lag <= 1 ~ 5,
      reporting_lag >= 2 & reporting_lag <= 3 ~ 4,
      reporting_lag >= 4 & reporting_lag <= 7 ~ 3,
      reporting_lag >= 8 & reporting_lag <= 14 ~ 2,
      reporting_lag > 14 | reporting_lag < 0 ~ 1,
      TRUE ~ 1
    ),
    
    net_lag_score = case_when(
      net_lag >= 0 & net_lag <= 3 ~ 5,
      net_lag >= 4 & net_lag <= 7 ~ 4,
      net_lag >= 8 & net_lag <= 14 ~ 3,
      net_lag >= 15 & net_lag <= 21 ~ 2,
      net_lag > 21 | net_lag < 0 ~ 1,
      TRUE ~ 1
    )
  ) %>%
  select(Country.Code, Year, Pathogen, diagnostic_lag_score, reporting_lag_score, net_lag_score)

# Write to CSV
write.csv(reporting_scores, "working_data/reporting_scores.csv", row.names = FALSE)
rm(reporting_dates)

print("REPORTING SCORES WRITTEN")
    
