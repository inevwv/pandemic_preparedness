# assign_raw_surveillance_score.R

# Load the data cleaned in the first script
gdp_subset <- read.csv("working_data/gdp_subset.csv")
health_exp_subset <- read.csv("working_data/health_exp_subset.csv")
hosp_beds_subset <- read.csv("working_data/hosp_beds_subset.csv")
exp_gdp_ratio_subset <- read.csv("working_data/exp_gdp_ratio_subset.csv")

# Define the scoring function
assign_raw_surveillance <- function(value, thresholds, is_testing = FALSE, is_descending = FALSE) {
  if (is.na(value)) return(NA)  # Handle missing values
  
  if (is_descending) {  # Higher value is better (e.g., genome sharing, tracing)
    if (value >= thresholds[1]) return(3)
    else if (value >= thresholds[2]) return(2)
    else if (value >= thresholds[3]) return(1)
    else return(1)  # Ensure at least a score of 1
  } else {  # Lower value is better (e.g., reporting time)
    if (value <= thresholds[1]) return(3)
    else if (value <= thresholds[2]) return(2)
    else if (value <= thresholds[3]) return(1)
    else return(1)
  }
}

# Test dataset with a year range
surveillance_score <- data.frame(
  Country.Name = c("USA", "UK", "Vietnam", "China"),
  reporting_days_2005 = c(3, 5, 7, 2),  
  reporting_days_2006 = c(4, 6, 8, 3),
  reporting_days_2007 = c(5, 7, 9, 4),
  reporting_days_2008 = c(6, 8, 10, 5),
  reporting_days_2009 = c(7, 9, 11, 6),
  reporting_days_2010 = c(8, 10, 12, 7),
  
  testing_access_2005 = c(90, 85, 60, 95),  
  testing_access_2006 = c(91, 86, 61, 96),
  testing_access_2007 = c(92, 87, 62, 97),
  testing_access_2008 = c(93, 88, 63, 98),
  testing_access_2009 = c(94, 89, 64, 99),
  testing_access_2010 = c(95, 90, 65, 100),
  
  genome_sharing_2005 = c(7, 10, 15, 5),
  genome_sharing_2006 = c(8, 11, 16, 6),
  genome_sharing_2007 = c(9, 12, 17, 7),
  genome_sharing_2008 = c(10, 13, 18, 8),
  genome_sharing_2009 = c(11, 14, 19, 9),
  genome_sharing_2010 = c(12, 15, 20, 10),
  
  contact_tracing_2005 = c(60, 60, 80, 90),
  contact_tracing_2006 = c(61, 61, 81, 91),
  contact_tracing_2007 = c(62, 62, 82, 92),
  contact_tracing_2008 = c(63, 63, 83, 93),
  contact_tracing_2009 = c(64, 64, 84, 94),
  contact_tracing_2010 = c(65, 65, 85, 95)
)

# Define thresholds for scoring
reporting_thresholds <- c(3, 7, 14)   # Lower is better (reporting time)
testing_thresholds <- c(40, 60, 80)   # Higher is better (testing access)
genome_thresholds <- c(5, 10, 20)     # Higher is better (genome sharing)
tracing_thresholds <- c(40, 60, 80)   # Higher is better (contact tracing)

# Apply the scoring function for each category and year
for (year in 2005:2010) {
  surveillance_score[paste0("total_score_", year)] <- 
    apply(surveillance_score[, c(paste0("reporting_days_", year), 
                                 paste0("testing_access_", year), 
                                 paste0("genome_sharing_", year), 
                                 paste0("contact_tracing_", year))], 
          1, function(x) {
            # Assign scores based on the correct criteria
            reporting_score <- assign_raw_surveillance(x[1], reporting_thresholds, is_testing = FALSE)
            testing_score <- assign_raw_surveillance(x[2], testing_thresholds, is_testing = TRUE, is_descending = TRUE)
            genome_score <- assign_raw_surveillance(x[3], genome_thresholds, is_testing = TRUE, is_descending = TRUE)
            tracing_score <- assign_raw_surveillance(x[4], tracing_thresholds, is_testing = TRUE, is_descending = TRUE)
            
            # Sum up the total surveillance score
            return(sum(c(reporting_score, testing_score, genome_score, tracing_score), na.rm = TRUE))
          })
}

# Save the raw surveillance scores
write.csv(surveillance_score, "working_data/raw_surveillance_scores.csv", row.names = FALSE)

# Print confirmation message
print("ASSIGNED AND SAVED")