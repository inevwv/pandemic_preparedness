# ---- Load Libraries ----
source("install_and_load.R")
install_and_load("dplyr", "lubridate", "ROI", "ROI.plugin.glpk", "ggplot2")

# ---- Load Data ----
cases <- read.csv("working_data/h5n1_current.csv")
predictors <- read.csv("working_data/predictors_zscores.csv")

# Add Year and Month columns to cases data
cases <- cases %>%
  mutate(
    Day = as.Date(Day),
    Year = year(Day),
    Month = month(Day)
  )

# ---- Define Coefficients from Regression ----
coefs <- c(
  gdp_z = 200.0193,
  health_exp_z = -2124.4534,
  hosp_beds_z = -83.2239,
  physicians_z = 251.7954,
  poultry_prod_z = -194.6546,
  reporting_scores_z = -0.2996,
  testing_access_z = 14.6359
)
vars <- names(coefs)

# ---- Optimization Function ----
optimize_month <- function(row) {
  # Select relevant variables for the row
  input_vars <- row[vars]
  
  # Set the objective function and bounds
  objective <- coefs
  lower_bounds <- rep(-2, length(vars))
  upper_bounds <- rep(2, length(vars))
  
  objective_LP <- L_objective(objective, names = vars)
  bounds_LP <- V_bound(
    li = 1:length(vars), lb = lower_bounds,
    ui = 1:length(vars), ub = upper_bounds
  )
  
  # Create the optimization model
  opt_model <- OP(objective = objective_LP, bounds = bounds_LP, maximum = FALSE)
  
  # Solve the optimization problem
  solution <- ROI_solve(opt_model, solver = "glpk")
  
  # Calculate predicted optimized value
  predicted_optimized <- sum(coefs * solution$solution)
  
  # Return both the optimization solution and the predicted optimized value
  return(c(solution$solution, Predicted_Optimized = predicted_optimized))
}

# ---- Join Datasets ----
merged <- cases %>%
  left_join(predictors, by = c("Country.Code", "Year", "Month"))

# ---- Run Optimization for Each Row ----
optimized_results <- merged %>%
  group_by(Country.Code, Year, Month) %>%
  mutate(out = list(optimize_month(select(., all_of(vars))))) %>%
  unnest_wider(out, names_sep = "_opt") %>%
  ungroup()

# ---- Save Output ----
write.csv(optimized_results, "working_data/optimized_output.csv", row.names = FALSE)

# ---- Optional: Plot a country's optimized vs actual ----
target_country <- "CHN"
plot_df <- optimized_results %>% filter(Country.Code == target_country)

ggplot(plot_df, aes(x = Day)) +
  geom_line(aes(y = Total.Cases), color = "red", linetype = "dashed") +
  geom_line(aes(y = Predicted_Optimized), color = "blue") +
  labs(
    title = paste("H5N1 Optimization for", target_country),
    y = "H5N1 Cases (Actual vs Optimized)"
  )
print("HERE IS WHERE IT BREAKS")
