# Load the necessary library
source("install_and_load.R")
install_and_load("tidyr", "dplyr")

# Function to check if data is already in long format
is_long_format <- function(df) {
  return("year" %in% colnames(df) && ncol(df) > 2)
}

reshape_long <- function(df, value_column_name, df_name = deparse(substitute(df))) {
  if (is_long_format(df)) {
    message(paste0(df_name, " is already in long format. Renaming as ", df_name, "_long and removing original."))
    assign(paste0(df_name, "_long"), df, envir = .GlobalEnv)
    if (exists(df_name, envir = .GlobalEnv)) rm(list = df_name, envir = .GlobalEnv)
    return(get(paste0(df_name, "_long")))
  }
  
  x_cols <- grep("^X\\d{4}$", colnames(df), value = TRUE)
  
  if (length(x_cols) == 0) {
    message(paste0("⚠️ ", df_name, " could not be reshaped: no year columns found. Renaming as ", df_name, "_long and removing original."))
    assign(paste0(df_name, "_long"), df, envir = .GlobalEnv)
    if (exists(df_name, envir = .GlobalEnv)) rm(list = df_name, envir = .GlobalEnv)
    return(get(paste0(df_name, "_long")))
  }
  
  message(paste0("Reshaping ", df_name, " into long format..."))
  
  df_long <- df %>%
    pivot_longer(
      cols = all_of(x_cols),
      names_to = "year",
      values_to = value_column_name
    ) %>%
    mutate(year = as.numeric(gsub("X", "", year)))
  
  message(paste0(df_name, " has been reshaped to long format and saved as ", df_name, "_long. Original removed."))
  
  assign(paste0(df_name, "_long"), df_long, envir = .GlobalEnv)
  if (exists(df_name, envir = .GlobalEnv)) rm(list = df_name, envir = .GlobalEnv)
  return(get(paste0(df_name, "_long")))
}
