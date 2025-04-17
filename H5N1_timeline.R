library(tidyverse)

# Load your data
data <- read.csv("working_data/H5N1_incidence_subset.csv")

# Pivot data: years as column headers
timeline_data <- data %>%
  pivot_wider(
    names_from = Year,
    values_from = Total.Cases
  )

# View the result
print(timeline_data)
write.csv(timeline_data, "working_data/H5N1_timeline", row.names = FALSE)
install.packages("writexl")
library(writexl)
write_xlsx(timeline_data, "working_data/H5N1_timeline.xslx")

