# we be anti-establishment up in this joint

gov_type <- data.frame(
  Country.Name = c("United States", "United Kingdom", "Vietnam", "China"),
  Country.Code = c("USA", "GBR", "VNM", "CHN"),
  Government_Type = c(
    "Federal Presidential Democracy",
    "Unitary Parliamentary Democracy",
    "One-Party Socialist Republic",
    "One-Party Authoritarian State"
  ),
  Coordination_Score = c(1, 2, 3, 3)
)

write.csv(gov_type, "working_data/gov_type.csv", row.names = FALSE)
print("GOVERNMENT TYPED")