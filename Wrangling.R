library(plyr)

# Convert categorical variables to numeric
df$ReAdmis <- as.numeric(revalue(df$ReAdmis, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$HighBlood <- as.numeric(revalue(df$HighBlood, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Stroke <- as.numeric(revalue(df$Stroke, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Complication_risk <- factor(df$Complication_risk, levels = c("Low", "Medium", "High"))
df$Overweight <- as.numeric(revalue(df$Overweight, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Arthritis <- as.numeric(revalue(df$Arthritis, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Diabetes <- as.numeric(revalue(df$Diabetes, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Hyperlipidemia <- as.numeric(revalue(df$Hyperlipidemia, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$BackPain <- as.numeric(revalue(df$BackPain, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Anxiety <- as.numeric(revalue(df$Anxiety, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Allergic_rhinitis <- as.numeric(revalue(df$Allergic_rhinitis, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Reflux_esophagitis <- as.numeric(revalue(df$Reflux_esophagitis, c("Yes" = 1, "No" = 0, "NA" = NA)))
df$Asthma <- as.numeric(revalue(df$Asthma, c("Yes" = 1, "No" = 0, "NA" = NA)))

# Convert into factors
df$Initial_admin <- factor(df$Initial_admin, levels = c("Elective Admission", "Emergency Admission", "Observation Admission"))
df$Complication_risk <- factor(df$Complication_risk, levels = c("Low", "Medium", "High"))

# Keep only the specified columns
df_subset <- df[, c(
  "ReAdmis" ,"Age", "Initial_admin", "VitD_levels", "Doc_visits",
  "Complication_risk", "HighBlood", "Stroke", "Overweight", 
  "Arthritis", "Diabetes", "Hyperlipidemia", "BackPain", "Anxiety", 
  "Allergic_rhinitis", "Reflux_esophagitis", "Asthma"
)]

# View the resulting subset of the dataset
head(df_subset)

# Write substted data into .csv
write.csv(df_subset, "medical_clean_subsetted_data.csv", row.names = TRUE)

