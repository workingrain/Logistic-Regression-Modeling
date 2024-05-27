# Load necessary libraries
library(ggplot2)

# Independent samples t-test
ttest_Age_Readmis <- t.test(Age ~ ReAdmis, data = df)
# Print the t-test result
print(ttest_Age_Readmis)

# Create a box plot for Readmis and Age
ggplot(df, aes(x = factor(ReAdmis), y = Age, fill = ReAdmis)) +
  geom_boxplot(fill = c("lightblue", "lightcoral"), color = "black") +
  labs(title = "Age vs. Readmission", x = "Readmission", y = "Age") +
  theme_minimal()


# Create a contingency table
ct_ia_read <- table(df$Initial_admin, df$ReAdmis)
# Display the contingency table
print(ct_ia_read)
# Chi-square test
chi_ia_read <- chisq.test(ct_ia_read)
# Print the results
print(chi_ia_read)

# Create a bar plot for Readmis and initial admin
ggplot(df, aes(x = factor(Initial_admin, levels = c("Elective Admission", "Emergency Admission", "Observation Admission")), fill = factor(ReAdmis))) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Initial Admin vs. Readmission", x = "Initial Admin", y = "Frequency") +
  scale_fill_manual(values = c("hotpink4", "lightblue")) +
  theme_minimal()

# Independent samples t-test
tt_vitd_Readmis <- t.test(VitD_levels ~ ReAdmis, data = df)
# Print the t-test result
print(tt_vitd_Readmis)

# Create a box plot for Readmis and vitd levels 
ggplot(df, aes(x = factor(ReAdmis), y = VitD_levels)) +
  geom_boxplot(fill = c("lightblue", "lavender"), color = "black") +
  labs(title = "VitD_levels vs. Readmission", x = "Readmission", y = "VitD_levels") +
  theme_minimal()


# Independent samples t-test
tt_Doc_Readmis <- t.test(Doc_visits ~ ReAdmis, data = df)
# Print the t-test result
print(tt_Doc_Readmis)

# Create a box plot for Readmis and doc visits
ggplot(df, aes(x = factor(ReAdmis), y = Doc_visits)) +
  geom_boxplot(fill = c("gold1", "lightcoral"), color = "black") +
  labs(title = "Doc_visits vs. Readmission", x = "Readmission", y = "Doc_visits") +
  theme_minimal()


# Create a contingency table
ct_Comp_Readmis <- table(df$Complication_risk, df$ReAdmis)
# Chi-square test
cr_Comp_Readmis <- chisq.test(ct_Comp_Readmis)
# Print the chi-square test result
print(cr_Comp_Readmis)

# Create a stacked bar plot for readmis and complication risk
ggplot(df, aes(x = Complication_risk, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Complication Risk vs. Readmission", x = "Complication Risk", y = "Count") +
  scale_fill_manual(values = c("lightblue", "hotpink4")) +
  theme_minimal()


# Independent samples t-test
tt_id_read <- t.test(Initial_days ~ ReAdmis, data = df)
# Print the t-test result
print(tt_id_read)

# Create a box plot for readmis and initial days
ggplot(df, aes(x = factor(ReAdmis), y = Initial_days)) +
  geom_boxplot(fill = c("sienna1", "paleturquoise"), color = "black") +
  labs(title = "Initial_days vs. Readmission", x = "Readmission", y = "Initial_days") +
  theme_minimal()


# Create a contingency table
ct_HighBlood_Readmis <- table(df$HighBlood, df$ReAdmis)
# Chi-square test
cr_HighBlood_Readmis <- chisq.test(ct_HighBlood_Readmis)
# Print the chi-square test result
print(cr_HighBlood_Readmis)

# Create a stacked bar plot for readmis and high blood
ggplot(df, aes(x = HighBlood, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "High Blood vs. Readmission", x = "High Blood", y = "Count") +
  scale_fill_manual(values = c("lightblue", "magenta4")) +
  theme_minimal()


# Create a contingency table
ct_Stroke_Readmis <- table(df$Stroke, df$ReAdmis)
# Chi-square test
cr_Stroke_Readmis <- chisq.test(ct_Stroke_Readmis)
# Print the chi-square test result
print(cr_Stroke_Readmis)

# Create a stacked bar plot for readmis and stroke
ggplot(df, aes(x = Stroke, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Stroke vs. Readmission", x = "Stroke", y = "Count") +
  scale_fill_manual(values = c("darkseagreen1", "lightcoral")) +
  theme_minimal()


# Create a contingency table
ct_Overweight_Readmis <- table(df$Overweight, df$ReAdmis)
# Chi-square test
cr_Overweight_Readmis <- chisq.test(ct_Overweight_Readmis)
# Print the chi-square test result
print(cr_Overweight_Readmis)

# Create a stacked bar plot for readmis and overweight
ggplot(df, aes(x = Overweight, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Overweight vs. Readmission", x = "Overweight", y = "Count") +
  scale_fill_manual(values = c("mistyrose", "darkgreen")) +
  theme_minimal()


# Create a contingency table
ct_Arthritis_Readmis <- table(df$Arthritis, df$ReAdmis)
# Chi-square test
cr_Arthritis_Readmis <- chisq.test(ct_Arthritis_Readmis)
# Print the chi-square test result
print(cr_Arthritis_Readmis)

# Create a stacked bar plot for readmis and arthritis
ggplot(df, aes(x = Arthritis, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Arthritis vs. Readmission", x = "Arthritis", y = "Count") +
  scale_fill_manual(values = c("mediumaquamarine", "peachpuff")) +
  theme_minimal()


# Create a contingency table
ct_Diab_Readmis <- table(df$Diabetes, df$ReAdmis)
# Chi-square test
cr_Diab_Readmis <- chisq.test(ct_Diab_Readmis)
# Print the chi-square test result
print(cr_Diab_Readmis)

# Create a stacked bar plot for readmis and diabetes
ggplot(df, aes(x = Diabetes, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Diabetes vs. Readmission", x = "Diabetes", y = "Count") +
  scale_fill_manual(values = c("pink", "darkmagenta")) +
  theme_minimal()


# Create a contingency table
ct_Hyperlipidemia_Readmis <- table(df$Hyperlipidemia, df$ReAdmis)
# Chi-square test
cr_Hyperlipidemia_Readmis <- chisq.test(ct_Hyperlipidemia_Readmis)
# Print the chi-square test result
print(cr_Hyperlipidemia_Readmis)

# Create a stacked bar plot for readmis and hyperlipidemia
ggplot(df, aes(x = Hyperlipidemia, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Hyperlipidemia vs. Readmission", x = "Hyperlipidemia", y = "Count") +
  scale_fill_manual(values = c("gold1", "cadetblue4")) +
  theme_minimal()


# Create a contingency table
ct_BackPain_Readmis <- table(df$BackPain, df$ReAdmis)
# Chi-square test
cr_BackPain_Readmis <- chisq.test(ct_BackPain_Readmis)
# Print the chi-square test result
print(cr_BackPain_Readmis)

# Create a stacked bar plot for readmis and backpain
ggplot(df, aes(x = BackPain, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "BackPain vs. Readmission", x = "BackPain", y = "Count") +
  scale_fill_manual(values = c("plum1", "lightblue")) +
  theme_minimal()


# Create a contingency table
ct_Anxiety_Readmis <- table(df$Anxiety, df$ReAdmis)
# Chi-square test
cr_Anxiety_Readmis <- chisq.test(ct_Anxiety_Readmis)
# Print the chi-square test result
print(cr_Anxiety_Readmis)

# Create a stacked bar plot for readmis and anxiety
ggplot(df, aes(x = Anxiety, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Anxiety vs. Readmission", x = "Anxiety", y = "Count") +
  scale_fill_manual(values = c("lemonchiffon", "lavenderblush3")) +
  theme_minimal()


# Create a contingency table
ct_Allergic_rhinitis_Readmis <- table(df$Allergic_rhinitis, df$ReAdmis)
# Chi-square test
cr_Allergic_rhinitis_Readmis <- chisq.test(ct_Allergic_rhinitis_Readmis)
# Print the chi-square test result
print(cr_Allergic_rhinitis_Readmis)

# Create a stacked bar plot for readmis and allergic rhinitis
ggplot(df, aes(x = Allergic_rhinitis, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Allergic Rhinitis vs. Readmission", x = "Allergic Rhinitis", y = "Count") +
  scale_fill_manual(values = c("maroon", "powderblue")) +
  theme_minimal()


# Create a contingency table
ct_Reflux_esophagitis_Readmis <- table(df$Reflux_esophagitis, df$ReAdmis)
# Chi-square test
cr_Reflux_esophagitis_Readmis <- chisq.test(ct_Reflux_esophagitis_Readmis)
# Print the chi-square test result
print(cr_Reflux_esophagitis_Readmis)

# Create a stacked bar plot for readmis and reflux esophagitis
ggplot(df, aes(x = Reflux_esophagitis, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Reflux Esophagitis vs. Readmission", x = "Reflux Esophagitis", y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme_minimal()


# Create a contingency table
ct_Asthma_Readmis <- table(df$Asthma, df$ReAdmis)
# Chi-square test
cr_Asthma_Readmis <- chisq.test(ct_Asthma_Readmis)
# Print the chi-square test result
print(cr_Asthma_Readmis)

# Create a stacked bar plot for readmis and asthma
ggplot(df, aes(x = Asthma, fill = ReAdmis)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Asthma vs. Readmission", x = "Asthma", y = "Count") +
  scale_fill_manual(values = c("mediumorchid4", "mistyrose")) +
  theme_minimal()