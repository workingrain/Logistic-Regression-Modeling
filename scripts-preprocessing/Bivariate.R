# Load necessary libraries
library(ggplot2)

# Bar plot for readmission
ggplot(df, aes(x = factor(ReAdmis))) +
  geom_bar(fill = "lightsteelblue1", color = "black") +
  labs(title = "Bar Plot: ReAdmission Frequency",
       x = "ReAdmission",
       y = "Frequency") +
  theme_minimal()

# Calculate the frequency of each category
freq_readmis <- table(df$ReAdmis)
print(freq_readmis)

# Histogram of age
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "lavender", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

# Create a bar plot for the "Initial admin" variable
ggplot(df, aes(x = Initial_admin)) +
  geom_bar(fill = "mistyrose3", color = "black") +
  labs(title = "Distribution of Initial Admin",
       x = "Initial admin",
       y = "Count") +
  theme_minimal()

# Histogram of Vitamin D levels
ggplot(df, aes(x = VitD_levels)) +
  geom_histogram(binwidth = 1, fill = "springgreen4", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Vitamin D levels",
       x = "Vitamin D levels",
       y = "Frequency") +
  theme_minimal()

# Create a histogram for the "Doc_visits" variable
ggplot(df, aes(x = Doc_visits)) +
  geom_histogram(binwidth = 1, fill = "darkseagreen1", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Doctor Visits",
       x = "Doctor Visits",
       y = "Frequency") +
  theme_minimal()

# Create a bar plot for the "Complication_risk" variable
ggplot(df, aes(x = Complication_risk, fill = Complication_risk)) +
  geom_bar() +
  labs(title = "Distribution of Complication Risk",
       x = "Complication Risk Level",
       y = "Count") +
  theme_minimal()


# Create a histogram for the "Initial days" variable
ggplot(df, aes(x = Initial_days)) +
  geom_histogram(binwidth = 5, fill = "lightsalmon", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Initial Days",
       x = "Initial Days",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_hb <- table(df$HighBlood)
print(freq_hb)

# Create a bar plot for HighBlood
ggplot(df, aes(x = factor(HighBlood))) +
  geom_bar(fill = "rosybrown1", color = "black") +
  labs(title = "Bar Plot: High Blood Frequency",
       x = "High Blood",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_stroke <- table(df$Stroke)
print(freq_stroke)

# Create a bar plot for Stroke
ggplot(df, aes(x = factor(Stroke))) +
  geom_bar(fill = "orange1", color = "black") +
  labs(title = "Bar Plot: Stroke Frequency",
       x = "Stroke",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_ow <- table(df$Overweight)
print(freq_ow)

# Create a bar plot for Overweight
ggplot(df, aes(x = factor(Overweight))) +
  geom_bar(fill = "peachpuff", color = "black") +
  labs(title = "Bar Plot: Overweight Frequency",
       x = "Overweight",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_ar <- table(df$Arthritis)
print(freq_ar)

# Create a bar plot for Arthritis
ggplot(df, aes(x = factor(Arthritis))) +
  geom_bar(fill = "steelblue4", color = "black") +
  labs(title = "Bar Plot: Arthritis Frequency",
       x = "Arthritis",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_dia <- table(df$Diabetes)
print(freq_dia)

# Bar plot for Diabetes
ggplot(df, aes(x = factor(Diabetes))) +
  geom_bar(fill = "powderblue", color = "black") +
  labs(title = "Bar Plot: Diabetes Frequency",
       x = "Diabetes",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_hyp <- table(df$Hyperlipidemia)
print(freq_hyp)

# Bar plot for Hyperlipidemia
ggplot(df, aes(x = factor(Hyperlipidemia))) +
  geom_bar(fill = "maroon", color = "black") +
  labs(title = "Bar Plot: Hyperlipidemia Frequency",
       x = "Hyperlipidemia",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_bp <- table(df$BackPain)
print(freq_bp)

# Bar plot for BackPain
ggplot(df, aes(x = factor(BackPain))) +
  geom_bar(fill = "lavenderblush2", color = "black") +
  labs(title = "Bar Plot: Back Pain Frequency",
       x = "Back Pain",
       y = "Frequency") +
  theme_minimal()

# Calculate the frequency of each category
freq_an <- table(df$Anxiety)
print(freq_an)

# Bar plot for Anxiety
ggplot(df, aes(x = factor(Anxiety))) +
  geom_bar(fill = "lemonchiffon1", color = "black") +
  labs(title = "Bar Plot: Anxiety Frequency",
       x = "Anxiety",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_alr <- table(df$Allergic_rhinitis)
print(freq_alr)

# Bar plot for Allergic rhinitis
ggplot(df, aes(x = factor(Allergic_rhinitis))) +
  geom_bar(fill = "mediumorchid4", color = "black") +
  labs(title = "Bar Plot: Allergic Rhinitis Frequency",
       x = "Allergic Rhinitis",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_re <- table(df$Reflux_esophagitis)
print(freq_re)

# Bar plot for Allergic rhinitis
ggplot(df, aes(x = factor(Reflux_esophagitis))) +
  geom_bar(fill = "lightsteelblue1", color = "black") +
  labs(title = "Bar Plot: Reflux Esophagitis Frequency",
       x = "Reflux Esophagitis",
       y = "Frequency") +
  theme_minimal()

# Calculate and print the frequency of each category
freq_asthma <- table(df$Asthma)
print(freq_asthma)

# Bar plot for Asthma
ggplot(df, aes(x = factor(Asthma))) +
  geom_bar(fill = "plum1", color = "black") +
  labs(title = "Bar Plot: Asthma Frequency",
       x = "Asthma",
       y = "Frequency") +
  theme_minimal()


