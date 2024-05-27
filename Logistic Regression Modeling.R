# Run logistic regression
init_model <- glm(ReAdmis ~ Age + Initial_admin + Doc_visits + VitD_levels + Complication_risk +
                     HighBlood + Stroke + Overweight + Arthritis + Diabetes +
                     Hyperlipidemia + BackPain + Anxiety + Allergic_rhinitis +
                     Reflux_esophagitis + Asthma, 
                   family = binomial(link = "logit"), data = df_subset)
summary(init_model) # Summary of the initial model
# Stepwise backward elimination
red_model <- step(init_model, direction = "backward")
summary(red_model) # Summary of the final model

library(caret)

# Convert into binary predictions for red_model
red_predictions <- predict(red_model, newdata = df_subset, type = "response")
threshold <- 0.5  # Adjust this threshold as needed
red_binary_predictions <- ifelse(red_predictions > threshold, 1, 0)

# Ensure the levels are consistent
red_binary_predictions <- factor(red_binary_predictions, levels = levels(as.factor(df_subset$ReAdmis)))

# Create a confusion matrix
red_conf_matrix <- confusionMatrix(data = red_binary_predictions, reference = as.factor(df_subset$ReAdmis))
# Print the confusion matrix
red_conf_matrix


# Convert into binary predictions for init_model
predictions <- predict(init_model, newdata = df_subset, type = "response")
threshold <- 0.5  # Adjust this threshold as needed
binary_predictions <- ifelse(predictions > threshold, 1, 0)

# Ensure the levels are consistent
binary_predictions <- factor(binary_predictions, levels = levels(as.factor(df_subset$ReAdmis)))

# Create a confusion matrix
conf_matrix <- confusionMatrix(data = binary_predictions, reference = as.factor(df_subset$ReAdmis))
# Print the confusion matrix
conf_matrix


# Extract coefficients from the model
coefficients <- red_model$coefficients
# Calculate odds ratios
odds_ratios <- exp(coefficients)
# Print the odds ratios
print(odds_ratios)

# Calculate percentage change in odds
percentage_change <- (odds_ratios - 1) * 100
# Print the percentage change
print(percentage_change)

