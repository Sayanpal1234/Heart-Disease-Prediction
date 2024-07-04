data=framingham
# View the first few rows of the dataset
head(data)

# Get summary information about the columns
summary(data)

# Get information about the structure of the dataset
str(data)

# Get information about the dimensions of the dataset
dim(data)

# Get column names
colnames(data)

# Check for missing values in all columns
missing_values <- colSums(is.na(data))
missing_values


names_with_missing <- names(missing_values[missing_values > 0])
print(names_with_missing)

data <- subset(data, select = -c(education))

colnames(data)



# Replace missing values with median for specified columns
median_cigsPerDay <- median(data$cigsPerDay, na.rm = TRUE)
median_BPMeds <- median(data$BPMeds, na.rm = TRUE)
median_totChol <- median(data$totChol, na.rm = TRUE)
median_BMI <- median(data$BMI, na.rm = TRUE)
median_heartRate <- median(data$heartRate, na.rm = TRUE)
median_glucose <- median(data$glucose, na.rm = TRUE)

data$cigsPerDay[is.na(data$cigsPerDay)] <- median_cigsPerDay
data$BPMeds[is.na(data$BPMeds)] <- median_BPMeds
data$totChol[is.na(data$totChol)] <- median_totChol
data$BMI[is.na(data$BMI)] <- median_BMI
data$heartRate[is.na(data$heartRate)] <- median_heartRate
data$glucose[is.na(data$glucose)] <- median_glucose


summary(data)

missing_values <- colSums(is.na(data))
missing_values                         






# Load required library
library(ggplot2)

# Subset numeric variables
numeric_vars <- colnames(data)[sapply(data, is.numeric)]

# Plot boxplots for numeric variables
for (var in numeric_vars) {
  ggplot(data, aes_string(y = var)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", var),
         y = var) +
    theme_minimal()
}

# Load necessary packages
library(ggplot2)

# Create bar plot for currentSmoker
ggplot(data, aes(x = factor(currentSmoker), fill = factor(TenYearCHD))) +
  geom_bar(position = "fill", width = 0.5) +
  labs(title = "Current Smoker vs. TenYearCHD", x = "Current Smoker", y = "Proportion") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create bar plot for BPMeds
ggplot(data, aes(x = factor(BPMeds), fill = factor(TenYearCHD))) +
  geom_bar(position = "fill", width = 0.5) +
  labs(title = "BPMeds vs. TenYearCHD", x = "BPMeds", y = "Proportion") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create bar plot for prevalentStroke
ggplot(data, aes(x = factor(prevalentStroke), fill = factor(TenYearCHD))) +
  geom_bar(position = "fill", width = 0.5) +
  labs(title = "Prevalent Stroke vs. TenYearCHD", x = "Prevalent Stroke", y = "Proportion") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create bar plot for prevalentHyp
ggplot(data, aes(x = factor(prevalentHyp), fill = factor(TenYearCHD))) +
  geom_bar(position = "fill", width = 0.5) +
  labs(title = "Prevalent Hypertension vs. TenYearCHD", x = "Prevalent Hypertension", y = "Proportion") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create bar plot for diabetes
ggplot(data, aes(x = factor(diabetes), fill = factor(TenYearCHD))) +
  geom_bar(position = "fill", width = 0.5) +
  labs(title = "Diabetes vs. TenYearCHD", x = "Diabetes", y = "Proportion") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Bin cigsPerDay into categories
data$cigsPerDay_category <- cut(data$cigsPerDay, breaks = c(0, 5, 10, 20, max(data$cigsPerDay)), labels = c("0-5", "6-10", "11-20", "21+"))

# Create stacked bar plot for cigsPerDay
ggplot(data, aes(x = cigsPerDay_category, fill = factor(TenYearCHD))) +
  geom_bar(position = "fill", width = 0.5) +
  labs(title = "Cigarettes Per Day vs. TenYearCHD", x = "Cigarettes Per Day", y = "Proportion") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



#EDA

str(data)


library(ggplot2)



numeric_columns <- c("age", "cigsPerDay", "totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")

# Creating boxplots for each numeric variable
par(mfrow = c(3, 3))  # Setting up a grid of plots
for (col in numeric_columns) {
  boxplot(data[[col]], main = col, col = "skyblue", border = "black", horizontal = TRUE)
}




numeric_columns <- c("age", "cigsPerDay", "totChol", "sysBP", "diaBP", "BMI", "heartRate", "glucose")

# Creating boxplots for each numeric variable
par(mfrow = c(3, 3))  # Setting up a grid of plots
for (col in numeric_columns) {
  boxplot(data[[col]], main = col, col = "skyblue", border = "black", horizontal = TRUE)}




# Male
ggplot(data, aes(x = factor(male))) +
  geom_bar(fill = "skyblue", width = 0.5) +
  labs(title = "Distribution of Gender") +
  theme_minimal()


ggplot(data, aes(x = age)) +
  geom_histogram(fill = "#FF6F61", color = "white", bins = 30) +  # Set custom fill color and white border
  labs(title = "Distribution of Age", x = "Age", y = "Frequency") +  # Add axis labels
  theme_minimal() +  # Use minimal theme
  theme(plot.title = element_text(size = 18, face = "bold"),  # Increase title size and make it bold
        axis.title = element_text(size = 14),  # Increase axis label size
        axis.text = element_text(size = 12),   # Increase axis tick label size
        panel.grid.major = element_blank(),    # Remove major gridlines
        panel.grid.minor = element_blank())    # Remove minor gridlines


# Create a list of column names
columns <- c("currentSmoker", "cigsPerDay", "BPMeds", "prevalentStroke", 
             "prevalentHyp", "diabetes", "totChol", "sysBP", "diaBP", 
             "BMI", "heartRate", "glucose", "TenYearCHD")

# Create a list of colors for each column
colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", 
            "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", 
            "#AEC7E8", "#FFBB78", "#FF9896")

# Create histograms for each column
plots <- lapply(1:length(columns), function(i) {
  ggplot(data, aes(x = .data[[columns[i]]])) +
    geom_histogram(fill = colors[i], color = "white", bins = 30) +
    labs(title = paste("Distribution of", columns[i]), x = columns[i], y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
})

# Output plots
plots[[1]]  
plots[[2]]  
plots[[3]]  
plots[[4]]
plots[[5]]  
plots[[6]]
plots[[7]]  
plots[[8]]
plots[[9]]  
plots[[10]]
plots[[11]]  
plots[[12]]
plots[[13]]



library(ggplot2)

# Create a list of column names
columns <- c("BPMeds", "prevalentStroke", "prevalentHyp", "diabetes", 
             "currentSmoker", "TenYearCHD")

# Define colors for each bar plot
bar_colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B")

# Create histograms for each column
plots <- lapply(1:length(columns), function(i) {
  ggplot(data, aes(x = factor(.data[[columns[i]]]))) +
    geom_bar(fill = bar_colors[i]) +  # Bar color
    labs(title = paste("Distribution of", columns[i]), x = columns[i], y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 18, face = "bold", color = "darkblue"),  # Title properties
          axis.title = element_text(size = 14, color = "black"),                    # Axis label properties
          axis.text = element_text(size = 12, color = "black"))                     # Axis tick label properties
})

# Output plots
plots[[1]]  # Plot for "BPMeds"
plots[[2]]  # Plot for "prevalentStroke"
plots[[3]]  # Plot for "prevalentHyp"
plots[[4]]  # Plot for "diabetes"
plots[[5]]  # Plot for "currentSmoker"
plots[[6]]  # Plot for "TenYearCHD"



correlation_df <- as.data.frame(correlation_matrix)
correlation_df <- cbind(variable = row.names(correlation_df), correlation_df)
correlation_df

# Convert correlation matrix to long format
correlation_long <- reshape(correlation_df, idvar = "variable", varying = list(names(correlation_df)[-1]), 
                            direction = "long", v.names = "correlation")

correlation_long 


install.packages("corrplot")
library(corrplot)


correlation_matrix <- cor(data)

correlation_matrix


corrplot(correlation_matrix, method = "color", type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45)


install.packages("caret")
install.packages("MASS")
install.packages("pROC")
install.packages("tidyverse")


library(caret)   # for train/test split
library(MASS)    # for lda function
library(pROC)    # for AUC calculation
library(tidyverse)



# Assuming 'data' is your dataset
# Store output variable 'TenYearCHD' in 'y'
y <- data$TenYearCHD
X <- subset(data, select = -c(TenYearCHD))  # Independent variables

X
y




set.seed(123) # For reproducibility
library(caTools)
split <- sample.split(y, SplitRatio = 0.7)
X_train <- X[split, ]
X_test <- X[!split, ]
y_train <- y[split]
y_test <- y[!split]

y_test

install.packages("caTools")



model <- glm(TenYearCHD ~ ., data = as.data.frame(cbind(X_train, TenYearCHD = y_train)), family = binomial)

model
# Step 6: Predictions
predictions <- predict(model, newdata = as.data.frame(X_test), type = "response")

predictions



# Step 7: Evaluate Model
library(pROC)
# Calculate ROC curve
roc_curve <- roc(y_test, predictions)
roc_curve
# Calculate accuracy
accuracy <- mean((predictions >= 0.5) == y_test)
accuracy

conf_matrix <- table(y_test, predictions >= 0.5)

conf_matrix

# Calculate sensitivity (true positive rate)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Calculate specificity (true negative rate)
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])

# Calculate recall (same as sensitivity)
recall <- sensitivity

# Calculate precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])

# Calculate F1 score
f1_score <- 2 * precision * recall / (precision + recall)


roc_curve

accuracy

conf_matrix

sensitivity

specificity

recall 

f1_score

precision

print(paste("Accuracy:", accuracy))
print("Confusion Matrix:")
print(conf_matrix)
print(paste("Sensitivity (Recall):", sensitivity))
print(paste("Specificity:", specificity))
print(paste("Precision:", precision))
print(paste("F1 Score:", f1_score))


# Plot ROC Curve
plot(roc_curve, main = "ROC Curve", col = "blue")
abline(a = 0, b = 1, lty = 2, col = "red") # Add diagonal line
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lwd = 2)




# Create confusion matrix
conf_matrix <- matrix(c(1069, 181, 9, 12), nrow = 2, byrow = TRUE,
                      dimnames = list(Actual = c("0", "1"),
                                      Predicted = c("0", "1")))

# Load required libraries
library(ggplot2)

# Convert confusion matrix to data frame
conf_matrix_df <- as.data.frame.table(conf_matrix)

# Plot heatmap
ggplot(data = conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "pink", high = "blue") +
  labs(title = "Confusion Matrix",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()



#Scaling


library(caret)
preproc <- preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preproc, X)


set.seed(123) # For reproducibility
library(caTools)
split <- sample.split(y, SplitRatio = 0.7)
X_train <- X_scaled[split, ]
X_test <- X_scaled[!split, ]
y_train <- y[split]
y_test <- y[!split]



model <- glm(TenYearCHD ~ ., data = as.data.frame(cbind(X_train, TenYearCHD = y_train)), family = binomial)

coefficients <- coef(model)

coefficients 


# Extract coefficients
B0 <- coefficients[1]  # Intercept
B <- coefficients[-1]  # Coefficients for independent variables

B0
B




library(pROC)
roc_curve <- roc(y_test, predictions)
accuracy <- mean((predictions >= 0.5) == y_test)
conf_matrix <- table(y_test, predictions >= 0.5)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
recall <- sensitivity
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])

roc_curve
accuracy 




#K-Nearest Neighbors (KNN):
library(class)

# KNN model training
knn_model <- knn(train = X_train, test = X_test, cl = y_train, k = 5)

# KNN predictions
knn_predictions <- as.integer(knn_model)

# KNN evaluation
knn_accuracy <- mean(knn_predictions == y_test)

# Print KNN accuracy
cat("KNN Accuracy:", knn_accuracy, "\n")


#DECISION TREE

# Load required library
library(rpart)

# Decision Tree model training
tree_model <- rpart(TenYearCHD ~ ., data = as.data.frame(cbind(X_train, TenYearCHD = y_train)), method = "class")

# Decision Tree predictions
tree_predictions <- predict(tree_model, newdata = as.data.frame(X_test), type = "class")

# Decision Tree evaluation
tree_accuracy <- mean(tree_predictions == y_test)

# Print Decision Tree accuracy
cat("Decision Tree Accuracy:", tree_accuracy, "\n")




#SVM

# Load required library
library(e1071)

# SVM model training
svm_model <- svm(TenYearCHD ~ ., data = as.data.frame(cbind(X_train, TenYearCHD = y_train)), kernel = "radial")

# SVM predictions
svm_predictions <- predict(svm_model, newdata = as.data.frame(X_test))

# SVM evaluation
svm_accuracy <- mean(svm_predictions == y_test)

# Print SVM accuracy
cat("SVM Accuracy:", svm_accuracy, "\n")


#naived bayes 

# Naive Bayes model training
nb_model <- naiveBayes(TenYearCHD ~ ., data = as.data.frame(cbind(X_train, TenYearCHD = y_train)))

# Naive Bayes predictions
nb_predictions <- predict(nb_model, newdata = as.data.frame(X_test))

# Naive Bayes evaluation
nb_accuracy <- mean(nb_predictions == y_test)

# Print Naive Bayes accuracy
cat("Naive Bayes Accuracy:", nb_accuracy, "\n")


cat("Logistic Regression Accuracy:", accuracy, "\n")
cat("KNN Accuracy:", knn_accuracy, "\n")
cat("Decision Tree Accuracy:", tree_accuracy, "\n")
cat("Naive Bayes Accuracy:", nb_accuracy, "\n")




# Create a data frame for accuracies
model_accuracies <- data.frame(Model = c("Logistic Regression", "KNN", "Decision Tree", "Naive Bayes"),
                               Accuracy = c(accuracy, knn_accuracy, tree_accuracy, nb_accuracy))

# Plot the bar plot
library(ggplot2)
ggplot(model_accuracies, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Model Accuracies",
       x = "Model",
       y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Fit the full logistic regression model
full_model <- glm(TenYearCHD ~ ., data = data, family = binomial)

# Fit the reduced model (with a subset of predictors)
reduced_model <- glm(TenYearCHD ~ age + currentSmoker + diabetes, data = data, family = binomial)

# Calculate log-likelihoods
loglik_full <- logLik(full_model)
loglik_reduced <- logLik(reduced_model)
loglik_full 
loglik_reduced 
# Calculate likelihood ratio test statistic
LR_stat <- -2 * (loglik_reduced - loglik_full)
LR_stat
# Perform hypothesis test
p_value <- pchisq(LR_stat, df = (length(coef(full_model)) - length(coef(reduced_model))), lower.tail = FALSE)

# Print test statistic and p-value
cat("Likelihood Ratio Test Statistic:", LR_stat, "\n")
cat("P-value:", p_value, "\n")


#Null Hypothesis (H0): The null hypothesis of the likelihood ratio test is that the reduced model (with fewer predictors) provides an adequate fit for the data. In other words, the additional predictors included in the full model do not improve the model's fit significantly.
#Alternative Hypothesis (H1): The alternative hypothesis is that the full model (with additional predictors) provides a better fit for the data compared to the reduced model.



#A p-value of 1.034875e-25 is an extremely small value, indicating very strong evidence against the null hypothesis.


library(pROC)
# Calculate ROC curve
roc_curve <- roc(y_test, predictions)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")

# Add AUC value to the plot
text(0.8, 0.2, paste("AUC =", round(auc(roc_curve), 2)), col = "black", cex = 1.5)



# Assuming you have already fitted a logistic regression model named 'model'

# Get coefficients
coefficients <- coef(model)

# Interpret coefficients
odds_male_vs_female <- exp(coefficients["male"])  # Odds ratio for males vs. females
odds_age <- exp(coefficients["age"])  # Odds ratio for one year increase in age
odds_cigsPerDay <- exp(coefficients["cigsPerDay"])  # Odds ratio for one additional cigarette per day
odds_sysBP <- exp(coefficients["sysBP"])  # Odds ratio for one unit increase in systolic blood pressure


odds_male_vs_female <- exp(coefficients["male"])  # Odds ratio for males vs. females
odds_age <- exp(coefficients["age"])  # Odds ratio for one year increase in age
odds_cigsPerDay <- exp(coefficients["cigsPerDay"])  # Odds ratio for one additional cigarette per day
odds_sysBP <- exp(coefficients["sysBP"])  # Odds ratio for one unit increase in systolic blood pressure


print(paste("Odds ratio for males vs. females:", round(exp(coefficients["male"]), 2),
            "\nOdds ratio for one year increase in age:", round(exp(coefficients["age"]), 2),
            "\nOdds ratio for one additional cigarette per day:", round(exp(coefficients["cigsPerDay"]), 2),
            "\nOdds ratio for one unit increase in systolic blood pressure:", round(exp(coefficients["sysBP"]), 2)))


cat("Odds ratio for males vs. females:", round(exp(coefficients["male"]), 2), "\n",
    "Odds ratio for one year increase in age:", round(exp(coefficients["age"]), 2), "\n",
    "Odds ratio for one additional cigarette per day:", round(exp(coefficients["cigsPerDay"]), 2), "\n",
    "Odds ratio for one unit increase in systolic blood pressure:", round(exp(coefficients["sysBP"]), 2), "\n")


# Convert odds ratios to percentage change
percent_change_male_vs_female <- (odds_male_vs_female - 1) * 100
percent_change_age <- (odds_age - 1) * 100
percent_change_cigsPerDay <- (odds_cigsPerDay - 1) * 100
percent_change_sysBP <- (odds_sysBP - 1) * 100

# Print interpretations
print(paste("Odds of heart disease for males over females are", round(odds_male_vs_female, 2)))
print(paste("This represents a", round(percent_change_male_vs_female, 2), "% increase in odds for males over females"))

print(paste("For every one year increase in age, there is a", round(percent_change_age, 2), "% increase in odds of heart disease"))

print(paste("With every extra cigarette smoked per day, there is a", round(percent_change_cigsPerDay, 2), "% increase in odds of heart disease"))

print(paste("For every one unit increase in systolic blood pressure, there is a", round(percent_change_sysBP, 2), "% increase in odds of heart disease"))


# Print interpretations
cat("Odds of heart disease for males over females are", round(odds_male_vs_female, 2), "\n",
    "This represents a", round(percent_change_male_vs_female, 2), "% increase in odds for males over females", "\n",
    "For every one year increase in age, there is a", round(percent_change_age, 2), "% increase in odds of heart disease", "\n",
    "With every extra cigarette smoked per day, there is a", round(percent_change_cigsPerDay, 2), "% increase in odds of heart disease", "\n",
    "For every one unit increase in systolic blood pressure, there is a", round(percent_change_sysBP, 2), "% increase in odds of heart disease", "\n")

# Load necessary packages
library(ggplot2)

# Create histogram for totChol
ggplot(data, aes(x = totChol, fill = factor(TenYearCHD))) +
  geom_histogram(binwidth = 10, position = "identity", alpha = 0.7) +
  labs(title = "Total Cholesterol vs. TenYearCHD", x = "Total Cholesterol", y = "Frequency") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create histogram for sysBP
ggplot(data, aes(x = sysBP, fill = factor(TenYearCHD))) +
  geom_histogram(binwidth = 10, position = "identity", alpha = 0.7) +
  labs(title = "Systolic Blood Pressure vs. TenYearCHD", x = "Systolic Blood Pressure", y = "Frequency") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create histogram for diaBP
ggplot(data, aes(x = diaBP, fill = factor(TenYearCHD))) +
  geom_histogram(binwidth = 10, position = "identity", alpha = 0.7) +
  labs(title = "Diastolic Blood Pressure vs. TenYearCHD", x = "Diastolic Blood Pressure", y = "Frequency") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create histogram for BMI
ggplot(data, aes(x = BMI, fill = factor(TenYearCHD))) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  labs(title = "BMI vs. TenYearCHD", x = "BMI", y = "Frequency") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create histogram for heartRate
ggplot(data, aes(x = heartRate, fill = factor(TenYearCHD))) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(title = "Heart Rate vs. TenYearCHD", x = "Heart Rate", y = "Frequency") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create histogram for glucose
ggplot(data, aes(x = glucose, fill = factor(TenYearCHD))) +
  geom_histogram(binwidth = 10, position = "identity", alpha = 0.7) +
  labs(title = "Glucose vs. TenYearCHD", x = "Glucose", y = "Frequency") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()

# Create histogram for age
ggplot(data, aes(x = age, fill = factor(TenYearCHD))) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(title = "Age vs. TenYearCHD", x = "Age", y = "Frequency") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels = c("0" = "No CHD", "1" = "CHD")) +
  theme_minimal()


correlation_df1 <- as.data.frame(correlation_matrix)
correlation_df1

summary(model)
