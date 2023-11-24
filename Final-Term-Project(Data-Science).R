library(readr)
data <- read_csv("F:/Data-Science/Project/Final-term/winequality-red.csv")

str(data)

colSums(is.na(data))


quality <- data$quality
data <- data[, -which(names(data) == "quality")]
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
normalized_data <- as.data.frame(lapply(data, min_max_normalize))
normalized_data <- cbind(normalized_data, quality)
normalized_data


write_csv(normalized_data, "F:/Data-Science/Project/Final-term/normalized_red-wine.csv")

dataset<- read.csv("F:/Data-Science/Project/Final-term/normalized_red-wine.csv")


correlation_matrix <- cor(dataset[c("fixed.acidity", "volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")], dataset$quality)
View(correlation_matrix)

correlation_with_quality <- cor(dataset)
View(correlation_with_quality)

library(corrplot)
correlation_matrix <- cor(dataset)
corrplot(correlation_matrix, method = "color", type = "upper")


attributes_with_zero_correlation <- names(correlation_with_quality[correlation_with_quality == 0])
if (length(attributes_with_zero_correlation) > 0) {
  print("Attributes with zero correlation to quality:", "\n")
  print(attributes_with_zero_correlation)
} else {
  print("No attributes with zero correlation to quality.")
}



library(class)
library(caret)
features <- dataset[, !colnames(dataset) %in% c("quality")]
target <- dataset$quality
set.seed(123)
train_indices <- sample(seq_len(nrow(features)), size = 0.70 * nrow(features))
train_data <- features[train_indices, ]
train_target <- target[train_indices]
test_data <- features[-train_indices, ]
test_target <- target[-train_indices]
knn_model <- knn(train_data, test_data, train_target, k = 10)
knn_model
accuracy <- sum(knn_model == test_target) / length(test_target)
cat("The accuracy of trainig and testing is: ", accuracy)

confusion_matrix <- table(Actual = test_target, Predicted = knn_model)
confusion_matrix
conf_matrix<- confusion_matrix
recall <- numeric(nrow(conf_matrix))
precision <- numeric(nrow(conf_matrix))
for (i in 1:nrow(conf_matrix)) {
  true_positives <- conf_matrix[i, i]
  false_negatives <- sum(conf_matrix[i, ]) - true_positives
  false_positives <- sum(conf_matrix[, i]) - true_positives
  precision[i] <- true_positives / (true_positives + false_positives)
  recall[i] <- true_positives / (true_positives + false_negatives)
}
cat("Precision Value :", precision,"\n")
cat("Recall Value :",recall)



features <- dataset[, !colnames(dataset) %in% c("quality")]
target <- dataset$quality
k <- 10
set.seed(123)
num_folds <- 10
fold_indices <- cut(seq_along(target), breaks = num_folds, labels = FALSE)
accuracy_scores <- numeric(num_folds)
sum<- 0
for (fold in 1:num_folds) {
  test_indices <- fold_indices == fold
  train_data <- features[!test_indices, ]
  train_target <- target[!test_indices]
  test_data <- features[test_indices, ]
  test_target <- target[test_indices]
  
  knn_model <- knn(train_data, test_data, train_target, k)
  accuracy_scores[fold] <- sum(knn_model == test_target) / length(test_target)
}
knn_model

mean_accuracy <- mean(accuracy_scores)
cat("Accuracy:", mean_accuracy)




