install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

Decision_Tree = rpart(is_obese ~ ., data = training_set, method = "class")
rpart.plot(Decision_Tree, extra = 106, main = "Decision Tree - Obesity Data")

Decision_Tree_prediction = predict(Decision_Tree, newdata = testing_set, type = "class")

Decision_Tree_confusion_matrix = table(actual = testing_set$is_obese, predicted = Decision_Tree_prediction)

print(Decision_Tree_confusion_matrix)

Decision_Tree_accuracy = sum(diag(Decision_Tree_confusion_matrix))/sum(Decision_Tree_confusion_matrix)
print(Decision_Tree_accuracy)
