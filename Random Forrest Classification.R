install.packages("randomForest")
library(randomForest)

Random_Forest = randomForest(is_obese ~., data=training_set, ntree = 500, importance = TRUE)
plot(Random_Forest, main = ("Error Rate vs No. Trees"))

Random_Forest_prediction = predict(Random_Forest, newdata = testing_set)

Random_Forest_confusion_matrix = table(actual = testing_set$is_obese, predicted = Random_Forest_prediction)
print(Random_Forest_confusion_matrix)

Random_Forest_accuracy = sum(diag(Random_Forest_confusion_matrix))/sum(Random_Forest_confusion_matrix)
print(Random_Forest_accuracy)

