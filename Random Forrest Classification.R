install.packages("readxl")
install.packages("randomForest")
library(readxl)
library(randomForest)


Obesity_Levels <- read_excel("~/PROJECT/Obesity Levels.xlsx")
View(Obesity_Levels)

Obesity_Levels$is_obese = ifelse(grepl("Obesity", Obesity_Levels$NObeyesdad),1,0)
Obesity_Levels$is_obese = as.factor(Obesity_Levels$is_obese)

Obesity_Levels$NObeyesdad = NULL
Obesity_Levels$Weight = NULL
Obesity_Levels$Height = NULL
#need to NULL height and weight so that the code can use the other factors as predictors of obesity, rather than just calculating BMI

set.seed(123)

split_data = sample.split(Obesity_Levels$is_obese, SplitRatio = 0.70)
training_set = subset(Obesity_Levels, split_data = TRUE)  
testing_set = subset(Obesity_Levels, split_data = FALSE) 

print(training_set)  
print(testing_set)  

Random_Forest = randomForest(is_obese ~., data=training_set, ntree = 500, importance = TRUE)
plot(Random_Forest, main = ("Error Rate vs No. Trees"))

Random_Forest_prediction = predict(Random_Forest, newdata = testing_set)

Random_Forest_confusion_matrix = table(actual = testing_set$is_obese, predicted = Random_Forest_prediction)
print(Random_Forest_confusion_matrix)

Random_Forest_accuracy = sum(diag(Random_Forest_confusion_matrix))/sum(Random_Forest_confusion_matrix)
print(Random_Forest_accuracy)


