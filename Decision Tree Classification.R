install.packages("readxl")
install.packages("rpart")
install.packages("rpart.plot")
library(readxl)
library(rpart)
library(rpart.plot)

Obesity_Levels = read_excel("~/PROJECT/Obesity Levels.xlsx")
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

Decision_Tree = rpart(is_obese ~ ., data = training_set, method = "class")
rpart.plot(Decision_Tree, extra = 106, main = "Decision Tree - Obesity Data")

Decision_Tree_prediction = predict(Decision_Tree, newdata = testing_set, type = "class")

Decision_Tree_confusion_matrix = table(actual = testing_set$is_obese, predicted = Decision_Tree_prediction)

print(Decision_Tree_confusion_matrix)

Decision_Tree_accuracy = sum(diag(Decision_Tree_confusion_matrix))/sum(Decision_Tree_confusion_matrix)
print(Decision_Tree_accuracy)


