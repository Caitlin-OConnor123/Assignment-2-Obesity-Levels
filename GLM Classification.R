install.packages("readxl")
install.packages("caTools")
library(readxl)
library(caTools)

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

Logistic_GLM = glm(formula = is_obese ~ ., data = training_set, family = binomial)
summary(Logistic_GLM)

Logistic_coefficient = exp(coef(Logistic_GLM))
print(Logistic_coefficient)

Logistic_prediction_testing = predict(Logistic_GLM, newdata = testing_set, type = "response")
Logistic_prediction_testing = ifelse(Logistic_prediction_testing > 0.5, 1, 0)

Logistic_confusion_matrix = table(actual = testing_set$is_obese, predicted = Logistic_prediction_testing)
print(Logistic_confusion_matrix)

Logistic_accuracy = sum(diag(Logistic_confusion_matrix))/sum(Logistic_confusion_matrix)
print(Logistic_accuracy)


