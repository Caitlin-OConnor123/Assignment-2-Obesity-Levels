install.packages("readxl")
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("neuralnet")
library(readxl)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)

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

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# LOGISTIC REGRESSION - GLM
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
Logistic_GLM = glm(formula = is_obese ~ ., data = training_set, family = binomial)
summary(Logistic_GLM)

# Odds Ratio and Confidence Interval
Logistic_odds_ratio = exp(coef(Logistic_GLM))
Logistic_confidence_interval = exp(confint(Logistic_GLM))
Logistic_odds_ratio_table = cbind(odds_ratio = Logistic_odds_ratio, Logistic_confidence_interval)
print(Logistic_odds_ratio_table)

# Variable Selection
Variable_selection = step(Logistic_GLM, direction = "both", trace = 0)
print(Variable_selection)

# Prediction and Accuracy of the model
Logistic_prediction_testing = predict(Logistic_GLM, newdata = testing_set, type = "response")
Logistic_prediction_testing = ifelse(Logistic_prediction_testing > 0.5, 1, 0)

Logistic_confusion_matrix = table(actual = testing_set$is_obese, predicted = Logistic_prediction_testing)
print(Logistic_confusion_matrix)

Logistic_accuracy = sum(diag(Logistic_confusion_matrix))/sum(Logistic_confusion_matrix)
print(Logistic_accuracy)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# DECISION TREE
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Make the tree small using complexity parameter (cp)#
Decision_Tree = rpart(is_obese ~ ., data = training_set, method = "class", control = rpart.control(cp = 0.01))
rpart.plot(Decision_Tree, extra = 106, main = "Decision Tree - Obesity Data, CP = 0.01")

# Prediction and Accuracy of the model
Decision_Tree_prediction = predict(Decision_Tree, newdata = testing_set, type = "class")

Decision_Tree_confusion_matrix = table(actual = testing_set$is_obese, predicted = Decision_Tree_prediction)

print(Decision_Tree_confusion_matrix)

Decision_Tree_accuracy = sum(diag(Decision_Tree_confusion_matrix))/sum(Decision_Tree_confusion_matrix)
print(Decision_Tree_accuracy)

# Variable Importance
print(Decision_Tree$variable.importance)
rpart.rules(Decision_Tree)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# RANDOM FOREST
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Optimise the random forest by tuning it
Random_forest_tuned = tuneRF(x = training_set[, -which(names(training_set) == "is_obese")], y = training_set$is_obese, ntreeTry = 500, stepFactor = 1.5, 
                             improve = 0.01, trace = TRUE, plot = TRUE, doBest = TRUE)
Random_forest = randomForest(is_obese ~., data = training_set, ntree = 500, mtry = 4, importance = TRUE)

print(Random_forest)
varImpPlot(Random_forest)

Random_Forest_prediction = predict(Random_forest_tuned, newdata = testing_set, type = "class")
Random_Forest_confusion_matrix = table(actual = testing_set$is_obese, predicted = Random_Forest_prediction)

print(Random_Forest_confusion_matrix)

Random_Forest_accuracy = sum(diag(Random_Forest_confusion_matrix))/sum(Random_Forest_confusion_matrix)
print(Random_Forest_accuracy)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# DEEP LEARNING - NEURAL NETWORK
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

Obesity_Levels$is_obese = as.numeric(as.factor(Obesity_Levels$is_obese))-1
Obesity_Levels$is_obese = ifelse(Obesity_Levels$is_obese == 1, 1, 0)

print(table(Obesity_Levels$is_obese))

NN_matrix = model.matrix(~ . - is_obese, data = Obesity_Levels)[,-1]
colnames(NN_matrix) = make.names(colnames(NN_matrix))

maxs = apply(NN_matrix, 2, max)
mins = apply(NN_matrix, 2, min)

NN_scaled = as.data.frame(scale(NN_matrix, center = mins, scale = maxs - mins))

NN = as.data.frame(cbind(NN_scaled, is_obese = Obesity_Levels$is_obese))

NN_Split = sample.split(NN$is_obese, SplitRatio = 0.70)
Training_NN = subset(NN, NN_Split == TRUE)
Testing_NN = subset(NN, NN_Split == FALSE)

NN_Model = neuralnet(is_obese ~., data = Training_NN, hidden = c(5,3), linear.output = FALSE, stepmax = 1e5)
plot(NN_Model, rep = "best")

# To make the plot clearer:
install.packages("ggplot2")
library(ggplot2)

install.packages("NeuralNetTools")
library(NeuralNetTools)


plotnet(NN_Model, alpha = 0.6, circle_cex = 3, cex_val = 0.6, pos_col = "red", neg_col = "blue")

olden(NN_Model) + coord_flip() + ggtitle("Variable Impotance in Predicting Obesity")

temptest = subset(Testing_NN, select = -is_obese)
results = compute(NN_Model, temptest)

NN_prediction = ifelse(results$net.result > 0.5,1,0)

NN_confusion_matrix = table(Testing_NN$is_obese, NN_prediction)

NN_accuracy = sum(diag(NN_confusion_matrix))/sum(NN_confusion_matrix)

print(NN_accuracy)





