# # Install necessary packages
# install.packages("data.table")
# install.packages("randomForest")
# install.packages("gbm")
# install.packages("e1071")

# Load necessary packages
library(data.table)
library(randomForest)
library(gbm)
library(e1071)

# Read training data and cast to data table
counties <- read.csv("train_potus_by_county.csv")
counties <- data.table(counties)

# Exploratory analysis
summary(counties) # Overview of columns
plot(counties) # Look for correlations, distributions, and outliers
aggregate(. ~ Winner, data=counties, FUN=median) # See which variables differ by who won the county

# Function to prepare data for cross-validation
# (Note: With a larger dataset, duplicating the data would be impossible / unreasonable)
kFold <- 10
extractTrainingSet <- function(df, fold.i){
  fold.column <- which(colnames(df) == "fold")
  train.set <- df[fold == fold.i, -fold.column, with=F]
  return(train.set)
}
extractEvaluationSet <- function(df, fold.i){
  fold.column <- which(colnames(df) == "fold")
  eval.set <- df[fold != fold.i, -fold.column, with=F]
  return(eval.set)  
}
prepareForCrossValidation <- function(df.train, kFold){
  df.train <- df.train[sample(nrow(df.train))]
  df.train$fold <- rep(1:kFold)
  train.sets <- lapply(1:kFold, FUN=extractTrainingSet, df=df.train)
  eval.sets <- lapply(1:kFold, FUN=extractEvaluationSet, df=df.train)
  return(list(train=train.sets, eval=eval.sets))
}

# Function to evaluate models using k-Fold Cross Validation
evaluateModel <- function(model.function, categorical, predict.function=predict, cv.splits=cross.validation.splits){
  actual <- lapply(cv.splits$eval, function(x) x$Winner)
  models <- lapply(cv.splits$train, model.function)
  fitted.values <- mapply(FUN=predict.function, models, cv.splits$eval)
  if (categorical){
    accuracies <- mapply(FUN=function(fit, actual) mean(fit == actual), fitted.values, actual)
    confusion.matrices <- mapply(FUN=table, fitted.values, actual, SIMPLIFY=F)
  } else{
    accuracies <- mapply(FUN=function(fit, actual) mean((fit > 0.5) == (actual == "Barack Obama")), fitted.values, actual)
    confusion.matrices <- mapply(FUN=function(fit, actual) table(fit > 0.5, actual == "Barack Obama"), fitted.values, actual, SIMPLIFY=F)
  }
  mean.accuracy <- mean(accuracies)
  mean.confusion.matrix <- prop.table(Reduce("+", confusion.matrices))
  
  performance <- list(accuracy=mean.accuracy, confusion.matrix=mean.confusion.matrix)
  class(performance) <- "model.performance"
  return(performance)
}

# Function for printing objects of class "model.performance"
print.model.performance <- function(perf){
  cat("Cross-Validated Accuracy:", perf$accuracy, "\nCross-Validated Confusion Matrix:\n")
  print(perf$confusion.matrix)
}

# Prepare objects for model evaluation
kFold <- 10
cross.validation.splits <- prepareForCrossValidation(counties, kFold)

# Evaluate logistic regression (defines Obama winning = TRUE and Romney winning = FALSE)
trainLogistic <- function(df) glm(Winner == "Barack Obama" ~ ., family="binomial", data=df)
logistic.performace <- evaluateModel(trainLogistic, categorical=F)

# Evaluate Supper Vector Machine
trainSVM <- function(df) svm(Winner ~ ., data=df, kernel="linear")
svm.performance <- evaluateModel(trainSVM, categorical=T)

# Evaluate RandomForest
trainRandomForest <- function(df) randomForest(Winner ~ ., data=df, n.trees=1000)
random.forest.performance <- evaluateModel(trainRandomForest, categorical=T)

# Evaluate Generalized Boosting Model
trainBoosting <- function(df) gbm(Winner ~ ., data=df, distribution="multinomial", n.trees=1000, interaction.depth=3)
fitBoosting <- function(model, df) predict.gbm(model, df, n.trees=800, type="response")[, 1, 1]
boosting.performance <- evaluateModel(trainBoosting, categorical=F, predict.function=fitBoosting)

# Fit most predictive model (Random Forest) on full dataset and save
random.forest.model <- trainRandomForest(counties)
save(random.forest.model, file="model.RData")

# Write expected performance to file
capture.output(print(random.forest.performance), file="performance.txt")