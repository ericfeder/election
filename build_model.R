# # Install necessary packages
# install.packages("RandomForest")
# install.packages("data.table")

# Load necessary packages
library(randomForest)
library(data.table)

# Read training data
counties <- read.csv("train_potus_by_county.csv")

# Cast to data table and rename columns
counties <- data.table(counties)
setnames(counties, c("total.population", "median.age", "perc.bachelors", "unemployment.rate", "per.capita.income", "total.households", "average.household.size", "perc.owner.occupied", "perc.renter.occupied", "perc.vacant.housing", "median.home.value", "population.growth", "household.growth", "per.capita.income.growth", "winner"))

# Exploratory analysis
summary(counties) # Overview of columns
plot(counties) # Look for correlations, distributions, and outliers
aggregate(. ~ winner, data=counties, FUN=median) # See which variables differ by who won the county

# Randomly assign counties to k differently folds and prepare data for cross-validation
# Note: With a larger dataset, duplicating the data would be impossible / unreasonable
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
  df.train <- df.train[sample(nrow(counties))]
  df.train$fold <- rep(1:kFold)
  train.sets <- lapply(1:kFold, FUN=extractTrainingSet, df=df.train)
  eval.sets <- lapply(1:kFold, FUN=extractEvaluationSet, df=df.train)
  return(list(train=train.sets, eval=eval.sets))
}
kFold <- 10
cross.validation.splits <- prepareForCrossValidation(counties, kFold)

# Extract actual values from evaluation sets
# Note: I am formulating the problem as a binary classification problem where Obama winning = TRUE and Romney winning = FALSE
eval.actual <- lapply(cross.validation.splits$eval, function(x) x$winner == "Barack Obama")

# Function to evaluate models using k-Fold Cross Validation
evaluateModel <- function(model.function, cv.splits=cross.validation.splits, actual=eval.actual){
  models <- lapply(cv.splits$train, model.function)
  fitted.values <- mapply(FUN=predict, models, cv.splits$eval, type="response")
  accuracies <- mapply(FUN=function(fit, actual) mean((fit > 0.5) == actual), fitted.values, actual)
  mean.accuracy <- mean(accuracies)
  return(mean.accuracy)
}
