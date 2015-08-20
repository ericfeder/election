# Install and load necessary packages
install.packages("data.table")
library(data.table)

# Read training data
train.data <- read.csv("train_potus_by_county.csv")

# Cast training data to data.table and rename columns
train.data <- data.table(train.data)
setnames(train.data, c("total.population", "median.age", "perc.bachelors", "unemployment.rate", "per.capita.income", "total.households", "average.household.size", "perc.owner.occupied", "perc.renter.occupied", "perc.vacant.housing", "median.home.value", "population.growth", "household.growth", "per.capita.income.growth", "winner"))

# Exploratory analysis
cor(train.data[, 1:14, with=F])

# Fit logistic regression
glm.model <- glm(winner == "Barack Obama" ~ ., data=train.data)
plot(glm.model)
