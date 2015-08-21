# Load model workspace
load("model.RData")

# Read in test counties data
counties.test <- read.csv("test_potus_by_county.csv")

# Make predictions for test data
test.predictions <- predict(random.forest.model, counties.test)

# Write to file
writeLines(as.character(test.predictions), "predictions.csv")
