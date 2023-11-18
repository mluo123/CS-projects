# Load in required libraries
library(glmnet)
library(data.table)

# Read in files
X.train <- read.table('/Users/mluo/Desktop/cs cm124/Challenge1/data/ancestry_train.data', sep = ' ', header = FALSE);
y.train <- read.table('/Users/mluo/Desktop/cs cm124/Challenge1/data/ancestry_train.solution', sep = ' ', header = FALSE);
X.test <- read.table('/Users/mluo/Desktop/cs cm124/Challenge1/data/ancestry_test.data', sep = ' ', header = FALSE);

# Perform ridge regression on ancestry compositions separately
ridge_1 <- cv.glmnet(x = as.matrix(X.train), y = as.matrix(y.train$V1), alpha = 0);
ridge_2 <- cv.glmnet(x = as.matrix(X.train), y = as.matrix(y.train$V2), alpha = 0);
ridge_3 <- cv.glmnet(x = as.matrix(X.train), y = as.matrix(y.train$V3), alpha = 0);

# Get optimal lambda for each model
lambda1 <- ridge_1$lambda.min;
lambda2 <- ridge_2$lambda.min;
lambda3 <- ridge_3$lambda.min;

# Predict new ancestry compositions and combine them into one matrix
y.test1 <- predict(ridge_1, newx = as.matrix(X.test), s = lambda1);
y.test2 <- predict(ridge_2, newx = as.matrix(X.test), s = lambda2);
y.test3 <- predict(ridge_3, newx = as.matrix(X.test), s = lambda3);

y.test <- cbind(y.test1, y.test2, y.test3);

# Write predictions to CSV file
fwrite(as.data.frame(y.test), file = "/Users/mluo/Desktop/cs cm124/Challenge1/predictions.csv", sep = " ", quote = FALSE, row.names = FALSE, col.names = FALSE);

