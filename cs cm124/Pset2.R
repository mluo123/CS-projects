# Question 3 ####

# b.

# Read in data
genos <- read.table('/Users/mluo/Desktop/cs cm124/PS2/data/PS2_Q3_data/gwas.geno', sep = ' ', header = FALSE);
phenos <- read.table('/Users/mluo/Desktop/cs cm124/PS2/data/PS2_Q3_data/gwas.pheno', sep = ' ', header = FALSE);

# Perform linear regression

alpha_bonferroni <- 0.05 / ncol(genos);
pvals <- data.frame(matrix(nrow = ncol(genos), ncol = ncol(phenos)));

for (pheno in 1:ncol(phenos)) {
    for (geno in 1:ncol(genos)) {
        lm <- lm(phenos[, pheno] ~ genos[, geno]);
        pvals[geno, pheno] <- summary(lm)$coefficients[2, 'Pr(>|t|)'];
    }
}

sig.pvals <- pvals <= alpha_bonferroni;
significant_indices <- which(sig.pvals, arr.ind = TRUE);

apply(significant_indices, 1, function(row) {
    row_index <- row["row"]
    col_index <- row["col"]
    print(paste("SNP:", row_index, "Phenotype:", col_index))
})


# c.
qq_plots <- list()
for (i in 1:ncol(pvals)) {
    qqnorm(pvals[, i], main = paste("Q-Q Plot for Phenotype", i))
    qqline(pvals[, i])
    qq_plots[[i]] <- recordPlot() 
    dev.off()
}
print(qq_plots)

# d.
boxplot(phenos$V2 ~ genos$V258, xlab = 'Genotype', ylab = 'Phenotype', main = 'SNP 258, Phenotype 2')
boxplot(phenos$V3 ~ genos$V119, xlab = 'Genotype', ylab = 'Phenotype', main = 'SNP 119, Phenotype 3')
boxplot(phenos$V4 ~ genos$V43, xlab = 'Genotype', ylab = 'Phenotype', main = 'SNP 43, Phenotype 4')

# Problem 4 ####

library(glmnet)

test.genos <- read.table('/Users/mluo/Desktop/cs cm124/PS2/data/PS2_Q4_data/ridge.test.geno', sep = ' ', header = FALSE);
test.phenos <- read.table('/Users/mluo/Desktop/cs cm124/PS2/data/PS2_Q4_data/ridge.test.pheno', sep = ' ', header = FALSE);
training.genos <- read.table('/Users/mluo/Desktop/cs cm124/PS2/data/PS2_Q4_data/ridge.training.geno', sep = ' ', header = FALSE);
training.phenos <- read.table('/Users/mluo/Desktop/cs cm124/PS2/data/PS2_Q4_data/ridge.training.pheno', sep = ' ', header = FALSE);

lambda_values <- c(0.001, 2, 5, 8);

ridge_model <- glmnet(x = as.matrix(training.genos), y = as.matrix(training.phenos), alpha = 0, lambda = lambda_values);

predictions_test <- predict(ridge_model, newx = as.matrix(test.genos), s = lambda_values);
predictions_train <- predict(ridge_model, newx = as.matrix(training.genos), s = lambda_values);

mse_train_values <- numeric(length(lambda_values));
mse_test_values <- numeric(length(lambda_values));

for (i in seq_along(lambda_values)) {
    mse_train_values[i] <- mean((predictions_train[,i] - training.phenos[,1])^2);
    mse_test_values[i] <- mean((predictions_test[,i] - test.phenos[,1])^2);
}

plot(lambda_values, mse_train_values, type = "b", col = "blue", pch = 16, xlab = "Lambda", ylab = "Mean Squared Error", main = "Ridge Regression Training Performance");
plot(lambda_values, mse_test_values, type = "b", col = "red", pch = 16, xlab = "Lambda", ylab = "Mean Squared Error", main = "Ridge Regression Testing Performance");

