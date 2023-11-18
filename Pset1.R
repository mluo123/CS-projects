# Problem 1f ####

pval <- pchisq(c(-2*log(.03125)), df = 1);
pval <- 1 - pval;
pval;

# Problem 3a ####

# Read in files
genos <- read.table('/Users/mluo/Downloads/PS1/data/ps1.genos', sep = ' ', header = FALSE);
phenos <- read.table('/Users/mluo/Downloads/PS1/data/ps1.phenos', header = FALSE);

# Calculate T1
T1 <- 250 * cor(phenos, genos$V1, method = 'pearson')^2;

# Initialize vector for permutation results
T.permutated <- data.frame(matrix(nrow = 100000, ncol = 1));

# Perform permutations 
T.permutated[, 1] <- replicate(100000, {
    phenos.random <- data.frame(V1 = sample(phenos$V1));
    T.val <- 250 * cor(phenos.random, genos$V1, method = 'pearson')^2;
});


# Histogram
hist(T.permutated[,1], main = 'Histogram of Permutation Test', xlab = 'T statistic');
abline(v = T1, col = 'red');

# Problem 3b ####

p.val <- sum(T.permutated[, 1] >= T1[,1]) / 100000;
p.val;

# Problem 3c ####

curve(dchisq(x, df = 1), 
      from = 0, to = 10,
      main = "Chi Squared Distribution with 1 Degree of Freedom",
      ylab = "Density");

p_val <- pchisq(c(T1), df = 1);
p_val <- 1 - p_val;
p_val;