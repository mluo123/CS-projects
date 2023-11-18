# Exercise 1 ####

# a.

n <- 100
p <- .34

# b.

mean <- n*p
variance <- mean*(1-p)
SD <- sqrt(variance)

# c.

dbinom(x = 27, size = n, prob = p)

# d.

pbinom(34, size = n, prob = p) - pbinom(15, size = n, prob = p)

# e.

n * p >= 10
n * (1 - p) >= 10
mn <- n * p
std.d <- sqrt(n * p * (1 - p))
pnorm(34, mean = mn, sd = std.d) - pnorm(15, mean = mn, sd = std.d)

# Exercise 2 ####

grads <- read.csv("C:/Users/Latitude 7280/OneDrive/Desktop/Stats 10/new_recent_grads.csv")

# a.

nrow(grads)

# b.

mean(grads$Median)
mean(grads$Major_category == "Engineering") 

# c.

set.seed(1256)
n <- nrow(grads)
sample_dex <- sample(1:n, size = 100)
grads_sample <- grads[sample_dex,]
head(grads_sample)

# d.

xbar <- mean(grads_sample$Median)
phat <- mean(grads_sample$Major_category == "Engineering") 

# f.

se <- sqrt(phat * (1 - phat) / 100)
z1 <- qnorm(p = .95)
z2 <- qnorm(p = .975)
z3 <- qnorm(p = .995)
phat + c(-1, 1) * z1 * se
phat + c(-1, 1) * z2 * se
phat + c(-1, 1) * z3 * se

# Exercise 3 ####

# a.

n <- 50  
N <- nrow(grads)  
M <- 1000  
phats <- numeric(M) 
set.seed(123) 
for(i in seq_len(M)){ 
  index <- sample(N, size = n) 
  sample_i <- grads[index, ] 
  phats[i] <- mean(sample_i$ Major_category == "Engineering")} 

hist(phats, main = "Sampling Distribution of Sample Proportions", prob = TRUE)
curve(dnorm(x, mean(phats), sd(phats)), add = TRUE)

# b.

mean(phats)
sd(phats)

# d.

true_prop <- mean(grads$Major_category == "Engineering")
true_se <- sqrt(true_prop*(1-true_prop) / 50)
true_prop - mean(phats)
true_se - sd(phats)

# Exercise 4 ####

# a.

n <- 50
N <- nrow(grads)
M <- 1000

set.seed(1234)

sample_means <- rep(0, M)

for (i in 1:M) {
  sample_index <- sample(1:N, size = n, replace = FALSE)
  grads_sample <- grads[sample_index,]
  sample_means[i] <- mean(grads_sample$Median)
}

# b.

hist(sample_means, main = "Sampling Distribution of Sample Mean Medians", prob = TRUE)
curve(dnorm(x, mean(sample_means), sd(sample_means)), add = TRUE)