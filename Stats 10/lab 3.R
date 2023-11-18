# Exercise 1 ####

grads <- read.csv("C:/Users/Latitude 7280/OneDrive/Desktop/Stats 10/recent-grads.csv")
grads <- na.omit(grads)

# a.
linear_model <- lm(Full_time ~ Men, data = grads)
summary(linear_model)

# b.
plot(grads$Men, grads$Full_time, main = "# of Full-Time Employees vs. # of Men", xlab = "# of Men", ylab = "# of Full-Time Employees")
abline(linear_model, col = "red")

# c.
plot(grads$Men, linear_model$residuals, main = "Residuals From Regressing Full-Time on Men", ylab = "Residuals", xlab = "# of Men")
abline(h = 0, col = "red")

# e.
predict(linear_model, newdata = data.frame(Men = 55000))

# f.
b <- linear_model$coefficients[2]
b * 500

# g.
linear_summary <- summary(linear_model)
linear_summary$r.squared

# Exercise 2 ####

ice <- read.csv("C:/Users/Latitude 7280/OneDrive/Desktop/Stats 10/sea_ice.csv", header = TRUE)
ice$Date <- as.Date(ice$Date, "%m/%d/%Y") 

# a.
ice_model <- lm(Extent ~ Date, data = ice)
summary(ice_model)


# b.
plot(ice$Date, ice$Extent, main = "Sea Ice Extent over Time", xlab = "Time", ylab = "Extent", type = "l")
abline(ice_model, col = "red")

# c.
plot(ice$Date, ice_model$residuals, main = "Residuals from Regressing Sea Ice on Time", ylab = "Residuals", xlab = "Time", type = "l")
abline(h = 0, col = "red")

# Exercise 3 ####

# a.
rolls <- expand.grid(dice1 = 1:6, dice2 = 1:6)
roll_sums <- rowSums(rolls)
prob_win <- mean(roll_sums == 4 | roll_sums == 11)
prob_lose <- mean(roll_sums %in% c(2,5,12))

# b.
set.seed(123)
rolls <- replicate(10000, sample(1:6, 2, replace = TRUE))
roll_sums <- colSums(rolls)
prob_win <- mean(roll_sums == 4 | roll_sums == 11)
prob_lose <- mean(roll_sums %in% c(2,5,12))
barplot(table(roll_sums), main = "Outcome of 10000 Craps Games")

# c.
prob_win <- mean(roll_sums == 4 | roll_sums == 11)
prob_lose <- mean(roll_sums %in% c(2,5,12))

# e.
rolls <- expand.grid(dice1 = 1:6, dice2 = 1:6)
roll_sums <- rowSums(rolls)
win <- roll_sums == 4 | roll_sums == 11
lose <- roll_sums %in% c(2,5,12)
win_lose <- win & lose
mean(win_lose)
mean(win) * mean(lose)