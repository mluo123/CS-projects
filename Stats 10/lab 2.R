# Exercise 1 ####

# a.
grads <- read.csv("C:/Users/Latitude 7280/OneDrive/Desktop/Stats 10/recent-grads.csv")
nrow(grads)
ncol(grads)

# b,
mean(grads$Unemployment_rate < .041)

# c.
ed_index <- grads$Major_category == "Education"
mean(grads$Median[ed_index])
sd(grads$Median[ed_index])

# d.
mean(grads$Median[!ed_index])
sd(grads$Median[!ed_index])

# e.
boxplot(grads$Median, main = "Median Earnings for all Majors Boxplot ($)")


# Exercise 2 ####

life <- read.csv("C:/Users/Latitude 7280/OneDrive/Desktop/Stats 10/countries_life.csv")

# a.
plot(life$Income, life$Life, ylab = "Life Expenctancy", xlab = "Per Capita Income", main = "Life Expectancy vs. Per Capita Income")

# b.
boxplot(life$Income, main = "Boxplot of Per Capita Income")
hist(life$Income, xlab = "Per Capita Income", main = "Histogram of Per Capita Income")

# c.
median(life$Income)

# d.
less700 <- life$Income < 700
less700DF <- life[less700,]
more700DF <- life[!less700,]

# e.
plot(more700DF$Income, more700DF$Life, ylab = "Life Expectancy", xlab = "Per Capita Income", main = "Life Expectancy vs. Per Capita Income (Income >= $700)")
cor(more700DF$Life, more700DF$Income)


# Exercise 3 ####

maas <- read.csv("C:/Users/Latitude 7280/OneDrive/Desktop/Stats 10/soil.csv")

# a.
summary(maas)

# b.
hist(maas$lead, main = "Histogram of Lead Concentrations", xlab = "Lead PPM")
hist(maas$zinc, main = "Histogram of Zinc Concentrations", xlab = "Zinc PPM")

# c.
hist(log(maas$lead), main = "Histogram of Log Lead Concentrations", xlab = "Log Lead PPM")
hist(log(maas$zinc), main = "Histogram of Log Zinc Concentrations", xlab = "Log Zinc PPM")

# d.
plot(log(maas$zinc), log(maas$lead), main = "Log Zinc vs. Log Lead PPM", xlab = "Log Zinc PPM", ylab = "Log Lead PPM")
cor(log(maas$zinc), log(maas$lead))

# e.
maas$danger_level <- ifelse(maas$lead < 120, "forestgreen", "blue")
maas$danger_level <- ifelse(maas$lead > 400, "red", maas$danger_level)
plot(maas$x, maas$y, col = maas$danger_level, pch = 19, xlab = "", ylab = "", cex = 2 * (maas$lead / max(maas$lead)), main = "Lead PPM Measures along Maas River")
legend("topleft", legend = c("Lead Free", "Lead Safe", "Lead Hazard"), col = c("forestgreen", "blue", "red"), pch = 19, cex = 1)


# Exercise 4 ####

LA <- read.csv("C:/Users/Latitude 7280/OneDrive/Desktop/Stats 10/la_data.csv")
library(maps)

# a.
plot(LA$Longitude, LA$Latitude, xlim = c(-118.8, -118), ylim = c(33.7, 34.4), xlab = "Longtitude", ylab = "Latitude", main = "Centers of LA Neighborhoods", pch = 19, cex = .5)
map("county", "california", add = TRUE)

# b.
income_subset <- LA$Income[LA$Schools > 0]
school_subset <- LA$Schools[LA$Schools > 0]
plot(income_subset, school_subset, main = "Income vs. # of Schools\nin LA Neighborhoods", xlab = "Income", ylab = "# Schools", pch = 19, cex = .5)
