install.packages("mosaic")
library(mosaic)

# Section 1

# 1.

# a.
pinky <- c(1.5, 2.4, 1.9)
pinky

# b.
cities <-c("San Diego","Los Angeles","New York City")
cities

# c.
mymatrix <- cbind(pinky,cities)
class(mymatrix)

# 2.

# a.
NCbirths <- read.csv("births2022.csv")

# b.
head(NCbirths)

# 3.

# a. 
install.packages("maps")
find.package("maps")

# b.
library(maps)
map("state")

# 4.
weights <-NCbirths$weight
weights_in_pounds <- weights/16
weights_in_pounds[1:10]

# Section 2

# 1.

# a.
mean(NCbirths$Fage)
sd(NCbirths$Fage)

# b.
tab <- table(NCbirths$Habit) #Mom smoking variable: 'Habit'
prop.table(table(NCbirths$Habit))

# c.
percent_smoke <- tab[2]/(tab[1]+tab[2])
.17 - percent_smoke

# Section 3

# 1.
dotPlot(weights_in_pounds)

# 2.
hist(weights_in_pounds, breaks = 5)
hist(weights_in_pounds, breaks = 20)
hist(weights_in_pounds, breaks = 100)

# 3.
boxplot(NCbirths$Fage, NCbirths$Mage)

# 4.
histogram(~ weight | Habit, data = NCbirths, layout = c(1, 2))
hist(NCbirths$weight[NCbirths$Habit == "Smoker"])
hist(NCbirths$weight[NCbirths$Habit != "Smoker"])

# Section 4

# 1.
tab <- tally(~Habit | BirthDef, data = NCbirths, format = "proportion") 

# 2.
barplot(tab)

# Section 5

# 1.
plot(weight ~ Mage, data = NCbirths, col = "blue", cex = 1.5, pch = 1,  
     xlab = "Mother's age (years)", ylab = "Baby weight (oz.)",  
     main = "Baby weight vs. Mother's age at birth")
