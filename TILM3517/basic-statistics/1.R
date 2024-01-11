# Necessary since the RNG has been changed after the exercises have been crafted.
RNGkind(sample.kind = "Rounding")

# Part 1:
# Covariance of speed and dist
round(cov(cars$speed, cars$dist), digits = 2)

# 3rd quartile of dist
summary(cars$dist)[5]

# Correlation of speed and dist
round(cor(cars$speed, cars$dist), digits = 2)

# Maximum of speed
max(cars$speed)

# 1st quartile of speed
summary(cars$speed)[2]

# Median of dist
median(cars$dist)

# Minimum of dist
min(cars$dist)

# Mean of speed
mean(cars$speed)

# Part 2:
# A vector called x has been created for you. Calculate the mean, variance, 
# median and range of x and store them to variables a, b, c and d respectively.
a = mean(x)
b = var(x)
c = median(x)
d = range(x)

# Part 3:
# Run the following commands
grades <- c("L","E","M","C","B","A","I")
probs <- c(0.004,0.054,0.242,0.399,0.242,0.054,0.004)
set.seed(111112)
exam <- grades[sample(1:7,100,replace = T,prob=probs)]
# and use the table() function to match the grades and their frequencies.
table(exam)
