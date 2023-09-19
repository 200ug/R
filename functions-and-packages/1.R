# Required for the set.seed() function to work the intended way.
RNGkind(sample.kind = "Rounding")

# Part 1:
# How many observations are there in total that are between -0.01 and 0.01?
length(x[x > -0.01 & x < 0.01])
x=read.csv("functions-and-packages/xyz.csv")

# Part 2:
# Find the product of the elements in each of the columns in the matrix m1.
set.seed(10)
m1 <- matrix(sample(1:10, 25, replace = T), nrow = 5, ncol = 5)
apply(m1, 2, prod)

# Part 3:
# Use the lapply() function to apply the summary() function to the cars 
# dataset's columns.
lapply(cars, summary)

# Part 4:
# Use the grep() function to find the indices of the letter "x" from the 
# randomletters vector. Which of the following are the indices?
set.seed(10)
randomletters <- letters[sample(1:26, 100, replace = T)]
grep("x", randomletters)

# What is the number of letter a's in the randomletters vector?
length(grep("a", randomletters))

# Part 5:
# Use the grep() function to find how many codes contain the character "R" in 
# the  my_data data frame.
my_data=read.table("functions-and-packages/my_data.txt")
grep("R", my_data$codes)

# Use the grep() function to find the last code that contains the string "XY" 
# in the  my_data data frame.
results=grep("XY", my_data$codes, value=TRUE)
results[length(results)]

# Use the substr() function to find how many codes end with the character "S" 
# in the  my_data data frame.
last_chars=substr(my_data$codes, nchar(my_data$codes), nchar(my_data$codes))
sum(last_chars=="S")

# Part 6:
# Use the solve() function to solve the following system of equations:
# { x+2y=1
# { 2x−y=2
# Store the solution to a variable called "solution".
a=matrix(c(1,2,2,-1), nrow=2)
b=matrix(c(1,2), nrow=2)
solution=solve(a,b)

# Part 7:
# Use the polyroot() function to solve the equation 
# x^6+9x^5+12x^4-63x^3-60x^2+225x-125=0 and store the solution to an object 
# called "poly". Also use the uniroot() function to find the intersection of e^x
# and sin x when x in [-4, -3] and store the solution to an object called uni.
poly=polyroot(c(-125,225,-60,-63,12,9,1))
uni=uniroot(function(x) exp(x)-sin(x), interval=c(-4,-3))
