# Required for the set.seed() function to work the intended way.
RNGkind(sample.kind = "Rounding")

# Part 1:
# Variables x and y have been defined. Write an if-else statement that stores 
# the smaller of the two to a new variable z.
if (x<y) {
  z = x
} else {
  z = y
}

# Part 2:
# Use for loop to calculate the sum of the positive numbers in the vector x.
set.seed(1337)
x <- rnorm(1000, mean = 1, sd = 2)
sum = 0
for (i in x) {
  if (i>0) {
    sum = sum + i
  }
}

# Part 3:
# A vector of length 10 called "vec" has been defined. Write a for loop that
# changes every negative element of vec to a zero.
vec[vec < 0] = 0
