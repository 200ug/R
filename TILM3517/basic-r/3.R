# You are given a vector called x. 
# Write a code that takes every 3rd element of x and saves them to a new vector 
# called y. Then raise the value of every element in y by 3.

set.seed(1234)
x<-runif(30,0,10)

# Part 1
y=x[seq(from=1, to=length(x), by=3)]
y=y+3

# Write a code that changes the value of the 4th element of vector y to 6, 
# then changes the value of last element to the same as 1st element and finally
# calculates the sum of all the elements.

# Part 2
y[4]=6
y[length(y)]=y[1]
sum(y)
