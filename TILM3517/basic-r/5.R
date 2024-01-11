# Create a data frame called "abc" of variables  a, b, and c which have been 
# already created for you. After creating the data frame, calculate the product
# of the 3rd and 6th row. Then calculate the value of the 1st element in the 1st
# row dividided by the 3rd element in the 10th row. Store these calculations to 
# variables  product and  division respectively.

abc=data.frame(a, b, c)
product=abc[3,]*abc[6,]
division=abc[1,1]/abc[10,3]
