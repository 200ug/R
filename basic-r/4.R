# You are given a matrix called a. First find out its dimensions. Then change 
# all the negative values to 0. Finally calculate the sum of the smallest and
# largest values on the first row of a.

dim(a)
a[a<0]=0
sum(max(a[1,]), min(a[1,]))
