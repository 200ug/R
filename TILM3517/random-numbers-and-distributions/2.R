# Necessary since the RNG has been changed after the exercises have been crafted.
RNGkind(sample.kind = "Rounding")

# Part 1:
# X~N(0,1), X>=1.96, solve X
round(pnorm(1.96, lower.tail = F), digits = 2)

# X~N(0,1), X<=0.59, solve X
round(pnorm(0.59), digits = 2)

# X~Po(3), X<=2, solve X
round(ppois(2, lambda = 3), digits = 2)

# X~U(-1,1), X>=0.24, solve X
round(punif(0.24, min = -1, max = 1, lower.tail = F), digits = 2)

# X~Bin(48,0.35), X<=11, solve X
round(pbinom(11, size = 48, prob = 0.35), digits = 2)

# Part 2:
# X~N(0,1), X<=q=0.74, solve q
round(qnorm(0.74), digits = 2)

# X~N(4,1), X>=q=0.78, solve q
round(qnorm(0.78, mean = 4, sd = 1, lower.tail = F), digits = 2)

# X~Exp(2.5), X<=q=0.67, solve q
round(qexp(0.67, rate = 2.5), digits = 2)

# X~U(-2,0), X>=q=0.24, solve q
round(qunif(0.24, min = -2, max = 0, lower.tail = F), digits = 2)

# X~Gamma(1,2) (shape, rate), X<=q=0.5, solve q
round(qgamma(0.5, shape = 1, rate = 2), digits = 2)

# Part 3:
# Simulate 100 observations of height, which follows the normal distribution 
# with parameter values μ=170 and σ=8 and the same amount of weight observations
# with parameters μ=70 and σ=5. Calculate the average of BMI.
heights = rnorm(100, mean = 1.7, sd = 0.08)
weights = rnorm(100, mean = 70, sd = 5)
mean(weights/(heights^2))

# Part 4:
# Calculate nCk=(7,2) using the choose() function and store it to an object called 
# bincoef. Also, find all the possible combinations of three random integers 
# between 1 and 10 using the combn() function and store the answer to an object 
# called combs.
bincoef = choose(7, 2)
combs = combn(1:10, 3)
