# Necessary since the RNG has been changed after the exercises have been crafted.
RNGkind(sample.kind = "Rounding")

# Plotting example with squares instead of circles
par(pch=0)
plot(cars$speed, cars$dist, main = "Distance ~ Speed", xlab = "Speed", ylab = "Distance", sub = "Scatterplot")

# Draw a histogram of a vector called "v" that has been defined for you and the
# curve of log x, when x∈[0,10], to the same graph using the hist() and curve() 
# functions. First follow these instructions to set the plotting layout and the 
# graphical parameters with the par() function:
# - Set the plotting layout so that the plots are next to each other.
# - Set the text size to 150% larger than default.
# - Set the plotting color to red. 
# - Set the line type to a dashed line.
# Also don't only draw the plots. Store the return value of the hist() and curve()
# functions with the right arguments to variables h and c respectively.
par(mfrow = c(1, 2), cex = 1.5, col = "red", lty = 2)
h = hist(v)
c = curve(log(x), from = 0, to = 10)
