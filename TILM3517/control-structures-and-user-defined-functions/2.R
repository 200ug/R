# Required for the set.seed() function to work the intended way.
RNGkind(sample.kind = "Rounding")

# Define a function called "evens" that calculates and returns the number of 
# even numbers in a vector that it gets as an argument.
evens = function(vec) {
  total = 0
  for (num in vec) {
    if (num %% 2 == 0) {
      total = total + 1
    }
  }
  
  return(total)
}
