# Parameters: x_1, x_2, limit
fib = function(a, b, n) {
  for (i in 3:n) {
    c = a + b
    a = b
    b = c
    
    #print(c)
  }
  
  return(c)
}

print(fib(0, 1, 50))
