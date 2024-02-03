# t. 1)

x <- c(36, 43, 38, 39, 37, 38, 44)
n <- length(x)

# otoskeskiarvo
x_mean <- mean(x)
print(paste("keskiarvo: ", x_mean))

# otoskeskihajonta
s <- sd(x)
print(paste("otoskeskihajonta: ", s))

# määr. alfa 90 % luottamusvälille
alfa <- 0.1

# lasketaan t-jakauman kvantiili vapausasteella n-1
t <- qt(1-(alfa/2), n-1)
print(paste("t-jakauman kvantiili: ", t))

# lasketaan luottamusvälin rajat ja tulostetaan ne
theta_l <- x_mean-t*s/sqrt(n)
theta_u <- x_mean+t*s/sqrt(n)
c(theta_l, theta_u)
