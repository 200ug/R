# t.2)

Tku <- c(62.4, 32.5, 72.8, 10, 20.9, 11.3, 40.8, 146.1, 46.5, 100.9, 76.6, 26.1)
Hki <- c(82.5, 39, 65, 10.2, 17.5, 4.5, 94.3, 172.5, 70.7, 130.6, 60.6, 67.4)
Ina <- c(27.3, 15.6, 25.6, 13.5, 55.6, 13.4, 105.9, 82.3, 87, 15.8, 22.2, 20.5)

# Hajontakuvio plot-komennolla, PNS-suora abline-komennolla ja
# korrelaatiokerroin cor-komennolla

# Turku-Helsinki
plot(Tku, Hki, xlab="Turku", ylab="Helsinki", main="Turku-Helsinki")
abline(lm(Tku ~ Hki))
cor(Tku, Hki)

# Turku-Inari
plot(Tku, Ina, xlab="Turku", ylab="Inari", main="Turku-Inari")
cor(Tku, Ina)

# Helsinki-Inari
plot(Hki, Ina, xlab="Helsinki", ylab="Inari", main="Helsinki-Inari")
cor(Hki, Ina)

# t.4)

# b) n=10
2*(1-pnorm(sqrt(5)*0.1))

# c) n=?

ceiling((10*sqrt(2)*qnorm(0.995))^2)
