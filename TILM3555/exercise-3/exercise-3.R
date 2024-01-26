# 2. (R) Taulukossa alla on kuukausittaiset sademäärät (mm) vuonna 2023 Turun Artukaisissa, 
# Helsingin Kaisaniemessä ja Inarin Kaamasessa (lähde Ilmatieteen laitos). Piirrä
# kaikki pareittaiset hajontakuviot (Turku–Helsinki, Turku–Inari ja Helsinki–Inari). 
# Minkä paikkakuntien sademäärissä on selvää riippuvutta? Näyttääkö riippuvuus keskimäärin
# lineaariselta? Jos näyttää, niin sovita PNS-suora. Laske myös korrelaatiokertoimet.

Tku <- c(62.4, 32.5, 72.8, 10, 20.9, 11.3, 40.8, 146.1, 46.5, 100.9, 76.6, 26.1)
Hki <- c(82.5, 39, 65, 10.2, 17.5, 4.5, 94.3, 172.5, 70.7, 130.6, 60.6, 67.4)
Ina <- c(27.3, 15.6, 25.6, 13.5, 55.6, 13.4, 105.9, 82.3, 87, 15.8, 22.2, 20.5)

# Turku-Helsinki
plot(Tku, Hki, xlab="Turku", ylab="Helsinki", main="Turku-Helsinki")
abline(lm(Tku ~ Hki)) # PNS-suora
cor(Tku, Hki) # Korrelaatiokerroin

# Turku-Inar
plot(Tku, Ina, xlab="Turku", ylab="Inari", main="Turku-Inari")
cor(Tku, Ina) # Korrelaatiokerroin

# Helsinki-Inari
plot(Hki, Ina, xlab="Helsinki", ylab="Inari", main="Helsinki-Inari")
cor(Hki, Ina) # Korrelaatiokerroin

# 4. Oletetaan, että populaatiojakauma on normaalijakauma N (1, 2) ja siitä on poimittu
# (riippumaton) satunnaisotos kokoa n.

# b.) Määritä todennäköisyys P(|\overline{X} − 1| > 0.1), jos n = 10.

# Määr. jakauman parametrit ja otoskoko
mu <- 1
sigma <- 2
n <- 10

# Lasketaan otoskeskiarvon keskihajonta
sigma_x <- sigma / sqrt(n)
# Lasketaan tod.näk. P(|\overline{X} − 1| > 0.1) ja tulostetaan se
p_greater_than_0_1 <- 1 - (pnorm(1.1, mu, sigma_x) - pnorm(0.9, mu, sigma_x))
p_greater_than_0_1

# c.) Kuinka suuri otoskoon n tulisi olla, jotta P(|\overline{X} − 1| > 0.1) <= 0.01?

# Määr. jakauman parametrit ja tavoiteltu tod.näk.
mu <- 1
sigma <- 2
alpha <- 0.01

# Lasketaan z-arvo (standardoidun normaalijakauman kertymäfunktion arvo)
z_value <- qnorm(1 - alpha / 2)
# Lasketaan z-arvon avulla otoskoko n ja tulostetaan se
n <- (z_value * sigma / 0.1)^2
n
