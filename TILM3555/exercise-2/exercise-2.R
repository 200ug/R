# t.1)

x <- c(-3.4,-2.1,3.8,4.0,2.8,-4.3,-1.5,0.8,2.5,3.1,1.9,-6.0,-0.6,-16.6)
boxplot(x)

# Kvantiilien, otoskeskiarvon ja -keskihajonnan tarkistuslaskelmat
quantile(x, probs=c(0.25, 0.5, 0.75), type=2)
mean(x)
sd(x)

# t.3)

terveys <- read.csv("/home/homer/Projects/uni-assignments/R/TILM3555/materials/terveys.csv", sep=";")
terveys$bmi <- terveys$paino/(terveys$pituus/100)^2

# Painoindeksin histogrammi
hist(terveys$bmi, main="Painoindeksin histogrammi")

# Jaetaan aineisto miehiin ja naisiin
terveys.m <- subset(terveys, sukupuoli=="Mies")
terveys.n <- subset(terveys, sukupuoli=="Nainen")

# Tukeyn laatikkokuviot
boxplot(terveys.m$bmi, main="Miehet")
boxplot(terveys.n$bmi, main="Naiset")

# Normaalikvantiilikuviot
qqnorm(terveys.m$bmi, main="Miehet")
qqline(terveys.m$bmi)

qqnorm(terveys.n$bmi, main="Naiset")
qqline(terveys.n$bmi)

# t.5)

# Tehtävänannon vinouden määritelmään perustuva funktio
vinous <- function(x) {
  n <- length(x) # havaintojen lkm.
  ka <- mean(x)  # otoskeskiarvo
  (1/n)*sum((x-ka)^3) / ((1/n)*sum((x-ka)^2))^(3/2)
}

# Esimerkkituloksia (vinous<0: vasemmalle vino; vinous>0: oikealle vino)
vinous(terveys$kolesteroli)
vinous(terveys$pituus)
vinous(terveys$paino)

# Jakauman huipukkuuden laskeva funktio
huipukkuus <- function(x) {
  n <- length(x) # havaintojen lkm.
  ka <- mean(x)  # otoskeskiarvo
  (1/n)*sum((x-ka)^4) / ((1/n)*sum((x-ka)^2))^2-3
}

# Esimerkkituloksia (huipukkuus<0: litteä; huipukkuus>0: huipukas)
huipukkuus(terveys$kolesteroli)
huipukkuus(terveys$pituus)
huipukkuus(terveys$paino)
