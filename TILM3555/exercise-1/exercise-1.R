# Vastaus tehtävään 1 palautettavassa PDF:ssä

# 2.) 
# Kevään 2023 tilastollisen päättelyn peruskurssin toisen tenttikerran arvosanat olivat:
# 1 0 0 0 5 2 2 0 1 1 0 0 2 0 2 3 0 0 1 2 1 0 0 0 0 1 1 4 0 1
# Muodosta näistä frekvenssijakauma ja piirrä vastaava pylväskuvio. Mikä on arvosanojen
# moodi?

getmode <- function(x) {
  # Tallennetaan x:n uniikit arvot muuttujaan ux
  ux <- unique(x) 
  # match(x, ux): Haetaan indeksit, jotka vastaavat x-vektorin arvoja ux-vektorissa
  # tabulate(...): Lasketaan, kuinka monta kertaa kukin uniikki arvo esiintyy x:ssä
  # which.max(...): Haetaan indeksi eniten x:ssä esiintyneelle arvolle
  ux[which.max(tabulate(match(x, ux)))]
}

# Arvosanat vektorissa
grades = c(1, 0, 0, 0, 5, 2, 2, 0, 1, 1, 0, 0, 2, 0, 2, 3, 0, 0, 1, 2, 1, 0, 0, 0, 0, 1, 1, 4, 0, 1)
# Frekvenssijakauma
grades_table = table(grades)
# Pylväskuvio
barplot(grades_table, col="blue")
# Moodi (yllä määr. funktion avulla)
getmode(grades)

# 3.)
# Jatkoa edelliseen tehtävään. Määritä arvosanojen (otos)keskiarvo, mediaani ja kvartiilit.

# Otoskeskiarvo
mean(grades)
# Mediaani
median(grades)
# Kvartiilit
quantile(grades)

# Vastaus tehtävään 4 palautettavassa PDF:ssä (tehtävään liittyvä skripti alla)

a <- 8
b <- 2
a+b
a*b
b^a
log (a)
sqrt (b)
x <- c (3 ,1 ,4 ,0 ,1 ,2 ,6 ,6)
sum (x)
y <- c (1:8)
x+y
