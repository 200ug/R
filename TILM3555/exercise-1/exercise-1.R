# t.2)

grades <- c(1, 0, 0, 0, 5, 2, 2, 0, 1, 1, 0, 0, 2, 0, 2, 3, 0, 0, 1, 2, 1, 0, 0, 0, 0, 1, 1, 4, 0, 1)

# Frekvenssijakauma
grades_table <- table(grades)

# Jakaumaa vastaava pylväskuvio
barplot(grades_table, col="blue")

# t.3)

# Kvartiilit (0.25, 0.5 [mediaani], 0.75)
# Huom. 'type=2' tarvitaan, jotta kvartiilit lasketaan oikealla algoritmilla
quantile(grades, probs=c(0.25, 0.5, 0.75), type=2)

# t.4)

# Tallennetaan arvot muuttujiin a ja b
a <- 8
b <- 2

# Suoritetaan määritellyillä muuttujilla yhteen-, vähennys-, ja potenssilasku
a+b
a*b
b^a

# Otetaan luonnollinen logaritmi ja neliöjuuri muuttujasta
log(a)
sqrt(b)

# Tallennetaan vektori arvoja muuttujaan x
x <- c(3, 1, 4, 0, 1, 2, 6, 6)

# Lasketaan yhteen kaikki vektorin arvot
sum(x)

# Tallennetaan vektori arvoja (kokonaisluvut 1..8) muuttujaan y
y <- c(1:8)

# Lasketaan yhteen vektorit alkiokohtaisesti
x+y
