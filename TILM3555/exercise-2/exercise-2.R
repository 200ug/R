# -- t. 1) --

x <- c(-3.4,-2.1,3.8,4.0,2.8,-4.3,-1.5,0.8,2.5,3.1,1.9,-6.0,-0.6,-16.6)
quantile(x, probs=c(0.25, 0.5, 0.75), type=2)
mean(x)
sd(x)

# -- t. 3) --

#################################################################
terveys <- read.csv("C:\\Users\\einis\\Projects\\R\\TILM3555\\materials\\terveys.csv", sep=";")
View(terveys)

# Aineistoa paasee nyt tarkastelemaan RStudiossa editori-ikkunan
# terveys-valilehdella. Riveina ovat tarkasteltavat havainnot
# (ihmiset) ja sarakkeina muuttujien arvot.

# Seuraava komento palauttaa aineiston koon (havaintojen lkm) ja
# muuttujien lukumaaran:
dim(terveys)

# Muuttujien nimet saadaan seuraavasti:
names(terveys)

# Yksittaisen muuttujan arvoista koostuvaan vektoriin voidaan
# viitata kayttamalla $-erotinta ja muuttujan nimea:
terveys$ika

# Piirretaan ika-muuttujan luokiteltu frekvenssijakauma eli
# histogrammi jakamalla iat tasavalisesti 9 luokkaan:
hist(terveys$ika, breaks=9)

# table-funktiolla voidaan muodostaa frekvenssijakauma
# muuttujalle. Sukupuolten frekvenssijakauma saadaan komennolla
table(terveys$sukupuoli)

# table-funktiolla voidaan muodostaa myos kaksiulotteinen
# frekvenssijakauma eli ns. kontingenssitaulukko kahdelle
# luokalliselle muuttujalle. Selvitetaan liikunnan harrastamista
# sukupuolittain:
taulukko <- table(terveys$sukupuoli, terveys$liikunta)
taulukko

# Edella R esittaa liikunnan 3 tasoa aakkosjarjestyksessa.
# Kerrotaan luonnollinen jarjestys eksplisiittisesti ja yritetaan
# uudestaan:
terveys$liikunta <- factor(terveys$liikunta, levels=c("ei lainkaan","saannollisesti","paivittain"))
taulukko <- table(terveys$sukupuoli, terveys$liikunta)
taulukko

# Piirretaan tasta summapylvaskuvio:
barplot(taulukko, main="Liikunta sukupuolittain", col=c("blue", "red"), legend=c("miehet", "naiset")) 
#################################################################

terveys$bmi <- terveys$paino/(terveys$pituus/100)^2
hist(terveys$bmi)
terveys.m <- subset(terveys, sukupuoli=="Mies")
terveys.n <- subset(terveys, sukupuoli=="Nainen")

boxplot(terveys.n$bmi)
qqnorm(terveys.n$bmi)
qqline(terveys.n$bmi)

boxplot(terveys.m$bmi)
qqnorm(terveys.m$bmi)
qqline(terveys.m$bmi)

