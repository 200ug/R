library(foreign)
kotit1.dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/elinolo.sav", to.data.frame=TRUE)
# attach(kotit1.dat)

# Esim. oma opiskelijanumero
set.seed(2102331)

# 700 kokoinen otos
oma.otos <- kotit1.dat[sample(nrow(kotit1.dat), 700),]

# (Tehtävät tehdään tällä omalla otoksella, voit muotoilla harjoituksissa käytettyjä R-koodeja)

# Raportti, jossa on mallin oletusten tarkastelu ja aineiston kuvailu ja keskeiset tilastollisen 
# mallin tulokset: Mitkä selittävät muuttujat ovat mallissasi merkitseviä (ja onko yhdysvaikutus 
# merkitsevä)  ja millaisia eroja on selittävän muuttujan luokkien välillä. 

# Kuvailevat tunnusluvut
with(oma.otos, tapply(pala, list(kumu, ahtas), mean))    # Keskiarvot
with(oma.otos, tapply(pala, list(kumu, ahtas), median))  # Mediaanit
with(oma.otos, tapply(pala, list(kumu, ahtas), sd))      # Keskihajonnat

# Alla oleva komento riittää, koska kullekin selittävän muuttujen luokkien yhdistelmälle
# on ainakin kaksi havaintoa.

# Shapiro-Wilkin testit
with(oma.otos, tapply(pala, list(kumu, ahtas), function(x) shapiro.test(x)$p.value))

# Tukeyn laatikko-janakuvio, joka visualisoi tutkittavia jakaumia
boxplot(pala~interaction(kumu, ahtas), data=oma.otos, main="Tutkittavat jakaumat")

# Hajontojen yhtäsuuruustestaus Levenen testillä
attach(oma.otos)
library(car)
leveneTest(pala~kumu*ahtas)

# Itse kaksisuuntainen varianssianalyysi
malli <- lm(pala~kumu*ahtas)
anova(malli)

# Tutkitaan selittäjien keskiarvojen profiileita
interaction.plot(ahtas, kumu, pala, las=1, lwd=2)

# Tukeyn testi yhdysvaikutukselle (erot selittäjien eri luokkien välillä)
malli_lm2 <- aov(malli)
TukeyHSD(malli_lm2)
