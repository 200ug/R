# R-esimerkki: t-luottamusvälin muodostaminen naisten keskipituudelle

# luetaan aineisto
terveys <- read.csv("~/Projects/uni-assignments/R/TILM3555/materials/terveys.csv",  sep=";")

# valitaan vektoriin naisten pituudet
x <- subset(terveys, sukupuoli=="Nainen")$pituus

# onko x (likimain) normaalisti jakautunut?
# piirretään normaalikvantiilikuvio
qqnorm(x)
qqline(x)

# lasketaan naisten pituuden populaatiotason keskiarvon (odotusarvon) 95 % 
# luottamusväli olettaen, että muuttuja noudattaa (likimain) normaalijakaumaa.
n <- length(x)  # otoskoko
xviiva <- mean(x)  # otoskeskiarvo
s <- sd(x)  # otoskeskihajonta
t <- qt(0.975, n-1)  # t-jakauman kvantiili, n-1 vapausastetta
c(n, xviiva, s, t)  # tulostus

lv_vas <- xviiva - t*s/sqrt(n)  # luottamusvälin vasen päätepiste
lv_oik <- xviiva + t*s/sqrt(n)  # luottamusvälin oikea päätepiste
c(lv_vas, lv_oik)  # tulostus

# vaihtoehtoisesti ja paljon helpommalla t-luottamusvälin saa myos osana 
# t.test-funktion suoritusta:
t.test(x)
