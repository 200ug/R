# Harjoitustyön raportti erillisenä dokumenttina (PDF-muodossa)

# Tallennetaan tuloste erilliseen tiedostoon, joka liitetään palautuksen yhteyteen
sink("/tmp/harjoitustyo-tuloste.txt")

# Numeeristen vastemuuttujien mallitus (elinolo2020, n=2199)
library(foreign)
ht1.dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/elinolo2020.sav", to.data.frame=TRUE)
attach(ht1.dat)
set.seed(2102331)

# 800 kokoinen otos
oma.otos1 <- ht1.dat[sample(nrow(ht1.dat), 800),]
attach(oma.otos1)

# 1.) Regressiomalli: 
# Onko kotitalouden kuluttajayksiköiden lkm., asumismenoilla yhteensä ja alueella asumisajalla
# yhteyttä asunnon pinta-alaan?

lm1 = lm(pala~rkyks+asmenot+alaika)
summary(lm1)
detach(oma.otos1)

# Tyypillinen kuvaaja residuaaleista sovitteita vasten
plot(lm1$fitted.values, residuals(lm1), xlab="Sovitteet", ylab="Residuaalit")
abline(h = 0, col = "red")

# Residuaalien histogrammi
hist(resid(lm1), xlab="Residuaali", ylab="Frekvenssi", main="Residuaalien jakauma")

# ---

library(foreign)
ht2.dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/Toistomittausaineisto2020.sav", to.data.frame=TRUE)
attach(ht2.dat)
set.seed(2102331)

# 500 kokoinen otos
oma.otos2 <- ht2.dat[sample(nrow(ht2.dat), 500),]
attach(oma.otos2)

# 2.) Toistomittausmalli:
# Hypoteesi: potilaan mielestä saatu ohjaus leikkauksen jälkeen toiminnallisista seikoista 
# (Functional_M2) on ollut vähäisempää kuin odotettu ennen leikkausta (Functional_M1) eli
# keskiarvo toisessa mittauksessa on matalampi. Lisäksi kiinnostaa myös, onko kys. ero mittausten
# väilllä erilainen sukupuolittain.
# 
# Saavatko tutkimushypoteesit tukea mallittamalla aineisto toistettujen mittausten varianssianalyysillä?

# Kuvaileva tarkastelu lineaarisille muuttujille
summary(Functional_M1)
summary(Functional_M2)
boxplot(Functional_M1, Functional_M2)

# Normaalisuustestit eri sukupuolille
with(oma.otos2, tapply(Functional_M1, D2, shapiro.test))
with(oma.otos2, tapply(Functional_M2, D2, shapiro.test))

# Muokataan aineistoa niin, että eri havainnot ovat allekkain
install.packages("tidyr")
library(tidyr)
oma.otos2_long <- pivot_longer(
  oma.otos2[, c("patient", "D2", "Functional_M1", "Functional_M2")], 
  cols=c("Functional_M1", "Functional_M2"), 
  names_to="Measurement", 
  values_to="Score"
)

# Toistettujen mittausten varianssianalyysi (sis. sukupuolen luokittelevana tekijänä)
fit1 <- aov(Score~Measurement*D2+Error(patient/Measurement*D2), data=oma.otos2_long)
summary(fit1)

# Parittaiset t-testit jokaiselle aineiston sukupuolelle
res_list <- list()

for (gender in c("male", "female")) {
  # Filtteröidään datasta vain tietyn sukupuolen osuus
  subset_data <- oma.otos2_long[oma.otos2_long$D2==gender,]
  
  # Lasketaan parittaisen t-testin tulos
  test_result <- pairwise.t.test(x=subset_data$Score, g=subset_data$Measurement, p.adjust.method="bonferroni")
  
  res_list[[gender]] <- test_result
}

print(res_list)
detach(oma.otos2)

# ---

library(foreign)
ht3.dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/EK2011.sav", to.data.frame=TRUE)
attach(ht3.dat)
set.seed(2102331)

# 800 kokoinen otos
oma.otos3<-ht3.dat[sample(nrow(ht3.dat), 800),]
attach(oma.otos3)

# 3.) Kaksiluokkainen selitettävä muuttuja:
# Tutkitaan muuttujien sukupuoli ja ikä yhteyttä työttömyyteen viimeisen 12 kk aikana
# käyttämällä logistista regressiomallia. Mitkä muuttujat selittävät työttömyyttä? 
# Mikä on mallin Nagelkerke-selitysaste?

# Logistinen binäärinen regressio
logr_d32 <- glm(d32~d2+ika, data=oma.otos3, family=binomial)
summary(logr_d32)

# Mallin odds ratiot (OR)
exp(cbind(OR=coef(logr_d32), confint(logr_d32)))

# Mallin selitysaste
install.packages("fmsb")
library(fmsb)
data.nagel <- NagelkerkeR2(logr_d32)
data.nagel

# ---

library(foreign)
ht4.dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/pankkiotos2020.sav", to.data.frame=TRUE)
attach(ht4.dat)
set.seed(2102331)

# 1600 kokoinen otos (huom. että kaikki aineiston muuttujat otetaan mukaan PCA:han)
oma.otos4<-ht4.dat[sample(nrow(ht4.dat), 1600),]
attach(oma.otos4)

# 4.) Muuttujien ryhmittely pääkomponenttianalyysin avulla:
# Muodostetaan luokitelluista muuttujista (yht. 41 kpl.) pääkomponentteja 
# ominaisarvokriteerien mukaan (Promax-rotaatio). Tämän jälkeen talleteteaan saadut pistemäärät
# havaintomatriisiin ja nimetään pääkomponentit kuvailevasti kohdan 5 klusterointia varten.

# Korrelaatiokertoimet
data.kor1 <- cor(oma.otos4, method="pearson", use="complete.obs")
data.kor1

# Korrelaatiomatriisin sopivuus pääkomponenttianalyysiin
KMO(data.kor1)

# Pääkomponenttianalyysi
pca <- prcomp(data.kor1, center=T, scale=T)
pca
summary(pca)

# Ominaisarvot (& niiden scree plot)
(pca$sdev)^2
screeplot(pca, type="lines") + abline(h=1, lty=2)

# Valitaan pääkomponentit 1-11 (yht. 11 kpl) Promax-rotaatioon
pca.chosen <- pca$rotation[,1:11]
pca.promax <- promax(pca.chosen)
pca.promax

# Talletetaan pääkomponenttipistemäärät havaintomatriisin uusiksi muuttujiksi
oma.otos4_scores <- pca$x[, 1:11]

# Talletetaan pääkomponentteja kuvailevat nimet (muodostettu latausten perusteella)
colnames(oma.otos4_scores) <- c(
  "Taloudellinen vakaus", 
  "Transaktioaktiivisuus", 
  "Perinteisten palvelujen käyttö", 
  "Asuntolainakeskeisyys", 
  "Korkeariskinen sijoitustoiminta", 
  "Matalariskinen sijoitustoiminta", 
  "Velkojen ja lainojen hallinta", 
  "Kuluttajapalveluiden käyttö", 
  "Kulutusluottojen hallinta", 
  "Riskinottokyky", 
  "Asuntomarkkinoihin sitoutuminen"
)

# 5.) Havaintojen ryhmittely K-mean klusterianalyysillä:
# Käytetään kohdassa 4 muodostettuja muuttujia, joiden avulla muodostetaan
# asiakasryhmiä kahdesta viiteen/kuuteen klusteriin. Kuvaillaan muodostettuja ryhmiä
# kohdan 4 pääkomponenttien nimien perusteella.

set.seed(2102331)

# K-means (k-keskiarvo) klusterointi pääkomponenttipistemäärillä (2..5 klusteria)
results <- list()

for (k in 2:5) {
  results[[paste("k", k, sep="")]] <- kmeans(oma.otos4_scores, centers=k, nstart=100)
}

results

# Suljetaan kirjoitusyhteys tiedostoon
sink()
