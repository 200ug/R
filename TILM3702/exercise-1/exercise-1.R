random_seed <- 9132831
set.seed(random_seed)

library(foreign)
dat_pre <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/demoaineisto2015r.sav", to.data.frame=TRUE)
dat <- dat_pre[sample(1:nrow(dat_pre),150),]

# Kiinnitetään aineisto, jotta sitä ei tarvitse erikseen toistaa
# esim. voidaan kirjoittaa x sen sijaan että kirjoitettaisiin dat$x
attach(dat)

# ---
# Muuttujien tarkastelu (3)
# ---

# keskiarvot
mean(tvsaasto)
mean(oma_tulo)

# mediaanit
median(tvsaasto)
median(oma_tulo)

# keskihajonnat
sd(tvsaasto)
sd(oma_tulo)

# laatikko-janakuvaajat
boxplot(tvsaasto)
title("tvsaasto")
boxplot(oma_tulo)
title("oma_tulo")

# MAD (median absolute deviation)
mad(tvsaasto)
mad(oma_tulo)

# ---
# Hajontakuvio (4)
# ---

# hajontakuvio ja suoran sovitus
plot(oma_tulo, tvsaasto)
abline(lm(tvsaasto~oma_tulo))

# ---
# Korrelaatiokertoimet (4)
# ---

# Pearson
cor(oma_tulo, tvsaasto)

# Spearman
cor(oma_tulo, tvsaasto, method="spearman")

# ---
# Yksinkertainen regressiomalli (5)
# ---

# lineaarinen regressiomalli (selittäjänä 'oma_tulo', vasteena 'tvsaasto')
lm1 = lm(tvsaasto~oma_tulo) # data-parametria ei tarvita, koska käytetään attach-komentoa
summary(lm1)

# ---
# Residuaalitarkastelut (5)
# ---

plot(fitted(lm1), resid(lm1), xlab="Sovite", ylab="Havaittu residuaali")
abline(h=0, lty=2)
hist(resid(lm1), xlab="Residuaali", ylab="Frekvenssi", main="Residuaalien jakauma (histogrammi)")

# Huom! Myös plot(lm1) tuottaisi hyviä kuvia

# ---
# Useamman selittäjän regressiomalli (6)
# ---

# lineaarinen regressiomalli (selittäjinä 'oma_tulo', 'ika', 'evsaasto' ja 
# 'tyotilan', vasteena jälleen 'tvsaasto')
lm2 = lm(tvsaasto~oma_tulo+ika+evsaasto+tyotilan)
summary(lm2)

# ---
# Residuaalitarkastelut (6)
# ---

plot(fitted(lm2),resid(lm2), xlab = "Sovite", ylab = "Havaittu residuaali")
abline(h=0, lty=2)
hist(resid(lm2), xlab = "Residuaali", ylab="Frekvenssi", main = "Residuaalien jakauma (histogrammi)")

# ---
# Epämerkitsevien selittäjien poisto (7)
# ---

# Poistetaan mallista aiemman summary-tulosteen perusteella selittäjät, joiden 
# p-arvo on yli 0.05, eli 'oma_tulo' ja 'tyotilan'
lm2 = lm(tvsaasto~ika+evsaasto)
summary(lm2)
