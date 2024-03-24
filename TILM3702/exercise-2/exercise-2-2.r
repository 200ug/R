library(foreign)
dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/demoaineisto2015r.sav", to.data.frame=TRUE)
attach(dat)

install.packages(c("car", "contrast"))

# ---

# Jatketaan vielä yksisuuntaista varianssianalyysiä

library(car)

# Hajontojen yhtäsuuruustestaus (2)
bartlett.test(oma_tulo~taltyyt)

# Yksisuuntainen varianssianalyysi, hajonnat ovat yhtäsuuret (2)
# Päätelmä siitä, että hajonnat ovat yhtäsuuret, perustuu osassa 1
# tehtyyn Bartlettin testiin, jossa p > 0.05, jolloin voidaan käyttää
# yksinkertaista F-testiä:
malli1 <- lm(oma_tulo~taltyyt)
malli_lm <- aov(malli1)
summary(malli_lm)
 
# Tukeyn HSD-testi (2)
tukey.test <- TukeyHSD(malli_lm)
tukey.test
plot(tukey.test)

# ---

# Siirrytään kahden selittäjän malliin

# Keskiarvot soluittain (2)
with(dat, tapply(oma_tulo, list(tyotilan, taltyyt), mean)) 

# Mediaanit soluittain (2)
with(dat, tapply(oma_tulo, list(tyotilan, taltyyt), median)) 

# Keskihajonnat soluittain (2)
with(dat, tapply(oma_tulo, list(tyotilan, taltyyt), sd)) 

# Mikäli kaikissa ryhmissä (tyytyväisyysluokissa sekä työttömille että työllisille) 
# olisi väh. kaksi havaintoa, voitaisiin käyttää pelkkää alla olevaa komentoa kolmen 
# yllä olevan sijasta:

# Mikäli kaikissa ryhmissä (tyytyväisyysluokissa työttömille ja työllisille erikseen)
# olisi väh. kaksi havaintoa, voitaisiin käyttää alla olevaa komentoa sitä seuraavan sijaan:
# with(dat, tapply(oma_tulo, list(tyotilan, taltyyt), shapiro.test))

# Shapiro-Wilk-testit, työssäolevat (2)
install.packages("dplyr")
library(dplyr)
raj.ain1 <- select(filter(dat, tyotilan=="tyossa"), c(tyotilan, taltyyt, oma_tulo))    
with(raj.ain1, tapply(oma_tulo, list(taltyyt), shapiro.test))
attach(raj.ain1)

# Tukeyn laatikko-janakuvio, työssäolevat (2)
boxplot(oma_tulo~taltyyt)
detach(raj.ain1)

# Shapiro-Wilk-testit, työttämät (ei erittäin tyytyväiset) (2)
raj.ain2 <- select(filter(dat, tyotilan=="tyoton" & taltyyt!="erittain tyytyvainen"), c(tyotilan, taltyyt, oma_tulo))    
with(raj.ain2, tapply(oma_tulo, list(taltyyt), shapiro.test))

# Tukeyn laatikko-jana-kuvio, työttämät (2)
raj.ain3 <- select(filter(dat, tyotilan=="tyoton"), c(tyotilan, taltyyt, oma_tulo))    
attach(raj.ain3)
boxplot(oma_tulo~taltyyt)  
detach(raj.ain3)

# ---

# Hajontojen yhtäsuuruustestaus (3)
attach(dat)
library(car)
leveneTest(oma_tulo~taltyyt*tyotilan)

# Kaksisuuntainen varianssianalyysi (3)
anova(lm(dat$oma_tulo~taltyyt*tyotilan))   

# Nimetään malli (3)
# fit1 <- lm(oma_tulo~taltyyt*tyotilan, data=dat) 
# summary(fit1)

# Keskiarvojen profiilikuvio (3)
interaction.plot(taltyyt, tyotilan, oma_tulo, xlab="Tyytyvaisyys", ylab="Omat tulot", trace.label="Tyotilanne", las=1, lwd=2)  

# Suljetaan data
detach(dat)

# ---

dat2 <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/demo2_2015r.sav", to.data.frame=TRUE)
attach(dat2)

# Keskiarvot soluittain (4)
with(dat2, tapply(hyl, list(tehdas, vuoro), mean))  

# Shapiro-Wilk-testit, tehdas1 (4)
tehdas1.dat <- select(filter(dat2, tehdas=="tehdas1"), c(tehdas, vuoro, hyl))    
with(tehdas1.dat, tapply(hyl, list(vuoro), shapiro.test))
attach(tehdas1.dat)

# Tukeyn laatikko-jana-kuvio, tehdas1 (4)
boxplot(hyl~vuoro, main="tehdas 1")
detach(tehdas1.dat)

# Shapiro-Wilk-testit, tehdas2 (4)
tehdas2.dat <- select(filter(dat2, tehdas=="tehdas2"), c(tehdas, vuoro, hyl))    
with(tehdas2.dat, tapply(hyl, list(vuoro), shapiro.test)) 

# Tukeyn laatikko-jana-kuvio, tehdas2 (4)
attach(tehdas2.dat)
boxplot(hyl~vuoro, main="tehdas 2")  
detach(tehdas2.dat)

# ---

# Hajontojen yhtäsuuruustestaus (5)
attach(dat2)
library(car)
leveneTest(hyl~tehdas*vuoro)

# Kaksisuuntainen varianssianalyysi (5)
anova(lm(hyl~tehdas*vuoro))   

# Nimetään malli (5)
fit2 <- lm(hyl~tehdas*vuoro, data=dat2) 
malli_lm2 <- aov(fit2)

# Vaihtoehtoinen keino anova-komennon sijaan, tuottaa saman tulosteen.
# summary(malli_lm2)

# ---

# Keskiarvojen profiilit (6)
interaction.plot(vuoro, tehdas, hyl, xlab="Vuoros", ylab="Hylatyt", trace.label="Tehdas", las=1, lwd=2)  

# Tukeyn testi yhdysvaikutukselle (6)
tukey.test2 <- TukeyHSD(malli_lm2)
tukey.test2
plot(tukey.test2)
