library(foreign)
dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/demoaineisto2015r.sav", to.data.frame=TRUE)
attach(dat)

# ---

# Logistinen binäärinen regressio (2)
logr_tyotilan <- glm(tyotilan~oma_tulo+ika+omistusa+koulutus, data=dat, family=binomial)
logr_tyotilan

# P-arvot (2)
summary(logr_tyotilan)

# Odds ratiot (2)
exp(cbind(OR=coef(logr_tyotilan), confint(logr_tyotilan)))

# Selitysaste (2)
install.packages ("fmsb")
library(fmsb)
data.nagel <- NagelkerkeR2(logr_tyotilan)
data.nagel

# ---

# Multinomiaalinen logistinen regressio (5, 6, 7, 8)
install.packages("mlogit")
install.packages("nnet")
library("mlogit")
library("nnet")
dat$taltyyt2 <- relevel(dat$taltyyt, ref="erittain tyytymaton")
test <- multinom(taltyyt2~oma_tulo+ika+omistusa, data=dat)
summary(test)
confint(test)

# Odds ratio (5, 6, 7, 8)
exp(coef(test))

# Z-arvot (5, 6, 7, 8)
z <- summary(test)$coefficients/summary(test)$standard.errors
z

# P-arvot (5, 6, 7, 8)
p <- (1-pnorm(abs(z), 0, 1))*2
p

# ---

# Loglineaarisen mallin muuttujien tarkastelu (9, 10, 11)

# Yhden muuttujan marginaalitaulut (9)
attach(dat)
table(omistusa)
table(tyotilan)
table(yltyyt)

# Kolmen muuttujan taulu (9)
mytable <- table(omistusa, tyotilan, yltyyt) 
ftable(mytable)

# ---

# Loglineaarinen mallitus (10)
library(MASS)
mytable <- xtabs(~omistusa+tyotilan+yltyyt, data=dat)

# Täydellinen riippumattomuus (10)
loglm(~omistusa+tyotilan+yltyyt, mytable)

# 'yltyyt' riippumaton muuttujaparista (10)
loglm(~omistusa+tyotilan+yltyyt+tyotilan*omistusa, mytable)

# 'omistusa' riippumaton muuttujaparista (10)
loglm(~omistusa+tyotilan+yltyyt+tyotilan*yltyyt, mytable)

# 'tyotilan' riippumaton muuttujaparista (10)
loglm(~omistusa+tyotilan+yltyyt+omistusa*yltyyt, mytable)

# Ehdollinen riippumattomuusmalli: 'omistusa' ja 'yltyyt' riippumattomia (10)
loglm(~omistusa+tyotilan+yltyyt+omistusa*tyotilan+tyotilan*yltyyt, mytable)

# Ehdollinen riippumattomuusmalli: 'toytilan' ja 'yltyyt' riippumattomia (10)
loglm(~omistusa+tyotilan+yltyyt+omistusa*tyotilan+omistusa*yltyyt, mytable)

# Ehdollinen riippumattomuusmalli: 'toytilan' ja 'omistusa' riippumattomia (10)
loglm(~omistusa+tyotilan+yltyyt+omistusa*yltyyt+tyotilan*yltyyt, mytable)

# Parittaisten riippuvuuksien malli (10)
loglm(~omistusa+tyotilan+yltyyt+omistusa*tyotilan+omistusa*yltyyt+tyotilan*yltyyt, mytable)

# ---

# Mallin jatkotarkastelu (11)
taulu1 <- table(omistusa, yltyyt)  
prop.table(taulu1, 1) 

taulu2 <- table(tyotilan, yltyyt)  
prop.table(taulu2, 1) 
 