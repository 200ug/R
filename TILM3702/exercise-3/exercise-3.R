library(foreign)
dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/toistetut-mittaukset-2015r.sav", to.data.frame=TRUE)
attach(dat)

# ---

# Tunnusluvut vuosittain (2)
summary(saasto2013)
summary(saasto2014)
summary(saasto2015)

# Laatikko-janakuvio (2)
boxplot(saasto2013, saasto2014, saasto2015)

# Shapiro-Wilkin testit (2)
shapiro.test(saasto2013)
shapiro.test(saasto2014)
shapiro.test(saasto2015)
detach(dat)

# ---

# Otetaan käyttöön data, jossa kunkin havainnon kohdalla säästöt ovat allekkain (3)
dat2 <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/toistetut-mittaukset-allekkain-2015r.sav", to.data.frame=TRUE)
attach(dat2)

# Toistettujen mittausten varianssianalyysi (3)
fit1 <- aov(euroa~vuosi+Error(id/vuosi), data=dat2)
summary(fit1)
detach(dat2)

# ---
 
# Vertailut parittaisin t-testein (4)
attach (dat)
t.test(saasto2013, saasto2015, paired=T)
t.test(saasto2014, saasto2015, paired=T)
detach(dat)

# ---

# Friedmanin testi (5, 6)
attach(dat2)
friedman.test(euroa~vuosi|id, data=dat2)
with(dat2, boxplot(euroa~vuosi))  
detach(dat2)

# Wilcoxonin testit (5, 7)
attach(dat)
wilcox.test(saasto2013, saasto2015, paired=T)
wilcox.test(saasto2014, saasto2015, paired=T)

# ---

# Tehdään normaalisuustestit taloudellisen tyytyväisyyden tasoilla (8)
with(dat, tapply(saasto2013, taltyyt, shapiro.test))
with(dat, tapply(saasto2014, taltyyt, shapiro.test))
with(dat, tapply(saasto2015, taltyyt, shapiro.test))

# ---

# Toistettujen mittausten varianssianalyysi, toistotekijä ja luokitteleva tekijä (9)
fit2 <- aov(euroa~taltyyt*vuosi+Error(id/vuosi), data=dat2)
summary(fit2)

# Yhdysvaikutuksen profiilikuvio (9)
attach(dat2)
interaction.plot(vuosi, taltyyt, euroa, data=dat2, col=c("red", "blue", "black", "purple"))
detach(dat2)

# ---

# Vuosien vertailut taloudellisen tyytyväisyyden luokissa (10)
attach(dat)
install.packages("dplyr")
library(dplyr)

data1 <- select(filter(dat, taltyyt=="erittain tyytyvainen"), c(taltyyt, saasto2013, saasto2014, saasto2015))
attach(data1)
t.test(saasto2013, saasto2015, paired=T)
t.test(saasto2014, saasto2015, paired=T)
detach(data1)

data2 <- select(filter(dat, taltyyt=="melko tyytyvainen"), c(taltyyt, saasto2013, saasto2014, saasto2015))    
attach(data2)
t.test(saasto2013,saasto2015, paired=T)
t.test(saasto2014,saasto2015, paired=T)
detach(data2)

data3 <- select(filter(dat, taltyyt=="melko tyytymaton"),c(taltyyt,saasto2013,saasto2014,saasto2015))    
attach(data3)
t.test(saasto2013, saasto2015, paired=T)
t.test(saasto2014, saasto2015, paired=T)
detach(data3)

data4 <- select(filter(dat, taltyyt=="erittain tyytymaton"), c(taltyyt, saasto2013, saasto2014, saasto2015))    
attach(data4)
t.test(saasto2013, saasto2015, paired=T)
t.test(saasto2014, saasto2015, paired=T)
detach(data4)
