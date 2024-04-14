library(foreign)
kotit2.dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/EK2011r.sav", to.data.frame=TRUE)
attach(kotit2.dat)

# Esim. oma opiskelijanumero
set.seed(2102331)

# 1000 kokoinen otos
oma.otos <- kotit2.dat[sample(nrow(kotit2.dat), 900),]
attach(oma.otos)

# Tutkimuksenalaisten kategoristen muuttujien frekvenssit
table(d2)
table(d4)
table(k23)

# Kolmen muuttujan taulu 
mytable <- table(d2,d4,k23) 
ftable(mytable)

# Tarkistetaan, että kaikissa kategorioissa on riittävä lkm. muuttujia
if (any(mytable == 0)) {
  print("Loglineaarista mallia ei voida sovittaa puutteellisten havaintojen vuoksi")
} else {
  print("Havaintoja on väh. 10 jokaisessa kategoriassa")
}

# Loglineaarinen mallitus
library(MASS)
mytable <- xtabs(~d2+d4+k23, data=oma.otos)

loglm(~d2+d4+k23, mytable)        # Täydellinen riippumattomuus
loglm(~d2+d4+k23+d4*k23, mytable) # d2 riippumaton muuttujaparista
loglm(~d2+d4+k23+d2*k23, mytable) # d4 riippumaton muuttujaparista
loglm(~d2+d4+k23+d2*d4, mytable)  # k23 riippumaton muuttujaparista

# Ehdolliset riippumattomuusmallit:
loglm(~d2+d4+k23+d2*d4+d2*k23, mytable)  # d4 ja k23 riippumattomia
loglm(~d2+d4+k23+d2*k23+d2*k23, mytable) # d2 ja d4 riippumattomia
loglm(~d2+d4+k23+d2*d4+d4*k23, mytable)  # d2 ja k23 riippumattomia

# Parittaisten riippuvuuksien malli:
loglm(~d2+d4+k23+d2*d4+d4*k23+d2*k23, mytable)

# 2 kpl ristiintauluja (lkm. valitun loglineaariseen malliin perustuen)
taulu1 <- table(d4, d2)
prop.table(taulu1, 1)
taulu2 <- table(k23, d2)
prop.table(taulu2, 1)
