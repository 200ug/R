# Esimerkki chi^2-yhteensopivuustestistä:
# --- 

# Otoksessa (kokoa n) on havaittu luokkafrekvenssit f.
# H_0: Luokkien tn.:t ovat p.

f <- c(184, 87, 109)  # havaitut frekvenssit
n <- sum(f)  # otoskoko
f/n  # havaitut %-osuudet

p <- c(46.2, 27.5, 26.3)/100  # H0-tn:t
e <- n*p  # odotetut frekvenssit
e

h_hav <- sum((f-e)^2/e)  # testisuureen hav. arvo
h_hav

p_arvo <- 1-pchisq(h_hav, length(f)-1)  # p-arvo
p_arvo

# Koko jutun olisi voinut tehda myos seuraavalla R:n funktiolla:

chisq.test(f,p=p)

# Esimerkki riippumattomuustestista
# ---

# Testataan sukupuolen ja verenpaineluokan riippumattomuutta
# terveys-aineistossa

terveys <- read.csv("~/Projects/uni-assignments/R/TILM3555/materials/terveys.csv", sep=";")

# kerrotaan verenpaineluokkien oikea jarjestys
terveys$verenpaineluokka <- ordered(terveys$verenpaineluokka, levels=c("alhainen", "normaali", "korkea"))

# kaksiulotteinen frekvenssitaulukko
taulu <- table(terveys$sukupuoli, terveys$verenpaineluokka)
taulu

# Miesten ja naisten summat eli rivisummat:
(m.sum <- sum(taulu[1,]))  # miehet
(n.sum <- sum(taulu[2,]))  # naiset
# Verenpainetasojen summat eli sarakesummat:
(alh.sum <- sum(taulu[,1]))  # alhainen
(norm.sum <- sum(taulu[,2])) # normaali
(kor.sum <- sum(taulu[,3]))  # korkea

(n <- m.sum+n.sum)  # havaintojen lkm

# odotettujen frekvenssien estimaatit
e <- c(m.sum*alh.sum/n,m.sum*norm.sum/n,m.sum*kor.sum/n,
       n.sum*alh.sum/n,n.sum*norm.sum/n,n.sum*kor.sum/n)
e
# havaitut frekvenssit vektorina samassa jarjestyksessa
f <- as.vector(t(taulu))
f

h.hav <- sum((f-e)^2/e)  # testisuureen havaittu arvo
h.hav
v <- (nrow(taulu)-1)*(ncol(taulu)-1)  # vapausasteluku
v
1-pchisq(h.hav, v)  # p-arvo

# Ylla oleva lasku oli tyolas! Koko testin voi tehda
# suoraan chisq.test-funktiolla:
chisq.test(taulu)
