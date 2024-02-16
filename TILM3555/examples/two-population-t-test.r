# R-esimerkki: kahden populaation t-testi PISA-pisteille

n1 <- 2954  # poikien lkm otoksessa
n2 <- 2856  # tyttojen lkm
x.ka <- 541.79  # poikien otoskeskiarvo
y.ka <- 539.21  # tyttojen otoskeskiarvo
s1 <- 80.00  # otoskeskihajonnat
s2 <- 73.02

pe <- x.ka - y.ka  # erotuksen piste-estimaatti
print(paste("keskiarvojen erotuksen piste-estimaatti:", pe))

# keskivirhe ilman oletusta varianssien samuudesta
se <- sqrt(s1^2/n1 + s2^2/n2)
print(paste("keskivirhe:", se))

# t-testisuureen havaittu arvo
# (H0: poikien ja tyttojen odotusarvot samat)
t_hav <- (x.ka-y.ka)/se
print(paste("t-testisuureen havaittu arvo:", t_hav))

# kaksipuolinen p-arvo (likiarvo normaalijakaumasta)
p <- 2*(1-pnorm(t_hav))
print(paste("kaksipuolinen p-arvo (likiarvo):", p))
