# t. 2.)
# Kahden populaation odotusarvojen vertailu t-testillä

terveys <- read.csv("~/Projects/uni-assignments/R/TILM3555/materials/terveys.csv", sep=";")

# a.)

x <- subset(terveys, sukupuoli=="Mies")$kolesteroli
y <- subset(terveys, sukupuoli=="Nainen")$kolesteroli

n_x <- length(x)
n_y <- length(y)

x.ka <- mean(x)
y.ka <- mean(y)

s_x <- sd(x)
s_y <- sd(y)

# Keskiarvojen erotuksen piste-estimaatti
pe <- x.ka-y.ka
print(paste("erotuksen piste-estimaatti:", pe))

# Keskivirhe ilman oletusta varianssien yhtäsuuruudesta
se <- sqrt(s_x^2/n_x+s_y^2/n_y)
print(paste("keskivirhe:", se))

# Testisuureen arvo, kun oletetaan nollahypoteesi todeksi
# (H_0: miesten ja naisten keskim. kolesteroliarvot samat)
t_hav <- (x.ka-y.ka)/se
print(paste("t-testisuureen havaittu arvo:", t_hav))

# Kaksipuolinen p-arvo 
# (likiarvo standardinormaalijakaumasta, koska otoskoko suht. on suuri)
p <- 2*(1-pnorm(t_hav))
print(paste("kaksipuolinen p-arvo:", p))

# b.)

x <- subset(terveys, liikunta=="ei lainkaan")$kolesteroli
y <- subset(terveys, liikunta!="ei lainkaan")$kolesteroli

n_x <- length(x)
n_y <- length(y)

x.ka <- mean(x)
y.ka <- mean(y)

s_x <- sd(x)
s_y <- sd(y)

# Keskiarvojen erotuksen piste-estimaatti
pe <- x.ka-y.ka
print(paste("erotuksen piste-estimaatti:", pe))

# Keskivirhe ilman oletusta varianssien yhtäsuuruudesta
se <- sqrt(s_x^2/n_x+s_y^2/n_y)
print(paste("keskivirhe:", se))

# Testisuureen arvo, kun oletetaan nollahypoteesi todeksi
# (H_0: miesten ja naisten keskim. kolesteroliarvot samat)
t_hav <- (x.ka-y.ka)/se
print(paste("t-testisuureen havaittu arvo:", t_hav))

# Kaksipuolinen p-arvo 
# (likiarvo standardinormaalijakaumasta, koska otoskoko suht. on suuri)
p <- 2*(1-pnorm(t_hav))
print(paste("kaksipuolinen p-arvo:", p))

# ---
# 4.)

# b.)
p <- pbinom(3, size=10, prob=0.5, lower.tail=TRUE) + pbinom(6, size=10, prob=0.5, lower.tail=FALSE)
print(paste("kaksipuolinen p-arvo:", p))
