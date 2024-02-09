# t. 1.)

x <- c(3.8, 4.0, 2.8, -4.3, -1.5, 0.8, 2.5, 3.1, 1.9, -6.0, -0.6, -16.6)
y <- c(1.9, 2.2, 4.2, -4.6, -1.7, -1.1, 0.3, 3.9, 0.3, -8.5, 0.3, -20.6)

# Erotukset:
d <- x - y
hist(d, breaks=20, main="Histogrammi lämpötilojen erotuksista")

# Erotusten otoskeskiarvo:
d_mean <- mean(d)
print(paste("erotusten keskiarvo: ", d_mean))

# 95 %:n luottamusvälin muodostus:
qqnorm(d, main="Normaalikvantiilikuvio")  # tutkitaan normaalisuutta normaalikvantiilikuvion avulla
s_d <- sd(d)                              # otoskeskihajonta
n <- length(d)
alpha <- 0.05
t <- qt(1-(alpha/2), n-1)                 # t-jakauman kvantiili vapausasteella n-1
theta_l <- d_mean-t*s_d/sqrt(n)           # luottamusvälin alaraja
theta_u <- d_mean+t*s_d/sqrt(n)           # luottamusvälin yläraja
c(theta_l, theta_u)

# --- 
# t. 2.)

terveys <- read.csv("~/Projects/uni-assignments/R/TILM3555/materials/terveys.csv", sep=";")

terveys$bmi <- terveys$paino/(terveys$pituus/100)^2
x <- subset(terveys, sukupuoli=="Mies")$bmi
y <- subset(terveys, sukupuoli=="Nainen")$bmi

# a.)

# Miesten havaintojen lkm., otoskeskiarvo ja -hajonta
n_x <- length(x)
x_mean <- mean(x)
s_x <- sd(x)
print(paste("x (miehet):", n_x, x_mean, s_x))

# Naisten havaintojen lkm., otoskeskiarvo ja -hajonta
n_y <- length(y)
y_mean <- mean(y)
s_y <- sd(y)
print(paste("y (naiset):", n_y, y_mean, s_y))

# b.)

# Vapausasteet Welch-Satterthwaite-approksimaatiolla:
v <- (s_x^2/n_x+s_y^2/n_y)^2 / (s_x^4/(n_x^2*(n_x-1))+s_y^4/(n_y^2*(n_y-1)))

# T-jakauman kvantiili 95 % luottamustasolla
alpha <- 0.05
t <- qt(1-(alpha/2), v)            # t-kvantiili aiemmin approksimoidulla vapausasteella
se=sqrt(s_x^2/n_x+s_y^2/n_y)
theta_l <- (x_mean-y_mean)-t*se    # luottamusvälin alaraja
theta_u <- (x_mean-y_mean)+t*se    # luottamusvälin yläraja
c(theta_l, theta_u)

# ---
# t. 4.)

# Tehtävänannon arvot:
x_mean <- 43.9
s <- 3.8
n <- 6

# Laatuvaatimus
mu_0 <- 40

# t-testisuure:
t <- (x_mean-mu_0)/(s/sqrt(n))

# p-arvo (havaittu merkitsevyystaso):
p <- 1-pt(t, n-1)                # pt-funktio antaa kertymäfunktion arvon t-jakaumalle

print(paste("t-testisuure:", t))
print(paste("p-arvo:", p))
