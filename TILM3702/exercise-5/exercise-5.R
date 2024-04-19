# 6. harjoitus: Monimuuttujamenetelmät

library(foreign)
dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/data_demo6_2019r.sav", to.data.frame=TRUE)
attach(dat)

# Uusi data, jossa ainoastaan hyötymuuttujat (10 kpl)
data_pca <- dat[,2:length(dat)]

# Korrelaatiokertoimet
data.kor2 <- cor(data_pca, method="pearson", use="complete.obs")
data.kor2

# Korrelaatiokertoimien p-arvot
install.packages("Hmisc")
library(Hmisc)
res2 <- rcorr(as.matrix(data_pca))
res2

# Korrelaatiokertoimet ja p-arvot rinnakkain
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  
  data.frame(
    row=rownames(cormat)[row(cormat)[ut]],
    column=rownames(cormat)[col(cormat)[ut]],
    cor=(cormat)[ut],
    p=pmat[ut]
  )
}
res3 <- rcorr(as.matrix(data_pca[,1:10]))
flattenCorrMatrix(res3$r, res3$P)

# Korrelaatioiden voimakkuudet kuviona
install.packages("corrplot")
library(corrplot)
corrplot(data.kor2, type="upper", order="hclust", tl.col="black", tl.srt=45)

# ---

# Korrelaatiomatriisin sopivuus pääkomponenttianalyysiin
install.packages("psych")
library(psych)
cortest.bartlett(data.kor2, n=53, diag=TRUE)
KMO(data.kor2)

# ---

# Pääkomponenttianalyysi
pca <- prcomp(data.kor2, center=T, scale=T)
pca
summary(pca)

# Ominaisarvot
(pca$sdev)^2

# Scree plot
screeplot(pca, type="lines") + abline(h=1, lty=2)

# ---

# Valitaan kolme pääkomponenttia (varimax-rotaatio)
pca.chosen <- pca$rotation[,1:3]
pca.varimax <- varimax(pca.chosen)
pca.varimax

# Valitaan kolme pääkomponenttia (promax-rotaatio)
pca.chosen <- pca$rotation[,1:3]
pca.promax <- promax(pca.chosen)
pca.promax

# Miten pääkomponentit selittävät muuttujia
p <- ncol(data_pca)
n <- nrow(data_pca)
e <- eigen(data.kor2) 
L <- e$values                   # Sij. ominaisarvot muuttujaan L
Vm <- matrix(0, nrow=p, ncol=p) # Luodaan p x p -matriisi (arvoina nollia)

# Vm on ortogonaalinen matriisi, koska kaikki korrelaatiot muuttujien välillä ovat 0
diag(Vm) <- L # Asetetaan ominaisarvot diagonaaliakselille
Vm            # Tarkistus: Matriisi, jossa ominaisarvot ovat diagonaaliakselilla

comp.matrix <- e$vectors%*%sqrt(Vm) # Kutsutaan P matriisiksi
#or eigenvectors x sqrt(Vm): P %*% t(P) is equal to the R matrix.

# Tai ominaisvektorit x sqrt(Vm): P %*% t(P) on yhtä suuri kuin R-matriisi
comp.matrix

# Pääkomponentin 1 selittämä osuus muuttujien vaihtelusta
comp.matrix[,1]^2 

# Pääkomponentin 2 selittämä osuus muuttujien vaihtelusta
comp.matrix[,2]^2

# Pääkomponentin 3 selittämä osuus muuttujien vaihtelusta
comp.matrix[,3]^2

# Kommunaliteetit
comp.matrix[,1]^2+comp.matrix[,2]^2+comp.matrix[,3]^2 

# ---

# Pääkomponenttien keskinäiset korrelaatiot
library(foreign)
dat2 <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/data_demo6pc_2019r.sav", to.data.frame=TRUE)
attach(dat2)

cor.test(apuvalineet, media, method="pearson")
cor.test(apuvalineet, laheiset, method="pearson")
cor.test(media, laheiset, method="pearson")

# ---

# K-means klusterointi pääkomponenttipistemäärillä
dat3 <- dat2[,c(12, 13, 14)]

# 2 klusteria
set.seed(7)
km2 <- kmeans(dat3, 2, nstart=100)
km2

# 3 klusteria
set.seed(7)
km3 <- kmeans(dat3, 3, nstart=100)
km3

# 4 klusteria
set.seed(7)
km4 <- kmeans(dat3, 4, nstart=100)
km4

# 5 klusteria
set.seed(7)
km5 <- kmeans(dat3, 5, nstart=100)
km5
