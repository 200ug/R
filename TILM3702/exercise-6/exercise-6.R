# Harjoitus perustuu Sanchezin kirjan esimerkkiin
# Juana Sanchez (2023): Time Series for Data Scientists, CUP 2023. 

# Rooms-aineisto
rooms <- scan("~/Projects/uni-assignments/R/TILM3702/materials/rooms.txt")
head(rooms); str(rooms)

# Aineisto: Hotellihuoneiden varausten lkm.
par(
  mfrow=c(1, 1),
  font.axis=2, 
  mar=c(5, 5, 5, 5),
  font.main=2,
  font.lab=2
)

rooms.ts <- ts(rooms, start=c(1977, 1), end=c(1990, 12), freq=12)

plot(rooms.ts, ylab="Huoneiden lkm.", xlab="Aika",
     main="Varattujen hotellihuoneiden lukumäärä", 
     lwd=1.5, cex=0.5)

# ---

# Aikasarjan hajottaminen eri komponentteihin (1)
rooms.md <- decompose(rooms.ts, type="mult")
rooms.md

# Multiplikatiivisesta hajotelmasta johtuen kausitasoitettu 
# aikasarja muodostetaan seuraavasti (1)
rooms.seas.adjusted <- rooms/rooms.md$seasonal
plot.ts(rooms.seas.adjusted, 
        main="Kausitasoitettu hotellihuonevarausten määrä", xlab="Aika",
        ylab="Huoneiden lkm.", lwd=1.5, cex=0.5)

# saved.data <- data.frame(rooms.seas.adjusted)
# write.csv(saved.data, "RoomsSA.csv")

# Tähän asti aineiston ja analyysin "alustusta"

# ---

# 1. differenssi kausitasoitetusta aikasarjasta (2)

dy <- diff(rooms.seas.adjusted, 1)
dy
plot(dy)
mean(dy)

# ---

# Autokorrelaation ja ositaisautokorrelaation mukainen arvio siitä,
# mikä malli sopisi alustavasti parhaiten (3)
acf(ts(dy, freq=1), lag=20, main="Estimoitu autokorrelaatiofunktio")
pacf <- pacf(ts(dy, freq=1), lag=20, main="Estimoitu osittaisautokorrelaatiofunktio") 

# Autokorrelaation olemassaolon havainnointi Ljung-Box-testin avulla (4)
Box.test(dy, lag=20, type=c("Ljung-Box"))

# install.packages("forecast")
library(forecast)

# AR(5) & MA(3) -malli (arvot aiempien tehtävien perusteella) (5)
fit.own <- Arima(dy, order=c(5, 0, 3), include.mean=TRUE, include.drift=FALSE, transform.pars=TRUE)  
fit.og

# ARMA(3, 2)-malli vertailukohtana (5)
fit.og <- Arima(dy, order=c(3, 0, 2), include.mean=TRUE, include.drift=FALSE, transform.pars=TRUE)  
fit.og

# ---

# Residuaalien diagnostiset tarkastelut omalle mallille (6.1)

tsdiag(fit.own)
resid <- fit.own$residuals 
acor <- acf(ts(resid), lag=20, type="correlation", 
         main="Residuaalien autokorrelaatiokertoimet", xlab="Viive")

# Ljung-Box-testi residuaaleille (6.1)
Box.test(resid, lag=20, type=c("Ljung-Box"), fitdf=length(fit.own$coef))

# ---

# Residuaalien diagnostiset tarkastelut ARMA(3, 2)-vertailumallille (6.2)

tsdiag(fit.og)
resid <- fit.og$residuals 
acor <- acf(ts(resid), lag=20, type="correlation", 
         main="Residuaalien autokorrelaatiokertoimet", xlab="Viive")

# Ljung-Box-testi residuaaleille (6.2)
Box.test(resid, lag=20, type=c("Ljung-Box"), fitdf=length(fit.og$coef))
