# Ladataan data
data(mtcars)
df2 = mtcars[,c("mpg", "cyl", "disp", "hp", "wt")]
head(df2) # 5 ensimm‰ist‰ havaintoa

# Perustunnuslukuja
colMeans(df2) # keskiarvot
cov(df2) # kovarianssimatriisi
sqrt(diag(cov(df2))) # keskihajonnat
cor(df2) # (Pearsonin) korrelaatiomatriisi
cor(df2, method = "spearman") # Spearmanin korrelaatiomatriisi

# Muuttujan mpg keskiarvot eri sylinterien m‰‰rill‰
mean(df2[df2$cyl==4, "mpg"])
mean(df2[df2$cyl==6, "mpg"])
mean(df2[df2$cyl==8, "mpg"])

# Kuvia
boxplot(df2$mpg ~ df2$cyl) # Muuttujan mpg laatikko-janakuvaaja eri sylinterien m‰‰rill‰
plot(df2) # Muuttujien v‰liset hajontakuviot

plot(x=df2$wt, y=df2$mpg, col = df2$cyl-2, xlab = "weight", ylab = "miles per gallon")
curve(49.93-13.38*x+1.17*x^2, add=TRUE, lty=2)

