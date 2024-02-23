# Tämän kerran tehtävissä ei varsinaisesti tarvita R:ää, mutta
# sillä voidaan alla olevan mukaisesti tarkistaa käsin laskettuja arvoja.

# t.1)

f <- c(10, 11, 11, 8, 12, 19, 19) # havaitut frekvenssit
p <- c(1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7) # H_0 mukaiset tn.:t

chisq.test(f, p=p)

# t.2)

# Sama aineisto kuin tehtävässä 1, mutta nyt luokkia on vain 2 kpl
# (arkipäivät & viikonloppu).

f <- c(10+11+11+8+12, 19+19) # havaitut frekvenssit
p <- c(5/7, 2/7)

chisq.test(f, p=p)

# t.3)

tab <- matrix(c(18, 28, 14, 20, 51, 28, 12, 25, 9), ncol=3, byrow=TRUE)
chisq.test(tab)
