library(foreign)
dat <- read.spss("~/Projects/uni-assignments/R/TILM3702/materials/demoaineisto2015r.sav", to.data.frame=TRUE)
attach(dat)

# 6 ensimmäistä havaintoa (havainnollistaa aineiston rakennetta/kenttiä)
head(dat)

# ---

# Kuvailevat tunnusluvut ryhmittäin (2)
with(dat, tapply(oma_tulo, taltyyt, summary))

# Tukeyn laatikko-janakuvio (2)
boxplot(oma_tulo~taltyyt, main="Tyytyväisyys taloudelliseen tilanteeseen")
# plot(oma_tulo~taltyyt, data=dat, main="Tyytyväisyys taloudelliseen tilanteeseen")

# Normaalijakaumatestaus (Shapiro-Wilkin testi) (2)
with(dat, tapply(oma_tulo, taltyyt, shapiro.test))

# ---

# Hajontojen yhtäsuuruustestaus (3)
bartlett.test(oma_tulo~taltyyt)
 
# Yksisuuntainen varianssianalyysi, hajonnat ovat yhtäsuuret (3)
anova(lm(oma_tulo~taltyyt))

# Yksisuuntainen varianssianalyysi, hajonnat ovat erisuuret (3)
# Koska aiemmin tehty Bartlettin testi osoitti hajontojen kutakuinkin yhtäsuuret, 
# käytetään varianssianalyysissä anova-komentoa oneway.test-komennon sijaan.
# oneway.test(oma_tulo~taltyyt)

# Monivertailu Bonferroni-korjauksella, hajonnat ovat yhtäsuuret (3)
pairwise.t.test(oma_tulo, taltyyt, p.adj="bonferroni")

# Monivertailu Holm-korjauksella, hajonnat ovat erisuuret (3)
# Koska aiemmin tehty Bartlettin testi osoitti hajontojen kutakuinkin yhtäsuuret, 
# käytetään varianssianalyysissä pairwise.t.test-komennon Bonferroni-korjattua
# versiota Holm-korjatun version sijaan.
# pairwise.t.test(oma_tulo, taltyyt, pool.sd=F) 

# ---

# Epäpararmetrinen yksisuuntainen malli sijaintien erolle (4)
kruskal.test(oma_tulo~taltyyt)

# U-testit Holm-korjauksella (4)
pairwise.wilcox.test(oma_tulo, taltyyt)

# Yksinkertaiset kontrastit, LSD (5)
# pairwise.t.test(oma_tulo, taltyyt, p.adj="none")
# Toteutetaan LSD-testit manuaalisesti, jotta Bonferroni-korjaus
# pystytään kohdistamaan 'erittäin tyytymätön' -ryhmään:

# Jaetaan data ryhmiin tyytyväisyyden perusteella
groups <- split(oma_tulo, taltyyt)

# Tehdään yksittäiset t-testit
test1 <- t.test(groups[["erittain tyytymaton"]], groups[["erittain tyytyvainen"]])
test2 <- t.test(groups[["erittain tyytymaton"]], groups[["melko tyytyvainen"]])
test3 <- t.test(groups[["erittain tyytymaton"]], groups[["melko tyytymaton"]])

# Yhdistetään p-arvot
p_vals <- c(test1$p.value, test2$p.value, test3$p.value)

# Tehdään Bonferroni-korjaus
p_adj <- p.adjust(p_vals, method="bonferroni")

print(paste("'erittäin tyytymaton' - 'erittain tyytyvainen':", p_adj[1]))
print(paste("'erittäin tyytymaton' - 'melko tyytyvainen':", p_adj[2]))
print(paste("'erittäin tyytymaton' - 'melko tyytymaton':", p_adj[3]))
