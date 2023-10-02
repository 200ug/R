# A certain microscope is used for imaging cancer cells cultured in a laboratory. 
# In order to obtain the best imaging results, high signal-to-noise ratio is 
# pursued in the luminance intensities. This ratio is defined as the average 
# intensity (μ) divided by the standard deviation of the intensities (σ): μ/σ

# Five different lenses are considered for the study. Using simulations, show 
# which one of the following five lences is the optimal choice in terms of the
# signal-to-noise ratio, when their luminance intensities are reported to be 
# distributed as follows:

#  Beta(4, 4)
#  Beta(9, 2)
#  Beta(0.5, 7)
#  Beta(9, 5)
#  Beta(2, 7)

set.seed(1234)
n = 10000 # number of simulations

# luminance intensities being tested:
i1 = rbeta(n, 4, 4)
i2 = rbeta(n, 9, 2)
i3 = rbeta(n, 0.5, 7)
i4 = rbeta(n, 9, 5)
i5 = rbeta(n, 2, 7)

# signal-to-noise ratios
stnr1 = mean(i1 / sd(i1))
stnr2 = mean(i2 / sd(i2))
stnr3 = mean(i3 / sd(i3))
stnr4 = mean(i4 / sd(i4))
stnr5 = mean(i5 / sd(i5))

print(data.frame(Lens=1:5, STNR = c(stnr1, stnr2, stnr3, stnr4, stnr5)))
