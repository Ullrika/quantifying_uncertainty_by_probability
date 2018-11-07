## R-packages needed
## SHELF
#install.packages('SHELF')
library('SHELF')


ej = elicit()
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

## Specify points from the probability distribution directly
v <- c(6, 10, 12) #three quantiles
p <- c(0.25, 0.5, 0.75) #three probabilities
ej <- fitdist(vals = v, probs = p, lower = 0, upper = 30)
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

## Specify points from the probability distribution directly from more than one expert
## this expression inlcude three quantiles from two experts
(v <- matrix(c(6, 10, 12, 8, 9, 10), 3, 2)) 
p <- matrix(c(0.25, 0.5, 0.75, 0.1, 0.4, 0.6),3, 2)
low = c(0,3)
up = c(40, 30)
ej <- fitdist(vals = v, probs = p, lower = low, upper = up)
plotfit(ej,ql = 0.05, qu = 0.95, int = TRUE )
plotfit(ej,ql = 0.05, qu = 0.95, d = 'gamma')

## Elcit by making a rough image of the probability density distribution
## by placing ships to bet on bins
ej = roulette(lower = 0, upper = 30, nbins = 15, gridheight = 10)
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

plotfit(ej, int = TRUE, ql = 0.05, qu = 0.95, d = 'gamma')
feedback(ej, quantiles = c(0.05, 0.95))

## When you are happy with the probability distribution for the mean waiting time for the bus
## you can use it in the risk analysis model from above
