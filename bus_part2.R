################################################################
## Tutorial Quantifying uncertainty by probability 2018
## Ullrika Sahlin
## When should I leave the office to not miss the last bus home?
## The purposes are to demonstrate probabilistic risk analysis, 
## separating between epistemic and aleatory uncertainty, using
## experts to inform parameters of a model, learn from data, and
## provide decision support
## ullrika.sahlin@cec.lu.se
################################################################

## You need to have JAGS on your computer
## Goto http://mcmc-jags.sourceforge.net/
## R-packages needed
## SHELF
#install.packages('SHELF')
library('SHELF')
## rjags
#install.packages('rjags')
library('rjags')

###################################################################
## A risk analysis in which uncertainty in parameters are quantified
## by Expert Knowledge Elicitation
###################################################################

## Ways to elicit the mean waiting time for the bus by specifying 
## points on the probabilty distribution in an interactive model 

ej = elicit()
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

## Specify points from the probability distribution directly
v <- c(3, 4, 12) #three quantiles
p <- c(0.25, 0.5, 0.75) #three probabilities
ej <- fitdist(vals = v, probs = p, lower = 0, upper = 30)
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

## Elcit by making a rough image of the probability density distribution
## by placing ships to bet on bins
ej = roulette(lower = 0, upper = 30, nbins = 15, gridheight = 10)
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

## Specify points from the probability distribution directly from more than one expert
## this expression inlcude three quantiles from two experts
(v <- matrix(c(6, 10, 12, 8, 9, 10), 3, 2)) 
p <- c(0.25, 0.5, 0.75)
ej <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
plotfit(ej,ql = 0.05, qu = 0.95, d = 'gamma' )

#plotfit(ej, int = TRUE, ql = 0.05, qu = 0.95, d = 'gamma')
#feedback(ej, quantiles = c(0.05, 0.95))

## When you are happy with the probability distribution for the mean waiting time for the bus
## you can use it in the risk analysis model from above

# this is the expected values of the parameter values
param = list(expected_waiting_time = ej$Gamma$shape*ej$Gamma$rate, mean_speed = 1.4)

# this is how you sample from the probability distributions of the parameter values
param = list(expected_waiting_time = rgamma(1,ej$Gamma$shape,ej$Gamma$rate), mean_speed = rnorm(1,1.4,0.2))

###################################################################
## Learn about the parameters from data using Bayesian updating and
## priors from EKE
##################################################################

## You have some observations  
obs_waiting_time = c(13, 2, 5, 10, 8)
obs_speed = c(1.234, 1.2, 1.4)

## Let us specify a Bayesian model for this inference problem
ms = "
model {

# priors - for parameters that are to be updated
mean_waiting_time ~ dgamma(shape, rate)
mean_speed ~ dnorm(mean_mean_speed,1)

# transformed parameters
sd_speed = mean_speed/10

# likelihood
for( i in 1 : n_obs_waiting_time ) {
obs_waiting_time[i] ~ dexp( 1/mean_waiting_time ) 
}

for( i in 1 : n_obs_speed ) {
obs_speed[i] ~ dnorm( mean_speed , 1/sd_speed ) 
}

}"


data_to_model = list(shape = ej$Gamma$shape, rate = ej$Gamma$rate, mean_mean_speed = 1.4, 
                     obs_waiting_time = obs_waiting_time, obs_speed = obs_speed, 
                     n_obs_waiting_time = length(obs_waiting_time), n_obs_speed = length(obs_speed))

## Initalise sampling
Bayesian_sampler = jags.model(textConnection(ms), data=data_to_model, 
               n.adapt=10^6, n.chains=3)

## Sample from the posterior
posterior_sample = coda.samples(Bayesian_sampler, 
  c('mean_waiting_time','mean_speed'), n.iter=round(N/3), thin=1)
## This is created to make it easier to execute some commands below
sim = as.data.frame(as.matrix(posterior_sample)) 

## Various things we can do

## Plot to examine the convergence
plot(posterior_sample)

## Derive summary statistics on the MCMC sample
summary(posterior_sample)

## Extract probability intervals
(post_int = HPDinterval(as.mcmc(sim$mean_waiting_time), prob = 0.95))
(post_int = HPDinterval(as.mcmc(sim$mean_speed), prob = 0.95))

## Compare the posterior distribution with prior and data
xx = seq(0.001,20, by = 0.1)
d1 = density(sim$mean_waiting_time)
d2 = dgamma(xx,data_to_model$shape, data_to_model$rate)
plot(range(0,xx,data_to_model$obs_waiting_time),range(0,d1$y,d2),
     type='n',xlab='waiting time',ylab='density',main='prior and posterior')
lines(d1$x,d1$y,col='red')
lines(xx,d2,col='blue')
points(data_to_model$obs_waiting_time,rep(0,data_to_model$n_obs_waiting_time))
legend('topright',c('posterior','prior'),lty=c(1,1),col=c('red','blue'),bty='n')

xx = seq(0.001,3, by = 0.1)
d1 = density(sim$mean_speed)
d2 = dnorm(xx,data_to_model$mean_mean_speed, 1)
plot(range(0,xx,data_to_model$obs_speed),range(0,d1$y,d2),
     type='n',xlab='walking speed',ylab='density',main='prior and posterior')
lines(d1$x,d1$y,col='red')
lines(xx,d2,col='blue')
points(data_to_model$obs_speed,rep(0,data_to_model$n_obs_speed))
legend('topright',c('posterior','prior'),lty=c(1,1),col=c('red','blue'),bty='n')

## Compare the predictive posterior distribution with data
## Here we sample from the predictive posterior without and with considering uncertainty in the parameters

waiting_time = rexp(N,1/mean(sim$mean_waiting_time)) 
plot(density(waiting_time),xlab='waiting time',ylab='density',main='predictive posterior and data')
waiting_time = rexp(dim(sim)[1],1/sim$mean_waiting_time) 
lines(density(waiting_time),col = 'red')
points(data_to_model$obs_waiting_time,rep(0,data_to_model$n_obs_waiting_time))
legend('topright',c('without uncertainty','with uncertainty'),lty=c(1,1),col=c('black','red'),bty='n')


speed = rnorm(N,mean(sim$mean_speed), mean(sim$mean_speed)/10 )
plot(density(speed),xlab='walking speed',ylab='density',main='predictive posterior and data')
speed = rnorm(dim(sim)[1],sim$mean_speed, sim$mean_speed/10 )
lines(density(speed),col = 'red')
points(data_to_model$obs_speed,rep(0,data_to_model$n_obs_speed))
legend('topright',c('without uncertainty','with uncertainty'),lty=c(1,1),col=c('black','red'),bty='n')

#################################################
### Final step
## perform the risk analysis with uncertainty in parameters using the MCMC sample
N = 10^4
distance = 500 # in meters
expected_waiting_time = mean(sim$mean_waiting_time)
waiting_time = rexp(N,1/expected_waiting_time )
speed = rnorm(N,mean(sim$mean_speed),mean(sim$mean_speed)/10)*60

## Time you decide to leave earlier than usual
leave_earlier = seq(0,12,by=0.05) # in minutes
## Calculate the likelihood you miss the bus for different times
calc_risk = function(leave_earlier){mean(((distance/speed) - leave_earlier) > waiting_time)}
risk = unlist(lapply(leave_earlier,calc_risk))
##########
N.mcmc = dim(sim)[1]
save_risk = matrix(0,nrow = N.mcmc, ncol = length(risk)) # matrix to save output
for(i in 1:N.mcmc){
  ## Uncertainty is added to the parameters 
  ## You can change how this is done
  param = list(expected_waiting_time = sim$mean_waiting_time[i], 
               mean_speed = sim$mean_speed[i])
  expected_waiting_time = param$expected_waiting_time # in minutes
  waiting_time = rexp(N,1/expected_waiting_time )
  speed = rnorm(N,param$mean_speed,param$mean_speed/10)*60
  mean((distance/speed) > waiting_time)
  calc_risk = function(leave_earlier){mean(((distance/speed) - leave_earlier) > waiting_time)}
  save_risk[i,] = unlist(lapply(leave_earlier,calc_risk))
}
# Calculate expectation of the probability distribution charactersing
# uncertianty in the probability to miss the bus
plot(range(leave_earlier),range(risk), type = 'n', ylim = c(0,1),xlab = 'time to leave earlier', ylab = 'P( miss the bus )')
ind = sample.int(N.mcmc,100)
for(i in ind){
  lines(leave_earlier,save_risk[i,], type = 'l', col = 'pink', lwd = 0.5)
}
lines(leave_earlier,risk,  col = 'red', lwd = 2)
abline(h = 0.05, col = 'gray', lwd = 2, lty = 2)
segments(x0=max(leave_earlier[risk>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'blue')
text(x=0.25,0.05+0.05,'5%',col='gray')
text(x=max(leave_earlier[risk>0.05]), y=0, paste(round(max(leave_earlier[risk>0.05]),1),'min'), pos = 2, col = 'blue')

Erisk = colMeans(save_risk)
lines(leave_earlier,Erisk, type = 'l', col = 'green', lwd = 2)
segments(x0=max(leave_earlier[Erisk>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'black')
text(x=max(leave_earlier[Erisk>0.05]), y=0.5, paste(round(max(leave_earlier[Erisk>0.05]),1),'min'), pos = 2)

