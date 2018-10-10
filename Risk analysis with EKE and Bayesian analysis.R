################################################################
## Tutorial Quantifying uncertainty by probability 2018
## Ullrika Sahlin
## When should I leave the office to not miss the last bus home?
## The purposes are to demonstrate probabilistic risk analysis, 
## separating between epistemic and aleatory uncertainty, using
## experts to inform parameters of a model, learn from data, and
## provide decision support
## This is code in progress. Suggestions for improvement are welcome
## ullrika.sahlin@cec.lu.se
################################################################

## You need to have JAGS on your computer
## Goto http://mcmc-jags.sourceforge.net/
## R-packages needed
## SHELF
## rjags

###################################################################
## A simple risk model 
##################################################################

## Number of samples
N = 10^4

## Assign a value  to the distance 
distance = 540 # in meters

## The time T ~ exponential distribution 
expected_waiting_time = 5 # in minutes

## Simulate a sample from waiting time T
waiting_time = rexp(N,1/expected_waiting_time )

## Simulate possible values on your walking speed in meter/minute
## 1.4 m/s
speed = rnorm(N,1.4,1.4*0.1)*60

plot(range(waiting_time,distance/speed),c(0,1),type='n', ylab = 'cdf', xlab = 'time')
xx = seq(0,max(waiting_time),length.out = N)
lines(xx,pexp(xx,1/expected_waiting_time), col = 'blue')
lines(xx,1-pnorm(distance/xx/60,1.4,1.4*0.1))

## Calculate the likelihood you miss the bus
mean((distance/speed) > waiting_time)

## Time you decide to leave earlier than usual
leave_earlier = seq(0,10,by=0.05) # in minutes
## Calculate the likelihood you miss the bus for different times
calc_risk = function(leave_earlier){mean(((distance/speed) - leave_earlier) > waiting_time)}
risk = unlist(lapply(leave_earlier,calc_risk))
plot(leave_earlier,risk, type = 'l', col = 'red', lwd = 2, ylim = c(0,1))
abline(h = 0.05, col = 'gray', lwd = 2, lty = 2)
segments(x0=max(leave_earlier[risk>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'blue')
text(x=max(leave_earlier[risk>0.05]), y=0, paste(round(max(leave_earlier[risk>0.05]),1),'min'), pos = 2)


plot(range(waiting_time,distance/speed),c(0,1),type='n', ylab = 'cdf', xlab = 'time')
xx = seq(0,max(waiting_time),length.out = N)
lines(xx,pexp(xx,1/expected_waiting_time), col = 'blue')
leave_earlier = 0:5
for(i in 1:length(leave_earlier)){
  lines(xx,1-pnorm(distance/xx/60-leave_earlier[i],1.4,1.4*0.1))
}

###################################################################
## A risk analysis considering uncertainty in parameters
###################################################################

## We have made a function of the risk analysis we just did
risk_analysis = function(param){
  ## Number of samples
  N = 10^4
  ## Assign a value  to the distance 
  distance = 540 # in meters
  ## The time T ~ exponential distribution 
  expected_waiting_time = param$expected_waiting_time # in minutes
  ## Simulate a sample from waiting time T
  waiting_time = rexp(N,1/expected_waiting_time )
  ## Simulate possible values on your walking speed in meter/minute
  ## 1.4 m/s
  speed = rnorm(N,param$mean_speed,param$mean_speed/10)*60
  leave_earlier = seq(0,10,by=0.05) # in minutes
  ## Calculate the likelihood you miss the bus for different times
  calc_risk = function(leave_earlier){mean(((distance/speed) - leave_earlier) > waiting_time)}
  risk = unlist(lapply(leave_earlier,calc_risk))
  return(list(risk=risk,leave_earlier=leave_earlier))
}

## Run the risk analysis for different parameter values and identify when it is best to leave the office
out = risk_analysis(param = list(expected_waiting_time = 10, mean_speed = 1.4))
plot(out$leave_earlier,out$risk, type = 'l', col = 'red', lwd = 2, ylim = c(0,1))
abline(h = 0.05, col = 'gray', lwd = 2, lty = 2)
segments(x0=max(out$leave_earlier[out$risk>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'blue')
leave = max(out$leave_earlier[out$risk>0.05])
mtext(paste('Leave ',max(out$leave_earlier[out$risk>0.05]),' min early!'), 3)
Sys.sleep(0.5)

for(i in 1:25){
out = risk_analysis(param = list(expected_waiting_time = runif(1,7,13), mean_speed = rnorm(1,1.4,0.2)))
lines(out$leave_earlier,out$risk, col = 'red')
segments(x0=max(out$leave_earlier[out$risk>0.05]),y0=-1,y1=0.05, col = 'blue')
leave = c(leave, max(out$leave_earlier[out$risk>0.05]))
Sys.sleep(0.1)
}
lines(density(leave), col = 'blue')

## Compare the time to leave derived without considering uncertainty to the uncertainty 
## in time to leave given uncertainty in parameters 
legend('topright',c(paste('mean = ',round(mean(leave),1)), 
                    paste('95th percentile =',round(quantile(leave,probs = 0.95),1))),bty = 'n')

###################################################################
## A risk analysis in which uncertainty in parameters are quantified
## by Expert Knowledge Eliciation
###################################################################
library(SHELF)

## Ways to elicit the mean waiting time for the bus by specifying 
## points on the probabilty distribution in an interactive model 
ej = elicit()
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

## Specify points from the probability distribution directly
v <- c(6, 10, 12) #three quantiles
p <- c(0.25, 0.5, 0.75) #three probabilities
ej <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

## Specify points from the probability distribution directly from more than one expert
## this expression inlcude three quantiles from two experts
(v <- matrix(c(6, 10, 12, 8, 9, 10), 3, 2)) 
p <- c(0.25, 0.5, 0.75)
ej <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
plotfit(ej,ql = 0.05, qu = 0.95, d = 'gamma' )

## Elcit by making a rough image of the probability density distribution
## by placing ships to bet on bins
ej = roulette(lower = 0, upper = 100, nbins = 15, gridheight = 10)
hist(rgamma(N,ej$Gamma$shape,ej$Gamma$rate))

plotfit(ej, int = TRUE, ql = 0.05, qu = 0.95, d = 'gamma')
feedback(ej, quantiles = c(0.05, 0.95))

## When you are happy with the probability distribution for the mean waiting time for the bus
## you can use it in the risk analysis model from above

## Run the risk analysis considering uncertainty in parameters and identify when it is best to leave the office
out = risk_analysis(param = list(expected_waiting_time = ej$Gamma$shape*ej$Gamma$rate, mean_speed = 1.4))
plot(out$leave_earlier,out$risk, type = 'l', col = 'red', lwd = 2, ylim = c(0,1))
abline(h = 0.05, col = 'gray', lwd = 2, lty = 2)
segments(x0=max(out$leave_earlier[out$risk>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'blue')
leave = max(out$leave_earlier[out$risk>0.05])
mtext(paste('Leave ',max(out$leave_earlier[out$risk>0.05]),' min early!'), 3)
Sys.sleep(0.5)

for(i in 1:25){
  out = risk_analysis(param = list(expected_waiting_time = rgamma(1,ej$Gamma$shape,ej$Gamma$rate), mean_speed = rnorm(1,1.4,0.2)))
  lines(out$leave_earlier,out$risk, col = 'red')
  segments(x0=max(out$leave_earlier[out$risk>0.05]),y0=-1,y1=0.05, col = 'blue')
  leave = c(leave, max(out$leave_earlier[out$risk>0.05]))
  Sys.sleep(0.1)
}
lines(density(leave), col = 'blue')
legend('topright',c(paste('mean = ',round(mean(leave),1)), 
                    paste('95th percentile =',round(quantile(leave,probs = 0.95),1))),bty = 'n')

###################################################################
## Learn about the parameters from data
##################################################################
library(rjags)

## You have some observations  
obs_waiting_time = c(3, 4.4, 2.3, 10, 17, 5.6)
obs_speed = c(1.234, 1.66, 1.4)

## Let us specify a Bayesian model for this inference problem
ms = "
model {

# priors - for parameters that are to be updated
mean_waiting_time ~ dgamma(shape, rate)
mean_speed ~ dnorm(mean_mean_speed,2)

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


data_to_model = list(shape = 0.001, rate = 0.001, mean_mean_speed = 1.4, 
                     obs_waiting_time = obs_waiting_time, obs_speed = obs_speed, 
                     n_obs_waiting_time = length(obs_waiting_time), n_obs_speed = length(obs_speed))

## Initalise sampling
Bayesian_sampler = jags.model(textConnection(ms), data=data_to_model, 
               n.adapt=10^6, n.chains=3)

## Sample from the posterior
posterior_sample = coda.samples(Bayesian_sampler, c('mean_waiting_time','mean_speed'), n.iter=round(N/3), thin=1)
sim = as.data.frame(as.matrix(posterior_sample)) ## This is created to make it easier to execute some commands below

## Various things we can do

## Plot to examine the convergence
plot(posterior_sample)

## Derive summary statistics on the MCMC sample
summary(posterior_sample)

## Extract probability intervals
(post_int = HPDinterval(as.mcmc(sim$mean_waiting_time), prob = 0.95))
(post_int = HPDinterval(as.mcmc(sim$mean_speed), prob = 0.95))

## Compare the posterior distribution with prior and data
par(mfrow = c(2,1))
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
d2 = dnorm(xx,data_to_model$mean_mean_speed, 2)
plot(range(0,xx,data_to_model$obs_speed),range(0,d1$y,d2),
     type='n',xlab='walking speed',ylab='density',main='prior and posterior')
lines(d1$x,d1$y,col='red')
lines(xx,d2,col='blue')
points(data_to_model$obs_speed,rep(0,data_to_model$n_obs_speed))
legend('topright',c('posterior','prior'),lty=c(1,1),col=c('red','blue'),bty='n')

## Compare the predictive posterior distribution with data
## Here we sample from the predictive posterior without and with considering uncertainty in the parameters

par(mfrow = c(2,1))
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


################################################################################################
## Analyse risk after learning about the parameters from data using expert informed priors 
################################################################################################
## first we expand the dat to include the expert elicited prior of the mean_waiting_time
## we also add other parameters in the risk analysis
leave_earlier = seq(5,12,by=0.05) 
data_to_model = list(shape = ej$Gamma$shape, rate = ej$Gamma$rate, mean_mean_speed = 1.4, 
                     obs_waiting_time = obs_waiting_time, obs_speed = obs_speed, 
                     n_obs_waiting_time = length(obs_waiting_time), n_obs_speed = length(obs_speed),
                     N = N, distance = distance, leave_earlier = leave_earlier, n_alt = length(leave_earlier))

## now we expand the Bayesian model so it also performs predictions
ms = "
model {

# priors - for parameters that are to be updated
mean_waiting_time ~ dgamma(shape, rate)
mean_speed ~ dnorm(mean_mean_speed,2)

# transformed parameters
sd_speed = mean_speed/10

# likelihood
for( i in 1 : n_obs_waiting_time ) {
obs_waiting_time[i] ~ dexp( 1/mean_waiting_time ) 
}

for( i in 1 : n_obs_speed ) {
obs_speed[i] ~ dnorm( mean_speed , 1/sd_speed ) 
}

# prediction
  # forward simulation using N iterations (corresponds to a MC simulation at every iteration of the MCMC)
  for(i in 1:N){
    waiting_time[i] ~ dexp( 1/mean_waiting_time )
    speed[i] ~ dnorm( mean_speed , 1/sd_speed ) 
  }
  for(j in 1:n_alt){
    test[j,1:N] = step((distance/speed/60) - leave_earlier[j] - waiting_time)
    rel_freq[j] = mean(test[j,1:N])# fraction of times the bus is missed in the MC simulation
  }
}"

## Initalise sampling
Bayesian_sampler = jags.model(textConnection(ms), data=data_to_model, 
                              n.adapt=10^6, n.chains=3)

## Sample from the posterior
posterior_sample = coda.samples(Bayesian_sampler, c('mean_waiting_time','mean_speed'), n.iter=round(N/3), thin=1)
## Check convergence
plot(posterior_sample)
sim = as.data.frame(as.matrix(posterior_sample)) ## This is created to make it easier to execute some commands below

par(mfrow = c(2,1))
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

## Sample from the predictive posterior of the output from the risk analysis
predictive_sample = coda.samples(Bayesian_sampler, c('rel_freq'), n.iter=round(N/200), thin=1)
summary(predictive_sample)
pred = as.data.frame(as.matrix(predictive_sample)) ## This is created to make it easier to execute some commands below
pred_mean = colMeans(pred)
pred_int = as.data.frame(HPDinterval(as.mcmc(pred), prob = 0.90))

## Plot the results
par(mfrow = c(1,1))
plot(range(0,data_to_model$leave_earlier),range(0,pred),type='n',xlab='leave_earlier',ylab='risk')
lines(data_to_model$leave_earlier,pred_int$lower,lty = 2)
lines(data_to_model$leave_earlier,pred_int$upper,lty = 2)
lines(data_to_model$leave_earlier,pred_mean, col = 'black')
abline(h = 0.05, col = 'gray', lwd = 2, lty = 2)
segments(x0=max(data_to_model$leave_earlier[pred_mean>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'blue')
leave = max(data_to_model$leave_earlier[pred_mean>0.05])
mtext(paste('Leave',leave,'min early!'), 3, col ='blue')
segments(x0=max(data_to_model$leave_earlier[pred_int$upper>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'dark red')
leave_safe_side = max(data_to_model$leave_earlier[pred_int$upper>0.05])
text(6,max(pred),paste('To be on the safe side, leave',leave_safe_side,'min early!'), col ='dark red')
legend('left',c('Expected risk','90% probability interval'),lty = c(1,2), col = c(1,1),bty='n')

