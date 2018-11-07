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


###################################################################
## Probabilty of exceeding a treshold
## Missing the last bus home
##################################################################
    

  param = list(expected_waiting_time = 5, mean_speed = 1.4)
  ## Number of samples
  N = 10^4
  
  ## Assign a value  to the distance 
  distance = 500 # in meters
  
  ## The time T ~ exponential distribution 
  ## expected_waiting_time = 5 # in minutes
  expected_waiting_time = param$expected_waiting_time # in minutes
  
  ## Simulate a sample from waiting time T
  waiting_time = rexp(N,1/expected_waiting_time )
  
  ## Simulate possible values on your walking speed in meter/minute
  ## 1.4 m/s
  # speed = rnorm(N,1.4,1.4/10)*60
  speed = rnorm(N,param$mean_speed,param$mean_speed/10)*60
  
  ## Calculate the likelihood you miss the bus
  mean((distance/speed) > waiting_time)
  
  ## Time you decide to leave earlier than usual
  leave_earlier = seq(0,12,by=0.05) # in minutes
  ## Calculate the likelihood you miss the bus for different times
  calc_risk = function(leave_earlier){mean(((distance/speed) - leave_earlier) > waiting_time)}
  risk = unlist(lapply(leave_earlier,calc_risk))
  plot(leave_earlier,risk, type = 'l', col = 'red', lwd = 2, ylim = c(0,1),xlab = 'time to leave earlier', ylab = 'P( miss the bus )')
  abline(h = 0.05, col = 'gray', lwd = 2, lty = 2)
  segments(x0=max(leave_earlier[risk>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'blue')
  text(x=0.25,0.05+0.05,'5%',col='gray')
  text(x=max(leave_earlier[risk>0.05]), y=0, paste(round(max(leave_earlier[risk>0.05]),1),'min'), pos = 2, col = 'blue')
  
  
#################################################
## Redo with uncertainty in parameters
##########
  # number of samples from the probability distributions representing 
  # uncertianty in parameters
  N2 = 25 
  save_risk = matrix(0,nrow = N2, ncol = length(risk)) # matrix to save output
for(i in 1:N2){
  ## Uncertainty is added to the parameters 
  ## You can change how this is done
  param = list(expected_waiting_time = runif(1,3,10), mean_speed = rnorm(1,1.4,0.2))
  expected_waiting_time = param$expected_waiting_time # in minutes
  waiting_time = rexp(N,1/expected_waiting_time )
  speed = rnorm(N,param$mean_speed,param$mean_speed/10)*60
  mean((distance/speed) > waiting_time)
  calc_risk = function(leave_earlier){mean(((distance/speed) - leave_earlier) > waiting_time)}
  save_risk[i,] = unlist(lapply(leave_earlier,calc_risk))
  lines(leave_earlier,save_risk[i,], type = 'l', col = 'pink', lwd = 0.5)
}
  # Calculate expectation of the probability distribution charactersing
  # uncertianty in the probability to miss the bus
  Erisk = colMeans(save_risk)
  lines(leave_earlier,Erisk, type = 'l', col = 'green', lwd = 2)
  segments(x0=max(leave_earlier[Erisk>0.05]),y0=-1,y1=0.05, lwd = 4, col = 'black')
  text(x=max(leave_earlier[Erisk>0.05]), y=0.5, paste(round(max(leave_earlier[Erisk>0.05]),1),'min'), pos = 2)
  
