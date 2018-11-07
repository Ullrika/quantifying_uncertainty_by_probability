## Why probabilistic models are good instead of just data
sim <- function(p = 0.3, N = 500, prior_param = c(10,10), ylim=c(0,1),byval=5){
  ## the relative frequency of an undesired event
  # p = 0.3
  ## number of trials in which the event occur or do not occur
  # N = 500
  ## beta prior for p
  # prior_param = c(10,10)
  
  for(i in 1:200){
    ## number of cases where the event has occurred accumulated over the number of trials
    ind_cases = rbinom(N,1,p)
    cases = cumsum(ind_cases)
    
    ## Estimate using probability to characterise uncertainty
    ## beta-binomial model
    posterior_param = cbind(prior_param[1] + cases, prior_param[2] + (1:N) - cases)
    
    E_prior = prior_param[1] / sum(prior_param)
    E_posterior = posterior_param[,1] / rowSums(posterior_param)
    
    dprior = dbeta((1:999)/1000,prior_param[1],prior_param[2])
    dposterior = dbeta((1:999)/1000,posterior_param[N,1],posterior_param[N,2])
    md = max(dprior,dposterior)
    #byval = 5
    
    plot(c(0,N+byval),c(0,1),type='n',
         xlim = c(0,N+byval),lty= 2, ylab = '',
         main = 'estimated relative frequency \n of cases', xlab = 'sample size', ylim = ylim)
    Sys.sleep(1)
    lines(rep(0,999)+dprior/md*byval,(1:999)/1000,col = 'green')
    Sys.sleep(1)
    for(n in seq(2,N,by=byval*2)){
      points(1:n,ind_cases[1:n],cex=1,pch=18)
      if(n>1){
      lines(1:n,cases[1:n]/(1:n),lty=2)}
      Sys.sleep(1)
    #abline(h = E_prior, col = 'green')
      dposterior = dbeta((1:999)/1000,posterior_param[n,1],posterior_param[n,2])
      lines(rep(n,999)+dposterior/md*byval,(1:999)/1000,col = 'blue')
      if(n==1){
        points(n,E_posterior[n], col = 'blue',pch = '+')
      }else{
      lines(1:n,E_posterior[1:n], col = 'blue')}
    Sys.sleep(1)
    }
    abline(h=p,col = 'red', lty = 2)
    Sys.sleep(5)
  }}

########

sim(p = 0.5, N = 15, prior_param = c(1,1),ylim = c(0,1),byval = 2)

sim(p = 0.5, N = 15, prior_param = c(10,10),ylim = c(0,1),byval = 2)

sim(p = 0.5, N = 100, prior_param = c(1,1),ylim = c(0,1),byval = 5)

sim(p = 0.5, N = 100, prior_param = c(10,10),ylim = c(0,1),byval = 5)
