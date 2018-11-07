#################################
## Beta-binomial model 
## Specify prior
prior_param = list(s=2, t=0.5)

## Specify data
#data_to_model = list(a=prior_param$s*t=prior_param$t, b=prior_param$s*(1-prior_param$t), X=0, k=4, K = 88, N2 = 1000)
data_to_model = list(a=prior_param$s*prior_param$t, b=prior_param$s*(1-prior_param$t), X=1, k=2, K = 88, N2 = 1000)

## Update with conjugate properties
p_post_conj = list(a = data_to_model$a + data_to_model$X,
                   b = data_to_model$b + data_to_model$k - data_to_model$X)

## Update with MCMC sampling 
ms = "
model {
  p ~ dbeta(a, b)
  X ~ dbinom(p,k)
} "


m = jags.model(textConnection(ms), data=data_to_model, n.adapt=10^6, n.chains=3)

sam = coda.samples(m, c('p'), n.iter=N, thin=1)
mat = as.matrix(sam)
p_post = mat[,grep('p',colnames(mat))]
plot(density(p_post,from=0,to=1),xlab = 'p', main = 'prior and posterior of \n probability of infection in one item')
hist(p_post,add = TRUE,probability = TRUE, col = 'gray')
lines((1:999)/1000,dbeta((1:999)/1000,p_post_conj$a, p_post_conj$b),col = 'blue',lwd = 2)
lines((1:999)/1000,dbeta((1:999)/1000,prior_param$s*prior_param$t,prior_param$s*(1-prior_param$t)),col = 'red')

###########################################################
## Add prediction 
## 

## Update with MCMC sampling 
ms = "
model {
p ~ dbeta(a,b)
X ~ dbinom(p,k)

for(i in 1:N2){
Xall[i] ~ dbinom(p,K)
}
} "

m = jags.model(textConnection(ms), data=data_to_model, n.adapt=10^6, n.chains=3)

sam = coda.samples(m, c('Xall'), n.iter=N, thin=1)
mat = as.matrix(sam)
## Derive uncertianty in the probability that more than 10% of the items contains an infection
pred_post = rowMeans(mat>(data_to_model$K*0.1))
hist(pred_post)
mean(pred_post)
(pred_post_int = HPDinterval(as.mcmc(pred_post), prob = 0.95))
