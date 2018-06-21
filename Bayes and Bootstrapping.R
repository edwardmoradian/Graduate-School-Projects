#Part A

gammasample=c(4,1,3,1,3,3,3,1,1,1,6,4,4,2,2,1,1,4,2,5)
sample_mean=mean(gammasample) #store sample mean
set.seed(120)
B=5000 #number of bootstrap samples
N=length(gammasample)
bss=sample(gammasample,B*N,replace=T) #choosing the samples
bss_matrix=matrix(bss,nrow=B) #storing in a matrix
bs_mean=rowMeans(bss_matrix) #computing the bootstrap mean for each of the samples
hist(bs_mean)

bci=quantile(bs_mean, c(.025, .975))
bci #95% CI for Symmetric distribution

#Part ii,iii)

binwidth=.005
theta=seq(0.1, 8, 0.01)
a=2
b=1
y=sum(gammasample)
ptheta=dgamma(theta,a,b)
N=length(gammasample) 

pdatagiventheta=exp(-N*theta)*theta^(sum(gammasample))/prod(factorial(gammasample)) 
pthetagivendata=dgamma(theta,a+y,N+b)  

windows(10,10)
layout(matrix(c(1,2,3),nrow = 3, ncol = 1, byrow = FALSE))
maxy=max(c(ptheta,pthetagivendata))


plot(theta, ptheta, type="l", lwd = 3, main = "Prior")
plot(theta, pdatagiventheta, type="l", lwd = 3, main = "Likelihood")
plot(theta, pthetagivendata, type="l", lwd = 3, main = "Posterior")

pthetagivendata
mean(pthetagivendata)

#Part iv)

priorsample <-sample(ptheta, 300, replace=FALSE)
posteriorsample <-sample(pthetagivendata, 300, replace=FALSE)

plot(density(priorsample),main="Density Plot", col="Blue")
lines(density(posteriorsample),main="Density Plot", col="Green")
lines(density(pdatagiventheta),main="Density Plot", col="Red")

