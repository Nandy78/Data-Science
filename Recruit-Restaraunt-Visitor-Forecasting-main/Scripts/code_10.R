
# Section 10.2


# Exercise 1

set.seed(10)
sigma<-1
len<-10000
alpha_1<-0.9
z<-arima.sim(n = len, list( ar=c(rep(0,11),alpha_1)), sd = sigma)
# we pick out the last 120 observation (the first 880 are used for initialization)
y<-z[(len-120+1):len]
par(mfrow=c(2,1))
ts.plot(y,xlab="",ylab="",main="Stationary seasonal SAR(1): realization and sample acf")
acf(y)



set.seed(10)
sigma<-1
anzsim<-1000
len<-1000
alpha_1<-0.9
acf_len<-24
acf_sim<-matrix(nrow=anzsim,ncol=acf_len)
# 1000 replications of the SAR(1)
for (i in 1:anzsim)
{
  z_sim<-arima.sim(n = len, list( ar=c(rep(0,11),alpha_1)), sd = sigma)
  # we pick out the last 120 observation (the first 880 are used for initialization)
  y_sim<-z_sim[(len-120+1):len]
  # We compute the sample acf for eacg realization
  acf_sim[i,]<-acf(y_sim,plot=F,lag.max=acf_len)$acf[-1]
}
# Taking mean
mean_acf<-apply(acf_sim,2,mean)
mean_acf
# Taking RMSE (root mean-square error)
sigma_acf<-sqrt(apply(acf_sim^2,2,mean))
# We discard multiples of 12
sigma_acf[c(12,24)]<-NA
par(mfrow=c(2,1))
ts.plot(y,xlab="",ylab="",main="Stationary seasonal AR(1): realization and sample acf with empirical
        confidence band")
acf_y<-acf(y,plot=F,lag.max=acf_len)$acf[-1]
ts.plot(acf_y,ylim=c(min(-2*na.exclude(sigma_acf)),max(acf_y)))
lines(-2*sigma_acf,col="blue",lty=2)
lines(2*sigma_acf,col="blue",lty=2)



# Exercise 2


mat_acf<-matrix(acf(y,plot=F,lag.max=60)$acf[-1],ncol=5)
dimnames(mat_acf)[[2]]<-c("first year","second year",
                          "third year","fourth year","fifth year")
dimnames(mat_acf)[[1]]<-paste("Lag ",1:12,sep="")
mat_acf


p<-0
d<-0
q<-0
P<-1
D<-0
Q<-0
s<-12
y_obj<-arima(y, order = c(p, d, q),seasonal = list(order = c(P, D, Q), period = s))
tsdiag(y_obj)

# Exercise 3


h<-120
ymin<-min(predict(y_obj,n.ahead=h)$pred-2*predict(y_obj,n.ahead=h)$se)
ymax<-max(predict(y_obj,n.ahead=h)$pred+2*predict(y_obj,n.ahead=h)$se)
ts.plot(c(y,rep(NA,h)),xlab="",ylab="",main="Multi steps ahead interval forecast",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(y)),predict(y_obj,n.ahead=h)$pred),col="blue")
lines(c(rep(NA,length(y)),predict(y_obj,n.ahead=h)$pred-2*predict(y_obj,n.ahead=h)$se),
      col="blue",lty=2)
lines(c(rep(NA,length(y)),predict(y_obj,n.ahead=h)$pred+2*predict(y_obj,n.ahead=h)$se),
      col="blue",lty=2)

# Exercise 4



set.seed(10)
sigma<-1
len<-1000
beta_1<-0.9
y<-arima.sim(n = len, list( ma=c(rep(0,11),beta_1)), sd = sigma)
# Compute integration
zh<-y
for (i in 13:len)
{
  zh[i]<-zh[i-12]+y[i]
}
# Discard the first 880 observations which are used for initialization
z_1<-zh[(len-120+1):len]
# Generate the second realization with beta_1=-0.9
set.seed(10)
sigma<-1
len<-1000
beta_1<--0.9
y<-arima.sim(n = len, list( ma=c(rep(0,11),beta_1)), sd = sigma)
# Compute integration
zh<-y
for (i in 13:len)
{
  zh[i]<-zh[i-12]+y[i]
}
# Discard the first 880 observations which are used for initialization
z_2<-zh[(len-120+1):len]
par(mfrow=c(2,1))
ts.plot(z_1,xlab="",ylab="",main="Non-Stationary seasonal SARIMA(0,1,1): beta_1=0.9")
ts.plot(z_2,xlab="",ylab="",main="Non-Stationary seasonal SARIMA(0,1,1): beta_1=-0.9")

# Exercise 5

p<-0
d<-0
q<-0
P<-0
D<-1
Q<-1
s<-12
y_obj<-arima(z_1, order = c(p, d, q),seasonal = list(order = c(P, D, Q), period = s))
h<-120
ymin<-min(predict(y_obj,n.ahead=h)$pred-2*predict(y_obj,n.ahead=h)$se)
ymax<-max(predict(y_obj,n.ahead=h)$pred+2*predict(y_obj,n.ahead=h)$se)
ts.plot(c(z_1,rep(NA,h)),xlab="",ylab="",main="Multi steps ahead interval forecast",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(z_1)),predict(y_obj,n.ahead=h)$pred),col="blue")
lines(c(rep(NA,length(z_1)),predict(y_obj,n.ahead=h)$pred-2*predict(y_obj,n.ahead=h)$se),
      col="blue",lty=2)
lines(c(rep(NA,length(z_1)),predict(y_obj,n.ahead=h)$pred+2*predict(y_obj,n.ahead=h)$se),
      col="blue",lty=2)

# Generate the first realization with beta_1=0.9
set.seed(10)
sigma<-1
len<-10000
beta_1<-0.9
y<-arima.sim(n = len, list( ma=c(rep(0,11),beta_1)), sd = sigma)
# Compute integration
zh<-y
for (i in 13:len)
{
  zh[i]<-zh[i-12]+y[i]
}
z_1<-zh[(10*120+1):(11*120)]
z_2<-zh[(80*120+1):(81*120)]

par(mfrow=c(2,1))
ts.plot(z_1,xlab="",ylab="",main="SARIMA(0,1,1): data-window from 1201:1320")
ts.plot(z_2,xlab="",ylab="",main="SARIMA(0,1,1): data-window from 9601:9720
        (same realization)")