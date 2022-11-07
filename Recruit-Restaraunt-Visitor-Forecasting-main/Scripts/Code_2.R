# Code Section 2.1

set.seed(10)
len<-100
mu_1<-1:len/(len/4)
# linear trend
y_1<-mu_1+rnorm(len)
# cyclical component
mu_2<-cos(pi/12*(1:len))
y_2<-mu_2+rnorm(len)
# deterministic shift
mu_3<-c(rep(0,len/2),rep(1,len-len/2))
y_3<-mu_3+rnorm(len)

par(mfrow=c(3,1))
ts.plot(y_1,col="blue",main="Linear trend")
lines(mu_1,col="black")
ts.plot(y_2,col="blue",main="Cycle")
lines(mu_2,col="black")
ts.plot(y_3,col="blue",main="Shift")
lines(mu_3,col="black")


set.seed(10)
len<-100
# White noise
y_1<-rnorm(len)
# Random-walk
y_2<-cumsum(rnorm(len))
# cyclical AR(2)
eps<-rnorm(1000)
x<-rep(0,1000)
a1<-2*cos(pi/6)*0.95
a2<--0.95^2
for (i in 3:1000)
  x[i]<-a1*x[i-1]+a2*x[i-2]+eps[i]
y_3<-x[(1000-len+1):1000]


par(mfrow=c(3,1))
ts.plot(y_1,col="blue",main="Zero-mean white noise")
lines(rep(0,len),col="black")
ts.plot(y_2,col="blue",main="Zero-mean random-walk")
lines(rep(0,len),col="black")
ts.plot(y_3,col="blue",main="Zero-mean stationary (stochastic) cycle")
lines(rep(0,len),col="black")





#----------------------------------------------
# Code section 2.2

set.seed(10)
len<-100
# Random-walk
mu_1<-rep(0,len)
y_1<-cumsum(rnorm(len))
mu_2<-rep(0,len)
# stationary level but inflating variance
y_2<-mu_1+sqrt(1:len)*rnorm(len)
# non-stationary level and variance
mu_3<-1:len/(len/2)
y_3<-mu_3+sqrt(mu_3)*rnorm(len)

par(mfrow=c(3,1))
ts.plot(y_1,col="blue",main="Random-walk")
lines(mu_1,col="black")
ts.plot(y_2,col="blue",main="Constant level, inflating variance")
lines(mu_2,col="black")
ts.plot(y_3,col="blue",main="Variance proportional to level")
lines(mu_3,col="black")



#------------------------------------------------------
# Code section 2.3
set.seed(100)
len<-100
# fixed AR(1)
a1<-0.9
# Time-varying AR(1)-parameter
a1t<-0.9*cos(2*pi*(1:len)/len+pi/2)
y_1<-y_2<-rep(0,len)
eps<-rnorm(len)
# fixed coefficient and time varying AR(1)
for (i in 2:len) #i<-2
{
  y_1[i]<-a1*y_1[i-1]+eps[i]
  y_2[i]<-a1t[i]*y_2[i-1]+eps[i]
}

par(mfrow=c(2,1))
ts.plot(y_1,col="blue",main="Fixed (stationary) dependency")
ts.plot(y_2,col="blue",main="Changing dependency")


#-------------------------------------------------
# Code section 2.4

samples<-1000
set.seed(10)
len<-100
y_1<-matrix(ncol=samples,nrow=len)
y_2<-y_3<-y_1
# Linear trend
mu_1<-1:len/(len/4)
# cyclical component
mu_2<-cos(pi/12*(1:len))
# deterministic shift
mu_3<-c(rep(0,len/2),rep(1,len-len/2))
# Simulations: 1000 replications of data samples
for (i in 1:samples)
{
  y_1[,i]<-mu_1+rnorm(len)
  y_2[,i]<-mu_2+rnorm(len)
  y_3[,i]<-mu_3+rnorm(len)
}

set.seed(10)
len<-100
z_1<-matrix(ncol=samples,nrow=len)
z_2<-z_3<-z_1
for (i in 1:samples)#i<-1
{
  # White noise
  z_1[,i]<-rnorm(len)
  # Random-walk
  z_2[,i]<-cumsum(rnorm(len))
  # cyclical AR(2)
  eps<-rnorm(1000)
  x<-rep(0,1000)
  a1<-2*cos(pi/6)*0.95
  a2<--0.95^2
  for (j in 3:1000)
    x[j]<-a1*x[j-1]+a2*x[j-2]+eps[j]
  z_3[,i]<-x[(1000-len+1):1000]
}


par(mfrow=c(3,1))
ts.plot(apply(y_1,1,mean),col="blue",
        main=paste("Mean of ",samples," realizations: linear trend",sep=""),
        xlab="",ylab="")
lines(mu_1,col="black")
lines(apply(y_1,1,mean)-2*sqrt(apply(y_1,1,var)/samples),col="blue",lty=2)
lines(apply(y_1,1,mean)+2*sqrt(apply(y_1,1,var)/samples),col="blue",lty=2)
ts.plot(apply(y_2,1,mean),col="blue",
        main=paste("Mean of ",samples," realizations: cycle",sep=""),
        xlab="",ylab="")
lines(mu_2,col="black")
lines(apply(y_2,1,mean)-2*sqrt(apply(y_2,1,var)/samples),col="blue",lty=2)
lines(apply(y_2,1,mean)+2*sqrt(apply(y_2,1,var)/samples),col="blue",lty=2)
ts.plot(apply(y_3,1,mean),col="blue",
        main=paste("Mean of ",samples," realizations: shift",sep=""),
        xlab="",ylab="")
lines(mu_3,col="black")
lines(apply(y_3,1,mean)-2*sqrt(apply(y_3,1,var)/samples),col="blue",lty=2)
lines(apply(y_3,1,mean)+2*sqrt(apply(y_3,1,var)/samples),col="blue",lty=2)


par(mfrow=c(3,1))
ymin<-min(apply(z_1,1,mean)-2*sqrt(apply(z_1,1,var)/samples))
ymax<-max(apply(z_1,1,mean)+2*sqrt(apply(z_1,1,var)/samples))
ts.plot(apply(z_1,1,mean),col="blue",main=paste("Mean of ",samples," realizations: white noise",sep=""),
        xlab="",ylab="",ylim=c(ymin,ymax))
lines(rep(0,len),col="black")
lines(apply(z_1,1,mean)-2*sqrt(apply(z_1,1,var)/samples),col="blue",lty=2)
lines(apply(z_1,1,mean)+2*sqrt(apply(z_1,1,var)/samples),col="blue",lty=2)
ymin<-min(apply(z_2,1,mean)-2*sqrt(apply(z_2,1,var)/samples))
ymax<-max(apply(z_2,1,mean)+2*sqrt(apply(z_2,1,var)/samples))
ts.plot(apply(z_2,1,mean),col="blue",main=paste("Mean of ",samples," realizations: random-walk",sep=""),
        xlab="",ylab="",ylim=c(ymin,ymax))
lines(rep(0,len),col="black")
lines(apply(z_2,1,mean)-2*sqrt(apply(z_2,1,var)/samples),col="blue",lty=2)
lines(apply(z_2,1,mean)+2*sqrt(apply(z_2,1,var)/samples),col="blue",lty=2)
ymin<-min(apply(z_3,1,mean)-2*sqrt(apply(z_3,1,var)/samples))
ymax<-max(apply(z_3,1,mean)+2*sqrt(apply(z_3,1,var)/samples))
ts.plot(apply(z_3,1,mean),col="blue",main=paste("Mean of ",samples," realizations: AR(2)-cycle",sep=""),
        xlab="",ylab="",ylim=c(ymin,ymax))
lines(rep(0,len),col="black")
lines(apply(z_3,1,mean)-2*sqrt(apply(z_3,1,var)/samples),col="blue",lty=2)
lines(apply(z_3,1,mean)+2*sqrt(apply(z_3,1,var)/samples),col="blue",lty=2)



#--------------------------------------------------------------------------------