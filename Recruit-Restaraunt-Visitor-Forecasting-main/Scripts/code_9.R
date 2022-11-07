
# Section 9.2.2

set.seed(10)
par(mfrow=c(3,1))
ts.plot(cumsum(rnorm(100)),main="Random-Walk: length 100")
set.seed(10)
ts.plot(cumsum(rnorm(1000)),main="Random-Walk: length 1000")
set.seed(10)
ts.plot(cumsum(rnorm(100000)),main="Random-Walk: length 100'000")


#----------------------------------------------------------------------------------------------
# Section 9.3.2

set.seed(10)
len<-1000
x<-arima.sim(list(order = c(0,1,0)), n = len)
y_1<-x
len_1<-len
y_2<-diff(x)
len_2<-len-1
aucf<-matrix(ncol=2,nrow=30)
for (i in 0:29)
{
  aucf[i+1,1]<-(y_1[(1+i):len_1]-mean(y_1))%*%(y_1[1:(len_1-i)]-mean(y_1))/
    sum((y_1[1:len_1]-mean(y_1))^2)
  aucf[i+1,2]<-(y_2[(1+i):len_2]-mean(y_2))%*%(y_2[1:(len_2-i)]-mean(y_2))/
    sum((y_2[1:len_2]-mean(y_2))^2)
}
par(mfrow=c(2,1))
ts.plot(x,main="Random-Walk ARIMA(0,1,0)")
lines(c(0,diff(x)),col="blue")
mtext("Random-walk", side = 3, line = -1,at=len/2,col="black")
mtext("Differences", side = 3, line = -2,at=len/2,col="blue")
plot(aucf[,1],xlab="",ylab="",ylim=c(min(aucf[,2]),max(aucf[,2])),type="l",axes="F",main="Acf")
lines(aucf[,2],col="blue")
mtext("Acf random-walk", side = 3, line = -1,at=15,col="black")
mtext("Acf differences", side = 3, line = -2,at=15,col="blue")
axis(1,at=1:length(aucf[,1]),labels=-1+1:length(aucf[,1]))
axis(2)
box()



set.seed(10)
len<-1000
y_1<-arima.sim(list(order = c(0,2,0)), n = len)
len_1<-len
y_2<-diff(y_1)
len_2<-len-1
y_3<-diff(y_2)
len_3<-len_2-1
par(mfrow=c(2,1))
ts.plot(y_1,main="Integrated Random-Walk: ARIMA(0,2,0)",ylab="",xlab="")
ts.plot(y_2,main="First and second order differences of ARIMA(0,2,0)",
        col="blue",ylab="",xlab="")
lines(c(0,y_3),col="red")
mtext("First differences", side = 3, line = -1,at=len/2,col="blue")
mtext("Second differences", side = 3, line = -2,at=len/2,col="red")



set.seed(10)
len<-1000
b_1<--0.95
b_2<--0.7
y_1<-arima.sim(list(order = c(0,1,1),ma=b_1), n = len)
y_2<-arima.sim(list(order = c(0,1,1),ma=b_2), n = len)
len_1<-len
len_2<-len
ts.plot(y_2,main="ARIMA(0,1,1)",ylab="",xlab="",col="red")
lines(y_1,col="blue")
mtext(paste("b_1=",b_1,sep=""), side = 3, line = -1,at=len/2,col="blue")
mtext(paste("b_1=",b_2,sep=""), side = 3, line = -2,at=len/2,col="red")


#-------------------------------------------------------------------------------------------------
# Section 9.3.3

set.seed(10)
len<-1000
eps<-rnorm(len)
x_1<-rep(0,len)
for (i in 13:len)
  x_1[i]<-x_1[i-12]+eps[i]
x<-x_1[(len-120+1):len]
par(mfrow=c(2,1))
ts.plot(x,main="SARIMA(0,1,0)",ylab="",xlab="",col="red")
acf(x,lag.max=36)


polyroot(c(1,rep(0,11),-1))

abs(polyroot(c(1,rep(0,11),-1)))
2*pi/Arg(polyroot(c(1,rep(0,11),-1)))

#---------------------------------------------------------------------------------------------
# Section 9.5


# Exercise 1
set.seed(10)
sigma<-1
len<-100
a_1<-1.8
a_2<--0.9
a_vec<-c(a_1,a_2)
x<-arima.sim(n = len, list(order = c(2,1,0), ar = a_vec), sd = sigma)
par(mfrow=c(2,1))
ts.plot(x,xlab="",ylab="",main="Non-stationary ARIMA(1,1,0)")
ts.plot(diff(x),xlab="",ylab="",main="Stationary AR(2) differences")



# Exercise 2

x_obj<-arima(x,order=c(2,1,0))
tsdiag(x_obj)

# Exercise 3

h<-100
ymin<-min(predict(x_obj,n.ahead=h)$pred-2*predict(x_obj,n.ahead=h)$se)
ymax<-max(predict(x_obj,n.ahead=h)$pred+2*predict(x_obj,n.ahead=h)$se)
ts.plot(c(x,rep(NA,h)),xlab="",ylab="",
        main="ARIMA(2,1,0): multi steps ahead interval forecast ",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(x)),predict(x_obj,n.ahead=h)$pred),col="blue")
lines(c(rep(NA,length(x)),predict(x_obj,n.ahead=h)$pred-2*predict(x_obj,n.ahead=h)$se),
      col="blue",lty=2)
lines(c(rep(NA,length(x)),predict(x_obj,n.ahead=h)$pred+2*predict(x_obj,n.ahead=h)$se),
      col="blue",lty=2)

# Exercise 4
y<-diff(x)
y_obj<-arima(y,order=c(2,0,0),include.mean=F)
par(mfrow=c(2,1))
ymin<-min(predict(y_obj,n.ahead=h)$pred-2*predict(y_obj,n.ahead=h)$se)
ymax<-max(predict(y_obj,n.ahead=h)$pred+2*predict(y_obj,n.ahead=h)$se)
ts.plot(c(y,rep(NA,h)),xlab="",ylab="",
        main="AR(2): multi steps ahead interval forecast ",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(y)),predict(y_obj,n.ahead=h)$pred),col="blue")
lines(c(rep(NA,length(y)),predict(y_obj,n.ahead=h)$pred-2*predict(y_obj,n.ahead=h)$se),
      col="blue",lty=2)
lines(c(rep(NA,length(y)),predict(y_obj,n.ahead=h)$pred+2*predict(y_obj,n.ahead=h)$se),
      col="blue",lty=2)
# Integrate forecasts
ymin<-min(c(x,x[length(x)]+cumsum(predict(y_obj,n.ahead=h)$pred)))
ymax<-max(c(x,x[length(x)]+cumsum(predict(y_obj,n.ahead=h)$pred)))
ts.plot(c(x,rep(NA,h)),xlab="",ylab="",main="Cusum of AR(2) forecasts",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(y)),x[length(x)]+cumsum(predict(y_obj,n.ahead=h)$pred)),col="blue")


# Exercise 5

set.seed(10)
len<-100
x<-cumsum(rnorm(len))
z<-cumsum(x)
x_obj<-arima(x,order=c(0,1,0),include.mean=F)
z_obj<-arima(z,order=c(0,2,0),include.mean=F)
par(mfrow=c(2,1))
# random walk
ymin<-min(predict(x_obj,n.ahead=h)$pred-2*predict(x_obj,n.ahead=h)$se)
ymax<-max(predict(x_obj,n.ahead=h)$pred+2*predict(x_obj,n.ahead=h)$se)
ts.plot(c(x,rep(NA,h)),xlab="",ylab="",
        main="ARIMA(0,1,0): multi steps ahead interval forecast ",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(x)),predict(x_obj,n.ahead=h)$pred),col="blue")
lines(c(rep(NA,length(x)),predict(x_obj,n.ahead=h)$pred-2*predict(x_obj,n.ahead=h)$se),
      col="blue",lty=2)
lines(c(rep(NA,length(x)),predict(x_obj,n.ahead=h)$pred+2*predict(x_obj,n.ahead=h)$se),
      col="blue",lty=2)
# Integrated random-walk
ymin<-min(c(z,predict(z_obj,n.ahead=h)$pred-2*predict(z_obj,n.ahead=h)$se))
ymax<-max(c(z,predict(z_obj,n.ahead=h)$pred+2*predict(z_obj,n.ahead=h)$se))
ts.plot(c(z,rep(NA,h)),xlab="",ylab="",
        main="ARIMA(0,2,0): multi steps ahead interval forecast ",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(z)),predict(z_obj,n.ahead=h)$pred),col="blue")
lines(c(rep(NA,length(z)),predict(z_obj,n.ahead=h)$pred-2*predict(z_obj,n.ahead=h)$se),
      col="blue",lty=2)
lines(c(rep(NA,length(z)),predict(z_obj,n.ahead=h)$pred+2*predict(z_obj,n.ahead=h)$se),
      col="blue",lty=2)


# Exercise 6

set.seed(10)
len<-100
h<-100
# non-vanisshing mean
mu<-0.5
y<-mu+rnorm(len)
x<-cumsum(y)
z<-cumsum(x)
y_obj<-arima(y,order=c(0,0,0),include.mean=T)
y_obj
par(mfrow=c(2,1))
# random walk: integrate forecasts of y_t once
ymin<-min(c(x,x[length(x)]+cumsum(predict(y_obj,n.ahead=h)$pred)))
ymax<-max(c(x,x[length(x)]+cumsum(predict(y_obj,n.ahead=h)$pred)))
ts.plot(c(x,rep(NA,h)),xlab="",ylab="",main="Forecast random-walk + drift",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(y)),x[length(x)]+cumsum(predict(y_obj,n.ahead=h)$pred)),col="blue")
ymin<-min(c(z,z[length(z)]+cumsum(x[length(x)]+cumsum(predict(y_obj,n.ahead=h)$pred))))
ymax<-max(c(z,z[length(z)]+cumsum(x[length(x)]+cumsum(predict(y_obj,n.ahead=h)$pred))))
ts.plot(c(z,rep(NA,h)),xlab="",ylab="",main="Forecast integrated random-walk + drift",
        ylim=c(ymin,ymax))
lines(c(rep(NA,length(z)),z[length(z)]+cumsum(x[length(x)]+
                                                cumsum(predict(y_obj,n.ahead=h)$pred))),col="blue")