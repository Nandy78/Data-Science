
# Section 7.1.4

# Exercise 1

a_1<-0.9
b_1<-0.6
A_mat<-rbind(c(a_1,1),c(0,0))
B_mat<-rbind(c(-b_1,1),c(0,0))
A_mat
B_mat
len<-100
A_k<-diag(rep(1,2))
B_k<-A_k
a_vec<-c(1,-a_1)
b_vec<-c(1,b_1)
AR_weights<-1:len
MA_weights<-AR_weights
for (i in 1:len)
{
  AR_weights[i]<-(B_k%*%a_vec)[1]
  MA_weights[i]<-(A_k%*%b_vec)[1]
  A_k<-A_k%*%A_mat
  B_k<-B_k%*%B_mat
}
ts.plot(AR_weights,col="red",xlab="",ylab="",main="AR- and MA-inversions of ARMA(1,1)",ylim=c(min(AR_weights),max(MA_weights)))
lines(MA_weights,col="blue")
mtext("MA-inversion", side = 3, line = -1,at=h/2,col="blue")
mtext("AR_inversion", side = 3, line = -2,at=h/2,col="red")


#Exercise 2

len<-100
I_t<-rep(0,len)
x<-I_t
I_t[1]<-1
x[1]<-I_t[1]
x[2]<-a_1*x[1]+b_1
for (i in 3:len)
{
  x[i]<-a_1*x[i-1]
}
ts.plot(x,col="blue",xlab="",ylab="",main="Impulse response of ARMA(1,1)",ylim=c(min(AR_weights),max(MA_weights)))



#-----------------------------------------------------------------------------------

# Section 7.2.1

# Exercise 1

len<-length(MA_weights)-1
sigma<-sqrt(3)
R_k<-rep(0,len+1)
d_k<-MA_weights
for (i in 0:len)
{
  R_k[i+1]<-sigma^2*d_k[(1+i):(len+1)]%*%d_k[1:(len+1-i)]
}

# Exercise 2

acf_k<-R_k/R_k[1]


# Exercise 3


set.seed(10)
len<-10000
y<-arima.sim(n = len, list( ar=a_1,ma = b_1), sd = sigma)
par(mfrow=c(2,1))
ts.plot(acf_k,col="blue",xlab="",ylab="",main="True ACF")
acf(y,lag.max=100)

# Exercise 4
par(mfrow=c(2,1))
ts.plot(AR_weights,col="blue",xlab="",ylab="",main="Weights of AR-inversion: true partial derivative of lagged data")
acf(y,type="partial")

#------------------------------------------------------------------------------------------

# Section 7.3.2

# Exercise 1

set.seed(10)
len<-200
a<--0.8
b<--0.9
y<-arima.sim(n = len, list( ar=a,ma = b), sd = sigma)

# Exercise 2

par(mfrow=c(2,1))
acf(y)
acf(y,type="partial")

# Exercise 3

ar_obj<-arima(y, order=c(5,0,0))
ma_obj<-arima(y,order=c(0,0,5))

tsdiag(ar_obj)
tsdiag(ma_obj)

# Exercise 4

arma_obj<-arima(y, order=c(1,0,1))
tsdiag(arma_obj)

#------------------------------------------------------------------------------------------------------
# Section 7.4.1

# Exercise 1


polyroot(c(0.36,-1.2,1))
polyroot(c(-0.06,-0.5,1))

# Exercise 2

set.seed(10)
len<-100
sigma<-1
a<-c(1.2,-0.36)
b<-c(1,-0.5,-0.06)
eps<-rnorm(len)
y_original<-rep(0,len)
for (i in 3:len )
  y_original[i]<-2+a%*%y_original[(i-1):(i-2)]+b%*%eps[i:(i-2)]

a<-0.6
b<-c(1,0.1)
y_normal<-rep(0,len)
for (i in 2:len )
  y_normal[i]<-5+a%*%y_normal[(i-1)]+b%*%eps[i:(i-1)]


ts.plot(y_original,col="red")
lines(y_normal,col="blue")
mtext("Original", side = 3, line = -1,at=h/2,col="red")
mtext("Normal", side = 3, line = -2,at=h/2,col="blue")



#----------------------------------------------------------------------------
# Section 7.6.3

set.seed(11)
len<-100
sigma<-sqrt(3)
a_1<-1.7
a_2<--0.8
c<-10
mu<-c/(1-a_1-a_2)
y<-mu+arima.sim(n = len, list( ar = c(a_1,a_2)), sd = sigma)
acf(y,type="partial")

# Exercise 1



maxorder<-10
sigma_k<-rep(0,maxorder+1)
# Order zero
sigma_k[1]<-var(y)
for (k in 1:maxorder)         #k<-10
{
  AR_obj<-arima(y,order=c(k,0,0))
  sigma_k[k+1]<-mean(AR_obj$res^2)
  # This is the same as
  sigma_k[k+1]<-AR_obj$sigma
  print(paste("AR-Order=",k,"  AIC=",AR_obj$aic,sep=""))
}
par(mfrow=c(2,1))
anf<-1
plot((sigma_k)[anf:(maxorder+1)],col="blue",xlab="AR-order",ylab="",
     main="Sample mean-square error of AR(2)",type="l",axes="F")
axis(1,at=anf:(maxorder+1),labels=-1+anf:(maxorder+1))
axis(2)
box()
anf<-3
plot((sigma_k)[anf:(maxorder+1)],col="blue",xlab="AR-order",ylab="",
     main="Same as above for orders=2,3,...,10 (larger or equal than true order 2)",type="l",axes="F")
axis(1,at=1:(maxorder-anf+2),labels=-1+anf:(maxorder+1))
axis(2)
box()

# Exercise 2
par(mfrow=c(2,1))
anf<-1
plot((sigma_k)[anf:(maxorder+1)],col="blue",xlab="AR-order",ylab="",
     main="Sample MSE and expectation of sample MSE for AR(2)",type="l",axes="F")
lines(sigma_k[3]*(1-(anf:(maxorder+1))/len),col="black")
mtext("Sample mean-square error", side = 3, line = -1,at=maxorder/2,col="blue")
mtext("Expectation", side = 3, line = -2,at=maxorder/2,col="black")
axis(1,at=anf:(maxorder+1),labels=-1+anf:(maxorder+1))
axis(2)
box()
anf<-3
plot((sigma_k)[anf:(maxorder+1)],col="blue",xlab="AR-order",ylab="",
     main="Same as above for orders=2,...,10",type="l",axes="F")
lines(sigma_k[3]*(1-(anf:(maxorder+1))/len),col="black")
mtext("Sample mean-square error", side = 3, line = -1,at=maxorder/2,col="blue")
mtext("Expectation", side = 3, line = -2,at=maxorder/2,col="black")
axis(1,at=1:(maxorder-anf+2),labels=-1+anf:(maxorder+1))
axis(2)
box()



# Exercise 3

log_sigma_k<-log(sigma_k)
par(mfrow=c(2,1))
anf<-1
aic<-(log_sigma_k+2*(0:maxorder)/len)[anf:(maxorder+1)]
plot((log_sigma_k)[anf:(maxorder+1)],col="blue",xlab="AR-order",ylab="",
     main="log sample MSE and AIC for AR(2)",type="l",axes="F")
lines(aic,col="red")
mtext("log sample MSE", side = 3, line = -1,at=maxorder/2,col="blue")
mtext("AIC", side = 3, line = -2,at=maxorder/2,col="red")
axis(1,at=anf:(maxorder+1),labels=-1+anf:(maxorder+1))
axis(2)
box()
anf<-3
aic<-(log_sigma_k+2*(0:maxorder)/len)[anf:(maxorder+1)]
ymin<-min((log_sigma_k)[anf:(maxorder)])
ymax<-max((log_sigma_k+2*(0:maxorder)/len)[anf:(maxorder)])
plot((log_sigma_k)[anf:(maxorder+1)],col="blue",xlab="AR-order",ylab="",
     main="log sample MSE and AIC for AR(2)",type="l",axes="F",ylim=c(ymin,ymax))
lines(aic,col="red")
mtext("log sample MSE", side = 3, line = -1,at=maxorder/2,col="blue")
mtext("AIC", side = 3, line = -2,at=maxorder/2,col="red")
axis(1,at=1:(maxorder-anf+2),labels=-1+anf:(maxorder+1))
axis(2)
box()

# Exercise 4
par(mfrow=c(2,1))
anf<-1
aic<-(log_sigma_k+2*(0:maxorder)/len)[anf:(maxorder+1)]
bic<-(log_sigma_k+log(len)*(0:maxorder)/len)[anf:(maxorder+1)]
plot(bic,col="blue",xlab="AR-order",ylab="",
     main="BIC (blue) vs. AIC (red) for AR(2)",type="l",axes="F")
lines(aic,col="red")
mtext("BIC", side = 3, line = -1,at=maxorder/2,col="blue")
mtext("AIC", side = 3, line = -2,at=maxorder/2,col="red")
axis(1,at=anf:(maxorder+1),labels=-1+anf:(maxorder+1))
axis(2)
box()
anf<-3
aic<-(log_sigma_k+2*(0:maxorder)/len)[anf:(maxorder+1)]
bic<-(log_sigma_k+log(len)*(0:maxorder)/len)[anf:(maxorder+1)]
ymin<-min(aic)
ymax<-max(bic)
plot(bic,col="blue",xlab="AR-order",ylab="",
     main="BIC (blue) vs. AIC (red) for AR(2)",type="l",axes="F",ylim=c(ymin,ymax))
lines(aic,col="red")
mtext("BIC", side = 3, line = -1,at=maxorder/2,col="blue")
mtext("AIC", side = 3, line = -2,at=maxorder/2,col="red")
axis(1,at=1:(maxorder-anf+2),labels=-1+anf:(maxorder+1))
axis(2)
box()


# Exercise 5

maxorder<-10
sigma_k<-rep(0,maxorder+1)
# Order zero
sigma_k[1]<-var(y)
for (k in 1:maxorder)         #k<-10
{
  MA_obj<-arima(y,order=c(0,0,k))
  sigma_k[k+1]<-mean(MA_obj$res^2)
  # This is the same as
  sigma_k[k+1]<-MA_obj$sigma
  print(paste("MA-Order=",k,"  AIC=",MA_obj$aic,sep=""))
}
log_sigma_k<-log(sigma_k)

par(mfrow=c(2,1))
anf<-1
aic<-(log_sigma_k+2*(0:maxorder)/len)[anf:(maxorder+1)]
bic<-(log_sigma_k+log(len)*(0:maxorder)/len)[anf:(maxorder+1)]
plot(bic,col="blue",xlab="MA-order",ylab="",
     main="BIC (blue) vs. AIC (red) for AR(2) based on false MA-models",
     type="l",axes="F",ylim=c(min(aic),max(aic)))
lines(aic,col="red")
mtext("BIC", side = 3, line = -1,at=maxorder/2,col="blue")
mtext("AIC", side = 3, line = -2,at=maxorder/2,col="red")
axis(1,at=anf:(maxorder+1),labels=-1+anf:(maxorder+1))
axis(2)
box()
anf<-3
aic<-(log_sigma_k+2*(0:maxorder)/len)[anf:(maxorder+1)]
bic<-(log_sigma_k+log(len)*(0:maxorder)/len)[anf:(maxorder+1)]
ymin<-min(aic)
ymax<-max(bic)
plot(bic,col="blue",xlab="MA-order",ylab="",
     main="BIC (blue) vs. AIC (red) for AR(2)",type="l",axes="F",ylim=c(ymin,ymax))
lines(aic,col="red")
mtext("BIC", side = 3, line = -1,at=maxorder/2,col="blue")
mtext("AIC", side = 3, line = -2,at=maxorder/2,col="red")
axis(1,at=1:(maxorder-anf+2),labels=-1+anf:(maxorder+1))
axis(2)
box()


acf(y)

# Exercise 6

maxarorder<-5
maxmaorder<-5
sigma_jk<-matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                 ncol=maxmaorder+1,nrow=maxarorder+1)
# Order (zero,zero)
sigma_jk[1,1]<-var(y)
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)         #k<-10
  {
    ARMA_obj<-arima(y,order=c(j,0,k))
    sigma_jk[j+1,k+1]<-ARMA_obj$sigma
    print(paste("  AR-order=",j,"MA-Order=",k,"  AIC=",ARMA_obj$aic,sep=""))
  }
}
log_sigma_jk<-log(sigma_jk)
aic<-sigma_jk
dimnames(aic)[[2]]<-paste("MA-order",0:maxmaorder,sep=" ")
dimnames(aic)[[1]]<-paste("AR-order",0:maxarorder,sep=" ")
bic<-aic
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)
  {
    aic[j,k]<-log_sigma_jk[j,k]+2*(j+k)/len
    bic[j,k]<-log_sigma_jk[j,k]+log(len)*(j+k)/len
  }
}
aic
bic


#--------------------------------------------------------------------------------------------------------------

# Section 7.7.4

set.seed(10)
a_1<-0.9
b_1<-0.6
len<-100
mu<-10/(1-0.9)
sigma<-sqrt(3)
y<-mu+arima.sim(n = len, list( ar = a_1, ma = b_1), sd = sigma)

# Exercise 1


par(mfrow=c(2,1))
acf(y)
acf(y,type="partial")

# Exercise 2

maxarorder<-5
maxmaorder<-5
sigma_jk<-matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                 ncol=maxmaorder+1,nrow=maxarorder+1)
# Order (zero,zero)
sigma_jk[1,1]<-var(y)
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)         #k<-10
  {
    ARMA_obj<-arima(y,order=c(j,0,k))
    sigma_jk[j+1,k+1]<-ARMA_obj$sigma
  }
}
log_sigma_jk<-log(sigma_jk)
bic<-sigma_jk
dimnames(bic)[[2]]<-paste("MA-order",0:maxmaorder,sep=" ")
dimnames(bic)[[1]]<-paste("AR-order",0:maxarorder,sep=" ")
for (j in 0:maxarorder)
{
  for (k in 0:maxmaorder)
  {
    bic[j,k]<-log_sigma_jk[j,k]+log(len)*(j+k)/len
  }
}
bic



# Exercise 3

arorder<-2
maorder<-0
y_ar<-arima(y,order=c(arorder,0,maorder))
arorder<-1
maorder<-1
y_arma<-arima(y,order=c(arorder,0,maorder))
tsdiag(y_ar)
tsdiag(y_arma)



# Exercise 4



a_1<-y_arma$coef[1]
b_1<-y_arma$coef[2]
A_mat<-rbind(c(a_1,1),c(0,0))
B_mat<-rbind(c(-b_1,1),c(0,0))
A_mat
B_mat
len_inv<-101
A_k<-diag(rep(1,2))
B_k<-A_k
a_vec<-c(1,-a_1)
b_vec<-c(1,b_1)
AR_weights<-1:len
MA_weights<-AR_weights
for (i in 1:len_inv)
{
  AR_weights[i]<-(B_k%*%a_vec)[1]
  MA_weights[i]<-(A_k%*%b_vec)[1]
  A_k<-A_k%*%A_mat
  B_k<-B_k%*%B_mat
}
ts.plot(AR_weights,col="red",xlab="",ylab="",main="AR- and MA-inversions of ARMA(1,1)",ylim=c(min(AR_weights),max(MA_weights)))
lines(MA_weights,col="blue")
mtext("MA-inversion", side = 3, line = -1,at=len_inv/2,col="blue")
mtext("AR_inversion", side = 3, line = -2,at=len_inv/2,col="red")


# Exercise 5


h<-10
# Compute point forecasts
yhat<-1:h
# One-step ahead forecast
yhat[1]<--AR_weights[len_inv:2]%*%(y-y_arma$coef[3])+y_arma$coef[3]
z<-y
if (h>1)
{
  for (i in 2:h)
  {
    # We append recursively the last forecast to y
    z<-c(z[2:len],yhat[i-1])
    # we apply the multi-step ahead forecast function to the centered y
    yhat[i]<--AR_weights[len_inv:2]%*%(z-y_arma$coef[3])+y_arma$coef[3]
  }
}
# Variances
var_yhat<-1:h
sigma<-sqrt(y_arma$sigma)
for (i in 1:h) #i<-1
{
  var_yhat[i]<-sigma^2*sum(MA_weights[1:i]^2)
}
# Compute the forecast intervals
y_lower<-yhat-2*sqrt(var_yhat)
y_upper<-yhat+2*sqrt(var_yhat)
yhat
predict(y_arma,n.ahead=h)$pred

var_yhat
predict(y_arma,n.ahead=h)$se^2

ts.plot(c(rep(NA,31),yhat),col="red",xlab="",ylab="",
        main="Interval forecasts ARMA(1,1)",
        ylim=c(min(y[(len-30):len]),max(y_upper)))
lines(c(rep(NA,31),y_lower),col="red",lty=2)
lines(c(rep(NA,31),y_upper),col="red",lty=2)
lines(c(y[(len-30):len],rep(NA,h)),col="blue")
abline(v=31)





