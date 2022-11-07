# Section   6.1.3

# Exercise 1


set.seed(10)
len<-100
# AR(1) and c
a1<-0.9
c<-10
mu<-c/(1-a1)
x<-y<-z<-rep(0,len)
# z is initialized with mu
z[1]<-mu
eps<-rnorm(len)
# AR(1) and MA-inversion
for (i in 2:len) #i<-2
{
  # AR(1) recursion initialized with zero
  x[i]<-c+a1*x[i-1]+eps[i]
  # AR(1) recursion intilaized with mu
  z[i]<-c+a1*z[i-1]+eps[i]
  # MA inversion
  y[i]<-mu+a1^(0:(i-1))%*%eps[i:1]
}

mplot<-cbind(x,y,z)
ymin<-min(apply(mplot,1,min))
ymax<-max(apply(mplot,1,max))
ts.plot(x,type="l",ylim=c(ymin,ymax),xlab="",ylab="")
lines(y,col="red",type="l")
lines(z,col="green",type="l")
mtext("x: AR(1)-recursion initialized with zero", side = 3, line = -5,at=len/2,col="black")
mtext("z: AR(1)-recursion initialized with mu", side = 3, line = -7,at=len/2,col="green")
mtext("y: MA-inversion of AR(1) initialized with zero", side = 3, line = -9,at=len/2,col="red")


par(mfrow=c(2,1))
acf(z)
acf(z,type="partial")

# Exercise 2


a_1<-1.7
a_2<--0.8
A_mat<-matrix(nrow=2,ncol=2)
A_mat[1,]<-c(a_1,a_2)
A_mat[2,]<-c(1,0)
A_mat

abs(eigen(A_mat)$values)

set.seed(10)
len<-100
c<-10
mu<-solve(diag(rep(1,2))-A_mat)[1,1]*c
x<-y<-rep(0,len)
y[1:2]<-mu
eps<-rnorm(len)
# We need to compute A_mat^k recursively
A_rec<-diag(rep(1,2))
# a_weight collects the weights (1,1)-elements of A_mat^k
a_weight<-A_rec[1,1]
# AR(2) and MA-inversion
for (i in 3:len) #i<-2
{
  # AR(2) recursion initialized with zero
  x[i]<-c+a_1*x[i-1]+a_2*x[i-2]+eps[i]
  # MA inversion
  y[i]<-mu+a_weight%*%eps[i:3]
  # We compute A_mat^k
  A_rec<-A_rec%*%A_mat
  # and store the weight of the MA-inversion
  a_weight<-c(a_weight,A_rec[1,1])
}

mplot<-cbind(x,y)
ymin<-min(apply(mplot,1,min))
ymax<-max(apply(mplot,1,max))
ts.plot(x,type="l",ylim=c(ymin,ymax),xlab="",ylab="")
lines(y,col="red",type="l")
mtext("x: AR(2)-recursion initialized with zero", side = 3, line = -5,at=len/2,col="black")
mtext("y: MA-inversion of AR(2) initialized with zero", side = 3, line = -9,at=len/2,col="red")


par(mfrow=c(2,1))
acf(y)
acf(y,type="partial")

#------------------------------------------------------------------------------------
# Section 6.1.5

a_vec<-c(1.5,2,-4,2,0.9)
# Specify the system-matrix A_mat
A_mat<-matrix(ncol=length(a_vec),nrow=length(a_vec))
A_mat[1,]<-a_vec
A_mat[2:length(a_vec),1:(length(a_vec)-1)]<-diag(rep(1,length(a_vec)-1))
A_mat[2:length(a_vec),length(a_vec)]<-0
A_mat
# Compare eigenvalues of A_mat with roots of the characteristic polynomial
eigen(A_mat)$values
polyroot(c(-a_vec[length(a_vec):1],1))

#-------------------------------------------------------------------------------------
# Section 6.2.1

set.seed(10)
sigma<-sqrt(3)
a_1<-1.7
a_2<--0.8
len<-100
c<-10
mu<-c/(1-a_1-a_2)
y<-mu+arima.sim(n = len, list( ar = c(a_1,a_2)), sd = sigma)

summary(lm(y[3:len]~y[2:(len-1)]+y[1:(len-2)]))$coef
arima(y,order=c(2,0,0))$coef

#---------------------------------------------------------------------------------------
# Section 6.3.3

# Exercise 1

set.seed(10)
sigma<-sqrt(1)
a_1<-1.7
a_2<--0.8
len<-100
x_ar2<-arima.sim(n = len, list( ar = c(a_1,a_2)), sd = sigma)

# Exercise 2

acf(x_ar2,type="partial")

# Exercise 3


x_obj_ar2<-arima(x_ar2,order=c(1,0,0))
tsdiag(x_obj_ar2)

x_obj_ar2<-arima(x_ar2,order=c(2,0,0))
tsdiag(x_obj_ar2)

#--------------------------------------------------------------------------------------------
# Section 6.4.2

# Exercise 1
set.seed(10)
sigma<-1
a_1<-0.9
len<-100
acf_mat<-cbind(a_1^(0:(length(acf(x,plot=F)$acf)-1)),acf(x,plot=F)$acf)
dimnames(acf_mat)[[2]]<-c("True acf","Sample acf")
acf_mat


# Exercise 2



x<-arima.sim(n = len, list( ar = a_1), sd = sigma)
ahat_1<-arima(x,order=c(1,0,0))$coef[1]

ts.plot(acf(x,plot=F)$acf,xlab="",ylab="")
lines(a_1^(0:(length(acf(x,plot=F)$acf)-1)),col="blue")
lines(ahat_1^(0:(length(acf(x,plot=F)$acf)-1)),col="red")
lines(rep(2/sqrt(len),len),lty=2)
lines(rep(-2/sqrt(len),len),lty=2)
lines(rep(0,len))
mtext("Sample acf", side = 3, line = -1,at=length(acf(x,plot=F)$acf)/2,col="black")
mtext("True acf", side = 3, line = -2,at=length(acf(x,plot=F)$acf)/2,col="blue")
mtext("Estimated acf (based an estimated AR-coeff)", side = 3,
      line = -3,at=length(acf(x,plot=F)$acf)/2,col="red")


#-------------------------------------------------------------------------------------------------
# Section 6.4.4

# Exercise 1
a_1<-1.7
a_2<--0.8
A_mat<-matrix(nrow=2,ncol=2)
A_mat[1,]<-c(a_1,a_2)
A_mat[2,]<-c(1,0)
AkA<-kronecker(A_mat,A_mat)
AkA
vec_sigma<-c(sigma^2,rep(0,3))
vec_sigma
# Ricatti equation
R_mat_0_vec<-solve(diag(rep(1,4))-AkA)%*%vec_sigma
R_mat_0<-matrix(R_mat_0_vec,ncol=2)
R_mat_0


# Exercise 2

set.seed(10)
sigma<-1
len<-1000
x<-arima.sim(n = len, list( ar = c(a_1,a_2)), sd = sigma)
var(x)
cov(x[2:len],x[1:(len-1)])

# Exercise 3



R_mat_1<-A_mat%*%R_mat_0
R_mat_1[1,2]
R_mat_2<-A_mat%*%R_mat_1
R_mat_2[1,2]
R_mat_3<-A_mat%*%R_mat_2
R_mat_3[1,2]
cov(x[3:len],x[1:(len-2)])
cov(x[4:len],x[1:(len-3)])
cov(x[5:len],x[1:(len-4)])


# Exercise 4

R_mat_k<-R_mat_0
R_k<-0:50
for (i in 0:50)
{
  R_k[i+1]<-R_mat_k[1,1]
  R_mat_k<-A_mat%*%R_mat_k
}
acf_true<-R_k/R_mat_0[1,1]


ts.plot(acf(x,plot=F,lag.max=50)$acf,xlab="",ylab="")
lines(acf_true,col="blue")
lines(rep(2/sqrt(len),len),lty=2)
lines(rep(-2/sqrt(len),len),lty=2)
lines(rep(0,len))
mtext("Sample acf", side = 3, line = -1,at=length(acf(x,plot=F)$acf)/2,col="black")
mtext("True acf", side = 3, line = -2,at=length(acf(x,plot=F)$acf)/2,col="blue")

#--------------------------------------------------------------------------------------------------------
# Section 6.5.1

set.seed(10)
len<-100
# negative AR(1)
a_1n<--0.9
sigma<-1
xn<-arima.sim(n = len, list( ar=a_1n), sd = sigma)
impulse_response_n<-a_1n^(0:(len-1))
set.seed(10)
len<-100
# Positive AR(1)
a_1p<-0.9
sigma<-1
xp<-arima.sim(n = len, list( ar=a_1p), sd = sigma)
impulse_response_p<-a_1p^(0:(len-1))

par(mfrow=c(2,1))
ts.plot(xn,xlab="",ylab="",col="blue",main=paste("AR(1)=",a_1n,sep=""))
lines(3*impulse_response_n,col="red")
mtext("Realization", side = 3, line = -1,at=len/2,col="blue")
mtext("Impulse response magnified (scaled) by 3", side = 3, line = -2,at=len/2,col="red")
ts.plot(xp,xlab="",ylab="",col="blue",main=paste("AR(1)=",a_1p,sep=""))
lines(3*impulse_response_p,col="red")
mtext("Realization", side = 3, line = -1,at=len/2,col="blue")
mtext("Impulse response magnified (scaled) by 3", side = 3, line = -2,at=len/2,col="red")

#-----------------------------------------------------------------------------------------------------------
# Section 6.5.3

# Exercise 1
A_mat_k<-diag(rep(1,2))
A_k<-0:99
for (i in 0:(length(A_k)-1))
{
  A_k[i+1]<-A_mat_k[1,1]
  A_mat_k<-A_mat%*%A_mat_k
}
# Recursive form
x_t<-rep(0,length(A_k))
# in t=0 x_t receives the unit impulse
x_t[1]<-1
# in t=1 we have
x_t[2]<-a_1*x_t[1]
# for t=2,...,99 we have:
for (i in 3:length(A_k))
{
  x_t[i]<-a_1*x_t[i-1]+a_2*x_t[i-2]
}
A_k
x_t

ts.plot(A_k,xlab="",ylab="",col="blue",main="Impulse function AR(2)")

# Exercise 2

A_mat
Arg(eigen(A_mat)$values)
acos(a_1/(2*sqrt(-a_2)))

# Exercise 3


2*pi/acos(a_1/(2*sqrt(-a_2)))

ts.plot(x,xlab="",ylab="",col="blue",main="Realization of AR(2)")

# Exercise 4


abs(eigen(A_mat)$values)[1]^(0:99)
sqrt(-a_2)^(0:99)

ts.plot(A_k,xlab="",ylab="",col="blue",main="Impulse response and rate of convergence",ylim=c(-1,3))
lines(3*sqrt(-a_2)^(0:99),col="black")
mtext("Impulse response", side = 3, line = -1,at=50,col="blue")
mtext("3*|lambda|^k: rate of convergence stretched by 3", side = 3, line = -2,at=50,col="black")

#-------------------------------------------------------------------------------------------------------------------
# Section 6.6.4

A_mat
A_k

# Exercise 1

set.seed(10)
sigma<-1
len<-100
c<-10
mu<-c/(1-a_1-a_2)
y<-mu+arima.sim(n = len, list( ar = c(a_1,a_2)), sd = sigma)

# Exercise 2

par(mfrow=c(2,1))
acf(y)
acf(y,type="partial")

# Exercise 3


y_obj<-arima(y,order=c(2,0,0))
y_obj

# Exercise 4

h<-30
yhat_est<-1:h
yhat_true<-yhat_est
# One-step ahead forecast
yhat_est[1]<-y_obj$coef[1:2]%*%(y[len:(len-1)]-y_obj$coef[3])+y_obj$coef[3]
yhat_true[1]<-c(a_1,a_2)%*%(y[len:(len-1)]-mu)+mu
yhat_est[2]<-y_obj$coef[1:2]%*%(c(yhat_est[1],y[len])-y_obj$coef[3])+y_obj$coef[3]
yhat_true[2]<-c(a_1,a_2)%*%(c(yhat_true[1],y[len])-mu)+mu
for (i in 3:h)
{
  yhat_est[i]<-y_obj$coef[1:2]%*%(yhat_est[(i-1):(i-2)]-y_obj$coef[3])+y_obj$coef[3]
  yhat_true[i]<-c(a_1,a_2)%*%(yhat_true[(i-1):(i-2)]-mu)+mu
}

ts.plot(c(y[(len-30):len],yhat_est),col="red",xlab="",ylab="",main="Point forecasts of true and estimated models")
lines(c(y[(len-30):len],yhat_true),col="blue")
lines(c(y[(len-30):len],rep(NA,h)),col="black")
mtext("data", side = 3, line = -1,at=h,col="black")
mtext("Forecasts true model", side = 3, line = -2,at=h,col="blue")
mtext("Forecasts estimated model", side = 3, line = -3,at=h,col="red")
abline(v=30)


# Exercise 5


2*pi/acos(a_1/(2*sqrt(-a_2)))
2*pi/acos(y_obj$coef[1]/(2*sqrt(-y_obj$coef[2])))

# Exercise 6

Ahat_mat<-A_mat
Ahat_mat[1,]<-y_obj$coef[1:2]
Ahat_mat
Ahat_k<-1:h
Ahat<-diag(rep(1,2))
for (i in 1:h)
{
  Ahat_k[i]<-Ahat[1,1]
  Ahat<-Ahat_mat%*%Ahat
}

ts.plot(Ahat_k,col="red",xlab="",ylab="",main="Impulse functions of true and estimated models")
lines(A_k,col="blue")
mtext("true model", side = 3, line = -1,at=h/2,col="blue")
mtext("Estimated model", side = 3, line = -2,at=h/2,col="red")

# Exercise 7

var_hat_true<-1:h
for (i in 1:h)
  var_hat_true[i]<-sum(A_k[i:1]^2)*sigma^2
var_hat_true

# Exercise 8

sigmahat<-sqrt(y_obj$sigma)
var_hat_est<-1:h
for (i in 1:h)
  var_hat_est[i]<-sum(Ahat_k[i:1]^2)*sigmahat^2
var_hat_est


# Exercise 9

yhat_est
predict(y_obj,n.ahead=h)$pred
var_hat_est
predict(y_obj,n.ahead=h)$se^2

#Exercise 10



# True variance
R_mat_0[1]
# True 30-step ahead forecast error variance
var_hat_true[h]

# Exercise 11


interval_forecast_ar<-function(y,h)
{
  len<-length(y)
  # Estimation of parameters
  y_obj<-arima(y,order = c(2, 0, 0))
  # Define A-matrix
  A_mat=matrix(nrow=2,ncol=2)
  A_mat[1,]<-y_obj$coef[1:2]
  A_mat[2,1]<-1
  A_mat[2,2]<-0
  # Compute weights in inversion of AR(2)
  A_k<-diag(rep(1,2))
  Ak_11<-0:(len+h)
  for (i in 0:(len+h))
  {
    Ak_11[i+1]<-A_k[1,1]
    A_k<-A_k%*%A_mat
  }
  # Compute point forecasts
  yhat<-1:h
  yhat[1]<-y_obj$coef[1:2]%*%(y[len:(len-1)]-y_obj$coef[3])+y_obj$coef[3]
  yhat[2]<-y_obj$coef[1:2]%*%(c(yhat[1],y[len])-y_obj$coef[3])+y_obj$coef[3]
  for (i in 3:h)
  {
    yhat[i]<-y_obj$coef[1:2]%*%(yhat[(i-1):(i-2)]-y_obj$coef[3])+y_obj$coef[3]
  }
  # Variances
  var_yhat<-1:h
  # use the sample mean-square sigma (the true one is sqrt(2))
  sigma<-sqrt(y_obj$sigma)
  for (i in 1:h)
  {
    var_yhat[i]<-sigma^2*sum(Ak_11[1:i]^2)
  }
  # Compute the forecast intervals
  y_lower<-yhat-2*sqrt(var_yhat)
  y_upper<-yhat+2*sqrt(var_yhat)
  return(list(y_lower=y_lower,y_upper=y_upper))
}

# Exercise 12



set.seed(10)
sigma<-sqrt(1)
a_1<-1.7
a_2<--0.8
len<-100
c<-10
mu<-c/(1-a_1-a_2)
anzsim<-500
emp_sig<-rep(0,30)
# We compute 500 replications of length 130 of the AR(2)
for (j in 1:anzsim) #j<-1
{
  y<-mu+arima.sim(n = len+30, list( ar = c(a_1,a_2)), sd = sigma)
  # The first len observations are fed to interval_forecast
  z<-y[1:len]
  int_obj<-interval_forecast_ar(z,h)
  
  # we verify that x[len:(len+30)] are in the forecast intervals
  # If not (miss) the empirical significance level emp_sig is increased by 1
  for (i in 1:30)
  {
    if (y[len+i]<int_obj$y_lower[i]|y[len+i]>int_obj$y_upper[i])
      emp_sig[i]<-emp_sig[i]+1
  }
}
# The empirical significance level should be close to 5%
emp_sig/anzsim












