# Code section 5.1.1


# Exercises 1-3

set.seed(10)
sigma<-2
b_1<-0.5
len<-30
y_1<-arima.sim(n = len, list( ma = b_1), sd = sigma)
set.seed(10)
len<-100
y_2<-arima.sim(n = len, list( ma = b_1), sd = sigma)
set.seed(10)
len<-300
y_3<-arima.sim(n = len, list( ma = b_1), sd = sigma)

rho<-c(b_1/(1+b_1^2),rep(0,4))
rho

par(mfrow=c(3,1))
acf(y_1)
acf(y_2)
acf(y_3)

# Exercises 4-6


set.seed(10)
sigma<-2
b_1<-0.5
b_2<--0.8
b_3<-1.2
b_4<--0.4
b_vec<-c(b_1,b_2,b_3,b_4)
len<-30
y_1<-arima.sim(n = len, list( ma = b_vec), sd = sigma)
set.seed(10)
len<-100
y_2<-arima.sim(n = len, list( ma = b_vec), sd = sigma)
set.seed(10)
len<-300
y_3<-arima.sim(n = len, list( ma = b_vec), sd = sigma)

rho<-1:5
# b collects all MA-parameters
b<-c(1,b_vec)
for (i in 1:4)#i<-1
  rho[i]<-b[(1+i):5]%*%b[1:(5-i)]/sum(b^2)
rho[5]<-0
rho

par(mfrow=c(3,1))
acf(y_1)
acf(y_2)
acf(y_3)

#------------------------------------
# Section 5.3.1

# Exercises 1

estim<-function(parma,x)
{
  # Center the data
  x<-x-mean(x)
  # Determine length
  len<-length(x)
  # Define q
  q<-length(parma)
  b<-parma
  # Define a vector of epsilon's
  epsilon<-x
  # Compute epsilon_1
  epsilon[1]<-x[1]
  for (i in 2:q)
  {
    epsilon[i]<-x[i]-epsilon[(i-1):1]%*%b[1:(i-1)]
  }
  for (i in (q+1):len)
  {
    epsilon[i]<-x[i]-epsilon[(i-1):(i-q)]%*%b[1:q]
  }
  return(sum(epsilon^2))
}

# Exercise 2

set.seed(10)
sigma<-sqrt(2)
b_1<--1.3
b_2<-0.7
len<-100
x<-arima.sim(n = len, list( ma = c(b_1,b_2)), sd = sigma)

# Exercise 3

# white noise
parma<-c(0,0)
estim(parma,x)
# This is the same as
var(x)*(len-1)
# True parameter: criterion is smaller
parma<-c(-1.3,0.7)
estim(parma,x)

# Exercise 4


# initialization: white noise
parma<-c(0,0)
estim_obj<-nlminb(parma,estim,x=x)
round(estim_obj$par,4)

# The estimated coefficients are pretty close to the true values!

# Exercise 5


# CSS
ma_obj<-arima(x,order = c(0, 0, 2),method="CSS")
round(ma_obj$coef,4)
# Maximum Likelihood
ma_obj<-arima(x,order = c(0, 0, 2))
round(ma_obj$coef,4)



#-----------------------------------------------------------------------
# Section 5.4.3

# Exercise 1
set.seed(10)
sigma<-sqrt(2)
b_1<--1.3
b_2<-0.7
len<-100
x<-arima.sim(n = len, list( ma = c(b_1,b_2)), sd = sigma)

# Exercise 2
acf(x)

# Exercise 3

x_obj<-arima(x,order=c(0,0,1))
tsdiag(x_obj)

x_obj<-arima(x,order=c(0,0,2))
tsdiag(x_obj)


#-------------------------------------------------------------------------------------
# Section 5.5.4

# Exercise 1

B_mat=matrix(nrow=4,ncol=4)
B_mat[1,]<--c(0.5,-0.8,1.2,-0.4)
B_mat[2:4,1:3]<-diag(rep(1,3))
B_mat[2:4,4]<-0
B_mat
abs(eigen(B_mat)$values)

#Exercise 2
B_k<-diag(rep(1,4))
Bk_11<-0:10
for (i in 0:10)
{
  Bk_11[i+1]<-B_k[1,1]
  B_k<-B_k%*%B_mat
}

ts.plot(Bk_11)

# Exercise 3

B_mat=matrix(nrow=2,ncol=2)
B_mat[1,]<--c(-1.7,0.8)
B_mat[2,1]<-1
B_mat[2,2]<-0
B_mat
abs(eigen(B_mat)$values)

# Exercise 4
set.seed(10)
sigma<-1
b_1<--1.7
b_2<-0.8
len<-100
x<-arima.sim(n = len, list( ma = c(b_1,b_2)), sd = sigma)

# Exercise 5
par(mfrow=c(3,1))
ts.plot(x,main=paste("MA(2)-process",sep=""),
        xlab="",ylab="",col="blue")
acf(x)
acf(x,type="partial")


# Exercise 6


ma_obj<-arima(x,order = c(0, 0, 2),include.mean = FALSE)
ma_obj$coef

# Exercise 7

Bhat_mat=matrix(nrow=2,ncol=2)
Bhat_mat[1,]<--ma_obj$coef[1:2]
Bhat_mat[2,1]<-1
Bhat_mat[2,2]<-0
Bhat_mat
abs(eigen(Bhat_mat)$values)

# Exercise 8


B_k<-diag(rep(1,2))
Bk_11<-0:100
for (i in 0:100)
{
  Bk_11[i+1]<-B_k[1,1]
  B_k<-B_k%*%Bhat_mat
}

# Exercise 9

ts.plot(Bk_11)

# Exercise 10
-Bk_11[101:2]%*%x

# Exercise 11
predict(ma_obj,n.ahead=1)

# Exercise 12

set.seed(10)
sigma<-1
b_1<--1.7
b_2<-0.8
len<-101
x_101<-arima.sim(n = len, list( ma = c(b_1,b_2)), sd = sigma)
x_100<-x_101[1:(len-1)]

ts.plot(c(x_101[90:100],NA),type="l",ylim=c(-Bk_11[101:2]%*%x_100-2*sqrt(predict(ma_obj,n.ahead=1)$se),
                                            max(x_101[90:101])),xlab="",ylab="")
lines(c(rep(NA,11),-Bk_11[101:2]%*%x_100),col="green",type="o",lwd=2)
lines(c(rep(NA,11),x_101[101]),col="blue",type="o",lwd=3)
lines(c(rep(NA,11),-Bk_11[101:2]%*%x_100-2*sqrt(predict(ma_obj,n.ahead=1)$se)),
      col="red",type="o",lwd=2)
lines(c(rep(NA,11),-Bk_11[101:2]%*%x_100+2*sqrt(predict(ma_obj,n.ahead=1)$se)),
      col="red",type="o",lwd=2)



#--------------------------------------------------------------------------------
# Section 5.6.2

# Exercise 1
set.seed(10)
sigma<-sqrt(2)
mu<-10
b_1<--1.7
b_2<-0.8
len<-100
x<-arima.sim(n = len, list( ma = c(b_1,b_2)), sd = sigma)
# Add the non-vanishing mean level
x<-x+mu

#Exercise 2


ma_obj<-arima(x,order = c(0, 0, 2),include.mean = TRUE)
ma_obj$coef

# Exercise 3


Bhat_mat=matrix(nrow=2,ncol=2)
Bhat_mat[1,]<--ma_obj$coef[1:2]
Bhat_mat[2,1]<-1
Bhat_mat[2,2]<-0
Bhat_mat
abs(eigen(Bhat_mat)$values)

# Exercise 4

B_k<-diag(rep(1,2))
Bk_11<-0:100
for (i in 0:100)
{
  Bk_11[i+1]<-B_k[1,1]
  B_k<-B_k%*%Bhat_mat
}

# Exercise 5



# Center by using the mean-level
-Bk_11[101:2]%*%(x-mean(x))+mean(x)
# Center by using the mean-estimate provided by arima
-Bk_11[101:2]%*%(x-ma_obj$coef[3])+ma_obj$coef[3]

# Exercise 6

predict(ma_obj,n.ahead=1)


#------------------------------------------------------------------------------------
# Section 5.6.4

# Exercise 1

B_k<-diag(rep(1,2))
Bk_11<-0:110
for (i in 0:110)
{
  Bk_11[i+1]<-B_k[1,1]
  B_k<-B_k%*%Bhat_mat
}

# Exercise 2

h<-10
xhat<-1:h
# One-step ahead forecast
xhat[1]<--Bk_11[101:2]%*%(x-ma_obj$coef[3])+ma_obj$coef[3]
y<-x
for (i in 2:h)
{
  # We append recursively the last forecast to y
  y<-c(y,xhat[i-1])
  # we apply the multi-step ahead forecast function to the centered y
  xhat[i]<--Bk_11[(100+i):2]%*%(y-ma_obj$coef[3])+ma_obj$coef[3]
}
# Exercise 3


xhat
predict(ma_obj,n.ahead=10)

#--------------------------------------------------------------------------------------------------
# Section 5.8.1

# Exercise 1

set.seed(10)
sigma<-sqrt(2)
mu<-10
b_1<--1.7
b_2<-0.8
len<-100
x<-arima.sim(n = len, list( ma = c(b_1,b_2)), sd = sigma)
# Add the non-vanishing mean level
x<-x+mu
ma_obj<-arima(x,order = c(0, 0, 2),include.mean = TRUE)
ma_obj$coef
Bhat_mat=matrix(nrow=2,ncol=2)
Bhat_mat[1,]<--ma_obj$coef[1:2]
Bhat_mat[2,1]<-1
Bhat_mat[2,2]<-0
Bhat_mat

# Exercise 1

B_k<-diag(rep(1,2))
Bk_11<-0:103
for (i in 0:103)
{
  Bk_11[i+1]<-B_k[1,1]
  B_k<-B_k%*%Bhat_mat
}

# Exercise 2


h<-3
xhat<-1:h
# One-step ahead forecast
xhat[1]<--Bk_11[101:2]%*%(x-ma_obj$coef[3])+ma_obj$coef[3]
y<-x
for (i in 2:h)
{
  # We append recursively the last forecast to y
  y<-c(y,xhat[i-1])
  # we apply the multi-step ahead forecast function to the centered y
  xhat[i]<--Bk_11[(100+i):2]%*%(y-ma_obj$coef[3])+ma_obj$coef[3]
}

# Exercise 3


b_vec<-c(1,ma_obj$coef[1:2])
sigma<-sqrt(ma_obj$sigma)
h<-3
var_xhat<-1:h
# Use the estimates of the coefficient vector
b_vec<-c(1,ma_obj$coef[1:2])
# use the sample mean-square sigma (the true one is sqrt(2))
sigma<-sqrt(ma_obj$sigma)
for (i in 1:h) #i<-1
{
  var_xhat[i]<-sigma^2*sum(b_vec[1:i]^2)
}
var_xhat
var(x)

# Exercise 4


xhat
predict(ma_obj,n.ahead=3)$pred
var_xhat
predict(ma_obj,n.ahead=3)$se^2

# Exercise 5


interval_forecast<-function(x,h)
{
  len<-length(x)
  # Estimation of parameters
  ma_obj<-arima(x,order = c(0, 0, 2))
  # Define B-matrix
  B_mat=matrix(nrow=2,ncol=2)
  B_mat[1,]<--ma_obj$coef[1:2]
  B_mat[2,1]<-1
  B_mat[2,2]<-0
  # Compute weights in inversion of MA(2)
  B_k<-diag(rep(1,2))
  Bk_11<-0:(len+h)
  for (i in 0:(len+h))
  {
    Bk_11[i+1]<-B_k[1,1]
    B_k<-B_k%*%B_mat
  }
  # Compute point forecasts
  xhat<-1:h
  # One-step ahead forecast
  xhat[1]<--Bk_11[(len+1):2]%*%(x-ma_obj$coef[3])+ma_obj$coef[3]
  y<-x
  if (h>1)
  {
    for (i in 2:h)
    {
      # We append recursively the last forecast to y
      y<-c(y,xhat[i-1])
      # we apply the multi-step ahead forecast function to the centered y
      xhat[i]<--Bk_11[(len+i):2]%*%(y-ma_obj$coef[3])+ma_obj$coef[3]
    }
  }
  # Variances
  var_xhat<-1:h
  # Use the estimates of the coefficient vector
  b_vec<-c(1,ma_obj$coef[1:2])
  # use the sample mean-square sigma (the true one is sqrt(2))
  sigma<-sqrt(ma_obj$sigma)
  for (i in 1:h) #i<-1
  {
    var_xhat[i]<-sigma^2*sum(b_vec[1:min(i,3)]^2)
  }
  # Compute the forecast intervals
  x_lower<-xhat-2*sqrt(var_xhat)
  x_upper<-xhat+2*sqrt(var_xhat)
  return(list(x_lower=x_lower,x_upper=x_upper))
}

# Exercise 6

set.seed(100)
sigma<-sqrt(2)
b_1<--1.7
b_2<-0.8
len<-100
mu<-0
anzsim<-500
emp_sig<-rep(0,3)
sigma_vec<-1:anzsim
# We compute 500 replications of length 103 of the MA(2)
for (j in 1:anzsim)
{
  x<-mu+arima.sim(n = len+3, list( ma = c(b_1,b_2)), sd = sigma)
  # The first len observations are fed to interval_forecast
  z<-x[1:len]
  int_obj<-interval_forecast(z,h)
  
  # we verify that x[101:103] are in the forecast intervals
  # If not (miss) the empirical significance level emp_sig is increased by 1
  for (i in 1:3)
  {
    if (x[len+i]<int_obj$x_lower[i]|x[len+i]>int_obj$x_upper[i])
      emp_sig[i]<-emp_sig[i]+1
  }
}
# The empirical significance level should be close to 5%
emp_sig/anzsim