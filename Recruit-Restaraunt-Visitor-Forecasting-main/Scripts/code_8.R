

require(doParallel)
cl <- makeCluster(7)
registerDoParallel(cl)


# Section 8.4.1

len<-100
simanz<-1000
h<-10
set.seed(10)

# Function computes out-of-sample squared forecast errors
for_sim_out<-function(h,len,j)
{
  set.seed(j)
  x<-rnorm(len+h)
  y_ar3<-arima(x[1:len],order=c(3,0,0))
  y_ar9<-arima(x[1:len],order=c(9,0,0))
  forecast_error_true_out<-(mean(x[1:len])-x[(len+1):(len+h)])^2
  forecast_error_ar3_out<-(predict(y_ar3,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error_ar9_out<-(predict(y_ar9,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error<-c(forecast_error_true_out,forecast_error_ar3_out,forecast_error_ar9_out)
}


# Generate 1000 realizations and compute squared forecast errors
sample_id_out <- foreach(j = 1:simanz,.combine=rbind) %dopar% for_sim_out(h,len,j)
# Compute mean-square forecast errors for h=1,...,10
out_of_sample<-cbind(apply(sample_id_out[,1:h],2,mean),apply(sample_id_out[,h+1:h],2,mean),apply(sample_id_out[,2*h+1:h],2,mean))
ymin<-min(apply(out_of_sample,1,min))
ymax<-max(apply(out_of_sample,1,max))
ts.plot(out_of_sample[,1],xlab="Forecast horizon",ylab="MSFE",main="Out-of-sample",ylim=c(ymin,ymax))
lines(out_of_sample[,2],col="blue")
lines(out_of_sample[,3],col="red")
mtext("True (noise model)", side = 3, line = -1,at=h/2,col="black")
mtext("AR(3)", side = 3, line = -2,at=h/2,col="blue")
mtext("AR(9)", side = 3, line = -3,at=h/2,col="red")

#----------------------------------------------------------------------------------

# Section 8.4.2

simanz<-1000
h<-10
set.seed(10)
# Function computes in-sample squared forecast errors
for_sim_in<-function(h,len,j)
{
  set.seed(j)
  x<-rnorm(len+h)
  # White-noise
  forecast_error_true_in<-(mean(x)-x[(len+1):(len+h)])^2
  # AR(3)
  p<-3
  y_ar<-arima(x,order=c(p,0,0))
  x_for<-x[(len-p+1):(len)]
  a_for<-y_ar$coef[1:p]
  mu_hat<-y_ar$coef[p+1]
  for (i in 1:h)
  {
    x_for<-c(x_for,a_for%*%(x_for[length(x_for):(length(x_for)-p+1)]-mu_hat)+mu_hat)
  }
  x_for_h<-x_for[p+1:h]
  forecast_error_ar3_in<-(x_for_h-x[(len+1):(len+h)])^2
  # AR(9)
  p<-9
  y_ar<-arima(x,order=c(p,0,0))
  x_for<-x[(len-p+1):(len)]
  a_for<-y_ar$coef[1:p]
  mu_hat<-y_ar$coef[p+1]
  for (i in 1:h)
  {
    x_for<-c(x_for,a_for%*%(x_for[length(x_for):(length(x_for)-p+1)]-mu_hat)+mu_hat)
  }
  x_for_h<-x_for[p+1:h]
  forecast_error_ar9_in<-(x_for_h-x[(len+1):(len+h)])^2
  forecast_error<-c(forecast_error_true_in,forecast_error_ar3_in,forecast_error_ar9_in)
}

# Generate 1000 realizations and compute squared forecast errors
sample_id <- foreach(j = 1:simanz,.combine=rbind) %dopar% for_sim_in(h,len,j)
# Compute mean-square forecast errors for h=1,...,10
in_sample<-cbind(apply(sample_id[,1:h],2,mean),apply(sample_id[,h+1:h],2,mean),apply(sample_id[,2*h+1:h],2,mean))

ymin<-min(apply(in_sample,1,min))
ymax<-max(apply(in_sample,1,max))
ts.plot(in_sample[,1],xlab="Forecast horizon",ylab="MSFE",main="In-sample",ylim=c(ymin,ymax))
lines(in_sample[,2],col="blue")
lines(in_sample[,3],col="red")
mtext("True (white noise)", side = 3, line = -1,at=h/2,col="black")
mtext("AR(3)", side = 3, line = -2,at=h/2,col="blue")
mtext("AR(9)", side = 3, line = -3,at=h/2,col="red")



#------------------------------------------------------------------
# Section 8.5

set.seed(10)
len<-100
a_vec<-c(1.6,-1.4,0.5)
mu<-6/(1-sum(a_vec))
x<-mu+arima.sim(n=len,list(ar=a_vec))

par(mfrow=c(3,1))
ts.plot(x)
acf(x)
acf(x,type="partial")

#--------------------------------------------------------------------

# Section 8.5.1

simanz<-1000
h<-10
set.seed(10)
len<-100
a_vec<-c(1.6,-1.4,0.5)
mu<-6/(1-sum(a_vec))
x<-mu+arima.sim(n=len,list(ar=a_vec))
# Function computes out-of-sample squared forecast errors
for_sim_out_ar3<-function(h,len,a_vec,mu,j)
{
  set.seed(j)
  x<-mu+arima.sim(n=len+h,list(ar=a_vec))
  y_ar2<-arima(x[1:len],order=c(2,0,0))
  y_ar3<-arima(x[1:len],order=c(3,0,0))
  y_ar9<-arima(x[1:len],order=c(9,0,0))
  forecast_error_true_out<-(predict(y_ar2,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error_ar3_out<-(predict(y_ar3,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error_ar9_out<-(predict(y_ar9,n.ahead=h)$pred-x[(len+1):(len+h)])^2
  forecast_error<-c(forecast_error_true_out,forecast_error_ar3_out,forecast_error_ar9_out)
}

# Generate 1000 realizations and compute squared forecast errors
sample_id_out <- foreach(j = 1:simanz,.combine=rbind) %dopar% for_sim_out_ar3(h,len,a_vec,mu,j)
# Compute mean-square forecast errors for h=1,...,10
out_of_sample<-cbind(apply(sample_id_out[,1:h],2,mean),apply(sample_id_out[,h+1:h],2,mean),apply(sample_id_out[,2*h+1:h],2,mean))
ymin<-min(apply(out_of_sample,1,min))
ymax<-max(apply(out_of_sample,1,max))
ts.plot(out_of_sample[,1],xlab="Forecast horizon",ylab="MSFE",main="True model AR(3): MSFE out-of-sample",ylim=c(ymin,ymax))
lines(out_of_sample[,2],col="blue")
lines(out_of_sample[,3],col="red")
mtext("Misspecified AR(2)", side = 3, line = -1,at=h/2,col="black")
mtext("True AR(3)", side = 3, line = -2,at=h/2,col="blue")
mtext("Overparametrized AR(9)", side = 3, line = -3,at=h/2,col="red")



#-----------------------------------------------------------------------------
# Section 8.5.2

simanz<-1000
h<-10
set.seed(10)

# Function computes in-sample squared forecast errors
for_sim_in_ar3<-function(h,len,a_vec,mu,j)
{
  set.seed(j)
  x<-mu+arima.sim(n=len+h,list(ar=a_vec))
  # AR(2)
  p<-2
  y_ar<-arima(x,order=c(p,0,0))
  x_for<-x[(len-p+1):(len)]
  a_for<-y_ar$coef[1:p]
  mu_hat<-y_ar$coef[p+1]
  for (i in 1:h)
  {
    x_for<-c(x_for,a_for%*%(x_for[length(x_for):(length(x_for)-p+1)]-mu_hat)+mu_hat)
  }
  x_for_h<-x_for[p+1:h]
  forecast_error_true_in<-(x_for_h-x[(len+1):(len+h)])^2
  # AR(3)
  p<-3
  y_ar<-arima(x,order=c(p,0,0))
  x_for<-x[(len-p+1):(len)]
  a_for<-y_ar$coef[1:p]
  mu_hat<-y_ar$coef[p+1]
  for (i in 1:h)
  {
    x_for<-c(x_for,a_for%*%(x_for[length(x_for):(length(x_for)-p+1)]-mu_hat)+mu_hat)
  }
  x_for_h<-x_for[p+1:h]
  forecast_error_ar3_in<-(x_for_h-x[(len+1):(len+h)])^2
  # AR(9)
  p<-9
  y_ar<-arima(x,order=c(p,0,0))
  x_for<-x[(len-p+1):(len)]
  a_for<-y_ar$coef[1:p]
  mu_hat<-y_ar$coef[p+1]
  for (i in 1:h)
  {
    x_for<-c(x_for,a_for%*%(x_for[length(x_for):(length(x_for)-p+1)]-mu_hat)+mu_hat)
  }
  x_for_h<-x_for[p+1:h]
  forecast_error_ar9_in<-(x_for_h-x[(len+1):(len+h)])^2
  forecast_error<-c(forecast_error_true_in,forecast_error_ar3_in,forecast_error_ar9_in)
}

# Generate 1000 realizations and compute squared forecast errors
sample_id <- foreach(j = 1:simanz,.combine=rbind) %dopar% for_sim_in_ar3(h,len,a_vec,mu,j)
# Compute mean-square forecast errors for h=1,...,10
in_sample<-cbind(apply(sample_id[,1:h],2,mean),apply(sample_id[,h+1:h],2,mean),apply(sample_id[,2*h+1:h],2,mean))
ymin<-min(apply(in_sample,1,min))
ymax<-max(apply(in_sample,1,max))
ts.plot(in_sample[,1],xlab="Forecast horizon",ylab="MSFE",main="True model AR(3): in-sample MSFE",ylim=c(ymin,ymax))
lines(in_sample[,2],col="blue")
lines(in_sample[,3],col="red")
mtext("Misspecified AR(2)", side = 3, line = -1,at=h/2,col="black")
mtext("True AR(3)", side = 3, line = -2,at=h/2,col="blue")
mtext("Overparametrized AR(9)", side = 3, line = -3,at=h/2,col="red")




ymin<-min(100*(out_of_sample[,1]-in_sample[,1])/(in_sample[,1]))
ymax<-max(100*(out_of_sample[,3]-in_sample[,3])/(in_sample[,3]))
ts.plot(100*(out_of_sample[,1]-in_sample[,1])/(in_sample[,1]),
        xlab="Forecast horizon",ylab="% Difference out-of-sample vs in-sample MSFE",
        main="Percent performance gap between out-of sample and in-sample MSFE",ylim=c(ymin,ymax))
lines(100*(out_of_sample[,2]-in_sample[,2])/(in_sample[,2]),col="blue")
lines(100*(out_of_sample[,3]-in_sample[,3])/(in_sample[,3]),col="red")
mtext("%-Difference misspecified AR(2)", side = 3, line = -1,at=h/2,col="black")
mtext("%-Difference true AR(3)", side = 3, line = -2,at=h/2,col="blue")
mtext("%-Difference overparametrized AR(9)", side = 3, line = -3,at=h/2,col="red")