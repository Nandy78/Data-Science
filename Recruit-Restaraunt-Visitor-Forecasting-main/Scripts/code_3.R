# Code section 3.1


set.seed(10)
len<-100
y_1<-1:len+rnorm(len)
y_2<-rnorm(len)
mu_1<-mean(y_1)
mu_2<-mean(y_2)

par(mfrow=c(2,1))
ts.plot(y_1,col="blue",main="Linear trend",
        xlab="",ylab="")
lines(rep(mu_1,len),col="black")
ts.plot(y_2,col="blue",main="Constant mean",
        xlab="",ylab="")
lines(rep(mu_2,len),col="black")

#----------------------------------------------------
# Code section 3.5
set.seed(10)
len_1<-10
y_1<-rnorm(len_1)
len_2<-100
y_2<-rnorm(len_2)
len_3<-1000
y_3<-rnorm(len_3)


aucf<-matrix(ncol=3,nrow=10)
for (i in 0:9)
{
  aucf[i+1,1]<-(y_1[(1+i):len_1]-mean(y_1))%*%(y_1[1:(len_1-i)]-mean(y_1))/
    sum((y_1[1:len_1]-mean(y_1))^2)
  aucf[i+1,2]<-(y_2[(1+i):len_2]-mean(y_2))%*%(y_2[1:(len_2-i)]-mean(y_2))/
    sum((y_2[1:len_2]-mean(y_2))^2)
  aucf[i+1,3]<-(y_3[(1+i):len_3]-mean(y_3))%*%(y_3[1:(len_3-i)]-mean(y_3))/
    sum((y_3[1:len_3]-mean(y_3))^2)
}

par(mfrow=c(3,1))
ts.plot(aucf[,1],col="blue",main=paste("Sample length=",len_1,sep=""),
        xlab="",ylab="",ylim=c(min(aucf[,1]-1.96/sqrt(len_1)),
                               max(aucf[,1]+1.96/sqrt(len_1))))
lines(aucf[,1]-1.96/sqrt(len_1),lty=2,col="blue")
lines(aucf[,1]+1.96/sqrt(len_1),lty=2,col="blue")
lines(rep(0,len_1))
ts.plot(aucf[,2],main=paste("Sample length=",len_2,sep=""),
        xlab="",ylab="",ylim=c(min(aucf[,2]-1.96/sqrt(len_2)),
                               max(aucf[,2]+1.96/sqrt(len_2))),col="blue")
lines(aucf[,2]-1.96/sqrt(len_2),lty=2)
lines(aucf[,2]+1.96/sqrt(len_2),lty=2)
lines(rep(0,len_2))
ts.plot(aucf[,3],main=paste("Sample length=",len_3,sep=""),
        xlab="",ylab="",ylim=c(min(aucf[,3]-1.96/sqrt(len_3)),
                               max(aucf[,3]+1.96/sqrt(len_3))),col="blue")
lines(aucf[,3]-1.96/sqrt(len_3),lty=2)
lines(aucf[,3]+1.96/sqrt(len_3),lty=2)
lines(rep(0,len_3))


acf(y_1,plot=F,lag.max=9)
round(aucf[,1],3)
# sample length 100
acf(y_2,plot=F,lag.max=9)
round(aucf[,2],3)
# sample length 1000
acf(y_3,plot=F,lag.max=9)
round(aucf[,3],3)

par(mfrow=c(2,1))
ts.plot(aucf[,2],main=paste("Sample length=",len_2,sep=""),
        xlab="",ylab="",ylim=c(min(aucf[,2]-1.96/sqrt(len_2)),max(aucf[,2]+1.96/sqrt(len_2))),col="blue")
lines(aucf[,2]-1.96/sqrt(len_2),lty=2)
lines(aucf[,2]+1.96/sqrt(len_2),lty=2)
lines(rep(0,len_2))
acf(y_2,lag.max=9)



#---------------------------------------
# Code section 3.7

set.seed(100)
len<-200
# fixed AR(1)
a1<-0.9
y_1<-rep(0,len)
eps<-rnorm(len)
for (i in 2:len)
{
  y_1[i]<-a1*y_1[i-1]+eps[i]
}

par(mfrow=c(2,1))
acf(y_1)
acf(y_1,type="partial")


set.seed(100)
len<-200
# fixed AR(1)
b1<--0.9
y_1<-rep(0,len)
eps<-rnorm(len)
for (i in 2:len)
{
  y_2[i]<-eps[i]+b1*eps[i-1]
}

par(mfrow=c(2,1))
acf(y_2)
acf(y_2,type="partial")

