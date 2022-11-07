set.seed(10)
len<-1000
# GARCH(1,1) - use default omega and specify alpha/beta
spec = garchSpec(model = list(alpha = 0.2, beta = 0.7))
x<-garchSim(spec, n = len)

par(mfrow=c(2,1))
acf(x)
acf(x^2)