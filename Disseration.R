#Dissertation

#Packages

install.packages("quantmod")
install.packages("evir")
install.packages("qrmdata")
install.packages("fGarch")
library(quantmod)
library(fBasics)
library(evir)
library(qrmdata)
library(zoo)
library(moments)
library(tseries)
library(fGarch)
library(tseries)
library(moments)
library(sandwich)
library(fGarch)


myplot <- function( dates , y , col='darkblue' , t='l' , lwd=2 , ylim=NULL , main=NULL ){
  if( is.null(main) ){ par( mar=c(2,2,0.1,0.1) ) }
  plot( dates , y , t=t , col=col , lwd=lwd , axes=F , xlab='' , ylab='' , xaxs="i" , ylim=ylim , main=main )
  xticks <- axis.Date(1, x=dates, at=seq(dates[1], dates[length(dates)], "year") , lwd=0, lwd.tick=1, tck=0.02)
  yticks <- axis(2 , lwd=0, lwd.tick=1, tck=0.02)
  axis.Date(3, x=dates, at=seq(dates[1], dates[length(dates)], "year"), lwd=0, lwd.tick=1, tck=0.02, lab=F)
  axis(4, lwd=0, lwd.tick=1, tck=0.02, lab=F)
  abline( h=yticks , lty=3 )
  abline( v=xticks , lty=3 )
  box()
}

#SECTION 1: PRELIMINARY ANALYSIS

#Data Extraction using quantmod from Yahoo Finance Datasource. REFERENCE!

getSymbols('AAPL',src='yahoo')

#Plot Price Data visualization.
#Final plot. Define and explain Bollinger Bands + MACD + Signal.

barChart(AAPL$AAPL.Adjusted)
candleChart(AAPL,multi.col=TRUE,theme="white") 
chartSeries(AAPL, theme = "white") 
addMACD() 
addBBands() 

#Defining Time Series Variables.

T <- nrow(AAPL) #Time spam
AAPL <- AAPL[ seq(T,1,-1) , ]
head(AAPL)
tail(AAPL)
dates <- as.Date( as.character( index(AAPL) ) , '%Y-%m-%d' ) #Dates formatting
head(dates)
tail(dates)

#Selecting our proxy for price: Adjusted Price

price <- AAPL$AAPL.Adjusted 
barChart(price, theme="white")
candleChart(price,multi.col=TRUE,theme="white") 
chartSeries(price, theme = "white") 
addMACD() 
addBBands() 

#Early conclusions: financial data is not independent, it has noticable volatility, and has extremes values-behaviours.

#Defining the returns: 
#REFERENCE: https://quantivity.wordpress.com/2011/02/21/why-log-returns/

returns <- diff( log( price) )*100
barChart(returns[2:T], theme="white")
abline(h=0 , lwd=2)
returns <- cbind.data.frame(dates[2:T],returns[2:T])
return  <- returns$AAPL.Adjusted

#Again we can observe a clear presenceof volatility clusters and it is suggested tha the returs are more stationary than prices.

#Second return definition. Equivalent need to choose.

AAPL.close<-AAPL[,4]
str(AAPL.close)
AAPL.price<-as.zoo(AAPL.close)
AAPL.return <-diff(log(AAPL.price))[-1]*100
colnames(AAPL.return) <- "AAPL.return"
head(AAPL.return, n = 10)
plot( index(AAPL.return) , AAPL.return , col='red2' )
abline(h=0 , lwd=2)

#Augmented Dickey Fuller Test.

adf1<-adf.test(AAPL.price)
adf2<-adf.test(return)

# After the ADF our intuition is statistically confirmed: Prices nonstationary, returns they are stationary.

#Analysing the distribution of the data. WRITE THE TITLES!

#Histogram Plots

hist(AAPL.price,50,freq=FALSE,col='tomato',border='darkred',main='')
hist(return,50,freq=FALSE,col='tomato',border='darkred',main='Histogram Apple Log-Returns', xlab = "", xlim = c(-20,20))

#In the case of returns, we can claim a normal distribution or a t-distribution due to fat tails. Further exploration below:

#QQplots. Exploration of the normality assumption and fat tails evidences (kurtosis)
par( mar=c(2,2,1,1) , xaxs="i" , mfrow=c(1,1) )
qqnorm(AAPL.price,col='tomato',main='')
qqline(AAPL.price,lwd=2,lty=3)
qqnorm(return,col='tomato',main='QQ-Plot Apple Log-returns', pch=19)
qqline(return,lwd=2,lty=3)

#1: Evidences of fat tails
#2: We can notice that negative returs have stonger impact

#Kernel Distribution. More exploration for our distributional assumptions.
#DETAIL KERNEL UTILITY AND CHOICE OF BANDWITH.

kernel <- density(AAPL.price)
kernel

plot( kernel , main='' , yaxs='i' )
polygon( kernel , col="tomato" , border='darkred' )
lines( seq(-10,1000,0.1) , dnorm( seq(-10,1000,0.1) , mean(AAPL.price) , sd(AAPL.price) ) , col='darkblue' ,lwd=4)

kernel <- density(return)
kernel

plot( kernel , yaxs='i' , main="Kernel Density Estimator of Apple Log-Returns")
polygon( kernel , col="tomato" , border='darkred')
lines( seq(-10,10,0.1) , dnorm( seq(-10,10,0.1) , mean(AAPL.return) , sd(AAPL.return) ) , col='darkblue' ,lwd=4)

#Again evidences of fat tails in the returns side. 

#General Description and Jarque-Bera Test for normality

descr <- as.matrix( c( mean(return) , sqrt(252)*sd(return) , skewness(return) , kurtosis(return) ) ) 
dimnames(descr)[[1]] <- list('Mean','Annual Volatility','Skewness','Kurtosis')
print( descr )
jb<-jarque.test(return)

descr <- as.matrix( c( mean(AAPL.price) , sqrt(252)*sd(AAPL.price) , skewness(AAPL.price) , kurtosis(AAPL.price) ) ) 
dimnames(descr)[[1]] <- list('Mean','Annual Volatility','Skewness','Kurtosis')
print( descr )
AAPL.price<-as.vector(AAPL.price)
jarque.test(AAPL.price)

#First conclusions:
#1. We cannot deal with prices. The forecast of stock prices per se is not viable due to the distribution and characteristics of the data.
#1.1. Prices do not exhibit stationarity, their distribution is not homogenuous, Behaviour as arandom walk what makes extreamly difficult the characterisation and forecasting tasks.
#1.2. Thus, trying to forecast the conditional mean of prices is not a good deal.
#2. In terms of returns, nice attributes found.
#2.1. We can claim that they are stationary, their conditional mean is constant around 0 and we can find consistent approximations of it distribution (Normal or T-Student -dealing with fat tails)
#2.2. Some attibutes need to be tackled. Fat tales and importance of asymmetric effects.

#Serial Correlation Analysis
#ACF and PACF.
#Well documented explanation in the lecture notes. CHECK IN CASE OF DOUBTS.
par( mar=c(2,2,1,1) , xaxs="i" , mfrow=c(2,1))
ret.acf <- acf(return, plot=FALSE)
plot(ret.acf,  main= "Autocorrelation Function Apple Log-Returns", ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')
ret.pacf <- pacf(return, ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2', main= "Partial Autocorrelation Function Apple Log-Returns" )
mtext("Autocorrelation Function Apple Log-Returns", side = 3, line = 16)
mtext("Partial Autocorrelation Function Apple Log-Returns", side = 3, line = -3)

#We can see some cases for serial correlation between certain lags. (lag 4 and 16 specially)

#Ljung-Box Tests - Statistical serial correlation test.

Box.test( AAPL.return ,  type="Ljung-Box" )

#We reject for almost all the lags the serial correlation assumption.

#Training, Test and Validation Sets Definition.

#All data: 1st Jan 2007 - Today
T <- nrow(AAPL)
AAPL <- AAPL[ seq(T,1,-1) , ]
all<-AAPL$AAPL.Adjusted
head(all)
tail(all)
head(returns)
tail(returns)
length(returns$AAPL.Adjusted)
length(all)

#Train data: From 1st Jan 2007 - 30th Dec 2016 (10 years)
t<-AAPL[ seq(2518,1,-1) , ]
train<-t$AAPL.Adjusted
head(train)
tail(train)
ret.train<-returns[ 1:2517 ,]
head(ret.train)
tail(ret.train)
length(train)
length(ret.train$AAPL.Adjusted)

#Test data: From 1st Jan 2017 - 30th Dec 2018 (2 years)
tt<-AAPL[ seq(3020,2519,-1) , ]
test<-tt$AAPL.Adjusted
head(test)
tail(test)
ret.test<-returns[ 2518:3019 ,]
head(ret.test)
tail(ret.test)
length(test)
length(ret.test$AAPL.Adjusted)

#Validation set: From 1st Jan 2019 - Today (Half year)
v<-AAPL[ seq(length(AAPL$AAPL.Adjusted),3021,-1) , ]
val<-v$AAPL.Adjusted
head(val)
tail(val)
ret.val<-returns[ 3020:length(returns$`dates[2:T]`) ,]
head(ret.val)
tail(ret.val)
length(val)
length(ret.val$AAPL.Adjusted)

#Train + Test sets

x<-rbind(train, test)
dates <- as.Date(as.character(index(x)),'%Y-%m-%d')
y <- as.vector(train$AAPL.Adjusted)
y.out <-as.vector(test$AAPL.Adjusted)
s.train <-length(y) 
s.test <- length(y.out)

r<-rbind(ret.train, ret.test)
dates.r <- as.Date(as.character(r$`dates[2:T]`),'%Y-%m-%d')
y.r <- as.vector(ret.train$AAPL.Adjusted)
y.r.out <-as.vector(ret.test$AAPL.Adjusted)
s.r.train <-length(y.r) 
s.r.test <- length(y.r.out)

# Ljung-Box Test

Box.test( y, lag=22 , type="Ljung-Box" )

# ACF & PACF

par( mar=c(2,2,1,1) , mfrow=c(2,1) )
acf( y , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( y , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)

# Ljung-Box Test

Box.test( y.r, lag=22 , type="Ljung-Box" )

# ACF & PACF

par( mar=c(2,2,1,1) , mfrow=c(2,1) )
acf( y.r , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( y.r , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)

#SECTION 3: LINEAR MODELS

#Models

ar1    = arima(y.r,order=c(1,0,0))
ma1    = arima(y.r,order=c(0,0,1))
ma2    = arima(y.r,order=c(0,0,2))
arma11 = arima(y.r,order=c(1,0,1))

#Goodness of fit

#AIC
ar1_aic    <- (-2*ar1$loglik+2*3)/s.r.train
ma1_aic    <- (-2*ma1$loglik+2*3)/s.r.train
ma2_aic    <- (-2*ma2$loglik+2*4)/s.r.train
arma11_aic <- (-2*arma11$loglik+2*4)/s.r.train

#BIC
ar1_bic    <- (-2*ar1$loglik+log(s.r.train)*3)/s.r.train
ma1_bic    <- (-2*ma1$loglik+log(s.r.train)*3)/s.r.train
ma2_bic    <- (-2*ma2$loglik+log(s.r.train)*4)/s.r.train
arma11_bic <- (-2*arma11$loglik+log(s.r.train)*4)/s.r.train

#Summary
round( rbind( c(ar1$loglik,ma1$loglik.ma2$loglik,arma11$loglik), 
              c(ar1_aic,ma1_aic,ma2_aic,arma11_aic) , 
              c(ar1_bic,ma1_bic,ma2_bic,arma11_bic) ) ,  3 )

#Expected value and Residuals computation

ar1_mu     <- as.vector(y.r)-ar1$residuals
ma1_mu     <- y.r-ma1$residuals
ma2_mu     <- y.r-ma2$residuals
arma11_mu  <- y.r-arma11$residuals

ar1_res    <- as.numeric(ar1$residuals)
ma1_res    <- as.numeric(ma1$residuals)
ma2_res    <- as.numeric(ma2$residuals)
arma11_res <- as.numeric(arma11$residuals)

#Plots (mean and residuals)

# AR1
par( mar=c(2,2,1,1) , xaxs="i" , mfrow=c(2,2) )
myplot( dates[1:s.r.train] , ar1_mu , t='l' , lwd=2 , col='springgreen4' )
legend('topright',c('Mean AR(1)'),col=c('springgreen4'),lwd=3)
grid( lwd=1 , col="darkgrey" )
myplot( dates[1:s.r.train] , ar1_res/sd(ar1_res) , col='darkmagenta' , t='p'  , ylim=c(-4,4) )
legend('topright',c('Residuals  AR(1)'),col=c('darkmagenta'),lwd=3)
abline( h=0 , lwd=2 )
grid( lwd=1 , col="darkgrey" )
acf( ar1_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( ar1_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)

# MA1
par( mar=c(2,2,1,1) , xaxs="i" , mfrow=c(2,2) )
myplot( dates[1:s.r.train] , ma1_mu , col='springgreen4' )
legend('topright',c('Mean MA(1)'),col=c('springgreen4'),lwd=3)
grid( lwd=1 , col="darkgrey" )
myplot( dates[1:s.r.train] , ma1_res/sd(ma1_res) , col='darkmagenta' , t='p' , ylim=c(-4,4) )
abline( h=0 , lwd=2 )
legend('topright',c('Residuals MA(1)'),col=c('darkmagenta'),lwd=3)
grid( lwd=1 , col="darkgrey" )
acf( ma1_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( ma1_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)


# ARMA11
par( mar=c(2,2,1,1) , xaxs="i" , mfrow=c(2,2) )
myplot( dates[1:s.r.train] , arma11_mu , lwd=2 , col='springgreen4')
grid( lwd=1 , col="darkgrey" )
legend('topright',c('Mean ARIMA (1,0,1)'),col=c('springgreen4'),lwd=3)
myplot( dates[1:s.r.train] , arma11_res/sd(arma11_res) , col='darkmagenta' , t='p' , ylim=c(-4,4) )
abline( h=0 , lwd=2 )
legend('topright',c('Residuals ARIMA (1,0,1)'),col=c('darkmagenta'),lwd=3)
grid( lwd=1 , col="darkgrey" )
acf( arma11_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( arma11_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)


# Kernel Distribution Analysis

par( mar=c(2,2,3,2) , mfrow=c(3,1) )
kernel <- density(ar1_res/sqrt(ar1$sigma2))
plot( kernel , main='AR1' )
polygon( kernel , col="tomato" , border='darkred')
abline(h=0,lwd=2)
lines( seq(-10,20,0.1) , dnorm( seq(-10,20,0.1) ) , col='darkblue' ,lwd=2 )

kernel <- density(ma1_res/sqrt(ma1$sigma2))
plot( kernel , main='MA1' )
polygon( kernel , col="tomato" , border='darkred')
abline(h=0,lwd=2)
lines( seq(-10,20,0.1) , dnorm( seq(-10,20,0.1) ) , col='darkblue' ,lwd=2 )

kernel <- density(arma11_res/sqrt(arma11$sigma2))
plot( kernel , main='ARMA11' )
polygon( kernel , col="tomato" , border='darkred')
abline(h=0,lwd=2)
lines( seq(-10,20,0.1) , dnorm( seq(-10,20,0.1) ) , col='darkblue' ,lwd=2 )

#QQnorm Analysis

par( mar=c(2,2,3,2) , mfrow=c(3,1) )

qqnorm(ar1_res,col='tomato',main='AR1')
qqline(ar1_res,lwd=2,lty=3)
qqnorm(ma1_res,col='tomato',main='MA1')
qqline(ma1_res,lwd=2,lty=3)
qqnorm(arma11_res,col='tomato',main='ARMA11')
qqline(arma11_res,lwd=2,lty=3)

#Prediction all test using only training

#Predictions
ar1_pred    <- predict( ar1 , n.ahead=s.r.test )
ma1_pred    <- predict( ma1 , n.ahead=s.r.test)
arma11_pred <- predict( arma11 , n.ahead=s.r.test)

#MSE
ar1_mse    = sqrt( mean( (y.r.out - as.numeric(ar1_pred$pred) )**2 ) )
ma1_mse    = sqrt( mean( (y.r.out - as.numeric(ma1_pred$pred) )**2 ) )
arma11_mse = sqrt( mean( (y.r.out - as.numeric(arma11_pred$pred) )**2 ) )

#Plots
par( mar=c(2,2,3,2) , mfrow=c(1,1) )
myplot( dates.r[(s.r.train-60):(s.r.train+s.r.test)] , c(y.r[(s.r.train-60):s.r.train], y.r.out) , t='b', main=sprintf('AR(1) RMSE %3.3f',ar1_mse) , col='darkorange' ) 
abline( v=dates.r[s.r.train] , lwd=2 )
abline( h=ar1$coef['intercept'] , lwd=2 )
lines( dates.r[(s.r.train-60):s.r.train] , ar1_mu[(s.r.train-60):s.r.train] , t='l' , lwd=2 , col='blue3' )
lines( dates.r[(s.r.train+1):(s.r.train+s.test)] , as.numeric(ar1_pred$pred)  , t='b' , lwd=2 , col='blue3' )

myplot(dates.r[(s.r.train-60):(s.r.train+s.r.test)] , c(y.r[(s.r.train-60):s.r.train], y.r.out) , t='b' , main=sprintf('MA(1) RMSE %3.3f',ma1_mse)  , col='darkorange' ) 
abline( v=dates.r[s.r.train] , lwd=2 )
abline( h=ma1$coef['intercept'] , lwd=2 )
lines( dates.r[(s.r.train-60):s.r.train] , ma1_mu[(s.r.train-60):s.r.train] , t='l' , lwd=2 , col='blue3' )
lines( dates.r[(s.r.train+1):(s.r.train+s.r.test)] , as.numeric(ma1_pred$pred)  , t='b' , lwd=2 , col='blue3' )

myplot( dates.r[(s.r.train-60):(s.r.train+s.r.test)] , c(y.r[(s.r.train-60):s.r.train], y.r.out) , t='b' , main=sprintf('ARMA(1,1) RMSE %3.3f',arma11_mse) , col='darkorange' ) 
abline( v=dates.r[s.r.train] , lwd=2 )
abline( h=arma11$coef['intercept'] , lwd=2 )
lines( dates.r[(s.r.train-60):s.r.train] , arma11_mu[(s.r.train-60):s.r.train] , t='l' , lwd=2 , col='blue3' )
lines( dates.r[(s.r.train+1):(s.r.train+s.r.test)] , as.numeric(arma11_pred$pred)  , t='b' , lwd=2 , col='blue3' )

round(c( ar1_mse , ma1_mse , ma2_mse ,  arma11_mse )*100 , 3 )

#Predictions rolling basis

#Model specification and prediction computation

ma1_pred    <- rep(0,s.r.test)
ma2_pred    <- rep(0,s.r.test)
arma11_pred <- rep(0,s.r.test)

for( m in 0:(s.r.test-1) ){
  
  r <- as.vector(y.r)[(m):(s.r.train+m)]
  
  ma1    = arima(r,order=c(0,0,1))
  ma2    = arima(r,order=c(0,0,2))
  arma11 = arima(r,order=c(1,0,1))
  
  ma1_pred[1+m]    <- predict( ma1 , n.ahead=1 )$pred
  ma2_pred[1+m]    <- predict( ma2 , n.ahead=1 )$pred
  arma11_pred[1+m] <- predict( arma11 , n.ahead=1 )$pred
}

#Residuals
ma1_r2    = 1-mean( (y.r.out - ma1_pred )**2 )/mean( (mean(y.r)-y.r.out)**2 )
ma2_r2    = 1-mean( (y.r.out - ma2_pred )**2 )/mean( (mean(y.r)-y.r.out)**2 )
arma11_r2 = 1-mean( (y.r.out - arma11_pred )**2 )/mean( (mean(y.r)-y.r.out)**2 )

#Plot predictions

#MA1
myplot( dates.r[(s.r.train-100):(s.r.train+s.r.test)] , c(y.r[(s.r.train-100):s.r.train], y.r.out) , t='b' , main=sprintf('MA(1) R2 %3.3f',ma1_r2) , col='darkorange' ) 
abline( v=dates[s.r.train] , lwd=2 )
abline( h=ma1$coef['intercept'] , lwd=2 )
lines( dates.r[(s.r.train-100):s.r.train] , ma1_mu[(s.r.train-100):s.r.train] , t='l' , lwd=2 , col='blue3' )
lines( dates.r[(s.r.train+1):(s.r.train+s.r.test)] , ma1_pred  , t='b' , lwd=2 , col='blue3' )


#ARMA
myplot( dates.r[(s.r.train-100):(s.r.train+s.r.test)] , c(y.r[(s.r.train-100):s.r.train], y.r.out) , t='b', ylim = c(-0.1,0.1) , main=sprintf('ARMA(1,1) R2 %3.3f',arma11_r2) , col='darkorange' ) 
abline( v=dates[N] , lwd=2 )
abline( h=arma11$coef['intercept'] , lwd=2 )
lines( dates[1:s.r.train] , arma11_mu[1:s.r.train] , t='l' , lwd=2 , col='blue3' )
lines( dates[(s.r.train+1):(s.r.train+s.r.test)] , arma11_pred  , t='b' , lwd=2 , col='blue3' )

round(c(  ma1_r2 , ma2_r2 ,  arma11_r2 )*100 , 3 )

#GARCH

#Tgarch Function. Including asymmetric effects!

Tgarch11 = function(x,cond.dist="norm")
{
  # Estimation of TGARCH(1,1) model assuming Gaussian or t-Student residuals
  # 1: Initialize Time Series:
  Tx <<- x
  # 2: Define Model Parameters and Bounds:
  Meanx = mean(Tx); Varx = var(Tx); S = 1e-6
  if(cond.dist=="std"){
    params = c(mu = Meanx, omega = 0.1*Varx, alpha = 0.1, gam1= 0.02, beta = 0.81, shape=6)
    lowerBounds = c(mu = -10*abs(Meanx), omega = S^2, alpha = S, gam1=S, beta = S, shape=3)
    upperBounds = c(mu = 10*abs(Meanx), omega = 100*Varx, alpha = 1-S, gam1 = 1-S, beta = 1-S, shape=30)
  }
  else{
    params = c(mu = Meanx, omega = 0.1*Varx, alpha = 0.1, gam1= 0.02, beta = 0.81)
    lowerBounds = c(mu = -10*abs(Meanx), omega = S^2, alpha = S, gam1=S, beta = S)
    upperBounds = c(mu = 10*abs(Meanx), omega = 10*Varx, alpha = 1-S, gam1 = 1-S, beta = 1-S)
  }
  # 3: Set Conditional Distribution Function:
  garchDist = function(z, hh, cond.dist, nu1) { 
    if(cond.dist=="std"){LL=dstd(x = z/hh, nu=nu1)/hh}
    else{
      LL=dnorm(x = z/hh)/hh }
    LL
  }
  # 4: Log-Likelihood Function:
  garchLLH = function(parm) {
    mu = parm[1]; omega = parm[2]; alpha = parm[3]; gam1=parm[4]; beta = parm[5]
    shape = 0; 
    if(length(parm)==6){
      shape=parm[6]
      cond.dist="std"
    }
    else
    {cond.dist="norm"}
    z = (Tx-mu); Mean = mean(z^2)
    zm1=c(0,z[-length(z)])
    idx=seq(zm1)[zm1 < 0]; z1=rep(0,length(z)); z1[idx]=1
    # Filter Representation:
    e = omega + alpha * c(Mean, z[-length(z)]^2) + gam1*z1*c(Mean,z[-length(z)]^2)
    h = filter(e, beta, "r", init = Mean)
    hh = sqrt(abs(h))
    llh = -sum(log(garchDist(z, hh, cond.dist, shape)))
    llh }
  # 5: Estimation of the parameters and computation of the Hessian:
  fit = nlminb(start = params, objective = garchLLH,
               lower = lowerBounds, upper = upperBounds) ### control = list(trace=3))
  epsilon = 0.0001 * fit$par
  npar=length(params)
  Hessian = matrix(0, ncol = npar, nrow = npar)
  for (i in 1:npar) {
    for (j in 1:npar) {
      x1 = x2 = x3 = x4  = fit$par
      x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
      x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
      x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
      x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
      Hessian[i, j] = (garchLLH(x1)-garchLLH(x2)-garchLLH(x3)+garchLLH(x4))/
        (4*epsilon[i]*epsilon[j])
    }
  }
  cat("Log likelihood at MLEs: ","\n")
  print(-garchLLH(fit$par))
  # 6: Create and print results:
  se.coef = sqrt(diag(solve(Hessian)))
  tval = fit$par/se.coef
  matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
  dimnames(matcoef) = list(names(tval), c(" Estimate",
                                          " Std. Error", " t value", "Pr(>|t|)"))
  cat("\nCoefficient(s):\n")
  printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
  # Compute the output
  est=fit$par
  mu = est[1]; omega = est[2]; alpha = est[3]; gam1=est[4]; beta = est[5]
  z=(Tx-mu); Mean = mean(z^2)
  zm1=c(0,z[-length(z)])
  idx=seq(zm1)[zm1 < 0]; z1=rep(0,length(z)); z1[idx]=1
  e = omega + alpha * c(Mean, z[-length(z)]^2) + gam1*z1*c(Mean,z[-length(z)]^2)
  h = filter(e, beta, "r", init = Mean)
  sigma.t = sqrt(abs(h))
  
  Tgarch11 <- list(residuals = z, volatility = sigma.t, par=est)
}

#Data Formulation

r<-rbind(ret.train, ret.test)
dates.r <- as.Date(as.character(r$`dates[2:T]`),'%Y-%m-%d')
y.r <- as.vector(ret.train$AAPL.Adjusted)
y.r.out <-as.vector(ret.test$AAPL.Adjusted)
s.r.train <-length(y.r) 
s.r.test <- length(y.r.out)

par( mar=c(2,2,3,2) , mfrow=c(3,1) )
plot( dates.r, r$AAPL.Adjusted , col='tomato' , pch=19, main="APPLE Returns")
abline(h=0 , lwd=2)
plot( dates.r, abs(r$AAPL.Adjusted) , col='tomato' , pch=19, main="APPLE Absolute Returns")
abline(h=0 , lwd=2)
plot( dates.r , (r$AAPL.Adjusted)**2 , col='tomato' , pch=19, main="APPLE Squared Returns")
abline(h=0 , lwd=2)

# ACFs
# ACF ret
ret.acf <- acf(y.r , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2', main="ACF APPLE Returns")

# ACF |ret|
ret.acf <- acf(abs(y.r) , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2', main="ACF APPLE Absolute Returns")


# ACF ret^2
ret.acf <- acf(y.r**2 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2', main="ACF APPLE Squared Returns")


r.plus  <- r$AAPL.Adjusted[1:(T-1)]*(r$AAPL.Adjusted[1:(T-1)]>0) 
r.minus <- r$AAPL.Adjusted[1:(T-1)]*(r$AAPL.Adjusted[1:(T-1)]<0) 
levreg  <- lm( abs(r$AAPL.Adjusted[2:T]) ~ 0+r.plus + r.minus )
par( mar=c(10,2,1,1) , xaxs="i" , mfrow=c(1,1) )
plot( r$AAPL.Adjusted[1:(T-1)] , abs(r$AAPL.Adjusted[2:T]) , pch=16 , col='darkorange' , yaxs='i' , tck=0.02 , xlab='lagged ret', ylab='absolute ret')
abline( a=0 , b=coef(levreg)['r.plus']  , col='red2' , lwd=2 )
abline( a=0 , b=coef(levreg)['r.minus'] , col='red2' , lwd=2 )
grid()


garch11 <- garchFit(y.r ~ garch(1, 1), data = y.r, trace = FALSE)
summary(garch11)
sigma <- volatility(garch11, type = "sigma")
z     <- y.r/sigma
z.plus  <- z[1:(s.r.train-1)]*(z[1:(s.r.train-1)]>0) 
z.minus <- z[1:(s.r.train-1)]*(z[1:(s.r.train-1)]<0) 
levreg  <- lm( abs(z[2:s.r.train]) ~ 0+z.plus + z.minus )

par( mar=c(2,2,3,2) , mfrow=c(1,1) )
plot( z[1:(s.r.train-1)] , abs(z[2:s.r.train]) , pch=19 , col='orange' , yaxs='i' , tck=0.02 , xlab='Lagged Standarized Residual', ylab='Absolute Standarized Residuals' , xlim=c(-6,6) , ylim=c(0,6), main="Apple's lag returns against Apple's absoulte returns")
abline( a=0 , b=coef(levreg)['z.plus']  , col='springgreen4' , lwd=3 )
abline( a=0 , b=coef(levreg)['z.minus'] , col='springgreen4' , lwd=3 )
grid(lwd = 2)


neg.dummy          <- y.r[1:(s.r.train-1)]<0
neg_sign_bias.test <- lm( z[2:s.r.train]**2 ~ neg.dummy )
summary( neg_sign_bias.test )

neg.size           <- y.r[1:(s.r.train-1)]*(y.r[1:(s.r.train-1)]<0)
neg_size_bias.test <- lm( z[2:s.r.train]**2 ~ neg.size )
summary( neg_size_bias.test )

# ARCHLM test
y <- r$AAPL.Adjusted[4:length(r$AAPL.Adjusted)]**2
n <- length(r$AAPL.Adjusted)
X = cbind( r$AAPL.Adjusted[3:(length(r$AAPL.Adjusted)-1)]**2 , r$AAPL.Adjusted[2:(length(r$AAPL.Adjusted)-2)]**2 , r$AAPL.Adjusted[1:(length(r$AAPL.Adjusted)-3)]**2 )

archlm = lm(y ~ X)
archlm.statistic <- n*summary( archlm )$r.squared
archlm.p.value <- 1-pchisq(archlm.statistic,3)

arch3 <- garchFit(y.r ~ garch(1, 1), data = y.r, trace = FALSE)
summary(arch3)

# vol
sigma <- volatility(arch3, type = "sigma")

myplot( dates.r[1:length(sigma)] , sqrt(252)*sigma , col='red2' )

z <- y.r/sigma
myplot( dates.r[1:length(sigma)] , z , col='red2' )

qqnorm(y.r,col='tomato',main='')
qqline(y.r,lwd=2,lty=3)

qqnorm(z,col='tomato',main='')
qqline(z,lwd=2,lty=3)

jb.r <- jarque.test(y.r)
jb.z <- jarque.test(z[4:length(y.r)])

acf( abs(y.r) , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')

acf( abs(z[4:length(z)]) , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')

garch11 <-garchFit(y.r ~ garch(1, 1), data = y.r, trace = FALSE)
tarch11 <-Tgarch11(y.r)

summary(garch11)
summary(tarch11)

sigma <- volatility(garch11, type = "sigma")
sigmat<- tarch11$volatility

myplot( dates.r[1:length(sigma)] , sqrt(252)*sigma , col='red2' )
myplot( dates.r[1:length(sigmat)] , sigmat*sqrt(252) , col='red2' )

myplot( dates.r[1:length(sigma)] , sqrt(sigma) , col='red2' )
myplot( dates.r[1:length(sigmat)], sigmat , col='red2' )

par( mar=c(2,2,3,2) , mfrow=c(3,1) )
myplot( dates.r[1:length(sigma)] ,y.r, col='tomato' , main="APPLE Returns")
myplot( dates.r[1:length(sigma)] , sqrt(252)*sigma , col='yellowgreen', main= "APPLE Returns Estimated Annual Volatility by GARCH")
myplot( dates.r[1:length(y.r)] , y.r, col='orange', main= "APPLE Returns GARCH Estimated CI" )
lines( dates.r[1:length(y.r)] , 1.96*sigma , col='springgreen4')
lines( dates.r[1:length(y.r)] , -1.96*sigma , col='springgreen4')

myplot( dates.r[1:length(sigma)] , y.r , col='tomato' , main="APPLE Returns")
myplot( dates.r[1:length(sigmat)] , sigmat*sqrt(252) , col='yellowgreen' , main= "APPLE Returns Estimated Annual Volatility by TARCH" )
myplot( dates.r[1:length(y.r)] , y.r, col='orange',  main= "APPLE Returns and TARCH Estimated CI" )
lines( dates.r[1:length(y.r)] , 1.96*sigmat , col='springgreen4')
lines( dates.r[1:length(y.r)] , -1.96*sigmat , col='springgreen4')

z <- y.r/sigma
myplot( dates.r[1:length(y.r)] , z , col='red2' )

par( mar=c(2,2,0.1,0.1) )
qqnorm(y.r,col='tomato',main='')
qqline(y.r,lwd=2,lty=3)

par( mar=c(2,2,0.1,0.1) )
qqnorm(z,col='tomato',main='')
qqline(z,lwd=2,lty=3)

jb.r <- jarque.test(r$AAPL.Adjusted)
jb.z <- jarque.test(z[2:length(r$AAPL.Adjusted)])

par( mar=c(2,2,0.1,0.1) )
acf( abs(r$AAPL.Adjusted) , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')

par( mar=c(2,2,3,2) , mfrow=c(2,1))
acf( z , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2', main="ACF Standarized Residuals GARCH(1,1)")
pacf( z , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2', main="PACF Standarized Residuals GARCH(1,1)")


var.g = rep( 0 , s.r.test)
var.t = rep( 0 , s.r.test)
var.r = rep( 0 , s.r.test)

var.g = rep( 0 , s.r.test)
var.t = rep( 0 , s.r.test)
var.r = rep( 0 , s.r.test)

for( t in s.r.train:(s.r.train+s.r.test-1) ){
  
  cat('.')
  
  ret.in  <- r$AAPL.Adjusted[ 1:t]
  ret.out <- r$AAPL.Adjusted[ (t+1):(s.r.train+s.r.test)]
  
  # fit
  garch11     <- garchFit(ret.in ~ garch(1, 1), data = ret.in, trace = FALSE)
  vol.garch   <- volatility(garch11, type = "sigma")
  theta.garch <- coef(garch11)
  var.garch   <- theta.garch['omega'] + theta.garch['alpha1']*tail(ret.in,1)**2 + theta.garch['beta1']*tail(vol.garch,1)^2
  
  tarch11     <- Tgarch11(ret.in)
  vol.tarch   <- tarch11$volatility
  theta.tarch <- tarch11$par
  var.tarch   <- theta.tarch['omega'] + theta.tarch['alpha']*tail(ret.in,1)**2 + theta.tarch['gam1']*tail(ret.in,1)**2 * (tail(ret.in,1)<0) + theta.tarch['beta']*tail(vol.tarch,1)**2     
  
  var.roll <- var( ret.in[(length(ret.in)-504):length(ret.in) ])
  
  # forecast
  var.g[t-s.r.train+1] <- var.garch
  var.t[t-s.r.train+1] <- var.tarch
  var.r[t-s.r.train+1] <- var.roll    
}

dates.out <- dates.r[ (s.r.train+1):(s.r.train+s.r.test)]
ret2      <- returns$AAPL.Adjusted[(s.r.train+1):(s.r.train+s.r.test)]**2

par( mar=c(2,2,3,2) , mfrow=c(1,1) )
myplot( dates.out , sqrt(252*var.r) , col='steelblue4', lwd = 2.5 , ylim = c(0,80), main="Apple Log-Returns Volatility Forecast")
lines( dates.out , sqrt(252*var.g) , col='springgreen4', lwd = 2.5 )
lines( dates.out , sqrt(252*var.t) , col='tomato' , lwd = 2.5)
legend( 'topright' , c('ROLLING VARIANCE','GARCH','TARCH') , col=c('steelblue4','springgreen4','tomato') , lwd=2.5)

ql  <- cbind( ret2/var.g - log( ret2/var.g ) -1 , ret2/var.t - log( ret2/var.t ) -1 , ret2/var.r - log( ret2/var.r ) -1 )
ql[which(!is.finite(ql))] <- 0
mse <- cbind( (ret2-var.g)**2 ,  (ret2-var.t)**2  , (ret2-var.r)**2 )

par( mar=c(2,2,3,2) , mfrow=c(3,1) )
myplot( dates.out , mse[,1] , t='l' , lwd=3 , col='springgreen4', ylim=c(0,2500), main="Mean Square Error Loss Functions")
legend('topleft',c('GARCH'),col=c('springgreen4'),lwd=4)
myplot( dates.out , mse[,2] , t='l' , lwd=3 , col='tomato', ylim=c(0,2500) )
legend('topleft',c('TARCH'),col=c('tomato'),lwd=4)
myplot( dates.out , mse[,3] , t='l' , lwd=3 , col='steelblue4' , ylim=c(0,2500))
legend('topleft',c('ROLLING VARIANCE'),col=c('steelblue4'),lwd=4)

par( mar=c(2,2,3,2) , mfrow=c(3,1) )
myplot( dates.out , ql[,1] , t='l' , lwd=3 , col='springgreen4' , ylim=c(0,25), main="Quasi-Likelihood Loss Functions")
legend('topleft',c('GARCH'),col=c('springgreen4'),lwd=4)
myplot( dates.out , ql[,2] , t='l' , lwd=3 , col='tomato', ylim=c(0,25) )
legend('topleft',c('TARCH'),col=c('tomato'),lwd=4)
myplot( dates.out , ql[,3] , t='l' , lwd=3 , col='steelblue4', ylim=c(0,25) )
legend('topleft',c('ROLLING VARIANCE'),col=c('steelblue4'),lwd=4)

L <- rbind( colMeans( ql) , colMeans( mse ) )
colnames(L)=c('GARCH','TARCH', 'ROLLING VARIANCE')
rownames(L)=c('QL','MSE')

DM <- matrix(0,2,2)
for( m in 1:2){
  d = ql[,m] - ql[,3]
  DM[1,m] = mean( d )/sqrt(lrvar(d))

  d = mse[,m] - mse[,3]
  DM[2,m] = mean( d )/sqrt( lrvar(d))
}

DM.pval <- 1-pchisq( DM**2 , 1 )

dm<-rbind(DM,DM.pval)
colnames(dm)=c('GARCH','TARCH')
rownames(dm)=c('QL EPA Test','MSE EPA Test','QL Test P-value','MSE Test P-value')

DM <- matrix(0,1,1)
for( m in 1){
  d = ql[,m] - ql[,2]
  DM[1,m] = mean( d )/sqrt(lrvar(d))
}

DM.pval <- 1-pchisq( DM**2 , 1 )

dm<-rbind(DM,DM.pval)
colnames(dm)=c('GARCH vs TARCH')
rownames(dm)=c('QL EPA Test','QL Test P-value')
dm



r.plus  <- r$AAPL.Adjusted[1:(T-1)]*(r$AAPL.Adjusted[1:(T-1)]>0) 
r.minus <- r$AAPL.Adjusted[1:(T-1)]*(r$AAPL.Adjusted[1:(T-1)]<0) 
levreg  <- lm( abs(r$AAPL.Adjusted[2:T]) ~ 0+r.plus + r.minus )
par( mar=c(10,2,1,1) , xaxs="i" , mfrow=c(1,1) )
plot( r$AAPL.Adjusted[1:(T-1)] , abs(r$AAPL.Adjusted[2:T]) , pch=16 , col='darkorange' , yaxs='i' , tck=0.02 , xlab='lagged ret', ylab='absolute ret')
abline( a=0 , b=coef(levreg)['r.plus']  , col='red2' , lwd=2 )
abline( a=0 , b=coef(levreg)['r.minus'] , col='red2' , lwd=2 )
grid()


garch11 <- garchFit(r$AAPL.Adjusted ~ garch(1, 1), data = r$AAPL.Adjusted, trace = FALSE)

summary(garch11)

sigma <- volatility(garch11, type = "sigma")

z     <- r$AAPL.Adjusted/sigma


z.plus  <- z[1:(T-1)]*(z[1:(T-1)]>0) 
z.minus <- z[1:(T-1)]*(z[1:(T-1)]<0) 
levreg  <- lm( abs(z[2:T]) ~ 0+z.plus + z.minus )

plot( z[1:(T-1)] , abs(z[2:T]) , pch=16 , col='darkorange' , yaxs='i' , tck=0.02 , xlab='lagged stdret', ylab='absolute stdret' , xlim=c(-6,6) , ylim=c(0,6))
abline( a=0 , b=coef(levreg)['z.plus']  , col='red2' , lwd=2 )
abline( a=0 , b=coef(levreg)['z.minus'] , col='red2' , lwd=2 )
grid()


neg.dummy          <- r$AAPL.Adjusted[1:(T-1)]<0
neg_sign_bias.test <- lm( z[2:T]**2 ~ neg.dummy )

summary( neg_sign_bias.test )

neg.size           <- r$AAPL.Adjusted[1:(T-1)]*(r$AAPL.Adjusted[1:(T-1)]<0)
neg_size_bias.test <- lm( z[2:T]**2 ~ neg.size )

summary( neg_size_bias.test )

"Egarch" <- function(rtn){
  # Estimation of an EGARCH(1,1) model. Assume normal innovations
  # rtn: return series 
  #
  write(rtn,file='tmp.txt',ncol=1)
  # obtain initial estimates
  mu=mean(rtn)
  par=c(mu,0.1,0.1,0.1,0.7)
  #
  #
  mm=optim(par,glk,method="Nelder-Mead",hessian=T)
  low=c(-10,-5,0,-1,0)
  upp=c(10,5,1,0,1)
  #mm=optim(par,glk,method="L-BFGS-B",hessian=T,lower=low,upper=upp)
  ## Print the results
  par=mm$par
  H=mm$hessian
  Hi = solve(H)
  cat(" ","\n")
  cat("Estimation results of EGARCH(1,1) model:","\n")
  cat("estimates: ",par,"\n")
  se=sqrt(diag(Hi))
  cat("std.errors: ",se,"\n")
  tra=par/se
  cat("t-ratio: ",tra,"\n")
  # compute the volatility series and residuals
  ht=var(rtn)
  T=length(rtn)
  if(T > 40)ht=var(rtn[1:40])
  at=rtn-par[1]
  for (i in 2:T){
    eptm1=at[i-1]/sqrt(ht[i-1])
    lnht=par[2]+par[3]*(abs(eptm1)+par[4]*eptm1)+par[5]*log(ht[i-1])
    sig2t=exp(lnht)
    ht=c(ht,sig2t)
  }
  sigma.t=sqrt(ht)
  Egarch <- list(residuals=at,volatility=sigma.t)
}


glk <- function(par){
  rtn=read.table("tmp.txt")[,1]
  glk=0
  ht=var(rtn)
  T=length(rtn)
  if(T > 40)ht=var(rtn[1:40])
  at=rtn[1]-par[1]
  for (i in 2:T){
    ept=rtn[i]-par[1]
    at=c(at,ept)
    eptm1=at[i-1]/sqrt(ht[i-1])
    lnht=par[2]+par[3]*(abs(eptm1)+par[4]*eptm1)+par[5]*log(ht[i-1])
    sig2t=exp(lnht)
    ht=c(ht,sig2t)
    if (glk + 0.5*(lnht + ept^2/sig2t)==Inf) {
      glk=0
    } else {
        glk=glk + 0.5*(lnht + ept^2/sig2t) 
  }
  glk
  }}
    
garch11  <- garchFit(r$AAPL.Adjusted ~ garch(1, 1), data = r$AAPL.Adjusted, trace = FALSE)
tarch11  <- Tgarch11(r$AAPL.Adjusted)

# 
vol.garch  <- volatility(garch11, type = "sigma")
vol.tarch  <- tarch11$volatility

myplot( dates.r , sqrt(252)*vol.garch , col='red2' )
lines( dates.r , sqrt(252)*vol.tarch , col='blue2' )
legend( 'topright' , c('garch','tarch') , col=c('red2','blue2') , lwd=3)

sov.crisis <- dates.r >= as.Date('2011-05-01') & dates.r <= as.Date('2011-10-01') 
myplot( dates.r[sov.crisis] , cumsum( r$AAPL.Adjusted[sov.crisis] ) )

myplot( dates.r[sov.crisis] , sqrt(252)*vol.garch[sov.crisis] , col='red2' , ylim=c(0,60) )
lines( dates.r[sov.crisis] , sqrt(252)*vol.tarch[sov.crisis] , col='blue2' )
legend( 'topleft' , c('garch','tarch') , col=c('red2','blue2','green2') , lwd=3)


#Moreover, financial stocks have highly interdependent relationships that we will explore further in section 2 with the DCC analysis and covariance matrix estimation.
#Volatility is rarely constant and often has a structure (mean reversion) and is dependent on the past.
#Past shocks persist and may or may not dampen (rock in a pool).
#Extreme events are likely to happen with other extreme events.
#Negative returns are more likely than positive returns (left skew).


#Section 2 and 3:

library(devtools)  
library(githubinstall)
library(quantmod)
library(moments)
library(tseries)
library(fGarch)
library(rmgarch)
library(rugarch)
library(mvnormtest)
library(MVN)
library(tseries)
library(plyr)
library(ramify)


install_github("ctbrownlees/dynamo")
source('../lib/utilities.R')
source('../lib/tarch.R')
source('../lib/egarch.R')



##Retrieving Data  

#Apple - Technology
#Procter & Gamble - Consumer Goods 
#JP Morgan - Financials
#Boeing Company - Aircraft
#Walt Disney - Entertainment
#Verizon Communications -Telecom
#Pfizer - Pharmaceutical
#Chevron - Energy (oil)
#Caterpillar - Construction

lst <- c("AAPL", "PG", "JPM","BA","DIS","VZ","PFE", "CVX","CAT")
getSymbols(lst, from = "2000-01-01", to = "2018-12-31")
P   <- NULL
seltickers <- NULL

for(stock in lst) {     
  tmp = dailyReturn(Cl(to.daily(eval(parse(text = stock))))) ## Ad from quantmod
  if(is.null(P)){ timeP = time(tmp) }
  if(any(time(tmp)!=timeP)) next
  else P = cbind(P, as.numeric(tmp))
  seltickers = c(seltickers, stock)
  P = xts(P, order.by = timeP)
  colnames(P) = seltickers
}

df<- P[-c(1),]
Dates<- time(df)
Dates<- as.Date(as.character(Dates), '%Y-%m-%d')
dx<- df*100
dx<- as.data.frame(dx)
colnames(dx)<- c('Apple', 'Procter & Gamble', 'JP Morgan', 'Boeing', 'Walt Disney', 'Verizon', 'Pfizer', 'Chevron', 'Caterpillar')
m<- cbind(Dates, dx)
head(m)
attach(m)

m1<- m[,2:10]
head(m1)
matplot(m1, t="l")

##Stationarity
ht      <- lapply(m1, adf.test, alternative="stationary", k=0)
pvalues <- ldply(ht, function(m1){ m1$p.value })
pvalues  ##all are <0.05-> Stationary is accepted


## Static Covariance
m1         <- as.matrix(m1)
static.cov <- cov(m1)
static.cor <- cor(m1)
triu(static.cor)
matplot(abs(m1), type="l")

## Summary of Stats

Statistics<- matrix(c( min(m1[,1]), max(m1[,1]), mean(m1[,1]), median(m1[,1]), sd(m1[,1])*sqrt(252), skewness(m1[,1]), kurtosis(m1[,1]),
                       min(m1[,2]), max(m1[,2]), mean(m1[,2]), median(m1[,1]), sd(m1[,2])*sqrt(252), skewness(m1[,2]), kurtosis(m1[,2]),
                       min(m1[,3]), max(m1[,3]), mean(m1[,3]), median(m1[,1]), sd(m1[,3])*sqrt(252), skewness(m1[,3]), kurtosis(m1[,3]),
                       min(m1[,4]), max(m1[,4]), mean(m1[,4]), median(m1[,1]), sd(m1[,4])*sqrt(252), skewness(m1[,4]), kurtosis(m1[,4]),
                       min(m1[,5]), max(m1[,5]), mean(m1[,5]), median(m1[,1]), sd(m1[,5])*sqrt(252), skewness(m1[,5]), kurtosis(m1[,5]),
                       min(m1[,6]), max(m1[,6]), mean(m1[,6]), median(m1[,1]), sd(m1[,6])*sqrt(252), skewness(m1[,6]), kurtosis(m1[,6]),
                       min(m1[,7]), max(m1[,7]), mean(m1[,7]), median(m1[,1]), sd(m1[,7])*sqrt(252), skewness(m1[,7]), kurtosis(m1[,7]),
                       min(m1[,8]), max(m1[,8]), mean(m1[,8]), median(m1[,1]), sd(m1[,8])*sqrt(252), skewness(m1[,8]), kurtosis(m1[,8]),
                       min(m1[,9]), max(m1[,9]), mean(m1[,9]), median(m1[,1]), sd(m1[,9])*sqrt(252), skewness(m1[,9]), kurtosis(m1[,9])), ncol=9, nrow=7 )

rownames(Statistics)<- c('Minimum', 'Maximum', 'Mean', 'Median', 'Ann. Vol', 'Skewness', 'Kurtosis')
colnames(Statistics)<- c('Apple', 'Procter & Gamble', 'JP Morgan', 'Boeing', 'Walt Disney', 'Verizon', 'Pfizer', 'Chevron', 'Caterpillar')
Statistics



##Simple Rolling Cov

roll.cov         <- array(0, dim=c(9,9,4777))
roll.cov[,,1:504]<- cov(m1)

roll.cor         <- array(0, dim=c(9,9,4777))
roll.cor[,,1:504]<- cor(m1)

T<- length(as.Date(Dates))



for (t in 505:T) {
  
  roll.cov[,,t]<- cov(m1[(t-504):(t-1),])
  roll.cor[,,t]<- cor(m1[(t-504):(t-1),])
  
}

##Risk-Metrics

riskm.cov     <- array(0, dim=c(9,9, 4777))
riskm.cov[,,1]<- cov(m1)

riskm.cor     <- array(0, dim=c(9,9,4777))
riskm.cor[,,1]<- cor(m1)


lambda<- 0.94
T<- 4777

for( t in 2:T){
  riskm.cov[,,t]<- 0.06*m1[t-1,]%*%t(m1[t-1,])  + 0.94*riskm.cov[,,t-1]
  D             <- diag( diag(riskm.cov[,,t])**-0.5 )
  riskm.cor[,,t]<- D %*% riskm.cov[,,t] %*% D 
}

par( mar=c(2,2,3,2) , mfrow=c(2,1) )
myplot( Dates, riskm.cov[1,2,] , t="l", col='springgreen4', lwd = 2.5, main="Riskmetrics, Rolling Covariance and Static Covariance Estimator (AAPL & and PG)")
lines(Dates, roll.cov[1,2,], t="l", col='tomato', lwd = 2.5)
abline(h= static.cov[1,2], col='steelblue4', lwd = 2.5)
legend( 'topright' , c('ROLLING COVARIANCE','RISKMETRICS','STATIC COVARIANCE') , col=c('tomato','springgreen4','steelblue4') , lwd=2.5)

###Testing for dynamic correlation

myplot( Dates , roll.cor[1,2,] , col='red2' , ylim=c(-1,1))

myplot( Dates, riskm.cor[1,2,] , ylim = c(-1,1), t="l", col='springgreen4', lwd = 2.5, main="Riskmetrics and Rolling Correlation (AAPL & and PG)")
lines(Dates, roll.cor[1,2,], t="l", col='tomato', lwd = 2.5)
abline(h= static.cor[1,2], col='steelblue4', lwd = 2.5)
legend( 'bottomright' , c('ROLLING CORRELATION','RISKMETRICS','STATIC CORRELATION') , col=c('tomato','springgreen4','steelblue4') , lwd=2.5)


aaa<-DCCtest(m1, garchOrder = c(1,1), n.lags = 5, solver = "solnp")

### ARCHLM test
y <- m1[,9][4:length(m1[,9])]**2
n <- length(y)
X <- cbind( m1[,9][3:(length(m1[,9])-1)]**2 , m1[,1][2:(length(m1[,9])-2)]**2 , m1[,9][1:(length(m1[,9])-3)]**2 )


##testing for ARCH process 
archlm = lm(y ~ X)
archlm.statistic <- n*summary( archlm )$r.squared
archlm.p.value   <- 1-pchisq(archlm.statistic,3) 
archlm.p.value

##Testing for Asymmetric Effects

#Leverage Plot
T<- length(Dates)
r.plus  <- m1[1:(T-1),9]*(m1[1:(T-1),9]>0) 
r.minus <- m1[1:(T-1),9]*(m1[1:(T-1),9]<0) 
levreg  <- lm( abs(m1[2:T,9]) ~ 0+ r.plus + r.minus )

plot( m1[1:(T-1),9] , abs(m1[2:T,9]) , pch=16 , col='darkorange' , yaxs='i' , tck=0.02 , xlab='lagged ret', ylab='absolute ret')
abline( a=0 , b=coef(levreg)['r.plus']  , col='red2' , lwd=2 )
abline( a=0 , b=coef(levreg)['r.minus'] , col='red2' , lwd=2 )
grid()

#Negative Size&Sign Test

neg.dummy          <- m1[1:(T-1),9]<0


neg_sign_bias.test <- lm( z9[2:T]**2 ~ neg.dummy )

summary( neg_sign_bias.test )

neg.size           <- m1[1:(T-1),9]*(m1[1:(T-1),9]<0)
neg_size_bias.test <- lm( z9[2:T]**2 ~ neg.size )

summary( neg_size_bias.test )



### DCC model

uspec<- ugarchspec(variance.model = list(garchOrder= c(1,1), model="sGARCH"), distribution.model="norm")

spec1    <- dccspec( uspec= multispec(replicate(9, uspec)), dccOrder=c(1,1), distribution="mvnorm")
fit1     <- dccfit( spec1, data= m1, fit.control = list(eval.se=T))
dcc.cov  <- rcov(fit1)
dcc.cor  <- rcor(fit1)

uspec_na<- ugarchspec(variance.model = list(garchOrder= c(1,1), model="fGARCH", submodel="NAGARCH"), distribution.model = "norm")

spec1_na   <- dccspec( uspec= multispec(replicate(9, uspec_na)), dccOrder=c(1,1), distribution="mvnorm")
fit1_na    <- dccfit( spec1_na, data= m1, fit.control = list(eval.se=T))
dcc.cov_na <- rcov(fit1_na)
dcc.cor_na  <- rcor(fit1_na)


par( mar=c(2,2,3,2) , mfrow=c(2,1) )
myplot( Dates, riskm.cov[1,2,], t="l", col='springgreen4', lwd = 2, main="Rolling, Riskmetrics and DCC-GARCH Covariance (AAPL & and PG)")
lines(Dates, roll.cov[1,2,], t="l", col='tomato', lwd = 2)
lines(Dates, dcc.cov[1,2,], t="l", col='darkmagenta', lwd = 2)
legend( 'topright' , c('ROLLING COVARIANCE','RISKMETRICS', "DCC-GARCH") , col=c('tomato','springgreen4','darkmagenta') , lwd=2.5)
myplot( Dates, riskm.cor[1,2,], ylim=c(-1,1.5), t="l", col='springgreen4', lwd = 2, main="Rolling, Riskmetrics and DCC-GARCH Correltion (AAPL & and PG)")
lines(Dates, roll.cor[1,2,], t="l", col='tomato', lwd = 2)
lines(Dates, dcc.cor[1,2,], t="l", col='darkmagenta', lwd = 2)
legend( 'topright' , c('ROLLING','RISKMETRICS', "DCC-GARCH") , col=c('tomato','springgreen4','darkmagenta') , lwd=2.5)

par( mar=c(2,2,3,2) , mfrow=c(2,1) )
myplot( Dates, riskm.cov[1,2,], t="l", col='springgreen4', lwd = 2, main="Rolling, Riskmetrics, DCC-GARCH and DCC-NAGARCH Covariance (AAPL & and PG)")
lines(Dates, roll.cov[1,2,], t="l", col='tomato', lwd = 2)
lines(Dates, dcc.cov[1,2,], t="l", col='darkmagenta', lwd = 2)
lines(Dates, dcc.cov_na[1,2,], t="l", col='yellow3', lwd = 2)
legend( 'topright' , c('ROLLING COVARIANCE','RISKMETRICS', "DCC-GARCH", "DCC-NAGARCH") , col=c('tomato','springgreen4','darkmagenta','yellow3' ) , lwd=2.5)
myplot( Dates, riskm.cor[1,2,], ylim=c(-1,1), t="l", col='springgreen4', lwd = 2, main="Rolling, Riskmetrics, DCC-GARCH and DCC-NAGARCH Correltion (AAPL & and PG)")
lines(Dates, roll.cor[1,2,], t="l", col='tomato', lwd = 2)
lines(Dates, dcc.cor[1,2,], t="l", col='darkmagenta', lwd = 2)
lines(Dates, dcc.cor_na[1,2,], t="l", col='yellow3', lwd = 2)
legend( 'bottomright' , c('ROLLING','RISKMETRICS', "DCC-GARCH", "DCC-NAGARCH") , col=c('tomato','springgreen4','darkmagenta', 'yellow3') , lwd=2.5)


##Forecasting volatilities


#FORECAST VOLATILITY SETUP

x       <-as.factor(Dates)
dates   <-as.Date(as.character(x), '%Y-%m-%d')

T <- tail( which( dates <= as.Date('2018-07-01') ) , 1 )
N <- nrow(m1)

var.rolling        <- rep( 0 , N-T )
var.dcc.g          <- rep( 0 , N-T )
var.std.nag        <- rep( 0 , N-T )

cov.rolling        <- array(0, dim=c(9,9,4777))
cov.dcc.g          <- array(0, dim=c(9,9,251))
cov.dcc.nag        <- array(0, dim=c(9,9,4777))


for( t in T:(N-1)){
  
  cat('.')
  
  ret.in  <- m1[ 1:t, ]
  ret.out <- m1[ (t+1):N, ]
  
  cov.roll <- cov(ret.in[(nrow(ret.in)-504):(nrow(ret.in)-1),])
  
  f.spec1         <- dccspec( uspec= multispec(replicate(9, uspec)), dccOrder=c(1,1), distribution="mvnorm")
  f.fit1          <- dccfit( f.spec1, data= ret.in, fit.control = list(eval.se=T))
  g_fore          <- dccforecast(fit = f.fit1, n.ahead = 1)
  gdcc            <- rcov(g_fore)
  dccg.fore       <- gdcc[[1]]
  
 
  # forecast
  
  cov.rolling[,,t-T+1]        <- cov.roll
  cov.dcc.g[,, t-T+1]         <- dccg.fore
  
}


for( t in T:(N-1)){
  
  cat('.')
  
  ret.in  <- m1[ 1:t, ]
  ret.out <- m1[ (t+1):N, ]

f.spec2_na       <- dccspec( uspec= multispec(replicate(9, uspec_na)), dccOrder=c(1,1), distribution="mvnorm")
f.fit2_na        <- dccfit( f.spec2_na, data= ret.in, fit.control = list(eval.se=T))
na_fore          <- dccforecast(fit = f.fit2_na, n.ahead = 1)
nadcc            <- rcov(na_fore)
dccs.fore_na     <- nadcc[[1]]

cov.dcc.nag[,,t-T+1]    <- dccs.fore_na
}



cov.riskm <- array(0, dim=c(9,9, 4777))
cov.risk     <- array(0, dim=c(9,9, 4777))
cov.risk[,,4400]<- riskm.cov[,,4400]


for( t in T:(N-1)){
  
  cat('.')
  
  ret.in  <- m1[ 1:t, ]
  ret.out <- m1[ (t+1):N, ]
  
  
  cov.risk[,,t]<- 0.06*ret.in[t-1,]%*%t(ret.in[t-1,])  + 0.94*cov.risk[,,t-1]
  
  # forecast
  
  cov.riskm [,,t-T+1]        <- cov.risk[,,t]
  
}




cov.rolling   <- cov.rolling[,,1:126]
cov.rolling_1 <- array(0,dim=c(9,9,4777))
cov.rolling_1[,,4652:4777]<- cov.rolling

roll.cov.in<- array(0, dim=c(9,9,4651))
roll.cov.in<- roll.cov[,,1:4651]
cov.rolling_1[,,1:4651]<- roll.cov.in

cov.riskm   <- cov.riskm[,,1:126]
cov.riskm_1 <- array(0,dim=c(9,9,4777))
cov.riskm_1[,,4652:4777]<- cov.riskm

riskm.cov.in<- array(0, dim=c(9,9,4651))
riskm.cov.in<- riskm.cov[,,1:4651]
cov.riskm_1[,,1:4651]<- riskm.cov.in


cov.dcc.g <- cov.dcc.g[,,1:126]
cov.dcc.g_1 <- array(0, dim=c(9,9,4777))
cov.dcc.g_1 [,,4652:4777]<- cov.dcc.g

dcc.cov.in <- array(0, dim=c(9,9,4651))
dcc.cov.in <- dcc.cov[,,1:4651]
cov.dcc.g_1[,,1:4651] <- dcc.cov.in

cov.dcc.na <- cov.dcc.nag[,,1:126]
cov.dcc.na_1 <- array(0, dim=c(9,9,4777))
cov.dcc.na_1 [,,4652:4777]<- cov.dcc.na

dcc.cov.in <- array(0, dim=c(9,9,4651))
dcc.cov.na.in <- dcc.cov_na[,,1:4651]
cov.dcc.na_1[,,1:4651] <- dcc.cov.na.in




#Plot Forecasting

par( mar=c(2,2,3,2) , mfrow=c(2,1))
myplot( Dates, cov.dcc.g_1[1,2,], t="l", col='darkmagenta', lwd = 1, ylim=c(-6,15),main="Rolling, Riskmetrics, DCC-GARCH and DCC-NAGARCH Covariance Forecast (AAPL & and PG)")
lines( Dates, cov.dcc.na_1[1,2,], t="l", col='yellow3', lwd = 1)
lines(Dates, cov.riskm_1[1,2,], t="l", col='springgreen4', lwd = 1)
lines(Dates, cov.rolling_1[1,2,], t="l", col='tomato', lwd = 1)
abline(v = Dates[4651], lwd = 2, col="navy")
legend( 'topleft' , c('ROLLING COVARIANCE','RISKMETRICS', "DCC-GARCH", "DCC-NAGARCH") , col=c('tomato','springgreen4','darkmagenta','yellow3' ) , lwd=1.5)

myplot( Dates[4651:4776], cov.dcc.g_1[1,2,4651:4776], t="l", col='darkmagenta', ylim=c(-1,2), lwd = 2.5, main="Rolling, Riskmetrics, DCC-GARCH and DCC-NAGARCH Covariance Forecast (AAPL & and PG). Validation Set")
lines( Dates[4651:4776], cov.dcc.na_1[1,2,4651:4776], t="l", col='yellow3', lwd = 2.5)
lines(Dates[4651:4776], cov.riskm_1[1,2,4651:4776], t="l", col='springgreen4', lwd = 2.5)
lines(Dates[4651:4776], cov.rolling_1[1,2,4651:4776], t="l", col='tomato', lwd = 2.5)
legend( 'topleft' , c('ROLLING COVARIANCE','RISKMETRICS', "DCC-GARCH", "DCC-NAGARCH") , col=c('tomato','springgreen4','darkmagenta','yellow3' ) , lwd=1.5)


#Tests
D            <- matrix(0, nrow=4777, ncol=9)
D.dcc        <- matrix(0, nrow=4777, ncol=9)
D.dcc.na <- matrix(0, nrow=4777, ncol=9)
D.riskm      <- matrix(0, nrow=4777, ncol=9)



for( t in 4651:4776){
  
  D   [t,]            <-  diag(cov.rolling_1[,,t])
  D.dcc [t,]          <-  diag(cov.dcc.g_1[,,t]) 
  D.dcc.na[t,]      <-  diag(cov.dcc.na_1[,,t])
  D.riskm [t,]        <-  diag(cov.riskm_1[,,t])
}


qlr_1            <- matrix(0, ncol=9, nrow=4777) 
qlr.dcc_1        <- matrix(0, ncol=9, nrow=4777)
qlr.dcc.na_1 <- matrix(0, ncol=9, nrow=4777)
qlr.riskm_1      <- matrix(0, ncol=9, nrow=4777)

m2m<- m1**2

for (t in 4652:4776){
  
  qlr             <- m2m[t,]/D[t,]
  qlr.dcc         <- m2m[t,]/D.dcc[t,]
  qlr.std.dcc.na  <- m2m[t,]/D.dcc.na[t,]
  qlr.riskm       <- m2m[t,]/D.riskm[t,]
  
  ##Compute
  qlr_1 [t,]           <- qlr
  qlr.dcc_1[t,]        <- qlr.dcc 
  qlr.dcc.na_1[t,] <- qlr.std.dcc.na
  qlr.riskm_1[t,]      <- qlr.riskm  
  
}


##1st Company Analysis

##QL
ql.roll_1        <- rep(0, 126)
ql.dcc_1         <- rep(0, 126)
ql.dcc.na_1  <- rep(0, 126)
ql.riskm_1       <- rep(0, 126)

for (t in 4652:4776){
  
  ql.roll        <- qlr_1[t,1] - log( qlr_1[t,1] ) -1
  ql.dcc         <- qlr.dcc_1[t,1] - log( qlr.dcc_1[t,1] ) -1
  ql.dcc.na  <- qlr.dcc.na_1[t,1] -log( qlr.dcc.na_1[t,1] ) -1
  ql.riskm       <- qlr.riskm_1[t,1] -log( qlr.riskm_1[t,1] ) -1
  ##storage
  
  ql.roll_1[t-4651]        <- ql.roll 
  ql.dcc_1 [t-4651]        <- ql.dcc
  ql.dcc.na_1[t-4651]  <- ql.dcc.na
  ql.riskm_1 [t-4651]      <- ql.riskm
  
}


a                <- ql.roll_1=="Inf"
n.ql.roll_1      <- ql.roll_1[a==FALSE]
n.ql.dcc_1       <- ql.dcc_1[a==FALSE]
n.ql.dcc.na_1<- ql.dcc.na_1[a==FALSE]
n.ql.riskm_1     <- ql.riskm_1[a==FALSE]


par( mar=c(10,2,1,1) , xaxs="i" , mfrow=c(4,1) )
myplot( Dates[4652:4777] , n.ql.roll_1 , col='burlywood2', ylim=c(0,8) )
myplot( Dates[4652:4777] , n.ql.dcc_1 , col='mediumvioletred' )
myplot( Dates[4652:4777] , n.ql.dcc.na_1 , col='tomato' )
myplot( Dates[4652:4777] , n.ql.riskm_1 , col='deepskyblue3' )
legend( 'topright' , c('Rolling','DCC-GARCH','DCC-NAGARCH', 'Risk Metrics') , col=c('burlywood2','mediumvioletred', 'tomato','deepskyblue3' ) , lwd=3)


ql  <- cbind(n.ql.roll_1[1:125], n.ql.riskm_1[1:125], n.ql.dcc_1[1:125], n.ql.dcc.na_1[1:125])
ql

qlm  <- cbind(mean(n.ql.roll_1[1:124]), mean(n.ql.riskm_1[1:124]), mean(n.ql.dcc_1[1:124]), mean(n.ql.dcc.na_1[1:124]))
colnames(qlm) <- c("Rolling Cov","RiskMetrics","DCC-GARCH","DCC-NAGARCH")
rownames(qlm) <- c("QL Score")
qlm

par( mar=c(10,2,1,1) , xaxs="i" , mfrow=c(1,1) )
matplot( cbind( n.ql.roll_1, n.ql.dcc_1, n.ql.riskm_1 ,n.ql.dcc.na_1),t="l",
         ylab= "QL", xlab="Dates", main="QL FORECAST APPLE")


##DIEBOLD-MARIANO TEST


DM <- matrix( 0 , 1 , 3)
for( m in 1:3){
  d = ql[1:125,m] - ql[1:125,4]
  DM[1,m] = mean( d )/sqrt( lrvar(d) )
}

DM.pval <- 1-pchisq( DM**2 , 1 )


table           <- round(rbind( DM[1,],DM.pval[1,]),3)
colnames(table) <- c("Rolling Cov","Risk Metrics","DCC Cov")
rownames(table) <- c("D-M Statistic(QL)",  "P-value" )
table



#SECTION 3

###GMV for Moving Average

gmv.weights <- function(cov){
  
  dim <- dim( cov )
  N   <- dim[1]
  T   <- dim[3]
  
  one.vector<- rep(1, N)  
  w <- matrix(0,T,N)
  
  for( t in 1:T ){
    
    w[t,] <- as.numeric(solve(cov[,,t])%*%one.vector) /  as.numeric( t(one.vector)%*%solve(cov[,,t])%*%one.vector)
  }
  
  w
}

w.roll       <- gmv.weights(roll.cov)
w.rm         <- gmv.weights(riskm.cov)
w.dcc        <- gmv.weights(dcc.cov)
w.dcc.na     <- gmv.weights(dcc.cov_na)
w.naiv       <- matrix (1/9, 4777, 9)

ret.roll       <- rep(0, 4777 )
ret.riskm      <- rep(0, 4777 )
ret.dcc        <- rep(0, 4777)
ret.dcc.na     <- rep(0, 4777)
ret.naiv       <- rep(0, 4777)


for ( t in 4273:4777){
  ret.roll        [t]  <- sum(w.roll[t,]%*%m1[t,])
  ret.riskm       [t]  <- sum(w.rm[t,]%*%m1[t,])
  ret.dcc         [t]  <- sum(w.dcc[t,]%*%m1[t,])
  ret.dcc.na      [t]  <- sum(w.dcc.na[t,]%*%m1[t,])
  ret.naiv        [t]  <- sum(w.naiv[t,]%*% m1[t,])
}

ret.roll         <- ret.roll
ret.riskm        <- ret.riskm
ret.dcc          <- ret.dcc
ret.dcc.na       <- ret.dcc.na
ret.naiv         <- ret.naiv

GMV              <- matrix(c( var(ret.naiv)*100  ,var(ret.roll)*100,var(ret.riskm)*100, var(ret.dcc)*100, var(ret.dcc.na)*100))
rownames(GMV)    <- c("Naive Portfolio","Rolling Covariance", "Risk Metrics","DCC-GARCH", 
                      "DCC-NAGARCH")
colnames(GMV)    <- ("Annualized Variance (%)")
GMV

par( mar=c(2,2,2,2) , xaxs="i" , mfrow=c(3,1) )
myplot(Dates[4273:4777], w.dcc[4273:4777,1], lwd = 3, col="springgreen4", main="Apple daily position by predictive covariance model in the validation test")
lines(Dates[4273:4777], w.roll[4273:4777,1], lwd = 3, col="darkorange2")
lines( Dates[4273:4777], w.naiv[4273:4777,1], lwd = 3, col="navy")
legend( 'topright' , c('DCC-NAGARCH','Rolling', 'Naive') , col=c('springgreen4','darkorange2', 'navy' ) , lwd=3)
myplot(Dates[4273:4777], w.dcc[4273:4777,2], lwd = 3, col="springgreen4", main="Procter & Gamble daily position by predictive covariance model in the validation test")
lines(Dates[4273:4777], w.roll[4273:4777,2], lwd = 3, col="darkorange2")
lines( Dates[4273:4777], w.naiv[4273:4777,2], lwd = 3, col="navy")
myplot(Dates[4273:4777], w.dcc[4273:4777,3], lwd = 3, col="springgreen4", main="JP Morgan daily position by predictive covariance model in the validation test")
lines(Dates[4273:4777], w.roll[4273:4777,3], lwd = 3, col="darkorange2")
lines( Dates[4273:4777], w.naiv[4273:4777,3], lwd = 3, col="navy")


