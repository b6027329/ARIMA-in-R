#install.packages("tseries")
#install.packages("forecast")
#install.packages(astsa)


#######################################################
###load the package which required by this programme###
#######################################################
library(astsa)
library(tseries)
library(ggplot2)
library(forecast)


#################################
###read the data from CSV file###
#################################
data <- read.csv("C:/users/b6027329/Desktop/inflation.csv")
attach(data)


plot(data)    ###show the data on the graph###
inflation <- ts(data [,2], start = c(1960,1), frequency = 4)   ###generate time-series data###
y<- inflation


d.y<-diff(y)    ###get the first difference of time-series data 

plot(d.y)

summary(y)
summary(d.y)


###ADF test for original data###
adf.test(y, alternative="stationary", k=0)
adf.test(y, alternative="explosive", k=0)

#summary(lm(dppi ~ lppi, na.action=na.omit))
#summary(lm(dppi ~ lppi + trend, na.action=na.omit))


###ADF test for First-diff and Second-diff, for get the stationary of time-series data###
adf.test(d.y, k=0)
adf.test(d.y)  #Dickey-Fuller = -5.9953
adf.test(d2.y) #Dickey-Fuller = -7.1603


###KPSS test fore the first difference data###
kpss.test(d.y, null="Trend")  
#KPSS Trend = 0.058264, Truncation lag parameter = 3, p-value = 0.1


###generate ACf and PACF for original data and first difference data
acf(y)
pacf(y)

acf(d.y)
pacf(d.y)

#acf(d2.y)
#pacf(d2.y)
#plot(d2.y)


#arima(d.y, order = c(0,0,0))
#arima(d.y, order = c(0,0,1))
#arima(d.y, order = c(0,0,2))
arima103 <- arima(d.y, order = c(1,0,3))
#arima(d.y, order = c(1,0,0))
#arima(d.y, order = c(1,0,1))
arima104 <- arima(d.y, order = c(1,0,4))
arma104 = arima(y, order  =c(1,1,4))
#arima(d.y, order = c(2,0,0))
#arima(d.y, order = c(2,0,1))
arima202 <- arima(d.y, order = c(2,0,2))
arima302 <- arima(d.y, order = c(3,0,2))
#arima(d.y, order = c(3,0,3))
#arima(d.y, order = c(4,0,2))

AIC(arima103)  #-5.265451
BIC(arima103)  #15.38905

AIC(arima104)  #-6.240728
BIC(arima104)  #17.8562

AIC(arima302)  #11.8742
BIC(arima302)  #35.97112

AIC(arima202)  #21.55279
BIC(arima202)  #42.20729


##########################
###random walk forecast###
##########################

dataTure <- window (d.y, start=c(1980,1), end=c(2017,4))
forecastRW <- forecastAR1 <- forecastAR2 <- forecastAR4 <- ts(rep(0,152), start = c(1980,1), frequency = 4)
forecastTrueRW <- forecastTrueAR1 <- forecastTrueAR2 <- forecastTrueAR4 <- ts(rep(0,152), start = c(1980,1), frequency = 4)


forecastRW <- stats::lag(window(d.y, c(1979,4), c(2017,3)), -1)
forecastTrueRW <- stats::lag(window(y, c(1979,4), c(2017,3)), -1) + forecastRW


#########
###AR1###
#########
for (t in 1:152)
  {
    dataAR1 <- window(d.y, start = c(1965,1)+c(0,t-1), freq=4, end =c(1979,4)+c(0,t-1))       ###generate moving window
    modAR1 <- arima(dataAR1, order = c(1,0,0))                                                ###give parameter a ARIMA model
    forecastAR1[t] <- forecast(modAR1,1)$mean
    forecastTrueAR1[t] <- forecastAR1[t] + window(inflation, c(1979,4)+c(0,t-1), c(1979,4)+c(0,t-1))
  }


#########
###AR2###
#########
for (t in 1:152)
{
  dataAR2 <- window(d.y, start = c(1965,1)+c(0,t-1), freq=4, end =c(1979,4)+c(0,t-1))
  modAR2 <- arima(dataAR2, order = c(2,0,0))
  forecastAR2[t] <- forecast(modAR2, 1)$mean
  forecastTrueAR2[t] <- forecastAR2[t] + window(inflation, c(1979,4)+c(0,t-1), c(1979,4)+c(0,t-1))
}


#########
###AR4###
#########
for (t in 1:152)
{
  dataAR4 <- window(d.y, start = c(1965,1)+c(0,t-1), freq=4, end =c(1979,4)+c(0,t-1))
  modAR4 <- arima(dataAR4, order = c(4,0,0))
  forecastAR4[t] <- forecast(modAR4, 1)$mean
  forecastTrueAR4[t] <- forecastAR4[t] + window(inflation, c(1979,4)+c(0,t-1), c(1979,4)+c(0,t-1))
}



############################################
###Generate the plot for all the forecast###
############################################
plot(dataTure,main="inflation difference forecast 1980~2017",xlab="time",ylab="First difference of inflation")
legend("topright",c("true value","random walk","AR1","AR2","AR4"),lty=1,col=c("black","green","blue","yellow","red"))  
points(forecastRW, col="green", t="l" )
points(forecastAR1,col="blue",t="l")
points(forecastAR2,col="yellow",t="l")
points(forecastAR4,col="red",t= "l")


plot(window(inflation,c(1980,1),c(2017,4)),main="inflation forecast 1980~2017",xlab="time",ylab="inflation")
legend("topright",c("true value","random walk","AR1","AR2","AR4"),lty=1,col=c("black","green","blue","yellow","red")) 
points(forecastTrueRW, col="green", t="l" )
points(forecastTrueAR1,col = "blue",t="l")
points(forecastTrueAR2,col="yellow",t= "l")
points(forecastTrueAR4,col="red",t="l")




