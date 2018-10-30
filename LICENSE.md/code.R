# ΕΙΣΑΓΩΓΗ ΒΙΒΛΙΟΘΗΚΩΝ
library('forecast')
library('tseries')
library("ggplot2")
library('gamlss')
library('RSNNS')
library("TTR")
library(smooth)
library(Mcomp)

#-----------------------------------------------------------------------------#
# Διαβάζουμε τα δεδομένα που πήραμε από το πρώτο grep που κάναμε στο command line  
data<-read.table("C:/Users/Ani/Desktop/master/2 eksamino/Big Data Analytics Applications/Time series/years")
# φτιάχνουμε ένα data frame με μηδενικά στοιχειά 
Ndata<-rep(0,83)
Ndata<-as.data.frame(Ndata)
#εκχωρούμε στο Ndata το πλήθος των δημοσιεύσεων ανά χρόνο 
j=1
for(i in 1936:2019){
Ndata[j,1] <-length(grep(i,data[,1]))
j=j+1
}
Ndata<-data.frame(1936:2019,Ndata)
#αφαιρούμε της τελευταίες χρονιές 
Ndata<-Ndata[-c(84,83,82),]
#περνούμε την λογαριθμική κλίμακα 
Ndata[,2]<-log(Ndata[,2])
x<-ts(Ndata[,2], start = c(1936, 1), frequency = 1)
Train<-ts(x[1:70], start=c(1938,1), frequency = 1)
Test<-ts(x[71:78], start=c(2008,1), frequency = 1)
acf(x,main="Autocorrelations")
#-----------------------------------------------------------------------------#
#εκχωρούμε στο x  της πρώτες διαφορές για το ARIMA 
x<-diff(x,differences= ndiffs(x))
autoplot(x,xlab="Year",ylab="Yt",main="Time series with first differences")
adf.test(x, alternative = "stationary",k = trunc((length(x)-1)^(1/3)))
pp.test(x)
Acf(x, main='ACF for Differenced Series')
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#ARIMA
#Εκχωρούμε στο arimaMod το μονύελο arima με παραμέτρους (5,1,3)
arimaMod <- Arima(Train,order=c(5,1,3))
#κάνουμε το forecast στο μοντέλο για 8 χρόνια 
arimaMod.Fr <-forecast(arimaMod,h=8)
summary(arimaMod.Fr)
#φτιάχνουμε το διάγραμμα του μοντέλου μας 
autoplot(arimaMod.Fr,xlab="Year") +
  autolayer(Test, series="Data") +
  autolayer(arimaMod.Fr$mean, series="Forecasts")
#υπολογίζουμε την ακρίβεια του μοντέλου μας 
accuracy(arimaMod.Fr,Test)
#διαγραμματική απεικόνιση για τα κατάλοιπα μας 
tsdisplay(residuals(arimaMod))

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#Exponential smoothing
#εκχωρούμε στην μεταβλητή fit το μοντέλο μας  
fit <- bats(Train,biasadj = TRUE,use.box.cox = TRUE)
#εκχωρούμε στο fc το forecast που κάνουμε για 9 χρόνια 
fc<-forecast(fit,h=9)
#διαγραμματική απεικόνιση του μοντέλου της εκθετικής εξομάλυνσης 
autoplot(fc) +
  autolayer(Test, series="Data") +
  autolayer(fc$mean, series="Forecasts")
#υπολογίζουμε τιν ακρίβεια
accuracy(fc,Test)
hist(fc$residuals,prob=TRUE)
curve(dnorm(x, mean(fc$residuals), sd(fc$residuals)), add=TRUE, col="darkblue", lwd=2)

qqnorm(fc$residuals)
qqline(fc$residuals, col = 2)
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#Random Walk 
#εκχωρούμε στο md το μοντέλο μας τις τυχαίας διαδρομής 
md = rwf(Train,h=8,drift=T,level=c(90,95),fan=FALSE,lambda=2) 
summary(md)
#διαγραμματική απεικόνιση του μοντέλου μας 
autoplot(md,xlab="Year") +
  autolayer(Test, series="Data") +
  autolayer(md$mean, series="Forecasts")
#υπολογίζουμε τιν ακρίβεια του μοντέλου μας 
accuracy(md,Test)
hist(md$residuals,prob=TRUE)
curve(dnorm(x, mean(md$residuals), sd(md$residuals)), add=TRUE, col="darkblue", lwd=2)

