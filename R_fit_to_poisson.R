
failfreq<-c(7,2,17,8,23,31,43,34,49,42,79,47,0,0)
mean(failfreq)
days<-seq(1:14)
plot(days, failfreq)
hist(failfreq)
hist(failfreq, xlab ="#failures per day", labels= TRUE)
# boxplot(failfreq)
summary(failfreq)

#deciding the poisson distribution:
pquant<-seq(from =1, to=70, by =1)
poidist <- dpois(pquant, 27.28571)
plot(poidist)

# finding the instantaneous failure rate:
givenday = 10 #can vary.....
ppois(10, mean(failfreq))  # change the numerator... ppois is cdf
dpois(10, mean(failfreq))  #dpois is density function.
# at given date the instantaneous failure rate: ppois(freq already ocured,lamda)/(1-dpois(frequency occured, 27.28571))
dpois(47,mean(failfreq))/(1-ppois(10, mean(failfreq)))
dpois(65,mean(failfreq))

plot(dpois(11:50,mean(failfreq))/(1-ppois(10, mean(failfreq))))

#dpois(65,mean(failfreq))/(1-ppois(64, mean(failfreq)))
#dpois(20,mean(failfreq))
#sum(dpois())
sampledata<- failfreq
 survival<-function(sampledata,numberfailed, input)
 {
input <-40    # HOW MANY failing. comment if needed.
numberfailed<- 15  #how many already failed. comment if needed.
# the below gives probability of failures less than equal to input| given failure data:
sum(dpois(0:input,mean(sampledata)) / (1-ppois(numberfailed, mean(sampledata))))
# the below gives probability of failures when failure data is not available:
sum(dpois(0:input, mean(sampledata)))
x<- seq(0,input, by = 1)
plot(x, dpois(0:input,mean(sampledata)), type = 'l', col = 'red')
points(x, dpois(0:input,mean(sampledata)) / (1-ppois(numberfailed, mean(sampledata))), type='l', col="blue", xlab='x', ylab='p(x)' )     
}
# plot two curves one: given failure, and the varying parameter in plot is 'input' given by user.  use ggplot?.
#

end <- 60
seqpts<-seq(numberfailed+1,end,1)
ppt<-dpois(seqpts,mean(failfreq))
pptgivenfailures <- dpois(seqpts,mean(failfreq))/(1-ppois(numberfailed, mean(failfreq)))
plot(seqpts, ppt)
plot(seqpts, pptgivenfailures)
# Goodness of fit:
failfreq<-c(7,2,17,8,23,31,43,34,49,42,79,47,0,0)
failfreqsample<-c(7,2,17,8,23,31,43)
failfreqtestobs<-c(34,49,42,79,47,0,0)
#pointvector<-c(0,10,20,30,40,50)
px<- c(dpois(0:60, mean(failfreqsample)), 1-ppois(60,mean(failfreqsample)))
repx<-c(sum(dpois(0:10,mean(failfreqsample))),sum(dpois(11:20,mean(failfreqsample))),sum(dpois(21:30,mean(failfreqsample))),
        sum(dpois(31:40,mean(failfreqsample))),sum(dpois(41:50,mean(failfreqsample))),sum(dpois(51:60,mean(failfreqsample))), 
        1-ppois(60,mean(failfreqsample))     )
#If their were a 100 failures...
psimpoi <- 100*px 
#the above is incomplete... no data.

#goodness of fit with sample from poisson. 
lamda <- 25
pvector<- dpois(1:60,lamda)
plot(pvector)
sum(pvector)
scale <- 1000    # that many points/ n.
scaledpvector<-scale*pvector
plot(scaledpvector)
rsample<-rpois(scale,lamda)
hist(rsample)
range(rsample)
breaks = seq(12, 43, by=1)
rsample.cut = cut(rsample, breaks, right=FALSE) 
#sampfreq<-rsample.cut
rsample.freq
init<-rep(0,11)
end <- rep (0,18)
rpoivector
length(rpoivector)
length((scaledpvector))
#chi sq test:  rpoivector is the observed vector from random sample.

sq<-(scaledpvector - rpoivector)*(scaledpvector- rpoivector)
chisqobsvector <- ifelse(rpoivector > 0,sq/rpoivector, 0)
chisqobs<- sum(chisqobsvector)
chisqobs
pval<- 1-pchisq(chisqobs, 59)
pval 
###############################################################################################
#  Good ness of fit function....:
###############################################################################################

gofpoi<-function(inputdata)
{ #inputdata<-rpois(100, 25)
  lamda <- mean(inputdata)
  x <- range(inputdata)
  min<- x[1]
  max <- x[2]
  if( min > 0) 
  {len = max - min  + 1} else { len = max }
  max
  n<- length(inputdata) 
  pvector<- dpois(0:max, lamda)
  scaledpvector <- n*pvector
  breaks = seq(0, max+1, by=1)
  data.cut <- cut(inputdata,breaks,right = FALSE, include.lowest = FALSE)
  data.freq <- table(data.cut)
  data.freq
  scaledpvector
  sq<- (scaledpvector-data.freq)*(scaledpvector-data.freq)
  chisqobsvector <- ifelse(data.freq > 0,sq/data.freq, 0)
  chisqobs <- sum(chisqobsvector)
  chisqobs
  pval <- 1 - pchisq(chisqobs, max -1)
  pval
  x1axis <- seq(0,max, by=1)
  plot(x1axis, data.freq , ylim = range(data.freq, scaledpvector),type='p', col="red", xlab='x', ylab='y')
  points(x1axis, scaledpvector, ylim = range(data.freq, scaledpvector))
  
  
}

###################################################################################
rsample2<- rpois(100, lamda)
breaks = seq(1,60,by =1)
rsample2.cut <- cut(rsample2, breaks, right = FALSE)
rsample2.freq = table(rsample2.cut)
rsample2.freq
inputdata<-rpois(100, 5)