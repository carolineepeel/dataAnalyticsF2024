#lab 01 exercise 00

EPI <- read.csv('C:\\Users\\carol\\OneDrive\\Documents\\data_analytics\\lab01\\epi2024results06022024.csv')

View(EPI)
attach(EPI)
EPI.new

#exercise 1

summary(EPI.new)
fivenum(EPI.new, na.rm=TRUE)

stem(EPI.new)

hist(EPI.new)

hist(EPI.new, seq(20.,80.,1.0), prob = TRUE)

lines(density(EPI.new, na.rm=TRUE,bw=1.))

rug(EPI.new)


#Comparing Distribution
boxplot(EPI.new, APO.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw=1.))
rug(EPI.new)

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE, bw="SJ"))
rug(EPI.new)

x<-seq(20,80,1)
q<- dnorm(x,mean=41, sd=5, log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65,sd=5,log=FALSE)
lines(x,.12*q)

#EXERCISE 2
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
qqnorm(EPI.new); qqline(EPI.new)

qqplot(rnorm(ppoints(250)), EPI.new, xlab = "Q-Q plot norm dsn")
qqline(EPI.new)


