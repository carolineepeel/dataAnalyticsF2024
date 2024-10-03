## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ##
library("e1071")
classifier<-naiveBayes(iris[,1:5], iris[,6])
table(predict(classifier, iris[,-6]), iris[,6], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),
                    header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                       'viscera_wieght', 'shell_weight', 'rings' )
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
abalone.norm <- abalone[,-1]


##Exercise 1 - Navie Bayes
## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ##
library("e1071")

classifier<-naiveBayes(abalone[,2:8], abalone[,10])
table(predict(classifier, abalone[,2:8], abalone[,10], dnn=list('predicted', 'acutual')))
classifier$diameter
classifier$whole_weight
classifier$length
plot(function(x)dnorm(x, 1.462, 0.1736640), 0, 8, col = "red", main="Ages compared to Ring Size")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947), add = TRUE, col="green")

classifier<-naiveBayes(abalone[,3:7], abalone[,9])
table(predict(classifier, abalone[,3:7], abalone[,9], dnn=list('predicted', 'acutual')))
classifier$shucked_weight
classifier$whole_weight
classifier$shell_weight
plot(function(x)dnorm(x, 1.462, 0.1736640), 0, 8, col = "red", main="Shell Weight to Rings")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947), add = TRUE, col="green")

classifier<-naiveBayes(abalone[,2:8], abalone[,10])
table(predict(classifier, abalone[,2:8], abalone[,10], dnn=list('predicted', 'acutual')))
classifier$diameter
classifier$whole_weight
classifier$length
plot(function(x)dnorm(x, 1.462, 0.1736640), 0, 8, col = "red", main="Ages compared to Ring Size")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947), add = TRUE, col="green")

##Exercise 2 - kNN with Iris

abalone.norm <- abalone[,-1]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))
summary(abalone.norm$shucked_wieght)

s_abalone <- sample(4177,2924)
abalone.train <-abalone.norm[s_abalone,]
abalone.test <-abalone.norm[-s_abalone,]
abalone.train <-abalone[s_abalone,-1]
abalone.test <-abalone[-s_abalone,-1]
sqrt(2924)
k = 55
# k = 80
KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)
contingency.table <- table(KNNpred,abalone.test$age.group)
contingency.table

contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(abalone.test$age.group)
accuracy <- c()
ks <- c(35,45,55,65,75,85,95,105)
for (k in ks) {
  KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = abalone.test$age.group, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(abalone.test$age.group))
}
plot(ks,accuracy,type = "b", ylim = c(0.67,0.69))