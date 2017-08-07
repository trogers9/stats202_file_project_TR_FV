#SVM
install.packages('e1071', lib="./Rpackage", repos="http://cran.rstudio.com/")

library(e1071, lib.loc = "./Rpackage")
#load training
#data<-read.csv('./Documents/Stanford/2016-2017/Summer Quarter/Stats 202/final project/training.csv')

train_df <-read.csv('/Users/thomasrogers/stats202/training.csv')

#Random subset of data
set.seed(1)
data.length = dim(data)[1]
data.length
#Should we be using replacement?
random = sample(1:data.length, data.length, replace = FALSE)
#Should we be using replacement?
data_sub = data[random[1:(data.length/10)],]
data_sub
#Plot pairs and check correlation
pairs(data_sub)
cor(data_sub)

##lets try to quantify difficulty of a query by checking fraction of relevant pages returned?

#Subset of short queries
short <- data[,3]<3
short
short_queries <- data[short,]
##filtered queries less than 3 words long
short_queries
#random sample of these queries
short.length = dim(short_queries)[1]
short.length
#Again, might want to be using replacement
random = sample(1:short.length, short.length, replace = FALSE)
short_sub = short_queries[random[1:(short.length/10)],]
short_sub
#Seperate x's and y's
sy_sub<-as.factor(short_sub[,13])
sy_sub
sx_sub<-short_sub[,1:12]
sx_sub

##SVM of short queries degree 1 .3464175
svm_tune_short <- tune(svm, train.x=sx_sub, train.y=sy_sub,
                    kernel="polynomial", ranges=list(degree=(1:2)))
print(svm_tune_short)

#Print classifications
head(data)
data[,13]

#seperate x's and y's
y_sub<-as.factor(data_sub[,13])
y_sub
x_sub<-data_sub[,1:12]
x_sub

#SVM tune
#Best cost, gamma, epsilon = 1, .5, 0.
svm_tune <- tune(svm, train.x=x_sub, train.y=y_sub,
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2), epsilon=seq(0, 1, by = .1)))
print(svm_tune)
#svm_tune1 polynomial degree 1 had error rate of .347071
svm_tune1 <- tune(svm, train.x=x_sub, train.y=y_sub,
                 kernel="polynomial", ranges=list(degree=(1:9)))
#svm_tune2 degree 1, cost 100, gamma .5 error rate .3460708
system.time(svm_tune2 <- tune(svm, train.x=x_sub, train.y=y_sub,
                 degree=1,kernel="polynomial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2))))
print(svm_tune2)
#svm_tune3 degree 1, cost 150, gamma .75
system.time(svm_tune3 <- tune(svm, train.x=x_sub, train.y=y_sub,
                              degree=1,kernel="polynomial", ranges=list(cost=seq(50,150, by = 50), gamma=c(.25,.5,.75))))
print(svm_tune3)
#svm_tune3 degree 1, cost 250, gamma .75 error rate .3469526
system.time(svm_tune3 <- tune(svm, train.x=x_sub, train.y=y_sub,
                              degree=1,kernel="polynomial", ranges=list(cost=seq(150,350, by = 100), gamma=c(.75,1.5,3))))
print(svm_tune3)

#svm_tune4 degree 1, cost 250, gamma .85, error rate = .3464526
system.time(svm_tune4 <- tune(svm, train.x=x_sub, train.y=y_sub,
                              degree=1,kernel="polynomial", ranges=list(cost=seq(225,275, by = 25), gamma=c(.65,.75,.85))))
print(svm_tune4)

#svm_tune4 degree 1, cost 245, gamma .8, error rate = .3471977
system.time(svm_tune5 <- tune(svm, train.x=x_sub, train.y=y_sub,
                              degree=1,kernel="polynomial", ranges=list(cost=seq(245,255, by = 5), gamma=c(.8,.85,.9))))
print(svm_tune5)

#Once we have tuned we will use this format
fit_tuned <- svm(formula = y_sub ~ ., data = x_sub, kernel = "radial", cost = 1,
         gamma = 0.5)
summary(fit_tuned)
pred <- predict(fit_tuned, x_sub)
table(pred, y_sub)
