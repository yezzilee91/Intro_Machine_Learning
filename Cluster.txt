#Question 4.2
library(ggplot2)
#pulling off the dataset 
setwd('C:/Users/Lee_Y/Desktop/OMSA/ISYE6501/HW2')
iris = read.table('4.2irisSummer2018.txt', header=T)
#finding number of rows and columns in data
nrow(iris)
ncol(iris)
library(gridExtra)
install.packages("corrplot")
library(corrplot)
corrplot(cor(iris[,1:4]))
corrplot(cor(iris[,2:4]))
#The Best combination of predictors
corrplot()


#installing package

install.packages(stats)
library(stats)

p1<- ggplot(iris,aes(Petal.Length,Petal.Width,color=Species))+geom_point()
p1
p2<- ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+geom_point()
p2
p3 <- ggplot(iris,aes(Sepal.Length,Petal.Width,color=Species))+geom_point()
p3
p4 <- ggplot(iris,aes(Sepal.Width,Petal.Width,color=Species))+geom_point()
p4

iris_1<- iris[,-2]
summary(iris_1[,1:3])
s_iris <- scale(iris_1[,1:3])
summary(s_iris) #

set.seed(123)

km <- kmeans(s_iris, 3, iter.max=100, nstart = 5) #apply k-mens algorithm with no of centroid(k)=3
print(km)

IB <- km$betweenss/km$totss

#graph k-mean 
pairs(s_iris, col=km$cluster, las=1,
      main = paste("Iris data set\nk-means, k", 3, ",IB=", round(IB,4))
)

km2 <- kmeans(s_iris, 2, iter.max=100, nstart = 5) #apply k-mens algorithm with no of centroid(k)=5
print(km2) # this model predicts 73.7%
km4 <- kmeans(s_iris, 4, iter.max=100, nstart = 5) #apply k-mens algorithm with no of centroid(k)=5
print(km4) #this model predicts 90.0%

#elbow method for finding the optimal number of clusters
set.seed(123)
#compute and plot wss for k=2 to k=15
k.max <- 15 #max number of clusters
wss <- sapply(1:k.max,function(k){kmeans(s_iris,k,nstart=10)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

abline(v = 3, lty =2)


#Question 5.1

#pulling off the dataset 
setwd('C:/Users/Lee_Y/Desktop/OMSA/ISYE6501/HW2')
uscrime = read.table('5.1uscrimeSummer2018.txt', header=T)

str(uscrime)
#all Rs on top are 1s, the data is not randomized as required 
#Rs random number generators for mixing up data
set.seed(3)
summary(uscrime)
install.packages("outliers")
library("outliers")
table(uscrime$Crime) 
summary(uscrime$Crime) 


#I dont think I need to scale the data since I am only looking at one attibute 
uscrime1<- scale(uscrime[,16])

#boxplot Crime column and identify if there are outlier
boxplot(uscrime$Crime,outline=T,main="US Crime Data", ylab ="Crime per 100,000 people"
        ,col ="grey", outcol="red",id.n = Inf)
boxplot.stats(uscrime$Crime)
#plot Crime data 
plot(uscrime$Crime,ylab="Crime data for US",main = "US crime data without scaling")
library(outliers) 
#type=10 is a test for one outlier and testing for one.sided test
g1 = grubbs.test(uscrime$Crime, type = 10, opposite = FALSE, two.sided = FALSE);
g1
#type =10 is a test for one outlier end and for one.sided test
g2= grubbs.test(uscrime$Crime, type = 10, opposite = TRUE, two.sided = FALSE) 
g2
#type =20 for two outliers in lower tails (notworking whyyyy?) 
g3<-grubbs.test(uscrime[,16], type =20, opposite = F) 
#type =20 for two outliers in upper tails (notworking whyyyy?) 
g4<-grubbs.test(uscrime$Crime, type =20, opposite = T) 

#type=11 is a test for two outlier and testing for one.sided test
g5 = grubbs.test(uscrime$Crime, type = 11, opposite = FALSE, two.sided = FALSE);
g5
#type=10 is a test for two outlier and testing for one.sided test
g6 = grubbs.test(uscrime$Crime, type = 10, opposite = FALSE, two.sided = T);
g6

#Running grubbs test again without 1993
max(uscrime$Crime)
test1=uscrime[uscrime$Crime < 1993,]
max(test1$Crime)
#type=10 is a test for one outlier and testing for one.sided test
gtest1 = grubbs.test(test1$Crime, type = 10, opposite = FALSE, two.sided = FALSE);
gtest1 
#type =10 is a test for one outlier end and for one.sided test
gtest2= grubbs.test(test1$Crime, type = 10, opposite = TRUE, two.sided = FALSE) 
gtest2


test2=uscrime[uscrime$Crime < 1969,]
max(test2$Crime)
gtest2_1 = grubbs.test(test2$Crime, type = 10, opposite = FALSE, two.sided = FALSE);
gtest2_1
gtest2_2 =grubbs.test(test2$Crime, type = 10, opposite = TRUE, two.sided = FALSE) 


#Question 6.2

temp = read.table('6.2tempsSummer2018.txt', header=T)
plot(temp$X1996)
char<-as.character(temp$DAY)]
#as.date(temp$DAY, order = "dm")

#as.Date(as.character(temp$DAY),format="%d%m")



#install.packages("date")
#library("date")

temp_1 = read.table('6.2tempsSummer2018_1.txt', header=T)
rdate<-as.date(temp_1$DAY,order="mdy")
plot(temp_1$X1996~temp_1$DAY,type="l",xlab="DAY", ylab="X1996")
abline( = 3, lty =2)





summary(temp)
install.packages("qcc")
library(qcc)
attach(temp)
at_temp <- qcc.groups(temp[,2:20],temp[,1])
#example using cusum

data(pistonrings)
diameter <-qcc.groups(diameter,sample)

q<- cusum(diameter[1:25,], decision.interval=4,se.shift=1)
q<- cusum(diameter[1:25,], newdata=diameter[25:40,])
summary(q)
plot(q,chart.all=F)
