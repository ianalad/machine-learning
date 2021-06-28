#Part1. Regression Analysis

rm(list = ls())

#Loading our file
data<-read.csv('Housing_data.csv', header = TRUE, sep = ',')

#Determine the structure of our data
str(data)
#there are 506 observations and 10 variables. 

#Exploratory data analysis
head(data) #view the first observations to see how our data is structured

summary(data) #min,max,mean, 1st QR, 3rd QR
any(is.na(data)) #FALSE, means no NA values
is.null(data) #FALSE, means no NULL values
duplicated(data) # no duplicated rows

#visual exploratory analysis
library(ggplot2)

ggplot(data,aes(x=PCCR))+
  geom_histogram()

ggplot(data,aes(x=PRLZ))+
  geom_histogram()

ggplot(data,aes(x=INDUS))+
  geom_histogram()

ggplot(data,aes(x=NOX))+
  geom_histogram()

ggplot(data,aes(x=AVR))+
  geom_histogram()

ggplot(data,aes(x=AGE))+
  geom_histogram()

ggplot(data,aes(x=DIS))+
  geom_histogram()

ggplot(data,aes(x=RAD))+
  geom_histogram()

ggplot(data,aes(x=TAX))+
  geom_histogram()

ggplot(data,aes(x=MEDV))+
  geom_histogram()

#Variables in the data that seem to have outliers
boxplot(data$PCCR) #yes
boxplot(data$PRLZ) #yes
boxplot(data$INDUS) #no
boxplot(data$NOX) #no
boxplot(data$AVR) #yes
boxplot(data$AGE) #no
boxplot(data$DIS) #yes
boxplot(data$RAD) #no
boxplot(data$TAX) #no
boxplot(data$MEDV) #yes

#Correlation analysis between all variables
library(corrplot)
cor(data,method = 'pearson')

#Correlation visualized for all variables
corrplot(cor(data),'number') 

#As it can be seen, the variable that have the highest (absolute) linear 
#association with the prices of the house (MEDV) is AVR.


ggplot(data,aes(x=data$AVR,y=data$MEDV))+
  geom_point()+
  xlab('Average number of rooms')+
  ylab('Median value of owner-occupied homes')+
  ggtitle('Correlation')+
  theme(plot.title = element_text(hjust = 0.5)) #center title

#positive correlation, the prices increase as the value of AVR increases linearly. 
#However, there are few outliers.


# Linear regression with all variables
model1<-lm(MEDV~.,data)
summary(model1)


#Multiple R-squared:  0.6275,	
#p value of INDUS is high, which means it's not significant

#Step-by-step removal of variables with high p-value

model1<-lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX,data)
summary(model1) 
#Residual standard error: 5.665 
#Multiple R-squared:  0.6266

model1<-lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX+0,data)
summary(model1) 

#Multiple R-squared:  0.9467, all variables are significant

#Obtain info about our model
coef(model1)
fitted.values(model1) #predictions for the x-values in the data set 
residuals(model1) #the difference between the observed values and the predicted value


# Predictions using our linear regression model
predict(model1,data)

#Residual analysis
plot(residuals(model1),xlab="Observations",ylab="Residuals",
     main="Residual Analysis",col="blue",pch=16)#plot of our residuals
abline(a=0,b=0,col="black",lty=2) #mean
abline(a=-3*sd(residuals(model1)),b=0,col="red",lty=2)#set the line1
abline(a=3*sd(residuals(model1)),b=0,col="red",lty=2)#set the line2


# Plot density
plot(density(residuals(model1)),col='blue',main='Distribution of Residuals')
mean(residuals(model1))


#Plot of the original vs. the fitted values
ggplot(data,aes(x=1:length(MEDV),y=MEDV))+
  geom_line(col='blue')+
  geom_point(col='blue')+
  geom_line(aes(x=1:length(MEDV),y=fitted.values(model1)),col='red')+
  geom_point(aes(x=1:length(MEDV),y=fitted.values(model1)),col='red')+
  xlab("Observation")+ 
  ylab("Prices of houses")+
  ggtitle('Actual ("blue") vs Fitted ("red") Values')+
  theme(plot.title = element_text(hjust = 0.5))




#Part 2. Classification

rm(list = ls())

#Loading our file
data_Thyroid<-read.csv('Thyroid_data.csv', header = TRUE, sep = ',')

#Determine the structure of our data
str(data_Thyroid)

#Exploratory data analysis
summary(data_Thyroid) #mean,min,max
any(is.na(data_Thyroid)) #no missing values


# Is the data balanced?
table(data_Thyroid$CLASS) #no (70%-30%)

library(ggplot2)

ggplot(data_Thyroid,aes(x=T3))+
  geom_histogram()

ggplot(data_Thyroid,aes(x=TST))+
  geom_histogram()

ggplot(data_Thyroid,aes(x=TSTR))+
  geom_histogram()

ggplot(data_Thyroid,aes(x=TSH))+
  geom_histogram()

ggplot(data_Thyroid,aes(x=MAD.TSH))+
  geom_histogram()

#Data Normalization
library(scales)
data_Thyroid = apply(data_Thyroid,2,rescale,to=c(0,1))
data_Thyroid.df<-data.frame(data_Thyroid)


library(caTools)
# Split data
sample<-sample.split(data_Thyroid.df$CLASS, SplitRatio = 0.7) #70% for training and 30% for testing
training<-subset(data_Thyroid.df,sample == TRUE) #sample is in the training data
testing<-subset(data_Thyroid.df,sample == FALSE) #sample not in the training data



library(rpart) # decision tree
library(rpart.plot) # decision tree visualization
library(class) # KNN
library(ggplot2)
library(dplyr)

# How do the training and testing data look like?
ThyroidVis<-data_Thyroid.df%>%mutate(training=sample)

corrplot(cor(data_Thyroid.df),'number')

#Visualizaion based on training and testing
ggplot(ThyroidVis,aes(x=TSTR,y=TST,col=as.factor(CLASS)))+
  geom_point()+
  geom_jitter()+
  facet_wrap(~training)


# Let's make a (standard) decision tree classification model

treemodel<-rpart(CLASS ~ .,data = training, method = 'class',minbucket=20)

#we need to predict the CLASS labels

# Let's plot the resulting decision tree, visualization
rpart.plot(treemodel)


#model is less complicated when we use the high number of buckets


# Let's calcualte the training and test accuracies
treeTraining=predict(treemodel,training,type="class")
treeTesting=predict(treemodel,testing,type="class")

# Let's construct a confusion matrix
tab=table(training$CLASS,treeTraining)

table(training$CLASS)# number of training predictions for each class 

library(caret) 
confusionMatrix(tab) #accuracy

#how are we on the test data?

tab<-table(testing$CLASS, treeTesting)

confusionMatrix(tab) #accuracy

rpart.rules(treemodel) #decision roots


# Construct a loop to see the number of best minimum leave size (CART)
treeTrain=matrix(0,nrow=50,ncol=200)
treeTest=matrix(0,nrow=50,ncol=200)

for(i in 1:50){
  for(j in 1:200){
    sample=sample.split(data_Thyroid.df$CLASS,SplitRatio=0.7)
    training=subset(data_Thyroid.df,sample==TRUE)
    testing=subset(data_Thyroid.df,sample==FALSE)
    treemodel<-rpart(CLASS ~ .,data = training, method = 'class',minbucket = i)
    Training_pred=predict(treemodel,training,type="class")
    Testing_pred=predict(treemodel,testing,type="class")
    treeTrain[i,j]=1-mean(training$CLASS==Training_pred)
    treeTest[i,j]=1-mean(testing$CLASS==Testing_pred)
  }
}

treeTrain=rowMeans(treeTrain)
treeTest=rowMeans(treeTest)


#Visualize the results
results_dfr=data.frame(mls=1:50,train_perf=treeTrain, test_perf=treeTest)
ggplot(results_dfr,aes(x=mls, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=mls, y=test_perf),col='red')+
  xlab("Minimum Leave Size") + ylab("Error")+
  ggtitle('Training Error(blue) vs. Testing Error (red) based on Tree Model')+
  theme(plot.title = element_text(hjust = 0.5))



#k-nearest neighbor classification model (KNN classifier)
# Construct a loop to see how the number of neighbors
# impacts the training and test performance
KNNTrain=matrix(0,nrow=50,ncol=200)
KNNTest=matrix(0,nrow=50,ncol=200)

for(i in 1:50){
  for(j in 1:200){
    sample=sample.split(data_Thyroid.df$CLASS,SplitRatio=0.7)
    training2=subset(data_Thyroid.df,sample==TRUE)
    testing2=subset(data_Thyroid.df,sample==FALSE)
    train_pred = knn(training2[,2:6], training2[,2:6], factor(training2$CLASS),i)
    test_pred = knn(training2[,2:6], testing2[,2:6], factor(training2$CLASS),i)
    KNNTrain[i,j]=1-mean(training2$CLASS==train_pred) # Looking at the error (1-accuracy)
    KNNTest[i,j]=1-mean(testing2$CLASS==test_pred) # Looking at the error (1-accuracy)
  }
}

KNNTrain=rowMeans(KNNTrain)
KNNTest=rowMeans(KNNTest)

#Visualize the results
resultsdf=data.frame(neighbors=1:50,train_perf=KNNTrain, test_perf=KNNTest)
ggplot(resultsdf,aes(x=neighbors, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors, y=test_perf),col='red')+
  xlab("Number of k") + ylab("Error")+
  ggtitle('Training Error (blue) vs. Testing Error (red) based on KNN algorithm')+
  theme(plot.title = element_text(hjust = 0.5))

#k-nearest neighbor classification model (KNN classifier)

sample2=sample.split(data_Thyroid.df$CLASS,SplitRatio=0.7) #70% for training
training2=subset(data_Thyroid.df,sample==TRUE) #sample is in the training data
testing2=subset(data_Thyroid.df,sample==FALSE)


trainprediction = knn(training2[,2:6], training2[,2:6], as.factor(training2$CLASS), 9)

testprediction = knn(training2[,2:6], testing2[,2:6], as.factor(training2$CLASS), 9)

# Let's construct a confusion matrix
tab2=table(training2$CLASS,trainprediction)
confusionMatrix(tab2)

tab2=table(testing2$CLASS,testprediction)
confusionMatrix(tab2)



#KNN classification on the original data without scaling

data_Thyroid_copy<-read.csv('Thyroid_data.csv', header = TRUE, sep = ',')


KNNTrain=matrix(0,nrow=50,ncol=200)
KNNTest=matrix(0,nrow=50,ncol=200)

for(i in 1:50){
  for(j in 1:200){
    sample=sample.split(data_Thyroid_copy$CLASS,SplitRatio=0.7)
    training2=subset(data_Thyroid_copy,sample==TRUE)
    testing2=subset(data_Thyroid_copy,sample==FALSE)
    train_pred = knn(training2[,2:6], training2[,2:6], factor(training2$CLASS),i)
    test_pred = knn(training2[,2:6], testing2[,2:6], factor(training2$CLASS),i)
    KNNTrain[i,j]=1-mean(training2$CLASS==train_pred) # Looking at the error (1-accuracy)
    KNNTest[i,j]=1-mean(testing2$CLASS==test_pred) # Looking at the error (1-accuracy)
  }
}

KNNTrain=rowMeans(KNNTrain)
KNNTest=rowMeans(KNNTest)

#Visualize the results
resultsdf=data.frame(neighbors=1:50,train_perf=KNNTrain, test_perf=KNNTest)
ggplot(resultsdf,aes(x=neighbors, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors, y=test_perf),col='red')+
  xlab("Number of k") + ylab("Error")+
  ggtitle('Training Error (blue) vs. Testing Error (red) based on KNN algorithm (Unscaled data)')+
  theme(plot.title = element_text(hjust = 0.5))




#Part 3. Clustering

rm(list = ls())
#Loading our file
data_Wine<-read.csv('Wine_data.csv', header = TRUE, sep = ',')

#Determine the structure of our data
str(data_Wine)

#Exploratory data analysis
head(data_Wine) #view the first observations

summary(data_Wine) #mean, min, max, 1st QR, 3rd QR
any(is.na(data_Wine)) #there are some missing values

#Replacing missing values
colnames(data_Wine)[colSums(is.na(data_Wine)) > 0] #names of columns with missing values


table(is.na(data_Wine)) #7 missing values
which(is.na(data_Wine), arr.ind=TRUE) #exact location(row,column)

#Numbers derived from the open wine dataset online
any(is.na(data_Wine[3]))
data_Wine$MALIC[is.na(data_Wine$MALIC)] = 5.51

any(is.na(data_Wine[6]))
data_Wine$MAGNESIUM[is.na(data_Wine[6])] = 162

any(is.na(data_Wine[11]))
data_Wine$COLOR[is.na(data_Wine[11])] = 11.75

any(is.na(data_Wine[12]))
data_Wine$HUE[is.na(data_Wine[12])] = 1.71

any(is.na(data_Wine[14]))
data_Wine$PROLINE[is.na(data_Wine[14])] = c(1450,1680,1515)



library(dplyr)
data_Wine %>% 
  group_by(TYPE) %>% 
  summarize(n=n()) 
#view what is the amount of different types of wines

#visual exploratory analysis
corrplot(cor(data_Wine),'ellipse')
cor(data_Wine)

library(ggplot2)

ggplot(data_Wine,aes(x=PHENOLS,y=FLAVANOIDS))+
  geom_point(aes(color=TYPE))


pairs(data_Wine[,1:3], pch = 19, lower.panel = NULL,col=data_Wine$TYPE)
pairs(data_Wine[,3:5], pch = 19, lower.panel = NULL,col=data_Wine$TYPE)
pairs(data_Wine[,5:7], pch = 19, lower.panel = NULL,col=data_Wine$TYPE)
pairs(data_Wine[,7:9], pch = 19, lower.panel = NULL,col=data_Wine$TYPE)
pairs(data_Wine[,9:11], pch = 19, lower.panel = NULL,col=data_Wine$TYPE)
pairs(data_Wine[,11:13], pch = 19, lower.panel = NULL,col=data_Wine$TYPE)
pairs(data_Wine[,13:14], pch = 19, lower.panel = NULL,col=data_Wine$TYPE)


library(NbClust)


#Histograms of all variables
library(plyr)
library(psych)
multi.hist(data_Wine[2:14])

#Outlier Analysis
boxplot(ALCOHOL ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Alcohol")

boxplot(MALIC ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Malic")

boxplot(ASH ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Ash")

boxplot(ALCALINITY ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Alcalinity")

boxplot(MAGNESIUM ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Magnesium")

boxplot(PHENOLS ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Phenols")

boxplot(FLAVANOIDS ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Flavanoids")

boxplot(NONFLAVANOIDS ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Nonflavanoids")

boxplot(PROANTHOCYANINS ~ TYPE, data=data_Wine,
        xlab="Type", ylab="PROANTHOCYANINS")

boxplot(COLOR ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Color")

boxplot(HUE ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Hue")

boxplot(DILUTION ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Dilution")

boxplot(PROLINE ~ TYPE, data=data_Wine,
        xlab="Type", ylab="Proline")

#There are a few outliers

# Elbow method
library(purrr)
tot_within_ss=map_dbl(1:10,function(k){
  our_model=kmeans(data_Wine,centers=k)
  our_model$tot.withinss
})

plot(1:10,tot_within_ss,type="o",xlab="number of clusters",ylab="Total WSS",main = 'Elbow method')
#3 clusters


# Other methods
SilClust=NbClust(data_Wine,distance="euclidean",min.nc=2,max.nc=10,method="kmeans",index="silhouette")
GapClust=NbClust(data_Wine,distance="euclidean",min.nc=2,max.nc=10,method="kmeans",index="gap")

par(mfrow=c(1,2))
plot(2:10,SilClust$All.index,type="o",col="blue",
     xlab="number of clusters",ylab="Silhouette Value",main = 'Silhouette Method')#we tested from 2 to 10
plot(2:10,GapClust$All.index,type="o",col="blue",
     xlab="number of clusters",ylab="Gap Value",main = 'Gap Method')
#2,2 clusters


# The optimal number of clusters for the clustering
kmeansmdl=kmeans(data_Wine,centers=3,nstart=25) #25 random initializations
plot(data_Wine,col=kmeansmdl$cluster)
  
plot(data_Wine$FLAVANOIDS,data_Wine$PHENOLS,col=kmeansmdl$cluster,
     xlab = 'FLAVANOIDS',ylab ='PHENOLS')

plot(data_Wine$FLAVANOIDS,data_Wine$DILUTION,col=kmeansmdl$cluster,
     xlab = 'FLAVANOIDS',ylab ='DILUTION')

#(kmeansmdl$betweenss)/(kmeansmdl$totss) = 0.8652406


# Add a column for cluster membership
datanew=data_Wine%>%mutate(member=kmeansmdl$cluster)

#Analyse our data
library(dplyr)
datanew %>% 
  group_by(member) %>%
  summarise_all(list(mean = mean)) #mean for all variables 

datanew %>% 
  group_by(member) %>%
  summarise_all(list(sd = sd)) #standard deviation of all variables

# Correlation within clusters for given dimensions
datanew %>% 
  group_by(member) %>%
  summarise(correlation=cor(FLAVANOIDS,DILUTION))

