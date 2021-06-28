rm(list = ls())
hours_studied <- c(4.85, 8.62, 5.43, 9.21)
hours_slept <-c(9.63, 3.23, 8.23, 6.34)
passed <-c(1, 0, 1, 0)

DF <-data.frame(hours_studied, hours_slept, passed)

TB<-t(DF)

library(ggplot2)
ggplot(DF, aes(hours_studied, hours_slept, passed)) +
  geom_point(aes())+
  labs(x='Hours studied',y='Hours slept')

model <- glm(passed ~.,family=binomial(link='logit'),data=DF) #logistic regression
#view more into by using summary(model) to find p values

hours_studied <-6.5
hours_slept <-2
DF2 <- data.frame(hours_studied, hours_slept)
pass_predict <-predict(model, newdata=DF2, type = "response") #answer is 0.1443462

#Compute the posterior probability for the student by applying the Sigmoid function.
#Posterior Probability of passing exam <- 1/(1+exp(-(67.524-11.273*hours_studied+1.984*hours_slept))) #answer is 0.1439947

