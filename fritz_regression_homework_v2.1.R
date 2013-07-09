setwd("~/Data-Science-Regression-Homework/")

#define Mean Absolute Error
mae <- function(x,y)
{mean ( abs(x-y) )}
head
# check 
mae(5,1)
stopifnot(mae(5,1)==4)

alltrain <-read.csv("train_50k.csv")

alltrain$Dorking <- grepl('Dorking', alltrain$LocationNormalized)
alltrain$Glasgow <- grepl('Glasgow', alltrain$LocationNormalized)
alltrain$Hampshire <- grepl('Hampshire', alltrain$LocationNormalized)  
alltrain$Surrey <- grepl('Surrey', alltrain$LocationNormalized)

set.seed(55)
alltrain$fold <- sample(1:100, nrow(alltrain), replace=TRUE)

# split into training and test group based on  
training <- subset(alltrain, Category != 'Engineering Jobs')
test <- subset(alltrain, Category == 'Engineering Jobs')

# build a model that connects salary to location
model <- glm(SalaryNormalized ~ Dorking + Glasgow + Hampshire + Surrey , data=training )
summary(model)

mae(fitted(model), alltrain$SalaryNormalized)
#too high

mae(predict(model,test), alltrain$SalaryNormalized)

#lets add two extra parameters around contract type

alltrain$Permanent <- grepl('permanent', alltrain$ContractTime)  
alltrain$Contract <- grepl('contract', alltrain$ContractTime)

# split into new training and test group based on fold
training2 <- subset(alltrain, fold > 50)
test2 <- subset(alltrain, fold < 51)


#build model adding new parameters
model2 <- glm(SalaryNormalized ~ Dorking + Glasgow + Hampshire + Surrey + Permanent + Contract , data=training2)
summary(model2)

mae(fitted(model2), alltrain$SalaryNormalized)
#pretty much the same

finalmodel <- glm(SalaryNormalized ~ Dorking + Glasgow + Hampshire + Surrey + Permanent + Contract , data=test2)


prediction<-predict(finalmodel,test2)
submission<-data.frame(test2$Id,Salary=prediction)

write.csv(submission, "my_submission.csv", row.names=FALSE)






