setwd("~/Data-Science-Regression-Homework/")

#define Mean Absolute Error
mae <- function(x,y)
{mean ( abs(x-y) )}

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





