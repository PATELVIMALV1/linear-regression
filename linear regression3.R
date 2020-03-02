#LINEAR REGRESSION ASSIG.,,FOR Salary_Data-PREDICTION ON  Salary FOR YearsExperience 
attach(Salary_Data)
#summary of raw data
summary(Salary_Data)
#plot the data
plot(Salary_Data)
#find the coorelation 
cor(Salary_Data$YearsExperience,Salary_Data$Salary)
#lm model
smodel<-lm(Salary~YearsExperience,data = Salary_Data)
#summmary of model
summary(smodel)
#draw the linear regression line 
abline(smodel)
#view th fitted values 
fitted.values(smodel)
#plot the fitted values
plot(Salary_Data$YearsExperience,fitted.values(smodel))
#predict for 1 data
predict(smodel,list(YearsExperience=1.3))
#predict for more than one data
predict(smodel,data.frame(YearsExperience=c(11,1.8,5.5)),interval="prediction",level=0.9)
        
##########################################################################################################
#---------------------------------------calories consumed-------------------------------------------------
attach(calories_consumed)

summary(calories_consumed)

plot(calories_consumed)

cor(calories_consumed$Weight.gained..grams.,calories_consumed$Calories.Consumed)

cmodel<-lm(Weight.gained..grams.~ Calories.Consumed,data = calories_consumed)

summary(cmodel)

abline(cmodel)

fitted.values(cmodel)

plot(calories_consumed$Calories.Consumed,fitted.values(cmodel))

predict(cmodel,list(Calories.Consumed=2300))

