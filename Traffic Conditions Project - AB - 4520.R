traffic = read.csv("C:/Users/Austin/Downloads/Traffic.csv")

library(stringr)
library(ggplot2)
###### DATA CLEANING ######

#Creating a column with converted military times
ind=1 #My index counter
for (x in 1:nrow(traffic)) {
  if(str_detect(traffic$Time[ind], "AM")){       #All AM times
    temp = str_split(traffic$Time[ind], ":", simplify = TRUE)    #This function will split the time up into a char array according to ":" (the time is in the format "hour:min:sec AM/PM").
    temp2 = as.numeric(temp[1])       #Need the hours as a number so I can decide whether to have a zero before (no if 10 or more). 
    if(temp2 > 9){      #Case where the hour is double digits AM.
      if(temp2 == 12){    #Case where the hour is 12 AM (it will be 00 in military time)
        tempFinal = str_c("00",temp[2])
        traffic$mTime[ind] = tempFinal
      }
      else {            #Case where the hour is 10 or 11 AM
      tempFinal = str_c(temp[1],temp[2])
      traffic$mTime[ind] = tempFinal
      }
    }
    else {              #Case where the hour is single digits AM
      tempFinal = str_c("0",temp[1],temp[2])
      traffic$mTime[ind] = tempFinal
    }
  }
  else{               #All PM times
    temp = str_split(traffic$Time[ind], ":", simplify = TRUE)
    temp2 = as.numeric(temp[1])
    if(temp2 == 12){     #Case for all 12 PM times
      tempFinal = str_c(temp[1],temp[2])
      traffic$mTime[ind] = tempFinal
    }
    else {           #Case for all other PM times.
      temp2 = temp2 + 12      #All PM times other than 12 must have 12 added to them (0-24 scale).
      temp3 = as.character(temp2)     #Preparing to put the adjusted time into the final string
      tempFinal = str_c(temp3,temp[2])
      traffic$mTime[ind] = tempFinal
    }
  }  
  ind = ind+1     #My index method (caveman style)
}


#Switching the traffic conditions from low/normal/high/heavy to low/normal = "good" and high/heavy = "bad". This satisfies the binomial condition.
ind=1
for(x in 1:nrow(traffic)){
  if(traffic$Traffic.Situation[ind] == "low" | traffic$Traffic.Situation[ind] == "normal"){
    traffic$Condition[ind] = "good"
    traffic$Cond[ind] = 0
  }
  else {
    traffic$Condition[ind] = "bad"
    traffic$Cond[ind] = 1
  }
  ind = ind+1
}


#Numbering each day of the week starting with Monday==1.
ind=1
for(x in 1:nrow(traffic)){
  if(traffic$Day.of.the.week[ind] == "Monday"){
    traffic$DayOfWeek[ind] = 1
  }
  if(traffic$Day.of.the.week[ind] == "Tuesday"){
    traffic$DayOfWeek[ind] = 2
  }
  if(traffic$Day.of.the.week[ind] == "Wednesday"){
    traffic$DayOfWeek[ind] = 3
  }
  if(traffic$Day.of.the.week[ind] == "Thursday"){
    traffic$DayOfWeek[ind] = 4
  }
  if(traffic$Day.of.the.week[ind] == "Friday"){
    traffic$DayOfWeek[ind] = 5
  }
  if(traffic$Day.of.the.week[ind] == "Saturday"){
    traffic$DayOfWeek[ind] = 6
  }
  if(traffic$Day.of.the.week[ind] == "Sunday"){
    traffic$DayOfWeek[ind] = 7
  }
  ind = ind+1
}
traffic$Cond = as.numeric(traffic$Cond)

###### DATA EXPLORATION ######

#What determines the traffic condition?
ggplot(traffic, aes(x=mTime, y=Total, color=Condition))+
  geom_point()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs(x = "Time (12AM - 11:45PM)")+
  labs(y = "Total Number of Cars")+
  labs(title = "Traffic Condtions according to the Number of Cars Vs Time")

#This plot shows several things. For one, neither one of these predictors are perfect indicators of condition. This is obvious for time, but I guessed traffic condition would be bracketed as a function of the number of cars in the area. 
#This is not the case as there is clearly overlap between the good and bad data points according to total cars. As none of the other predictors would make logical sense in maintaining that kind of a relationship with the determined condition, there must be a determinant outside of this data choosing its distinction.
#With that being said, it generally appears that both of these predictors will be helpful for the model. Clear patterns can be distinguished in both the number of cars and the time of day. Very early and very late times, for example, never experienced bad traffic conditions (which makes intuitive sense as most people will be asleep at these times).

levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
ggplot(traffic, aes(x=factor(Day.of.the.week, level=levels), group=Condition, color=Condition, fill=Condition))+
  stat_count()+
  labs(x = "Day of the week")+
  labs(title = "Traffic Condtions according to the Day of the week")

#This is plot is a little challenging to read, but in general I am looking for differences in the relationship between good and bad conditions across the days. The count isn't important but the differences in the fills are. 



###### THE MODEL ######


traffic2 = traffic[ ,-c(1,3,9,11)]
traffic2$mTime = as.numeric(traffic2$mTime)

FullMod = glm(Cond ~ ., data=traffic2, family="binomial")
summary(FullMod)
1 - pchisq(deviance(FullMod), df.residual(FullMod))
exp(coef(FullMod))    #Taking coefficients out of the log-odds scale for interpretation.
#A p-val of 1 would indicate no lack of fit.

#All of the different counts are highly significant.The odds of bad traffic are 1.09 times greater with each increase in the number of bikes. With how well the counts fit, I won't test any transformations. 
#In general, I am going to have no trouble finding well fitting models. As the first visualization indicated, the total number of cars is highly correlated to the traffic condition.
#The next two models will test the difference between the individual car counts and the total car count. The original model actually excluded the total car count due to its high correlation with the individual counts.

modA = glm(Cond ~ Total, data=traffic2, family="binomial")
summary(modA)

modB = glm(Cond ~ CarCount+BikeCount+BusCount+TruckCount, data=traffic2, family="binomial")
summary(modB)

#Based on residual deviance, the model including all of the individual counts fits the data better. This is not to say the total cars models didn't perform well, as it had a p-val of 1 in the chi square test. 
#The advantage of the total cars model is that is it less computationally intensive. I will use the total cars model as a baseline for simplicity sake in testing, and then switch to the individual counts at the end for my final model. 

#The following models are going to tweak the parameters excluding the car counts.
#This first one will further examine days of the week. I will switch them to factors to see if that makes a difference.

traffic2$DayOfWeek = as.factor(traffic2$DayOfWeek)
modDays = glm(Cond ~ Total+DayOfWeek, data=traffic2, family = binomial)
summary(modDays)
#Two significant predictors are specifically Friday and Saturday. 
anova(modA, modDays, test="Chi")
#The inclusion of days of the week as a factor is a significant inclusion (p-val = 0.004).


#The next model will test the time of day. 
modTimeSq = glm(Cond ~ Total+DayOfWeek+mTime+I(mTime^2), data=traffic2, family = binomial)
summary(modTimeSq)
exp(coef(modTimeSq))
#while the time and time^2 are both significant, the coefficient for time is greater.
anova(modDays,modTimeSq, test="Chi")
#The addition is significant, but I'll test whether both terms are necessary. 
modTime = glm(Cond ~ Total+DayOfWeek+mTime, data=traffic2, family = binomial)
anova(modTime, modTimeSq, test="Chi")
#The time^2 term is significant so it will be left in the model.

#The last predictor to test is Date
#I'll turn Date into a factor to put more emphasis on the dates themselves instead of their change. 
traffic2$Date = as.factor(traffic2$Date)
modDate = glm(Cond ~ Total+DayOfWeek+mTime+I(mTime^2)+Date, data=traffic2, family = binomial)
summary(modDate)
#There are a few significant days, but not many.
anova(modTimeSq, modDate, test="Chi")
#Date is not significant and will be dropped. This would intuitively make sense as there should be any inherent reason why date would affect traffic. Day of the week would make sense as sleep schedules and standard work weeks would dictate traffic patterns.
#Date shouldn't show any patterns, at least not on a monthly basis. Holidays could create patterns on a yearly basis, but examining a month wouldn't show that.

modFinal = glm(Cond ~ CarCount+BikeCount+BusCount+TruckCount+DayOfWeek+mTime+I(mTime^2), data=traffic2, family = binomial)
summary(modFinal2)
#After changing out the total car count for the individual count, the significance for all non car count data changed.
modC = glm(Cond ~ CarCount+BikeCount+BusCount+TruckCount+mTime+I(mTime^2), data=traffic2, family = binomial)
anova(modC, modFinal, test="Chi")
#This alteration to the model made day of the week insignificant, so it will be dropped.
#This can be repeated to show that the only significant predictors in the individual counts model are the counts themselves.
t = aov(TruckCount ~ DayOfWeek, data=traffic2)
summary(t)
#This anova highlights that there is a difference in mean for bike count across days of the week. This hints at the issue with the individual count model being correlated data. 
#The best model is the model with solely the individual counts. Since the goal of this project is accurate predictions, I will select it over the model I was progressively creating. 
modFinal = glm(Cond ~ CarCount+BikeCount+BusCount+TruckCount, data=traffic2, family = binomial)


###### DIAGNOSTICS ######

#Checking for high leverage through standardized residuals
sr = rstandard(modFinal)
ind=1 
for(x in 1:length(sr)){
  if(sr[ind] > 2){
    print(paste0("Observation:",ind, "Standard Residual=", sr[ind]))
  }
  if(sr[ind] < -2){
    print(paste0("Observation:",ind, "Standard Residual=", sr[ind]))
  }
}

#There are no outliers according to this test, even after using the generally conservative cutoff of 2/-2.
#I also checked cooks distance but the highest yielded value was 0.05.

plot(predict(modFinal), residuals(modFinal), xlab="Predicted", ylab="Residuals", col=c("Cyan"))
lines(lowess(predict(modFinal),residuals(modFinal)))
#It appears that the plotted points very closely follow the regression line. It appears that the predictions for 0 (good traffic conditions) were especially accurate. 

#A quick test of the classification rate on the data.
t = table(fitted(modFinal)>0.5, traffic2$Cond)
misclass = 1-(1902+938)/(1902+938+65+71)
misclass
#The model is wrong 4.6% of the time, and is therefore accurate 95.4% of the time. The classification rate for good traffic is better than for bad traffic as I had assumed by looking at the previous plot (96.4% to 93.5%).


###### INTERPRETATION OF THE MODEL ######

exp(coef(modFinal))
#Cars: For every additional car (sedan/SUV/Pickup), the odds of bad traffic are increased by 1.092 times. 
#Bikes: For every additional bike (motorcycle), the odds of bad traffic are increased by 1.103 times.
#Buses: For every additional bus, the odds of bad traffic are increases by 1.296 times.
#Trucks: For every additional truck (semi), the odds of bad traffic are increased by 1.362 times.


####### PREDICTIONS #######

#Testing whether any specific type of vehicle can single-handedly cause bad traffic given their highest recorded count at any point in the month.
predict(modFinal, newdata = data.frame(CarCount=max(traffic$CarCount), BusCount=0, TruckCount=0, BikeCount=0), type="response")
predict(modFinal, newdata = data.frame(CarCount=0, BusCount=0, TruckCount=0, BikeCount=max(traffic$BikeCount)), type="response")
predict(modFinal, newdata = data.frame(CarCount=0, BusCount=0, TruckCount=max(traffic$TruckCount), BikeCount=0), type="response")
predict(modFinal, newdata = data.frame(CarCount=0, BusCount=max(traffic$BusCount), TruckCount=0, BikeCount=0), type="response")
#None of them were able to, as cars came in with the highest probability at 0.27 (if using a cutoff of 0.5)

#One thing that this model could be practically applied to is the separation of trucks from other forms of vehicles. 
#I've personally seen this applied in my hometown, where an alternate underpass was built for trucks in order to improve traffic conditions through a traffic focal point.

#I'll take the mean of all vehicles except for trucks, and then determine what number of trucks breaks the 50% probability point.
#This first prediction will ensure the mean of the three other types alone is enough for bad traffic conditions.
prob = predict(modFinal, newdata = data.frame(CarCount=mean(traffic$CarCount), BusCount=mean(traffic$BusCount), TruckCount=0, BikeCount=mean(traffic$BikeCount)), type="response")
#These conditions indicate a less than 1% of bad traffic
count=0
while(prob < 0.5){
  count = count+1
  prob = predict(modFinal, newdata = data.frame(CarCount=mean(traffic$CarCount), BusCount=mean(traffic$BusCount), TruckCount=count, BikeCount=mean(traffic$BikeCount)), type="response")
}

print(count)
mean(traffic$TruckCount)
#The traffic passes this test, as the number of trucks needed to break a 50% probability of bad traffic exceed the mean number of trucks by 9.

#I'll try this test again, only now I'll restrict times from 7am to 7pm. This should capture most times where traffic would realistically be at risk of being congested.
traffic3 = traffic2[(traffic2$mTime>700 & traffic2$mTime<1900), ]
count=0
while(prob<0.5){
  count = count+1
  prob = predict(modFinal, newdata = data.frame(CarCount=mean(traffic3$CarCount), BusCount=mean(traffic3$BusCount), TruckCount=count, BikeCount=mean(traffic3$BikeCount)), type="response")
}

print(count)
mean(traffic3$TruckCount)
#From 7am-7pm, the exact mean counts for all vehicle types will result in a 52% probability of bad traffic conditions. 
ind=1
count=0
for(x in 1:nrow(traffic3)){
  if(traffic3$CarCount[ind] > mean(traffic3$CarCount)){ 
    if(traffic3$BikeCount[ind] > mean(traffic3$BikeCount)){
      if(traffic3$BusCount[ind] > mean(traffic3$BusCount)){ 
        if(traffic3$TruckCount[ind] > mean(traffic3$TruckCount)){
          print(paste0("Day:",traffic3$Date[ind], ", Time:",traffic3$mTime[ind]))
          count=count+1
         }
        }
      }
  }
  ind=ind+1
}

#While my observation is very interesting, further analysis must be performed to identify the flow and tendencies of these types of traffic. As this shows, the mean vehicle counts have only been met simultaneously 8 times in the month (out of 1475 observations). 


