
#load tidyverse
library(tidyverse)
library(dplyr)

#olympic medal winners 
mydata <- read.csv("https://raw.githubusercontent.com/Prabhsharan139/Olympic_winners/main/summer.csv", header=TRUE)
mydata

str(mydata)
class(mydata)
summary(mydata)

colnames(mydata)
mydata$Year
#Making all info uppercase
mydata$City <- toupper(mydata$City)
mydata$Sport <- toupper(mydata$Sport)
mydata$Discipline <- toupper(mydata$Discipline)
mydata$Medal <- toupper(mydata$Medal)
View(mydata)

install.packages("stringr")
library(stringr)
#replacing same values before combing two coumns
mydata$Gender <- str_replace_all(mydata$Gender,c("Men"= "M","Women" = "F"))
mydata$Discipline <- str_replace_all(mydata$Discipline,c("ATHLETICS"="","FENCING"= "","JUDO"="","HOCKEY"="","HANDBALL"=""))
mydata$Discipline <- str_replace_all(mydata$Discipline,c( "SHOOTING"= "","TENNIS"= "","WEIGHTLIFTING" ="","ARCHERY" ="","CRICKET"= "","CROQUET"="" ))
mydata$Discipline <- str_replace_all(mydata$Discipline,c( "FOOTBALL"= "","GOLF"= "","POLO" ="","RUGBY" ="","ROWING"= "","sAILING"="","BOXING"="","LACROSSE"="" ))
View(mydata)

install.packages("tidyr")
library(tidyr)
#using unite to combine two columns
data1 <- unite(data = mydata, col =Sporting_discipline,Sport,Discipline)
View(data1)
     

names(mydata)[1]<-"YEAR"


Year <- filter(mydata,Year=="1999")
Year
City <- filter(mydata,City!="Berlin")
City
Athlete <- filter(mydata,Athlete=="KUMAR, Sushil")
Athlete
Athlete <- filter(mydata,Athlete=="KOM, Mary")
Athlete
Athlete <- filter(mydata,Athlete=="NEHWAL, Saina")
View(Athlete)

str(mydata)
subset_mydata <-select(mydata,Year,City,Country,Sport)
subset_mydata

#add India
subset_mydata_india <-filter(subset_mydata,Country=="IND",Year=="2008")
subset_mydata_india
nrow(subset_mydata_india)

#nested function
subset_mydata_india <-filter(select(mydata,Year,City,Country,Sport),Country=="IND",Year=="2008")
summary(subset_mydata_india)

#pipe operator
subset_mydata_india <-mydata %>%
  select(Year,City,Country,Athlete,Sport,Medal) %>%
  filter(Country=="IND", Year==2012)
subset_mydata_india

str(mydata)
china_medals <-mydata %>%
  select(Year,Country,Sport,Medal) %>%
  filter(Country=="CHN",Year=="2012",Medal=="Gold")
china_medals

#new varaiables using mutate
mydata_info <- mydata %>%
  mutate (Info=Sport,Event)

str(mydata_info)

#Total no of medals won
mydata %>%
  count(Sport,Medal)

mydata %>%
  group_by(Country=="USA")
summarise(mydata)

