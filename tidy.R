library(ggplot2)
library(tidyverse)
library(dplyr)

#olympic medal winners summer and winter dataset
summer <- read.csv("https://raw.githubusercontent.com/Prabhsharan139/Olympic_winners/main/summer.csv", header=TRUE)
winter <- read.csv("https://raw.githubusercontent.com/Prabhsharan139/Olympic_winners/main/winter.csv", header=TRUE)
View(summer)
View(winter)

#add India
subset_mydata_india <-filter(summer,Country=="IND",Year=="2008")
subset_mydata_india
nrow(subset_mydata_india)

#nested function
subset_mydata_india <-filter(select(summer,Year,City,Country,Sport),Country=="IND",Year=="2008")
summary(subset_mydata_india)

# use of pipe operator
#names of all winning athletes of India in 2012 summer 
subset_mydata_india <- summer %>%
  select(Year,City,Country,Athlete,Sport,Medal) %>%
  filter(Country=="IND", Year==2012)
subset_mydata_india

wrest <- summer %>%  filter(Sport == 'Wrestling')

#adding new column for season in both datasets
winter <- winter %>% mutate(Season = 'Winter')
summer <- summer %>% mutate(Season = 'Summer')

mydata <- winter %>% rbind(summer)
summary(mydata)

#count all medals awarded till 2012
mydata %>% count(Medal)


#Total no of different medals won in each category of sport
x <- summer %>%
  count(Sport,Medal)

#for winter
p <- winter %>%
  count(Sport,Medal)

#summary for USA
y <- summer %>%
  group_by(Country=="USA")
summarise(summer)

#medals won by each country in every event in descending order
z <- summer %>%
  group_by(Country, Sport) %>%
  summarise(Events = n_distinct(Event)) %>%
  arrange(desc(Events)) 

#get data of saina nehwal from summer olympics
Athlete <- filter(summer,Athlete=="NEHWAL, Saina")
View(Athlete)
Athlete <- filter(mydata,Athlete=="KOM, Mary")
Athlete
#all info about 1996 winter season
Year <- filter(winter,Year == "1994")
Year

con_med <- mydata %>%
  group_by(Country,Medal) %>%
  summarise(n_group = n())
head(mydata)

#gold medals won by china in 2012 in summer olympics in different sports
china_medals <- summer %>%
  select(Year,Sport,Country,Medal) %>%
  filter(Country=="CHN",Year=="2012",Medal=="Gold")
View(china_medals)

#medals won by mwn and women in every olympioc season
mydata %>% 
  count(Year, Season, Gender) %>%
  group_by(Year, Season) %>%
  mutate(Percent = n/sum(n)*100) %>%
  arrange(desc(Percent)) %>%
  ggplot(mapping = aes(x = Year, y = Percent, color = Gender)) + 
  geom_line(aes(color = Season), alpha = 0.3) + 
  geom_point() 

#number of events happening each year
mydata %>%
  group_by(Season, Year) %>%
  summarise(Events = n_distinct(Event)) %>%
  ggplot(mapping = aes(x = Year, y = Events, color = Season)) + 
  geom_line()

#different events every in summer and winter olympics
summer %>%
  group_by(Gender, Year, Sport) %>%
  summarise(Events = n_distinct(Event)) %>%
  ggplot(mapping = aes(x = Year, y = Events, color = Sport, group = Year)) +
  geom_path(color = 'gray') + 
  geom_point() 

#no of diff events are few in winter season so we get more clear visual.
winter %>%
  group_by(Gender, Year, Sport) %>%
  summarise(Events = n_distinct(Event)) %>%
  ggplot(mapping = aes(x = Year, y = Events, color = Sport, group = Year)) +
  geom_path(color = 'gray') + 
  geom_point()

#medals won each season
med <- mydata %>%
  group_by(Year,Season) %>%
  summarise(n_group = n())
head(mydata)

med %>%
  ggplot(aes(x=Year, y=n_group,fill=Season)) +
  xlab("year") +
  ylab("number of medals") +
  geom_histogram(binwidth=0.5,stat='identity')


a <- mydata %>%
  count(Event, Country) %>%
  group_by(Event) %>%
  mutate(Totals = sum(n),
         Percent_Medals = n/sum(n)) %>%
  arrange(desc(Percent_Medals)) %>%
  filter(n> 150)





