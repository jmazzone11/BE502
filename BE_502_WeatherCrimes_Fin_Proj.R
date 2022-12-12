library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("lubridate")
library(lubridate)


#read in the two datasets into R and name them rain & crime
rain <- read.csv("tucson_rain.csv")
crime <- read.csv("tucson_crimes.csv")


#function for cleaning the data
data_selection <- function(rain, crime) {
  rain$readingDate <- mdy(rain$readingDate) #will convert date into date format
  crimes$Date.Occurred <- ymd(crimes$Date.Occurred) #will convert date into date format
    select(c(Incident.ID, Date.Occurred, Incident.Type))%>%
    filter(between(Date.Occurred, as.Date('2018-01-01'), as.Date('2021-12-31'))) #will filter and select the crime data to include desired years & columns
  rain_select <- rain %>%
    select(c(readingId, readingDate, quality, rainAmount))%>%
    filter(quality == "Good") #will filter for good quality readings and include only desired columns
  rain_summary <- rain_select %>%
    mutate(month = month(readingDate), year = year(readingDate))%>%
    group_by(year, month)%>%
    summarise(rain_total = sum(rainAmount)) #will add a summary column that gives the sum of rain for each year and month
  crimes_summary2 <- crimes_select %>%
    mutate(month = month(Date.Occurred), year = year(Date.Occurred))%>% #this line adds a year and month column
    group_by(year, month, Incident.Type)%>%
    summarise(crimes_total = n()) #will add a summary column that gives the number of crime incidents per year, month and incident type
  m2<- merge(rain_summary, crimes_summary2, by.y = c("year", "month")) #will merge the rain summary and crime summary tables together by year and month
}

data<-data_selection(rain, crime)#example of how to use the function 

data$month<-as.factor(data$month)#convert month to factor

View(data)

##the section below contains summary plots

d <- data%>%
  filter((year != "2021" | month != "7"), Incident.Type != "06 - Larceny*")
View(d)

data%>%
  filter(Incident.Type != "06 - Larceny*")%>%
  ggplot() +
  geom_bar(aes(x= month, y = crimes_total, group = 1, fill = Incident.Type), stat = "identity")+
  geom_line(aes(x=month, y = rain_total, group = 1), color = "black", size = 1)+
  facet_wrap(~Incident.Type + year)+
  theme_bw()+
  xlab("rain amount (cm)")+
  ylab("crime (# of incidents)")+
  guides(fill=guide_legend(title = "Incident Type"))

ggsave('image1.png')


#linear regression models with custom function
#regression run by year because year is statistically contributing to the linear model 

year_select_regression <- function(data, year1) {
  yearselect <- data[ which(data$year == year1), ]
  reg <- lm(crimes_total~rain_total + Incident.Type, data = yearselect)
}

#shows there is a relationship between rain and several types of crime incident
#includes aggravated assault, burglary, larceny, & motor vehicle theft 

test <- year_select_regression(data, '2020')

summary(test)


data%>%
  filter(Incident.Type != "06 - Larceny*")%>%
  ggplot(aes(x = rain_total, y = crimes_total, fill = Incident.Type))+
  geom_boxplot(aes(group = Incident.Type))+
  theme_bw()+
  xlab("rain amount (cm)")+
  ylab("crime (# of incidents)")+
  guides(fill=guide_legend(title = "Incident Type"))
  

ggsave('image2.png')
