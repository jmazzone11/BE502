# BE 502 Final Project for Function 1 - Summary Plots & Function 2 - Specified Line plots 
# 12/15/22

# Load Library ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(reshape2)

# Import Files ---
rain <- read.csv("tucson_rain.csv")
crimes <- read.csv("tucson_crimes.csv")

## ================== Josie's Function 1 =====================

#---- Data Selection Function w/ Annotations-----

data_selection <- function(rain, crimes) {
  rain$readingDate <- ymd(rain$readingDate)
  # converts date into date format
  
  crimes$Date.Occurred <- ymd(crimes$Date.Occurred)
  # converts date into date format
  
  crimes_select <- crimes %>%
    select(c(Incident.ID, Date.Occurred, Incident.Type))%>%
    filter(between(Date.Occurred, as.Date('2018-01-01'), as.Date('2021-12-31')))
  # filters and selects the crime data to include desired years & columns
  
  rain_select <- rain %>%
    select(c(readingId, readingDate, quality, rainAmount))%>%
    filter(quality == "Good")
  # filters for good quality readings and includes only desired columns
  
  rain_summary <- rain_select %>%
    mutate(month = month(readingDate), year = year(readingDate))%>%
    group_by(year, month)%>%
    summarise(rain_total = sum(rainAmount)) 
  # adds a summary column that gives the sum of rain for each year and month
  
  crimes_summary2 <- crimes_select %>%
    mutate(month = month(Date.Occurred), year = year(Date.Occurred))%>%
    # this line adds a year and month col
    group_by(year, month, Incident.Type)%>%
    summarise(crimes_total = n())
  # adds a summary column that gives the number of crime incidents per year, month and incident type
  
  m2<- merge(rain_summary, crimes_summary2, by.y = c("year", "month"))# merges the rain summary and crime summary tables together by year and month
}


data<-data_selection(rain, crimes)
data$month<-as.factor(data$month)
# converts month to factor

# -------- Summary Plots --------

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

#-----linear regression models -----


#linear regression models with custom function
#regression run by year because year is statistically contributing to the linear model 

year_select_regression <- function(data, year1) {
  yearselect <- data[ which(data$year == year1), ]
  reg <- lm(crimes_total~rain_total + Incident.Type, data = yearselect)
}

# shows there is a relationship between rain and several types of crime incident
# includes aggravated assault, burglary, larceny, & motor vehicle theft 

test <- year_select_regression(data, '2019')
summary(test)

# Larceny was excluded because it caused the same effects as an oultier which skewed the visual results of the Box plot 

data%>%
  filter(Incident.Type != "06 - Larceny*")%>%
  ggplot(aes(x = rain_total, y = crimes_total, fill = Incident.Type))+
  geom_boxplot(aes(group = Incident.Type))+
  theme_bw()+
  xlab("rain amount (cm)")+
  ylab("crime (# of incidents)")+
  guides(fill=guide_legend(title = "Incident Type"))


# ================== Raine's Function 2 =====================

# Import Files ---
rain <- read.csv("tucson_rain.csv")
crimes <- read.csv("tucson_crimes.csv")


# Formatted the dates so they are in a specific format (year-month-date)
rain$readingDate <- ymd(rain$readingDate)
crimes$Date.Occurred <- ymd(crimes$Date.Occurred)


# Select specific columns for both rain and crime data frames, assigned to rain_select & crime_select
crime_select <- crimes %>% select(c(Incident.ID, Date.Occurred, Incident.Type, UCR.Number))

rain_select <- rain %>%
  select(c(readingId, readingDate, quality, rainAmount))%>%
  filter(quality == "Good")



#----- Optional: *Requires Direct Console Input -----

# Years of comparison: 
# 2018, 2019, 2020, or 2021

# Type the year into the console after running next line
input_year <- readline("Enter year > ")


# Crime Types:
# 1 = Homicide, 2 = Sexual Assault, 3 = Robbery, 
# 4 = Aggravated Assault, 5 = Burglary, 
# 6 = Larceny, 7 = Motor Vehicle Theft, 8 = Arson

# Type the number into the console after running next line
crime_type <- readline("Enter crime # > ")


# Note:
# input_year and crime_type are variables storing the values that will be called by crime_compare function at the end


#----- Comparison Function ------

#  Comparison Function for either designated input from above or can be selected afterwards

crime_compare <- function(input_year, crime_type) {
  
  rain_summary <- rain_select %>%
    mutate(month = month(readingDate), year = year(readingDate))%>%
    filter(year==input_year)%>%
    group_by(year, month)%>%
    summarize(rain_total = sum(rainAmount))
  
  names(rain_summary)[3] <- 'Total Rain'
  
  crime_summary <- crime_select %>%
    mutate(month = month(Date.Occurred), year = year(Date.Occurred))%>%
    filter(year==input_year, UCR.Number==crime_type)%>%
    group_by(year, month)%>%
    count()
  
  # If/Else - made to assign the crime name to the plot legend, instead of the default number 
  
  if (crime_type=="1") { 
    names(crime_summary)[3] <- 'Homicides'
  } else if (crime_type=='2') {
    names(crime_summary)[3] <- 'Sexual Assault'
  } else if (crime_type=='3') {
    names(crime_summary)[3] <- 'Robbery'
  }
  else if (crime_type=='4'){
    names(crime_summary)[3] <- 'Aggravated Assault'
  }
  else if (crime_type=='5'){
    names(crime_summary)[3] <- 'Burglary'
  }
  else if (crime_type=='6'){
    names(crime_summary)[3] <- 'Larceny'
  }
  else if (crime_type=='7') {
    names(crime_summary)[3] <- "Motor Vehicle Theft"
  }
  else {
    names(crime_summary)[3] <- 'Arson'
  }
  
  rain_crime <- merge(rain_summary, crime_summary, by="month")
  rain_crime <- (rain_crime[c(1,2,3,5)])
  
  rain_crime_long <- melt(rain_crime, id.vars=c("month", "year.x"))
  
  print(ggplot(rain_crime_long, aes(month, value)) + geom_line(aes(color=variable))+ scale_x_continuous(breaks = 1:12)
  )
  
}

# ---- Comparison Plot ----

# A plot will generate from the information you directly typed in the console
crime_compare(input_year, crime_type)

# OR ~
# Select Year & Crime Number to Generate Graph ----

# Input year:
# 2018 to 2021

# Crime Types:
# 1 = Homicide, 2 = Sexual Assault, 3 = Robbery, 4 = Aggravated Assault, 5 = Burglary, 6 = Larceny, 7 = Motor Vehicle Theft, 8 = Arson

crime_compare(input_year = '2021',crime_type = '7')


