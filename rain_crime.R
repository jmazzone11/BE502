library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(lubridate)

rain <- read.csv("C:/Users/raine/Downloads/tucson_rain.csv")
crimes <- read.csv("C:/Users/raine/Downloads/tucson_crimes.csv")

rain$readingDate <- ymd(rain$readingDate)

crimes$Date.Occurred <- ymd(crimes$Date.Occurred)

crime_select <- crimes %>% select(c(Incident.ID, Date.Occurred, Incident.Type, UCR.Number))

rain_select <- rain %>%
  select(c(readingId, readingDate, quality, rainAmount))%>%
  filter(quality == "Good")

input_year <- readline("Please enter the period (yyyy) for which you would like crime statistics compared to rain statistics")

crime_type <- readline("Enter the number for the crime type you would like to compare (1 = Homicide, 2 = Sexual Assault, 3 = Robbery, 4 = Aggravated Assault, 5 = Burglary, 6 = Larceny, 7 = Motor Vehicle Theft")


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
  
  print(ggplot(rain_crime_long, aes(month, value)) + geom_line(aes(color=variable)))
  
}

crime_compare(input_year, crime_type)

