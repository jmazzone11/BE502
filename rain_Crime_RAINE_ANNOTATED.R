#The point of this code is to take in a specified year and a specific crime (like HOMICIDE or ARSON) and plot them as lines on the same graph.


library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(lubridate)

#Called a bunch of packages to deal with dates, melting dataframes, and plotting

rain <- read.csv("C:/Users/raine/Downloads/tucson_rain.csv")
crimes <- read.csv("C:/Users/raine/Downloads/tucson_crimes.csv")

#Read .csv data files

rain$readingDate <- ymd(rain$readingDate)
crimes$Date.Occurred <- ymd(crimes$Date.Occurred)

#Formatted the dates so that they were readable and in a specific format (year-month-date)

crime_select <- crimes %>% select(c(Incident.ID, Date.Occurred, Incident.Type, UCR.Number))

rain_select <- rain %>%
  select(c(readingId, readingDate, quality, rainAmount))%>%
  filter(quality == "Good")

#rain_select and crime_select are new data frames with just the columns I wanted to have from the original dataframes.

input_year <- readline("Please enter the period (yyyy) for which you would like crime statistics compared to rain statistics")
crime_type <- readline("Enter the number for the crime type you would like to compare (1 = Homicide, 2 = Sexual Assault, 3 = Robbery, 4 = Aggravated Assault, 5 = Burglary, 6 = Larceny, 7 = Motor Vehicle Theft")

#input_year and crime_type are variables storing the values that will be called by crime_compare. 
#I designed it this way so that there would be some sort of user input. He seems to like that.

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

#Now for the big daddy function, crime_Compare. It takes the variables input_year and crime_compare.
#It then creates a new data frame with rain data, selecting the year specified by input_year, and summing the total rain each month.
#Same thing for crimes, it takes in the crime_type and then counts the number of that kind of crime for each month of the input_year. 
#I wanted to throw in an if/else thing to "show multiple skills" so I renamed the column of the new crime data frame based on the crime_type. 
#Then I merged the new rain and crime dataframes, and since it ended up with two year columns I got rid of one of those.
#Then I melted them together so I could properly plot two lines. Not sure if this is the best way, but this is what the internet said.
#Basically, instead of having a "total rain" column AND a "total crime" column, there is a "value" column that stores the values for both total rain and total crime.
#Then there's another column for "group" so all of the rain data is in the group "Total Rain" and all of the crime data is in the "Crime" group.
#FINALLY it plots them,

crime_compare(input_year, crime_type)
#Callng the function with the above-mentioned variables.

