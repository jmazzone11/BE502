library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("lubridate")
library(lubridate)
rain <- read.csv("tucson_rain.csv")
crimes <- read.csv("tucson_crimes.csv")




rain$readingDate <- mdy(rain$readingDate)

crimes$Date.Occurred <- ymd(crimes$Date.Occurred)


crimes_select <- crimes %>%
  select(c(Incident.ID, Date.Occurred, Incident.Type))

View(crimes_select)

rain_select <- rain %>%
  select(c(readingId, readingDate, quality, rainAmount))%>%
  filter(quality == "Good")

View(rain_select)

rain_summary <- rain_select %>%
  mutate(month = month(readingDate), year = year(readingDate))%>%
  group_by(year, month)%>%
  summarise(rain_total = sum(rainAmount))

View(rain_summary)

crimes_summary <- crimes_select %>%
  mutate(month = month(Date.Occurred), year = year(Date.Occurred))%>%
  group_by(year, month)%>%
  summarise(crimes_total = n())

View(crimes_summary)
