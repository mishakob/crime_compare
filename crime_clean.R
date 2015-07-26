setwd("~/Documents/data science/data incubator")
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

# reading data bases
washingtonDC_2013 <- read_csv("crime_washingtonDC_2013.csv")
chicago <- read_csv("crime_chicago.csv")
#######################################

# cleaning date_time format
washingtonDC_2013$REPORTDATETIME <- mdy_hms(washingtonDC_2013$REPORTDATETIME)
chicago$Date <- mdy_hms(chicago$Date)

# create chicago data base for year 2013
chicago_2013 <- chicago %>% 
  mutate(Year = year(Date)) %>%
  filter(Year == 2013)

# factorize
washingtonDC_2013$SHIFT <- factor(washingtonDC_2013$SHIFT)
washingtonDC_2013$OFFENSE <- factor(washingtonDC_2013$OFFENSE)
washingtonDC_2013$METHOD <- factor(washingtonDC_2013$METHOD)

chicago_2013$`Primary Type` <- factor(chicago_2013$`Primary Type`)
chicago_2013$Arrest <- factor(chicago_2013$Arrest)
chicago_2013$Domestic <- factor(chicago_2013$Domestic)

# keeping offenses similar across both data bases
chicago_2013.clean <- chicago_2013 %>%
  filter(`Primary Type` == "ARSON" | 
           `Primary Type` == "ASSAULT" |
           `Primary Type` == "BATTERY" |
           `Primary Type` == "BURGLARY" |
           `Primary Type` == "CRIM SEXUAL ASSAULT" |
           `Primary Type` == "HOMICIDE" |
           `Primary Type` == "MOTOR VEHICLE THEFT" |
           `Primary Type` == "ROBBERY" |
           `Primary Type` == "SEX OFFENSE" |
           `Primary Type` == "THEFT") %>%
  mutate(OFFENSE = factor(`Primary Type`))

chicago <- chicago_2013.clean
washington <- washingtonDC_2013

# settling discrepancies between offense classification
# for Chicago - combining "assault" and "battery" into "assault", "sex assault" and "sex offense" into "sex abuse"
# for Washington - combining "theft f/auto" and "theft/other" into "theft"
levels(chicago$OFFENSE) <- c("ARSON","ASSAULT","ASSAULT","BURGLARY","SEX ABUSE","HOMICIDE","MOTOR VEHICLE THEFT","ROBBERY","SEX ABUSE","THEFT")
levels(washington$OFFENSE) <- c("ARSON","ASSAULT","BURGLARY","HOMICIDE","MOTOR VEHICLE THEFT","ROBBERY","SEX ABUSE","THEFT","THEFT")

offense.chicago <- chicago %>%
  select(OFFENSE) %>%
  count(OFFENSE, sort = T) %>%
  mutate(offense.percent = round((n / nrow(chicago)*100),1))

offense.washington <- washington %>%
  select(OFFENSE) %>%
  count(OFFENSE, sort = T) %>%
  mutate(offense.percent = round((n / nrow(washington)*100),1))

joined.offense <- left_join(offense.chicago,offense.washington,by = "OFFENSE") %>%
  rename(n.chicago = n.x, n.washington = n.y, chicago = offense.percent.x, washington = offense.percent.y)

joined.offense.m <- melt(joined.offense,id.vars=c("OFFENSE","n.chicago","n.washington")) %>%
  arrange(desc(value))
offense.barplot <- qplot(x=OFFENSE, y=value, fill=variable,
                       data=joined.offense.m, geom="bar", stat="identity",
                       position="dodge",ylab="%",main="Crime in cities of Chicago and Washington DC: Offense type")