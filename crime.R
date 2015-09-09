setwd("~/Documents/datascience/dataincubator/crime_compare")
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

# glimpse(washingtonDC_2013)
# glimpse(chicago)
# glimpse(chicago_2013)
# glimpse(chicago_2013.clean)
# levels(chicago_2013$`Primary Type`)
# levels(chicago_2013.clean$Offense)
# levels(washingtonDC_2013$OFFENSE)
# table(chicago_2013$`Primary Type`)
# table(washingtonDC_2013$OFFENSE)
# table(washingtonDC_2013$METHOD)
# table(chicago_2013.clean$Offense)


# reading data bases
washingtonDC_2013 <- read_csv("crime_incidents_2013_CSV.csv")
chicago <- read_csv("crime_chicago.csv")
#######################################

# cleaning date_time format
washingtonDC_2013$LASTMODIFIEDDATE <- mdy_hms(washingtonDC_2013$LASTMODIFIEDDATE)
chicago$Date <- mdy_hms(chicago$Date)

# create chicago data base for year 2013
chicago_2013 <- chicago %>% 
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

# settling discrepancies between offense classification
# for Chicago - combining "assault" and "battery" into "assault", "sex assault" and "sex offense" into "sex abuse"
# for Washington - combining "theft f/auto" and "theft/other" into "theft"
levels(chicago_2013.clean$OFFENSE) <- c("ARSON","ASSAULT","ASSAULT","BURGLARY","SEX ABUSE","HOMICIDE","AUTO THEFT","ROBBERY","SEX ABUSE","THEFT")
levels(washingtonDC_2013$OFFENSE) <- c("ARSON","ASSAULT","BURGLARY","HOMICIDE","AUTO THEFT","ROBBERY","SEX ABUSE","THEFT","THEFT")

# reordering chicago levels
chicago_2013.clean$OFFENSE = factor(chicago_2013.clean$OFFENSE,levels(chicago_2013.clean$OFFENSE)[c(1:3,5:7,4,8)])

# calculating percentage of offence types
offense.chicago_2013.clean <- chicago_2013.clean %>%
  select(OFFENSE) %>%
  count(OFFENSE, sort = T) %>%
  mutate(offense.percent = round((n / nrow(chicago_2013.clean)*100),1))

offense.washingtonDC_2013 <- washingtonDC_2013 %>%
  select(OFFENSE) %>%
  count(OFFENSE, sort = T) %>%
  mutate(offense.percent = round((n / nrow(washingtonDC_2013)*100),1))

# joining two databases together
joined.offense <- left_join(offense.chicago_2013.clean,offense.washingtonDC_2013,by = "OFFENSE") %>%
  rename(n.chicago_2013.clean = n.x, n.washingtonDC_2013 = n.y, chicago_2013.clean = offense.percent.x, washingtonDC_2013 = offense.percent.y)

# plot 1
joined.offense.m <- melt(joined.offense,id.vars=c("OFFENSE","n.chicago_2013.clean","n.washingtonDC_2013")) %>%
  arrange(desc(value))
offense.barplot <- qplot(x=OFFENSE, y=value, fill=variable,
                       data=joined.offense.m, geom="bar", stat="identity",
                       position="dodge",ylab="%",main="Crime in cities of Chicago and Washington DC based on offense type (year 2013)")