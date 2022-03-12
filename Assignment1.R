# 1). load the package
library(tidyverse)

# 2). read avocado data set
# setwd("D:/wm/stocastic modeling/hw/ass2") # set working directory
rm(list=ls()) 
Avocado<-read_csv("C:/Users/Wu/Downloads/avocado.csv") 
spec(Avocado) # see columns
glimpse(Avocado)
summary(Avocado)

# 3). rename 1st column to "Week"
Avocado<-rename(Avocado, Week = ...1)

# 4). change date format
# install.packages("lubridate")
library(lubridate)
str(Avocado$Date)
Avocado %>% arrange(mdy(Date)) ->Avocado
#Avocado$Date <- as.Date(Avocado$Date,format = "%m/%d/%Y")

# 5). select specific regions in region column
Avocado %>% filter(region %in% c("West", "Southeast", "SouthCentral", "Northeast", "Midsouth", "Plains", "GreatLakes")) -> RegionalAvocado

# 6). group by region and type
# 7). create "LagAveragePrice"
# 8). create "Pchange"
# 9). Create "AvocadoPchange"
RegionalAvocado %>% group_by(region,type) %>% 
                    arrange(mdy(Date), .by_group = TRUE) %>%
                    mutate(LagAveragePrice = lag(AveragePrice)) %>%
                    mutate(Pchange = AveragePrice-LagAveragePrice) %>% 
                    mutate(Direction = if_else(Pchange > 0,"Up","Down")) -> AvocadoPchange

# 10). remove NAs
AvocadoPchange %>% drop_na(Pchange) -> AvocadoPchange

# 11). create new tibble with region and mean Pchange by type
AvocadoPchange %>% group_by(type,region) %>% summarise(MeanPchange = mean(Pchange)) -> AvocadoMean

# 12). create new tibble with region and std Pchange by type
AvocadoPchange %>% group_by(type,region) %>% summarise(StdevPchange = sd(Pchange)) -> AvocadoStdev

# 13). join AvocadoMean and AvocadoStdev by region and type
(SummaryTable <- left_join(AvocadoMean,AvocadoStdev,by=c("region","type")))

# 14). create a tibble with top 3 total volume
AvocadoPchange %>% select(`Total Volume`,type,region)%>% 
                   arrange(desc(`Total Volume`)) %>%
                   group_by(type,region)%>% # group by region and type
                   top_n(3,`Total Volume`) -> AvocadoTop3

