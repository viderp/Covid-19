library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(leaflet)
library(DT)
library(plotly)
library(ggplot2)
library(shinydashboard)
#####################################################################
# original data load and preprocessing
#####################################################################

covid_case_wide<-
  read_csv("C:/Users/ICBC/Desktop/exam requirment/Advance R/covid/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid_case_long<-gather(covid_case_wide,date,num
                        ,-`Province/State`,-`Country/Region`,-Lat,-Long) %>% 
  mutate(date=lubridate::mdy(date))

covid_case_long_country<-covid_case_long %>% 
  group_by(`Country/Region`,date) %>%
  summarise(Total_case_num=sum(num)) %>% 
  ungroup() %>%
  group_by(`Country/Region`) %>% 
  arrange(`Country/Region`,date) %>% 
  mutate(New_case_num=Total_case_num-lag(Total_case_num,default = 0))

covid_death_wide<-
  read_csv("C:/Users/ICBC/Desktop/exam requirment/Advance R/covid/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
covid_death_long<-gather(covid_death_wide,date,num,
                         -`Province/State`,-`Country/Region`,-Lat,-Long) %>%
  mutate(date=lubridate::mdy(date))

covid_death_long_country<-covid_death_long %>% 
  group_by(`Country/Region`,date) %>%
  summarise(Total_death_num=sum(num)) %>% 
  ungroup() %>%
  group_by(`Country/Region`) %>% 
  arrange(`Country/Region`,date) %>% 
  mutate(New_death_num=Total_death_num-lag(Total_death_num,default = 0))

#####################################################################
# case and death data merge
#####################################################################

covid_country<-
  left_join(covid_case_long_country ,
            covid_death_long_country, 
            by = c("Country/Region", "date")) 

#####################################################################
# clean country name
#####################################################################
covid_country$`Country/Region`<-
  gsub("[*]","",covid_country$`Country/Region`)
covid_country$`Country/Region`<-
  gsub("US","United States of America",
       covid_country$`Country/Region`)
covid_country$`Country/Region`<-
  gsub("Korea, South","South Korea",covid_country$`Country/Region`)
covid_country$`Country/Region`<-
  gsub("Serbia","Republic of Serbia",covid_country$`Country/Region`)
covid_country$`Country/Region`<-
  gsub("Bahamas","The Bahamas",covid_country$`Country/Region`)

#####################################################################
# generate long data format
#####################################################################
covid_country_long<-
  gather(covid_country,num_type,num,
         -`Country/Region`,-date) %>% 
  ungroup() %>%
  rename(Date=date, `Case number`=num, Type=num_type)


#####################################################################
# days after 10th death
#####################################################################
covid_country_long<-covid_country_long %>% 
  mutate(nDays=ifelse(Type=="Total_death_num" &`Case number`>=10,1,0)) %>% 
  group_by(`Country/Region`,Type) %>% arrange(`Country/Region`,Type,Date) %>%
  mutate(nDays=cumsum(nDays)) %>% ungroup()

#####################################################################
# map data
#####################################################################
# from https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json
world_case <- 
  geojsonio::geojson_read("C:/Users/ICBC/Desktop/exam requirment/Advance R/covid/countries.geo.json", what = "sp")

## name
world_case@data$name<-
  gsub("Czech Republic","Czechia",world_case@data$name)
world_case@data$name<-
  gsub("Myanmar","Burma",world_case@data$name)
world_case@data$name<-
  gsub("United Republic of Tanzania","Tanzania",world_case@data$name)
world_case@data$name<-
  gsub("Democratic Republic of the Congo","Congo (Kinshasa)",world_case@data$name)
world_case@data$name<-
  gsub("Republic of the Congo","Congo (Brazzaville)",world_case@data$name)
world_case@data$name<-
  gsub("Guinea Bissau","Guinea-Bissau",world_case@data$name)
world_case@data$name<-
  gsub("Macedonia","North Macedonia",world_case@data$name)

## color
bins <- c(0, 100,1000,10000,100000, Inf)
pal <- colorBin("YlOrRd", bins = bins)

#####################################################################
# top n country
#####################################################################
TopCaseCountry<-covid_country_long %>% 
  filter(Date==max(Date) & Type=="Total_case_num") %>% 
  arrange(desc(`Case number`)) %>% 
  pull(`Country/Region`)

TopNewCaseCountry<-covid_country_long %>% 
  filter(Date==max(Date) & Type=="New_case_num") %>% 
  arrange(desc(`Case number`)) %>% 
  pull(`Country/Region`)

TopDeathCountry<-covid_country_long %>% 
  filter(Date==max(Date) & Type=="Total_death_num") %>% 
  arrange(desc(`Case number`)) %>% 
  pull(`Country/Region`)

TopCFRCountry<-covid_country_long %>% 
  filter(Date==max(Date) & Type=="Total_CFR_num") %>% 
  arrange(desc(`Case number`)) %>% 
  pull(`Country/Region`)