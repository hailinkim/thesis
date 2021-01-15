library(dplyr)
library(readr)

path <- "/Users/angelica/Desktop/thesis"
filename <- "roaming_sample_final.csv"
roaming <- paste0(path,"/",filename)

roaming <- read.csv(roaming)

roaming_time <- roaming %>% 
  #create a time period variable covering 2 hours
  mutate(time_period = case_when(0 <= time & time < 200 ~ 'T01', #12am(midnight) ~ 2am
                                 200 <= time & time < 400 ~ 'T02', #2am ~ 4am
                                 400 <= time & time < 600 ~ 'T03', #4am ~ 6am
                                 600 <= time & time < 800 ~ 'T04', #6am ~ 8am
                                 800 <= time & time < 1000 ~ 'T05', #8am ~ 10am
                                 1000 <= time & time < 1200 ~ 'T06', #10am ~ 12pm(noon)
                                 1200 <= time & time < 1400 ~ 'T07', #12pm ~ 2pm
                                 1400 <= time & time < 1600 ~ 'T08', #2pm ~ 4pm
                                 1600 <= time & time < 1800 ~ 'T09', #4pm ~ 6pm
                                 1800 <= time & time < 2000 ~ 'T10', #6pm ~ 8pm
                                 2000 <= time & time < 2200 ~ 'T11', #8pm ~ 10pm 
                                 2200 <= time & time < 2400 ~ 'T12'))  #10pm ~ 12am(midnight)  

#data set for late-night(10pm ~ midnight) call records
roaming_night <- roaming_time %>% 
  filter(time_period == "T01" | time_period == "T12") %>%
  group_by(latitude, longitude) %>%
  summarize(count=n())

#export the data set
write_csv(roaming_night, "roaming_night.csv")


#data set grouped by location based on latitude and longitude
roaming_location <- roaming_time %>% 
  group_by(latitude, longitude) %>%
  summarize(count=n())

#export the data set
write_csv(roaming_location, "roaming_location.csv")


#data set grouped by time, latitude, longitude
roaming_time_location <- roaming_time %>% 
  group_by(time_period, latitude, longitude) %>%
  summarize(count=n())

#export the data set
write_csv(roaming_time_location, "roaming_time_location.csv")


#data set grouped by nationality(country_code), time, latitude, longitude
roaming_country_time_location <- roaming_time %>% 
  group_by(country_code, time_period, latitude, longitude) %>%
  summarize(count=n())

#export the data set
write_csv(roaming_country_time_location, "roaming_country_time_location.csv")


#data set grouped by nationality(country_code), latitude, longitude
roaming_country_location <- roaming_time %>% 
  group_by(country_code, latitude, longitude) %>%
  summarize(count=n())

#export the data set
write_csv(roaming_country_location, "roaming_country_location.csv")
