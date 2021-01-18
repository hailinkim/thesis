library(dplyr)
library(readr)
library(geosphere)
library(sp)
library(rgdal)

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
                                 2200 <= time & time < 2400 ~ 'T12'),
         time_period2 = case_when(time_period=='T01' ~ '12am ~ 2am',
                                  time_period=='T02' ~ '2am ~ 4am',
                                  time_period=='T03' ~ '4am ~ 6am',
                                  time_period=='T04' ~ '6am ~ 8am',
                                  time_period=='T05' ~ '8am ~ 10am',
                                  time_period=='T06' ~ '10am ~ 12pm',
                                  time_period=='T07' ~ '12pm ~ 2pm',
                                  time_period=='T08' ~ '2pm ~ 4pm',
                                  time_period=='T09' ~ '4pm ~ 6pm',
                                  time_period=='T10' ~ '6pm ~ 8pm',
                                  time_period=='T11' ~ '8pm ~ 10pm',
                                  time_period=='T12' ~ '10pm ~ 12am')) %>% 
           select(-c(date, time, zip)) 

write_csv(roaming_time, "roaming_time.csv")

roaming_seoul <- inner_join(x=roaming_time, y=seoul,by.x=c("latitude","longitude"), by.y=c("lat","long"))



roaming_country <- roaming_time %>% 
  group_by(country_code) %>%
  summarize(count=n())
write_csv(roaming_country, "roaming_country.csv")

roaming_time_period <- roaming_time %>% 
  group_by(time_period) %>%
  summarize(count=n())
write_csv(roaming_time_period, "roaming_time_period.csv")

roaming_time_country <- roaming_time %>% 
  group_by(time_period, country_code) %>%
  summarize(count=n())
write_csv(roaming_time_country, "roaming_time_country.csv")

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
  group_by(country_code, time_period, time_period2, latitude, longitude) %>%
  summarize(count=n())


#data set grouped by nationality(country_code), latitude, longitude
roaming_country_location <- roaming_time %>% 
  group_by(country_code, latitude, longitude) %>%
  summarize(count=n())

#export the data set
write_csv(roaming_dist, "roaming_dist.csv")


library(dplyr)
roaming2 <- roaming_time %>% 
  group_by(country_code, time_period, time_period2, latitude, longitude) %>% 
  dplyr::summarise(count=n())
write_csv(roaming2, "roaming2.csv")

# use the vegdist function to generate a geodesic distance matrix
require(plyr)
roaming_grid <- count(roaming_dist[,])

require(vegan)
dist_grid <- vegdist(roaming_grid, method = "gower")
d_matrix <- as.matrix(dist_grid)

# cluster all points using a hierarchical clustering approach
hc <- hclust(dist_grid, method="ward.D2")
cluster <- cutree(hc, k=900)
roaming_cluster <- cbind(roaming_grid, cluster, roaming_grid$freq)
roaming_cluster2 <- roaming_cluster %>% 
  select(-c(freq, `roaming_grid$freq`))
roaming_cluster2$id <- sample.int(10000, 9310)

#export the data set with clusters
write_csv(roaming_cluster2, "roaming_cluster.csv")
