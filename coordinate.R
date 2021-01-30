setwd('/Users/angelica/Downloads/CTPRVN_201407')
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)
from_crs <- CRS('+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y=2000000 +ellps=GRS80 +units=m')
to_crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
shp <- rgdal::readOGR('TL_SCCO_CTPRVN.shp')
summary(shp)

shp2 <- spTransform(shp, to_crs)
summary(shp2)
slotNames(shp2)
shp2@data
slotNames(shp2@data)
data_sido <- shp2@data; str(data_sido)
data_result <- shp2@data
str(data_result)
shp_result <- ggplot2::fortify(shp2)
str(shp_result)
head(shp_result)
Name <- cbind(id=row.names(data_result), sidoname=as.character(data_result$CTP_ENG_NM))
str(Name)
head(Name)

x <- as.data.frame(Name)
x$id <- as.character(x$id)
str(x); head(x)

result <- left_join(shp_result, x)
str(result)
head(result)
result <- result %>% 
  select(-c(CTP_KOR_NM, CTP_ENG_NM, CTPRVN_CD))
write.csv(result, file = 'longlat.csv', row.names = F)

seoul <- result %>% 
  filter(sidoname=="Seoul")