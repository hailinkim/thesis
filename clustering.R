library(rgdal)
library(dplyr)
#reading in shapefile and converting it to a dataframe
path <- "/Users/angelica/Desktop/thesis/GIS"
filename <- "seoul.shp"
seoul <- paste0(path,"/",filename)
shp <- readOGR(seoul)
df <- as(shp, "data.frame")

df2 <- df %>% 
  rename(numTourists="관광객수") %>% #rename the variable
  select(longitude, latitude, numTourists, X, Y, coords.x1, coords.x2) #reorder the columns

#function to create a distance matrix
geo.dist = function(df) {
  require(geosphere)
  d <- function(i,z){         
    dist <- rep(0,nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),1:2],z[i,1:2]) # z[,1:2] contains longitude, latitude
    return(dist)
  }
  dm <- do.call(cbind,lapply(1:nrow(df),d,df))
  return(as.dist(dm))
}

#agglomerative hierarchical clustering with complete linkage
hc <- hclust(geo.dist(df2), method = "complete") 
#plotting a dendrogram
plot(hc, cex=0.7)
# k = number of clusters
df2$clust <- cutree(hc,k=60)

df2 <- df2 %>% 
  mutate(id = row_number())

write_csv(df2, "roaming_cluster.csv")

#library(ggformula)
#gf_point(latitude ~ longitude, data = df2)
