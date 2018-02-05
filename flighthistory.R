.libPaths("~/R/rlib")

library(doSNOW)

library(MUCflights)
library(ggmap)
library (png)
#library(animation)
library(dplyr)
library(geosphere)
library(data.table)
#library(marmap)
#library(mapmate)
#library(ggrepel)

get_paths <- function(x, idx, ...) {
  gcInt <- function(x, x1, x2) {
    x <- gcIntermediate(x[x1, ], x[x2, ], ...)
    if (is.list(x)) {
      x <- x %>% purrr::map2(c(x1, x1 + 0.5), ~data.frame(.x, .y)) %>% 
        bind_rows %>% setnames(c("long", "lat", "group"))
    } else x <- data.frame(x, x1) %>% setnames(c("long", "lat", "group"))
    x
  }
  purrr::map(setdiff(1:length(x), idx), ~gcInt(x, .x, idx)) %>% bind_rows
}


# seperate data into long format
sepFromTo <- function(x){
  result <- data.frame()
  result <- data.frame(Date= rep(x[1], 2), Airline =  rep(x[2], 2),
                       IATA = c(x[4], x[5]) )
  result
}


#setwd("/Users/minchunzhou/Box Sync/My_project/flight_history")

setwd("/scratch/kangh1/minchun_other/map")
data <- read.csv("allflight.csv")
data$From <- as.character(data$From)
data$To <- as.character(data$To)

# get 
data_longlat <- apply(data, 1,  sepFromTo)
data_longlat <- do.call(rbind,data_longlat)
row.names(data_longlat) <- NULL

# remove duplicate data
data_longlat <- unique(data_longlat)
data_longlat$nextone <- lag(data_longlat$IATA) == data_longlat$IATA
data_longlat$nextone[1] <- FALSE
data_longlat<- data_longlat[ data_longlat$nextone == FALSE  , ]

data_longlat$id <- 1:nrow(data_longlat)

# get all airport information
data(airports)

# find lat and long for airports

sublat <- airports[ airports$IATA %in% unique(data_longlat$IATA )   , 
                    c("IATA", "Latitude" , "Longitude", "City") ]

data_longlat <- merge(data_longlat, sublat, by="IATA")
data_longlat <- data_longlat[order(data_longlat$Date),]
data_longlat <- data_longlat[ order(data_longlat$id),]

allpath <- data.frame()
for ( i in 2: nrow(data_longlat) ){
  
  test <- data_longlat[  (1:2)+i-2  ,c("Longitude", "Latitude",  "Date", "City")]
  colnames(test)[1:2] <- c("lon", "lat")
  p <- SpatialPoints(cbind(test$lon, test$lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
  idx1 <- 2  # great circles from coords in all other rows to coords in this row
  paths1 <- get_paths(p, idx1, addStartEnd = TRUE)
  paths1$Date <- test$Date[1]
  paths1$City <- paste0(test$City[1]," - ",test$City[2])
  paths1$start <- test$City[1]
  paths1$end <- test$City[2]
  paths1$group <- i-1
  paths1$truedis <- rep(distm(p[1,], p[2,], fun = distHaversine), 52)/1000
  allpath <- rbind(allpath, paths1)
  
}


allpath$distance <- allpath$truedis / max(allpath$truedis)

suball <- allpath[ seq(1,nrow(allpath), by=52) ,]
alldist <- c(0, cumsum(suball$truedis))


p0 <- readRDS("p0.rds")

setwd("images/")

#allpath_list <- split(allpath, seq(nrow(allpath)))

allpath_new <- data.frame()
for (i in 1:(nrow(data_longlat) -1)){
  temp_old <- allpath[allpath$group == i ,]
  mydist <- temp_old$distance[1]
  
  allunif <- sort(c(1,52,  sample( 2:51, round(50*mydist)  )))
  temp <- temp_old[allunif,]
  allpath_new <- rbind(allpath_new, temp)
}

allpath_new$id <- 1:nrow(allpath_new)

NumberOfCluster <- 12
cl <- makeCluster(NumberOfCluster) # Make clusters

registerDoSNOW(cl) # use the above cluster
foreach (i=1: (last(allpath_new$group) -1) ) %dopar% {

  library(MUCflights)
  library(ggmap)
  library (png)
  #library(animation)
  library(dplyr)
  library(geosphere)
  library(data.table)
  
  #for (i in 1:10) {
  setwd("/Users/minchunzhou/Box Sync/My_project/flight_history/accre/images/")
  
  i <- 1
    temp <- temp_old <- allpath_new[allpath_new$group == i ,]
    mydist <- temp_old$truedis[1]
    
    
    mycenter <- c( 2.359444 , 48.72528-10 )
    
    mydate <- temp$Date[1]
    mycity <- temp$City[1]
    
   # p0 <- ggmap(get_googlemap(center = mycenter, zoom=1, maptype = 'satellite', 
    #                          size = c(640,320)), extent='device') 
    p1 <- p0 + 
      geom_text(x=mycenter[1] , y=mycenter[2] -80 ,
                label=paste0(mydate, "\n", mycity, "\n", round(mydist,2), " KM"), size=3, col="white") + 
      geom_point(x= temp_old$long[1], y= temp_old$lat[1], shape=21, fill="yellow", size=2)  +
      geom_point(x= last(temp_old$long), y= last(temp_old$lat), shape=21, fill="yellow", size=2)  
    

    ind_long <-  ifelse(temp_old$long[1] < last(temp_old$long), 1, -1)
    ind_lat <-  ifelse(temp_old$lat[1] < last(temp_old$lat), 1,-1)
    
      p1 <- p1 + 
      geom_text(x= temp_old$long[1] - 5*ind_long , y= temp_old$lat[1]-5*ind_lat  ,
                label= temp_old$start[1] , size=3, col="white") + 
      geom_text(x= last(temp_old$long) + 5*ind_long , y= last(temp_old$lat) + 5*ind_lat ,
                label= temp_old$end[1] , size=3, col="white") 
      
      p1 <- p1 + geom_segment(x = temp_old$long[1], y = temp_old$lat[1], xend = last(temp_old$long),
                              yend = last(temp_old$lat), col='red', alpha=0.5)
      
      
    
    for (j in 1:nrow(temp)){
      x <- temp[j , ]
      
      if ( j >= 2) {
        
        xp <- temp[j-1 , ]
        df <- data.frame(x1 = x$long, x2 = xp$long[1], y1 = x$lat, y2 = xp$lat[1])
        
        if ( abs(x$long-xp$long[1]) < 300 ){
          
          p1 <- p1 + 
            geom_segment(x = x$long, y = x$lat, xend = xp$long[1], yend = xp$lat[1], col='red', alpha=0.5)
        }
      }
      
      p2 <- p1 + geom_point(data = x,aes(x = long, y = lat),colour = 'red', alpha=0.7) +     
        geom_text(x= 160 , y= 80 , 
                  label= paste0("Total Distance: ", round(alldist[i] + (j-1) * mydist/nrow(temp),2), " KM" ) , size=3, col="white")  
      png(sprintf(paste0( "myflight_%05d.png"), x$id), width = 640,
          height = 320, res = 100, bg="black")
      print(p2)
      dev.off()
      
    }
  #}

}
stopCluster(cl) # close clusters




setwd("/Users/minchunzhou/Box Sync/My_project/flight_history/accre/images/")

p <- "myflight_%05d.png"
out <- "flight_50.mp4"
mapmate::ffmpeg(pattern = p, output = out , rate = 50, overwrite = TRUE)


setwd("/Users/minchunzhou/Box Sync/My_project/flight_history/accre/clear_img/")

p <- "myflight_%05d.png"
out <- "flight_50.mp4"
mapmate::ffmpeg(pattern = p, output = out , rate = 50, overwrite = TRUE )




setwd("/Users/minchunzhou/Box Sync/My_project/map/frames/maptiles")

p <- "maptiles_%03d.png"
out <- "tile_10.gif"
mapmate::ffmpeg(pattern = p, output = out , rate = 10, overwrite = TRUE )
