# how many lakes in John Clark's entire shp have chl a samples?

# how many samples per year? per month?

setwd("O:/PRIV/NERL_ORD_CYAN/Salls_working/Economics")

sta <- read.csv("stations_WQDP_CHLa_09_27_17.csv", stringsAsFactors = FALSE)
result <- read.csv("result_chla_physical_chemical_09_27_17.csv", stringsAsFactors = FALSE)

result_sta <- merge(sta, result, 
                    by = "MonitoringLocationIdentifier", 
                    all.x = FALSE, 
                    all.y = TRUE)

# load lakes
library(rgdal)
lakes <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/geospatial_general/resolvableLakes/NHD_NLA_shoredist", 
                 "nhd_nla_subset_shore_dist")

library(rgeos)
lakes_buff <- gBuffer(lakes, width = 100, byid = TRUE)

#writeOGR(lakes_buff, "geospatial", "lakes_buff100", driver = "ESRI Shapefile")

# make a pointfile using the lat/long from the sample locations
coords <- coordinates(data.frame(long=result_sta$LongitudeMeasure, lat=result_sta$LatitudeMeasure))
samp_pts <- SpatialPoints(coords, proj4string=CRS("+init=epsg:4326"))
samp_ptsdf <- SpatialPointsDataFrame(samp_pts, data = result_sta)
#writeOGR(samp_ptsdf, "geospatial", "wqp_pts", driver =  "ESRI Shapefile")

# transform lakes to WGS
lakes_buff_wgs <- spTransform(lakes_buff, proj4string(samp_pts))


# query lake attributes by point
samp_pts_lakes <- over(samp_pts, lakes_buff_wgs)
samp_pts_data_lakes <- samp_pts_lakes
samp_pts_data_lakes <- cbind(samp_ptsdf@data, samp_pts_data_lakes) # add sample data back to df from pt file that now just has lake data

# take only points that fall in lakes
samp_pts_inlakes <- samp_pts_data_lakes[which(!is.na(samp_pts_data_lakes$COMID)), ]

sum(!is.na(samp_pts_lakes$COMID)) # n points

length(unique(samp_pts_inlakes$COMID)) # n lakes

write.csv(samp_pts_inlakes, "result_station_inLakes_100mBuff.csv")


# date counts
samp_pts_inlakes$year <- substr(samp_pts_inlakes$ActivityStartDate, 1, 4)
samp_pts_inlakes$month <- substr(samp_pts_inlakes$ActivityStartDate, 6, 7)

table(samp_pts_inlakes$year)
table(samp_pts_inlakes$month)

barplot(table(samp_pts_inlakes$year), xlab = "year", ylab = "frequency")
barplot(table(samp_pts_inlakes$month), xlab = "month", ylab = "frequency")


## filter lakes
length(unique(samp_pts_inlakes$COMID))

# GL
samp_pts_inlakes <- dplyr::filter(samp_pts_inlakes, !grepl("EPA_GLNPO", MonitoringLocationIdentifier))
length(unique(samp_pts_inlakes$COMID))

# neg chla
samp_pts_inlakes <- samp_pts_inlakes %>%  filter(ResultMeasureValue >= 0)
length(unique(samp_pts_inlakes$COMID))

# surface water
samp_pts_inlakes <- filter(samp_pts_inlakes, ActivityMediaSubdivisionName %in% "Surface Water")
length(unique(samp_pts_inlakes$COMID))


## other ways of counting
samps <- read.csv("O:/PRIV/NERL_ORD_CYAN/Salls_working/Economics/result_station_inLakes_100mBuff.csv")

length(unique(samps$COMID))

samps_surf <- samps[samps$ActivityMediaSubdivisionName == "Surface Water", ]
length(unique(samps_surf$COMID))

samps_subset <- samps[which(samps$ActivityMediaSubdivisionName %in% c("Surface Water", "Estuary")), ]
length(unique(samps_subset$COMID))



