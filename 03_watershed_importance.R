# Created by Jade D. 
# 2020AUG

##### Packages #####
library(sf)
library(maptools)
library(foreign)
library(raster)
library(rgdal)
library(dplyr)
library(exactextractr) # quick zonal stat
library(ggplot2)
library(gridExtra)
library(cowplot)
library(rgeos)
library(sp)
library(tidyverse)

##### A/ S1_CLIM0 - WATERSHED IMPORTANCE #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

#### 1/ Convert coral change mask to dataframe ##### 
# import the coral reef impact area aoi - s1
s1_marine_es <- raster("01_coral_reef_key/coral_footprint_tif/cor_s1_clim0_delta_footprint_wqm_na.tif")

# convert to coral footprint to point data
plot(s1_marine_es)
marine_es_sp <- rasterToPoints(s1_marine_es)
str(marine_es_sp)
# create a "crid" key to link coral reefs to watersheds
marine_es_df <-as.data.frame(marine_es_sp, row.names = T)
head(marine_es_df)
row.names(marine_es_df)
marine_es_df$crid = row.names(marine_es_df)
head(marine_es_df)
crid_mar <- marine_es_df[,c(4,1:2)]
head(crid_mar)
colnames(crid_mar)[1] <- "crid"
head(crid_mar)
crid_mar$crid <- as.integer(crid_mar$crid)
write.table(crid_mar, file.path("01_coral_reef_key", paste("crid_s1_clim0_cor.csv")), row.names=FALSE, col.names=TRUE, sep=",")

#### 2/ BZ - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s1_clim0"

# Create directory
dir.create(file.path("02_plume_delta_bz/s1_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_bz <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0"), pattern='.tif$')
plume_list_bz
wsid <- substr(plume_list_bz, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0", plume_list_bz[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s1_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s1_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("02_plume_delta_bz/s1_clim0", paste(plume_list_bz[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_bz_clip <- list.files(file.path("02_plume_delta_bz/s1_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_bz_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s1_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("02_plume_delta_bz/s1_clim0", plume_list_bz_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_bz <- crid_mar
# Join the clipped plume values to the coral reef aoi
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("02_plume_delta_bz/s1_clim0", plume_list_bz[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
summary(plum_delta_df_all)
head(r2r_key_bz)
r2r_key_bz_cor <- cbind(r2r_key_bz, plum_delta_df_all)
head(r2r_key_bz_cor)
r2r_key_bz_cor <- r2r_key_bz_cor [,-c(4:6)]
summary(r2r_key_bz_cor)
write.table(r2r_key_bz_cor, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_bz_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 3/ GT - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s1_clim0"

# Create directory
dir.create(file.path("03_plume_delta_gt/s1_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_gt <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/gt/s1_rest_fors_clim0"), pattern='.tif$')
plume_list_gt
wsid <- substr(plume_list_gt, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/gt/s1_rest_fors_clim0", plume_list_gt[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s1_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s1_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("03_plume_delta_gt/s1_clim0", paste(plume_list_gt[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_gt_clip <- list.files(file.path("03_plume_delta_gt/s1_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_gt_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s1_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("03_plume_delta_gt/s1_clim0", plume_list_gt_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_gt <- crid_mar
# Join the clipped plume values to the coral reef aoi
setwd("E:/Smart_Coast_project/5_watershed_importance/")
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("03_plume_delta_gt/s1_clim0", plume_list_gt_clip[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
summary(plum_delta_df_all)
head(r2r_key_gt)
r2r_key_gt_cor <- cbind(r2r_key_gt, plum_delta_df_all)
head(r2r_key_gt_cor)
r2r_key_gt_cor <- r2r_key_gt_cor [,-c(4:6)]
summary(r2r_key_gt_cor)
write.table(r2r_key_gt_cor, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_gt_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 4/ HN - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s1_clim0"

# Create directory
dir.create(file.path("04_plume_delta_hn/s1_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_hn <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/hn/s1_rest_fors_clim0"), pattern='.tif$')
plume_list_hn
wsid <- substr(plume_list_hn, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/hn/s1_rest_fors_clim0", plume_list_hn[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s1_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s1_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("04_plume_delta_hn/s1_clim0", paste(plume_list_hn[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_hn_clip <- list.files(file.path("04_plume_delta_hn/s1_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_hn_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s1_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("04_plume_delta_hn/s1_clim0", plume_list_hn_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_hn <- crid_mar
# Join the clipped plume values to the coral reef aoi
setwd("E:/Smart_Coast_project/5_watershed_importance/")
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("04_plume_delta_hn/s1_clim0", plume_list_hn_clip[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
head(r2r_key_hn)
r2r_key_hn <- cbind(r2r_key_hn, plum_delta_df_all)
head(r2r_key_hn)
r2r_key_hn <- r2r_key_hn [,-c(4:6)]
summary(r2r_key_hn)
write.table(r2r_key_hn, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_hn_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 5/ MAR - Combined clipped plumes delta dataframes #####
# import the coral reef area key data frame
r2r_key_bz <- read.csv("05_plume_delta_mar_table/s1_clim0_r2r_key_bz_cor.csv", header = TRUE, sep = ",")
r2r_key_gt <- read.csv("05_plume_delta_mar_table/s1_clim0_r2r_key_gt_cor.csv", header = TRUE, sep = ",")
r2r_key_hn <- read.csv("05_plume_delta_mar_table/s1_clim0_r2r_key_hn_cor.csv", header = TRUE, sep = ",")

# combine the 3 countries data frames into 1 dataframe to ases transbounadry impacts
r2r_key_mar <- cbind(r2r_key_hn, r2r_key_gt, r2r_key_bz)
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))
names(r2r_key_mar)
r2r_key_mar <- r2r_key_mar [,-c(79:81,98:100)] # Remove all the columns duplicates
names(r2r_key_mar)
summary(r2r_key_mar)
write.table(r2r_key_mar, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_mar_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

# clear working environment
rm(list = ls(all.names=TRUE))

##### 6/ Identify the top 3 watersheds driving the coral habitat change at each grid cell (key = wsid) #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s1_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# create a working directory
dir.create(file.path("06_wsid_crid_link/table"), showWarning=TRUE, recursive=TRUE)

# create a working directory
dir.create(file.path("06_wsid_crid_link/raster"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s1_clim0"

# import the mar table
r2r_key_mar <- read.csv("05_plume_delta_mar_table/s1_clim0_r2r_key_mar_cor.csv", header = TRUE, sep = ",")
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))

# create a copy
wq_2_cr_s1_clim0 <- r2r_key_mar

# create 3 data frame for each priority watershed
wsid_top_1 <- as.data.frame(1)
wsid_top_2 <- as.data.frame(1)
wsid_top_3 <- as.data.frame(1)

# Create a list of wsid
crid <- as.integer(r2r_key_mar$crid)
# Create a data frame to store top watersheds by crid
cr_2_ws_s1_clim0 <- data.frame(crid=rep(NA,nrow(r2r_key_mar)),
                               p1_wsid=rep(NA,nrow(wsid_top_1)),
                               p2_wsid=rep(NA,nrow(wsid_top_2)),
                               p3_wsid=rep(NA,nrow(wsid_top_3)))

# Start the clock!
ptm <- proc.time()
# identify the top 3 watersheds driving the reef pixel
for(i in 1:length(crid)) {
  cr_2_ws <- wq_2_cr_s1_clim0 %>%
    slice(i:i) # extract one row from the dataframe
  df <- as.data.frame(apply(cr_2_ws[-c(1:3)], 1, function(x) names(sort(x)))) # sort in descending way the dataframe and return the watershed name
  wsid_top_1 <- df %>%
    slice(1:1) # extract row 1 from the dataframe
  wsid_top_2 <- df %>%
    slice(2:2) # extract row 2 from the dataframe
  wsid_top_3 <- df %>%
    slice(3:3) # extract row 3 from the dataframe
  cr_2_ws_s1_clim0[i,1] <- crid[i]	  # crid
  cr_2_ws_s1_clim0[i,2] <- wsid_top_1[1,1]  # priority watershed 1
  cr_2_ws_s1_clim0[i,3] <- wsid_top_2[1,1]  # priority watershed 2
  cr_2_ws_s1_clim0[i,4] <- wsid_top_3[1,1] 	# priority watershed 3
}
# Stop the clock
proc.time() - ptm

# check the structure
head(cr_2_ws_s1_clim0)

# create single file per top watershed 1 (x, y, px_wsid)
top_1_ws_2_cr <- cr_2_ws_s1_clim0[,-c(3:4)]
head(top_1_ws_2_cr)
top_1_ws_2_cr <- cbind(crid_mar, top_1_ws_2_cr, by="crid")
head(top_1_ws_2_cr)
top_1_ws_2_cr <- top_1_ws_2_cr[,-c(4,6)]
top_1_ws_2_cr$p1_wsid <- as.numeric(top_1_ws_2_cr$p1_wsid)
head(top_1_ws_2_cr)
summary(top_1_ws_2_cr)
# Write the csv file
write.csv(top_1_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_1.csv" ,sep="_")), row.names = F)

# create single file per top watershed 2 (x, y, px_wsid)
top_2_ws_2_cr <- cr_2_ws_s1_clim0[,-c(2,4)]
head(top_2_ws_2_cr)
top_2_ws_2_cr <- cbind(crid_mar, top_2_ws_2_cr, by="crid")
head(top_2_ws_2_cr)
top_2_ws_2_cr <- top_2_ws_2_cr[,-c(4,6)]
top_2_ws_2_cr$p2_wsid <- as.numeric(top_2_ws_2_cr$p2_wsid)
head(top_2_ws_2_cr)
summary(top_2_ws_2_cr)
# Write the csv file
write.csv(top_2_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_2.csv" ,sep="_")), row.names = F)

# create single file per top watershed 3 (x, y, px_wsid)
top_3_ws_2_cr <- cr_2_ws_s1_clim0[,-c(2:3)]
head(top_3_ws_2_cr)
top_3_ws_2_cr <- cbind(crid_mar, top_3_ws_2_cr, by="crid")
head(top_3_ws_2_cr)
top_3_ws_2_cr <- top_3_ws_2_cr[,-c(4,6)]
top_3_ws_2_cr$p3_wsid <- as.numeric(top_3_ws_2_cr$p3_wsid)
head(top_3_ws_2_cr)
summary(top_3_ws_2_cr)
# Write the csv file
write.csv(top_3_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_3.csv" ,sep="_")), row.names = F)


##### 7/ Convert the dataframe with the linked crid to wsid into rasters  #####
# set the scenario
scenario <- "s1_clim0"

## Watershed # 1
# import priority wsid by crid table
top_1_ws_2_cr <- read.csv("06_wsid_crid_link/table/s1_clim0_top_1.csv", header = TRUE, sep = ",")
# remove the crid column
top_1_ws_2_cr_grid <- top_1_ws_2_cr[,-c(1)]
top_1_ws_2_cr_grid$p1_wsid <- as.numeric(top_1_ws_2_cr_grid$p1_wsid)
# set coordinates
coordinates(top_1_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_1_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_1_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_1", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)

## Watershed # 2
# import priority wsid by crid table
top_2_ws_2_cr <- read.csv("06_wsid_crid_link/table/s1_clim0_top_2.csv", header = TRUE, sep = ",")
# remove the crid column
top_2_ws_2_cr_grid <- top_2_ws_2_cr[,-c(1)]
top_2_ws_2_cr_grid$p2_wsid <- as.numeric(top_2_ws_2_cr_grid$p2_wsid)
# set coordinates
coordinates(top_2_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_2_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_2_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs" 
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_2", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)

## Watershed # 3
# import priority wsid by crid table
top_3_ws_2_cr <- read.csv("06_wsid_crid_link/table/s1_clim0_top_3.csv", header = TRUE, sep = ",")
# remove the crid column
top_3_ws_2_cr_grid <- top_3_ws_2_cr[,-c(1)]
top_3_ws_2_cr_grid$p3_wsid <- as.numeric(top_3_ws_2_cr_grid$p3_wsid)
# set coordinates
coordinates(top_3_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_3_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_3_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs" 
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_3", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

##### 8/ Convert the marine rasters identifying the top 3 watersheds in a polygon (zones)  #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# create a working directory
dir.create(file.path("06_wsid_crid_link/polyg/dissolve"), showWarning=TRUE, recursive=TRUE)

# convert raster to polygon, dissolved neighboring same values
# watershed P1
# import rasters linking crid to wsid
r_s1_clim0_top_1 <- raster("06_wsid_crid_link/raster/s1_clim0_top_1.tif")
s1_clim0_top_1 <- rasterToPolygons(r_s1_clim0_top_1, na.rm=TRUE, dissolve = T)
# rename atribute
names(s1_clim0_top_1@data) <- "wsid"
plot(s1_clim0_top_1)
names(s1_clim0_top_1)
# export shp
writeOGR(obj=s1_clim0_top_1, dsn="06_wsid_crid_link/polyg/dissolve", layer="s1_clim0_top_1", driver="ESRI Shapefile", overwrite_layer=TRUE)

# watershed P2
# import rasters linking crid to wsid
r_s1_clim0_top_2 <- raster("06_wsid_crid_link/raster/s1_clim0_top_2.tif")
s1_clim0_top_2 <- rasterToPolygons(r_s1_clim0_top_2, na.rm=TRUE, dissolve = T)
names(s1_clim0_top_2@data) <- "wsid"
plot(s1_clim0_top_2)
names(s1_clim0_top_2)
# export shp
writeOGR(obj=s1_clim0_top_2, dsn="06_wsid_crid_link/polyg/dissolve", layer="s1_clim0_top_2", driver="ESRI Shapefile", overwrite_layer=TRUE)

# watershed P3
# import rasters linking crid to wsid
r_s1_clim0_top_3 <- raster("06_wsid_crid_link/raster/s1_clim0_top_3.tif")
s1_clim0_top_3 <- rasterToPolygons(r_s1_clim0_top_3, na.rm=TRUE, dissolve = T)
names(s1_clim0_top_3@data) <- "wsid"
plot(s1_clim0_top_3)
names(s1_clim0_top_3)
# export shp
writeOGR(obj=s1_clim0_top_3, dsn="06_wsid_crid_link/polyg/dissolve", layer="s1_clim0_top_3", driver="ESRI Shapefile", overwrite_layer=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

# in ArcGIS project the shapefiles in NAD83
# in ArcGIS manually edit to remove the polygons where the change in WQ is 0

##### 9/ Calculate the sediment loads & fractions contributed by the top 3 watersheds #####
setwd("E:/Smart_Coast_project/5_watershed_importance")
dir.create(file.path("09_wsid_prioritization/1_sed_frac_by_crid"), showWarning=TRUE, recursive=TRUE)

# import the mar table
r2r_key_mar <- read.csv("05_plume_delta_mar_table/s1_clim0_r2r_key_mar_cor.csv", header = TRUE, sep = ",")
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))
names(r2r_key_mar)

# calculate the total sediment discharge per pixel
sum <- as.data.frame(rowSums(r2r_key_mar[,4:163], na.rm = T))
colnames(sum)[1] <- "sum"
# find the min value for each row
sed_1 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(x)[1]))) 
colnames(sed_1)[1] <- "sed_1"
# find the second min value for each row
sed_2 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(x)[2]))) 
colnames(sed_2)[1] <- "sed_2"
# find the third min value for each row
sed_3 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(x)[3]))) 
colnames(sed_3)[1] <- "sed_3"
# combine all
sed_load <- cbind(sum,sed_1,sed_2, sed_3)

# Combine the watershed names and sediment fractions
# import priority wsid by crid table
top_1_ws_2_cr <- read.csv("06_wsid_crid_link/table/s1_clim0_top_1.csv", header = TRUE, sep = ",")
top_2_ws_2_cr <- read.csv("06_wsid_crid_link/table/s1_clim0_top_2.csv", header = TRUE, sep = ",")
top_3_ws_2_cr <- read.csv("06_wsid_crid_link/table/s1_clim0_top_3.csv", header = TRUE, sep = ",")

# add the wsid names
sed_load <- cbind(top_1_ws_2_cr, sed_load[2], 
                  top_2_ws_2_cr[4], sed_load[3], 
                  top_3_ws_2_cr[4], sed_load[4],sed_load[1])
# Write the csv file
dir.create(file.path("09_wsid_prioritization/0_sed_load_by_crid"), showWarning=TRUE, recursive=TRUE)
write.csv(sed_load, file.path("09_wsid_prioritization/0_sed_load_by_crid", paste("s1_clim0_r2r_key_mar.csv")), row.names = F)

# convert the sediment loads into fractions
sed_load$p1_frac_sed <- sed_load$sed_1 / sed_load$sum
sed_load$p2_frac_sed <- sed_load$sed_2 / sed_load$sum
sed_load$p3_frac_sed <- sed_load$sed_3 / sed_load$sum
names(sed_load)

# bind all the wsid, sed loads, and sed fractions
crid_wsid_sed_frac <- cbind(top_1_ws_2_cr, sed_load[5], sed_load[11], 
                            top_2_ws_2_cr[4], sed_load[7], sed_load[12],
                            top_3_ws_2_cr[4], sed_load[9],sed_load[13],sed_load[10])
crid_wsid_sed_frac$tot_frac <- crid_wsid_sed_frac$p1_frac_sed+crid_wsid_sed_frac$p2_frac_sed+crid_wsid_sed_frac$p3_frac_sed

# Write the csv file
write.csv(crid_wsid_sed_frac, file.path("09_wsid_prioritization/1_sed_frac_by_crid", paste("s1_frac_sed_by_crid.csv")), row.names = F)

# in Excel delete the WSID 100 & 101 when fraction = 0

# delete the wsid = 100 and 101 when fraction = 0 # codes unfinisshed
names(crid_wsid_sed_frac)
head(crid_wsid_sed_frac)
crid_wsid_sed_frac -> crid_wsid_sed_frac_corrected
crid_wsid_sed_frac$p1_wsid_c <- ifelse(crid_wsid_sed_frac$p1_frac_sed=0 & crid_wsid_sed_frac$p1_wsid= 100, NA, 1)

##### 10/ Calculate the average sediment fraction from coral pixel to the watershed zone #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# create a working directory
dir.create(file.path("09_wsid_prioritization/2_sed_frac_by_wsid"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s1_clim0"

# import the sediment fraction by crid
s1_sed_frac_by_crid <- read.csv("09_wsid_prioritization/1_sed_frac_by_crid/s1_frac_sed_by_crid.csv", header = TRUE, sep = ",")

# priority wsid # 1
s1_frac_sed_by_wsid_1 <- aggregate(s1_sed_frac_by_crid$p1_frac_sed ~ s1_sed_frac_by_crid$p1_wsid, s1_sed_frac_by_crid, mean )
names(s1_frac_sed_by_wsid_1)[1] <- "wsid"
names(s1_frac_sed_by_wsid_1)[2] <- "frac_sed"
summary(s1_frac_sed_by_wsid_1)
# Write the csv file
write.csv(s1_frac_sed_by_wsid_1, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_1.csv" ,sep="_")), row.names = F)

# priority wsid # 2
s1_frac_sed_by_wsid_2 <- aggregate(s1_sed_frac_by_crid$p2_frac_sed ~ s1_sed_frac_by_crid$p2_wsid, s1_sed_frac_by_crid, mean )
names(s1_frac_sed_by_wsid_2)[1] <- "wsid"
names(s1_frac_sed_by_wsid_2)[2] <- "frac_sed"
summary(s1_frac_sed_by_wsid_2)
# Write the csv file
write.csv(s1_frac_sed_by_wsid_2, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_2.csv" ,sep="_")), row.names = F)

# priority wsid # 3
s1_frac_sed_by_wsid_3 <- aggregate(s1_sed_frac_by_crid$p3_frac_sed ~ s1_sed_frac_by_crid$p3_wsid, s1_sed_frac_by_crid, mean )
names(s1_frac_sed_by_wsid_3)[1] <- "wsid"
names(s1_frac_sed_by_wsid_3)[2] <- "frac_sed"
summary(s1_frac_sed_by_wsid_3)
# Write the csv file
write.csv(s1_frac_sed_by_wsid_3, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_3.csv" ,sep="_")), row.names = F)


##### 11/ Calculate the change in marine ES  by watershed linked to reefs #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# set the scenario
scenario <- "s1_clim0"

# import marine ES change outputs
# targeted fish biomass change
s1_targ_clim0 <- raster("07_marine_es_outputs/02_targ_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s1_targ_clim0.tif")
# recreation opportunity change
s1_rec_clim0 <- raster("07_marine_es_outputs/03_rec_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s1_rec_clim0.tif")
# coastal protection change
s1_cv_clim0 <- raster("07_marine_es_outputs/04_cv_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s1_cv_clim0.tif")

# Count of ES changing by crid
mes_count <- raster("07_marine_es_outputs/05_mes_delta_masks/1_raster_sum/s1_marine_es_sum.tif")

# function to rescale cell values between 0 and 1
raster_scale = function(r){
  # get the min max values
  minmax_r = range(values(r), na.rm=TRUE) 
  # rescale 
  return((r-minmax_r[1])/(diff(minmax_r)))}

# function to rescale between 0-1 
s1_targ_clim0_rs <- raster_scale(s1_targ_clim0)
writeRaster(s1_targ_clim0_rs, "08_marine_es_accouting/targ/mar_r2r_s1_targ_clim0_rs.tif", overwrite=TRUE)
s1_rec_clim0_rs <- raster_scale(s1_rec_clim0)
writeRaster(s1_rec_clim0_rs, "08_marine_es_accouting/rec/mar_r2r_s1_rec_clim0_rs.tif", overwrite=TRUE)
s1_cv_clim0_rs <- raster_scale(s1_cv_clim0)
writeRaster(s1_cv_clim0_rs, "08_marine_es_accouting/cv/mar_r2r_s1_cv_clim0_rs.tif", overwrite=TRUE)

# import marine ES change RESCALED outputs
# targeted fish biomass change
s1_targ_clim0_rs <- raster("08_marine_es_accouting/targ/mar_r2r_s1_targ_clim0_rs.tif")
# recreation opportunity change
s1_rec_clim0_rs <- raster("08_marine_es_accouting/rec/mar_r2r_s1_rec_clim0_rs.tif")
# coastal protection change
s1_cv_clim0_rs <- raster("08_marine_es_accouting/cv/mar_r2r_s1_cv_clim0_rs.tif")

# combine the 3 services weighted by change in ES
es_index <- (s1_targ_clim0_rs+s1_rec_clim0_rs+s1_cv_clim0_rs)*mes_count
writeRaster(es_index, filename=file.path("08_marine_es_accouting/mes_weighted_sum", paste(scenario, "mes_index", sep="_")), format="GTiff", overwrite=TRUE)

# import crid <-> wsid zones
wsid_marine_zone_1 <- st_read("06_wsid_crid_link/polyg/clean/s1_clim0_top_1.shp")
# run zonal stat - watershed # 1
# targeted fish biomass change
wsid_marine_zone_1$targ <- exact_extract(s1_targ_clim0, wsid_marine_zone_1, 'sum')
wsid_marine_zone_1$targ <- as.integer(wsid_marine_zone_1$targ) 
# recreation opportunity change
wsid_marine_zone_1$rec <- exact_extract(s1_rec_clim0, wsid_marine_zone_1, 'sum')
# coastal protection change
wsid_marine_zone_1$cv <- exact_extract(s1_cv_clim0, wsid_marine_zone_1, 'sum')
# cumulative change weighted
wsid_marine_zone_1$es_index <- exact_extract(es_index, wsid_marine_zone_1, 'sum')
# ES count change
wsid_marine_zone_1$es_count <- exact_extract(mes_count, wsid_marine_zone_1, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_1, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s1_clim0_mes_wsid_1", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_1.csv" ,sep="_")), row.names = F)

# run zonal stat - watershed # 2
# import crid <-> wsid zones
wsid_marine_zone_2 <- st_read("06_wsid_crid_link/polyg/clean/s1_clim0_top_2.shp")
# targeted fish biomass change
wsid_marine_zone_2$targ <- exact_extract(s1_targ_clim0, wsid_marine_zone_2, 'sum')
wsid_marine_zone_2$targ <- as.integer(wsid_marine_zone_2$targ) 
# recreation opportunity change
wsid_marine_zone_2$rec <- exact_extract(s1_rec_clim0, wsid_marine_zone_2, 'sum')
# coastal protection change
wsid_marine_zone_2$cv <- exact_extract(s1_cv_clim0, wsid_marine_zone_2, 'sum')
# cumulative change weighted
wsid_marine_zone_2$es_index <- exact_extract(es_index, wsid_marine_zone_2, 'sum')
# ES count change
wsid_marine_zone_2$es_count <- exact_extract(mes_count, wsid_marine_zone_2, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_2, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s1_clim0_mes_wsid_2", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_2.csv" ,sep="_")), row.names = F)

# run zonal stat - watershed # 3
# import crid <-> wsid zones
wsid_marine_zone_3 <- st_read("06_wsid_crid_link/polyg/clean/s1_clim0_top_3.shp")
# targeted fish biomass change
wsid_marine_zone_3$targ <- exact_extract(s1_targ_clim0, wsid_marine_zone_3, 'sum')
wsid_marine_zone_3$targ <- as.integer(wsid_marine_zone_3$targ) 
# recreation opportunity change
wsid_marine_zone_3$rec <- exact_extract(s1_rec_clim0, wsid_marine_zone_3, 'sum')
# coastal protection change
wsid_marine_zone_3$cv <- exact_extract(s1_cv_clim0, wsid_marine_zone_3, 'sum')
# cumulative change weighted
wsid_marine_zone_3$es_index <- exact_extract(es_index, wsid_marine_zone_3, 'sum')
# ES count change
wsid_marine_zone_3$es_count <- exact_extract(mes_count, wsid_marine_zone_3, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_3, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s1_clim0_mes_wsid_3", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_3.csv" ,sep="_")), row.names = F)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 12/ Rescale the change in marine ES  by watershed linked to reefs from 0-100 #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# set the scenario
scenario <- "s1_clim0"

# create a working directory
dir.create(file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale"), showWarning=TRUE, recursive=TRUE)

# function to rescale every column of your data frame between 0 and 100
## for every column of your data frame
rescale <- function(x) (x-min(x))/(max(x) - min(x))*100

# set the scenario
scenario <- "s1_clim0"

# wsid top #1
# import the mes accounting for the top 3 watersheds
s1_clim0_mes_account_top_1 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s1_clim0_mes_wsid_1.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s1_clim0_mes_account_top_1$wsid)
names(wsid)[1] <- "wsid"
# targ
s1_clim0_targ_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_1$targ))
names(s1_clim0_targ_rs)[1] <- "targ_rs"
summary(s1_clim0_targ_rs)
# rec
s1_clim0_rec_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_1$rec))
names(s1_clim0_rec_rs)[1] <- "rec_rs"
summary(s1_clim0_rec_rs)
# cv
s1_clim0_cv_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_1$cv))
names(s1_clim0_cv_rs)[1] <- "cv_rs"
summary(s1_clim0_cv_rs)
# mes index
s1_clim0_mes_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_1$es_index))
names(s1_clim0_mes_rs)[1] <- "mes_rs"
summary(s1_clim0_mes_rs)
# mes count
s1_clim0_mes_count <- as.data.frame(s1_clim0_mes_account_top_1$es_count)
names(s1_clim0_mes_count)[1] <- "mes_count"
# combine
s1_clim0_mes_top_1_rs <- cbind(s1_clim0_mes_account_top_1, s1_clim0_targ_rs, s1_clim0_rec_rs, s1_clim0_cv_rs,s1_clim0_mes_rs,s1_clim0_mes_count)
# Write the csv file
write.csv(s1_clim0_mes_top_1_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_1.csv" ,sep="_")), row.names = F)

# wsid top #2
# import the mes accounting for the top 3 watersheds
s1_clim0_mes_account_top_2 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s1_clim0_mes_wsid_2.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s1_clim0_mes_account_top_2$wsid)
names(wsid)[1] <- "wsid"
# targ
s1_clim0_targ_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_2$targ))
names(s1_clim0_targ_rs)[1] <- "targ_rs"
summary(s1_clim0_targ_rs)
# rec
s1_clim0_rec_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_2$rec))
names(s1_clim0_rec_rs)[1] <- "rec_rs"
summary(s1_clim0_rec_rs)
# cv
s1_clim0_cv_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_2$cv))
names(s1_clim0_cv_rs)[1] <- "cv_rs"
summary(s1_clim0_cv_rs)
# mes index
s1_clim0_mes_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_2$es_index))
names(s1_clim0_mes_rs)[1] <- "mes_rs"
summary(s1_clim0_mes_rs)
# mes count
s1_clim0_mes_count <- as.data.frame(s1_clim0_mes_account_top_2$es_count)
names(s1_clim0_mes_count)[1] <- "mes_count"
# combine
s1_clim0_mes_top_2_rs <- cbind(s1_clim0_mes_account_top_2, s1_clim0_targ_rs, s1_clim0_rec_rs, s1_clim0_cv_rs,s1_clim0_mes_rs,s1_clim0_mes_count)
# Write the csv file
write.csv(s1_clim0_mes_top_2_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_2.csv" ,sep="_")), row.names = F)

# wsid top #3
# import the mes accounting for the top 3 watersheds
s1_clim0_mes_account_top_3 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s1_clim0_mes_wsid_3.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s1_clim0_mes_account_top_3$wsid)
names(wsid)[1] <- "wsid"
# targ
s1_clim0_targ_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_3$targ))
names(s1_clim0_targ_rs)[1] <- "targ_rs"
summary(s1_clim0_targ_rs)
# rec
s1_clim0_rec_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_3$rec))
names(s1_clim0_rec_rs)[1] <- "rec_rs"
summary(s1_clim0_rec_rs)
# cv
s1_clim0_cv_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_3$cv))
names(s1_clim0_cv_rs)[1] <- "cv_rs"
summary(s1_clim0_cv_rs)
# mes index
s1_clim0_mes_rs <- as.data.frame(rescale(s1_clim0_mes_account_top_3$es_index))
names(s1_clim0_mes_rs)[1] <- "mes_rs"
summary(s1_clim0_mes_rs)
# mes count
s1_clim0_mes_count <- as.data.frame(s1_clim0_mes_account_top_3$es_count)
names(s1_clim0_mes_count)[1] <- "mes_count"
# combine
s1_clim0_mes_top_3_rs <- cbind(s1_clim0_mes_account_top_3, s1_clim0_targ_rs, s1_clim0_rec_rs, s1_clim0_cv_rs,s1_clim0_mes_rs,s1_clim0_mes_count)
# Write the csv file
write.csv(s1_clim0_mes_top_3_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_3.csv" ,sep="_")), row.names = F)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 13/ Combine the fraction for sediment with the change in MES by WSID #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# create a working directory
dir.create(file.path("09_wsid_prioritization/5_wsid_weight_raw"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s1_clim0"

# import the sediment fraction csv files 
s1_clim0_frac_sed_1 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s1_clim0_frac_sed_1.csv", header = TRUE, sep = ",")
s1_clim0_frac_sed_2 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s1_clim0_frac_sed_2.csv", header = TRUE, sep = ",")
s1_clim0_frac_sed_3 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s1_clim0_frac_sed_3.csv", header = TRUE, sep = ",")

# add a column for WSID priority (1,2,3)
s1_clim0_frac_sed_1$priority <- "1"
s1_clim0_frac_sed_2$priority <- "2"
s1_clim0_frac_sed_3$priority <- "3"

# import the MES change csv files 
s1_clim0_mes_1 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s1_clim0_mes_1.csv", header = TRUE, sep = ",")
s1_clim0_mes_2 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s1_clim0_mes_2.csv", header = TRUE, sep = ",")
s1_clim0_mes_3 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s1_clim0_mes_3.csv", header = TRUE, sep = ",")

# combine the 3 MES change by averaging the rescaled values 0-100 = MES change index
s1_clim0_mes_1$mes_index <- ((s1_clim0_mes_1$targ + s1_clim0_mes_1$rec + s1_clim0_mes_1$cv)/3)
s1_clim0_mes_2$mes_index <- ((s1_clim0_mes_2$targ + s1_clim0_mes_2$rec + s1_clim0_mes_2$cv)/3)
s1_clim0_mes_3$mes_index <- ((s1_clim0_mes_3$targ + s1_clim0_mes_3$rec + s1_clim0_mes_3$cv)/3)

# join the sediment fraction and change in MES for priority watershed
s1_clim0_wsid_1 <- full_join(s1_clim0_frac_sed_1, s1_clim0_mes_1, by = "wsid")
s1_clim0_wsid_2 <- full_join(s1_clim0_frac_sed_2, s1_clim0_mes_2, by = "wsid")
s1_clim0_wsid_3 <- full_join(s1_clim0_frac_sed_3, s1_clim0_mes_3, by = "wsid")

# Calculate the WSID weight = MES change index x Sediment fraction x mean # of ES change
s1_clim0_wsid_1$s1 <- s1_clim0_wsid_1$mes_index*s1_clim0_wsid_1$frac_sed*s1_clim0_mes_1$mes_count
s1_clim0_wsid_2$s1 <- s1_clim0_wsid_2$mes_index*s1_clim0_wsid_2$frac_sed*s1_clim0_mes_2$mes_count
s1_clim0_wsid_3$s1 <- s1_clim0_wsid_3$mes_index*s1_clim0_wsid_3$frac_sed*s1_clim0_mes_3$mes_count

# Combine the dataframes with sediment fraction & MES change for the 3 priority watersheds by stacking 
s1_clim0_wsid_all <- rbind(s1_clim0_wsid_1, s1_clim0_wsid_2, s1_clim0_wsid_3)
# Write the csv file
write.csv(s1_clim0_wsid_all, file.path("09_wsid_prioritization/5_wsid_weight_raw", paste(scenario, "wsid_weight.csv" ,sep="_")), row.names = F)

# Average the WSID weight by WSID to assign one final WSID weight to each watershed
s1_clim0_wsid <- aggregate(s1_clim0_wsid_all[,14:15], list(s1_clim0_wsid_all$wsid), mean)
names(s1_clim0_wsid)[1] <- "wsid"
# Write the csv file
write.csv(s1_clim0_wsid, file.path("09_wsid_prioritization/5_wsid_weight_raw", paste(scenario, "wsid_weight_mn.csv" ,sep="_")), row.names = F)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 14/ Rescale the WSID weight at the country scale for ROOT (SWM - 0-1) #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

### IT NEEDS TO BE BETWEEN 0 & 1 FOR ROOT!!

# create a working directory
dir.create(file.path("09_wsid_prioritization/6_wsid_weight_rescale"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s1_clim0"

# import the MES change csv files 
s1_clim0_wsid <- read.csv("09_wsid_prioritization/5_wsid_weight_raw/s1_clim0_wsid_weight_mn.csv", header = TRUE, sep = ",")

# convert wsid at character
s1_clim0_wsid$wsid <- as.character(s1_clim0_wsid$wsid)

# function to rescale between 0-1 
rescale <- function(x) (x-min(x))/(max(x) - min(x))

# BZ watershed weights
# set the country
country <- "bz"
# import watershed shapefile
wsid_bz <- st_read("00_aoi_watersheds/bz_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_bz$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s1_clim0_wsid_bz <- inner_join(wsid, s1_clim0_wsid, by ="wsid")
s1_rs <- as.data.frame(rescale(s1_clim0_wsid_bz$s1))
names(s1_rs)[1] <- "s1_rs"
summary(s1_rs)
# join the rescale wsid weight to bz wsid
s1_clim0_wsid_rs <- cbind(s1_clim0_wsid_bz, s1_rs)
# Write the csv file
write.csv(s1_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
bz_wsid_shp <- left_join(wsid_bz, s1_clim0_wsid_rs, 'wsid', 'wsid')
names(bz_wsid_shp)
# export as shp
wsid_sp <- as(bz_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="bz_s1_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# GT watershed weights
# set the country
country <- "gt"
# import watershed shapefile
wsid_gt <- st_read("00_aoi_watersheds/gt_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_gt$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s1_clim0_wsid_gt <- inner_join(wsid, s1_clim0_wsid, by ="wsid")
# rescale wsid weight
s1_rs <- as.data.frame(rescale(s1_clim0_wsid_gt$s1))
names(s1_rs)[1] <- "s1_rs"
summary(s1_rs)
# join the rescale wsid weight to gt wsid
s1_clim0_wsid_rs <- cbind(s1_clim0_wsid_gt, s1_rs)
# Write the csv file
write.csv(s1_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
gt_wsid_shp <- left_join(wsid_gt, s1_clim0_wsid_rs, 'wsid', 'wsid', how = 'left')
names(gt_wsid_shp)
# export as shp
wsid_sp <- as(gt_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="gt_s1_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# HN watershed weights
# set the country
country <- "hn"
# import watershed shapefile
wsid_hn <- st_read("00_aoi_watersheds/hn_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_hn$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s1_clim0_wsid_hn <- inner_join(wsid, s1_clim0_wsid, by ="wsid")
# rescale wsid weight
s1_rs <- as.data.frame(rescale(s1_clim0_wsid_hn$s1))
names(s1_rs)[1] <- "s1_rs"
summary(s1_rs)
# join the rescale wsid weight to hn wsid
s1_clim0_wsid_rs <- cbind(s1_clim0_wsid_hn, s1_rs)
# Write the csv file
write.csv(s1_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
hn_wsid_shp <- left_join(wsid_hn, s1_clim0_wsid_rs, 'wsid', 'wsid', how = 'left')
names(hn_wsid_shp)
# export as shp
wsid_sp <- as(hn_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="hn_s1_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))


##### C/ S2_CLIM0 - WATERSHED IMPORTANCE #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

#### 1/ Convert coral change mask to dataframe ##### 
# import the coral reef impact area aoi - s2
s2_marine_es <- raster("01_coral_reef_key/coral_footprint_tif/cor_s2_clim0_delta_footprint_wqm_na.tif")

# convert to coral footprint to point data
plot(s2_marine_es)
marine_es_sp <- rasterToPoints(s2_marine_es)
str(marine_es_sp)
# create a "crid" key to link coral reefs to watersheds
marine_es_df <-as.data.frame(marine_es_sp, row.names = T)
head(marine_es_df)
row.names(marine_es_df)
marine_es_df$crid = row.names(marine_es_df)
head(marine_es_df)
crid_mar <- marine_es_df[,c(4,1:2)]
head(crid_mar)
colnames(crid_mar)[1] <- "crid"
head(crid_mar)
crid_mar$crid <- as.integer(crid_mar$crid)
write.table(crid_mar, file.path("01_coral_reef_key", paste("crid_s2_clim0_cor.csv")), row.names=FALSE, col.names=TRUE, sep=",")

#### 2/ BZ - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s2_clim0"

# Create directory
dir.create(file.path("02_plume_delta_bz/s2_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_bz <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0"), pattern='.tif$')
plume_list_bz
wsid <- substr(plume_list_bz, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0", plume_list_bz[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s2_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s2_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("02_plume_delta_bz/s2_clim0", paste(plume_list_bz[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_bz_clip <- list.files(file.path("02_plume_delta_bz/s2_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_bz_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s2_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("02_plume_delta_bz/s2_clim0", plume_list_bz_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_bz <- crid_mar
# Join the clipped plume values to the coral reef aoi
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("02_plume_delta_bz/s2_clim0", plume_list_bz[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
summary(plum_delta_df_all)
head(r2r_key_bz)
r2r_key_bz_cor <- cbind(r2r_key_bz, plum_delta_df_all)
head(r2r_key_bz_cor)
r2r_key_bz_cor <- r2r_key_bz_cor [,-c(4:6)]
summary(r2r_key_bz_cor)
write.table(r2r_key_bz_cor, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_bz_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 3/ GT - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s2_clim0"

# Create directory
dir.create(file.path("03_plume_delta_gt/s2_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_gt <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/gt/s2_prot_fors_clim0"), pattern='.tif$')
plume_list_gt
wsid <- substr(plume_list_gt, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/gt/s2_prot_fors_clim0", plume_list_gt[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s2_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s2_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("03_plume_delta_gt/s2_clim0", paste(plume_list_gt[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_gt_clip <- list.files(file.path("03_plume_delta_gt/s2_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_gt_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s2_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("03_plume_delta_gt/s2_clim0", plume_list_gt_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_gt <- crid_mar
# Join the clipped plume values to the coral reef aoi
setwd("E:/Smart_Coast_project/5_watershed_importance/")
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("03_plume_delta_gt/s2_clim0", plume_list_gt_clip[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
summary(plum_delta_df_all)
head(r2r_key_gt)
r2r_key_gt_cor <- cbind(r2r_key_gt, plum_delta_df_all)
head(r2r_key_gt_cor)
r2r_key_gt_cor <- r2r_key_gt_cor [,-c(4:6)]
summary(r2r_key_gt_cor)
write.table(r2r_key_gt_cor, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_gt_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 4/ HN - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s2_clim0"

# Create directory
dir.create(file.path("04_plume_delta_hn/s2_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_hn <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/hn/s2_prot_fors_clim0"), pattern='.tif$')
plume_list_hn
wsid <- substr(plume_list_hn, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/hn/s2_prot_fors_clim0", plume_list_hn[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s2_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s2_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("04_plume_delta_hn/s2_clim0", paste(plume_list_hn[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_hn_clip <- list.files(file.path("04_plume_delta_hn/s2_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_hn_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s2_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("04_plume_delta_hn/s2_clim0", plume_list_hn_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_hn <- crid_mar
# Join the clipped plume values to the coral reef aoi
setwd("E:/Smart_Coast_project/5_watershed_importance/")
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("04_plume_delta_hn/s2_clim0", plume_list_hn_clip[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
head(r2r_key_hn)
r2r_key_hn <- cbind(r2r_key_hn, plum_delta_df_all)
head(r2r_key_hn)
r2r_key_hn <- r2r_key_hn [,-c(4:6)]
summary(r2r_key_hn)
write.table(r2r_key_hn, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_hn_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 5/ MAR - Combined clipped plumes delta dataframes #####
# import the coral reef area key data frame
r2r_key_bz <- read.csv("05_plume_delta_mar_table/s2_clim0_r2r_key_bz_cor.csv", header = TRUE, sep = ",")
r2r_key_gt <- read.csv("05_plume_delta_mar_table/s2_clim0_r2r_key_gt_cor.csv", header = TRUE, sep = ",")
r2r_key_hn <- read.csv("05_plume_delta_mar_table/s2_clim0_r2r_key_hn_cor.csv", header = TRUE, sep = ",")

# combine the 3 countries data frames into 1 dataframe to ases transbounadry impacts
r2r_key_mar <- cbind(r2r_key_hn, r2r_key_gt, r2r_key_bz)
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))
names(r2r_key_mar)
r2r_key_mar <- r2r_key_mar [,-c(79:81,98:100)] # Remove all the columns duplicates
names(r2r_key_mar)
summary(r2r_key_mar)
write.table(r2r_key_mar, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_mar_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

# clear working environment
rm(list = ls(all.names=TRUE))

##### 6/ Identify the top 3 watersheds driving the coral habitat change at each grid cell (key = wsid) #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s2_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# create a working directory
dir.create(file.path("06_wsid_crid_link/table"), showWarning=TRUE, recursive=TRUE)

# create a working directory
dir.create(file.path("06_wsid_crid_link/raster"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s2_clim0"

# import the mar table
r2r_key_mar <- read.csv("05_plume_delta_mar_table/s2_clim0_r2r_key_mar_cor.csv", header = TRUE, sep = ",")
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))

# create a copy
wq_2_cr_s2_clim0 <- r2r_key_mar

# create 3 data frame for each priority watershed
wsid_top_1 <- as.data.frame(1)
wsid_top_2 <- as.data.frame(1)
wsid_top_3 <- as.data.frame(1)

# Create a list of wsid
crid <- as.integer(r2r_key_mar$crid)
# Create a data frame to store top watersheds by crid
cr_2_ws_s2_clim0 <- data.frame(crid=rep(NA,nrow(r2r_key_mar)),
                               p1_wsid=rep(NA,nrow(wsid_top_1)),
                               p2_wsid=rep(NA,nrow(wsid_top_2)),
                               p3_wsid=rep(NA,nrow(wsid_top_3)))

# Start the clock!
ptm <- proc.time()
# identify the top 3 watersheds driving the reef pixel
for(i in 1:length(crid)) {
  cr_2_ws <- wq_2_cr_s2_clim0 %>%
    slice(i:i) # extract one row from the dataframe
  df <- as.data.frame(apply(cr_2_ws[-c(1:3)], 1, function(x) names(sort(-x)))) # sort in descending way the dataframe and return the watershed name
  wsid_top_1 <- df %>%
    slice(1:1) # extract row 1 from the dataframe
  wsid_top_2 <- df %>%
    slice(2:2) # extract row 2 from the dataframe
  wsid_top_3 <- df %>%
    slice(3:3) # extract row 3 from the dataframe
  cr_2_ws_s2_clim0[i,1] <- crid[i]	  # crid
  cr_2_ws_s2_clim0[i,2] <- wsid_top_1[1,1]  # priority watershed 1
  cr_2_ws_s2_clim0[i,3] <- wsid_top_2[1,1]  # priority watershed 2
  cr_2_ws_s2_clim0[i,4] <- wsid_top_3[1,1] 	# priority watershed 3
}
# Stop the clock
proc.time() - ptm

# check the structure
head(cr_2_ws_s2_clim0)

# create single file per top watershed 1 (x, y, px_wsid)
top_1_ws_2_cr <- cr_2_ws_s2_clim0[,-c(3:4)]
head(top_1_ws_2_cr)
top_1_ws_2_cr <- cbind(crid_mar, top_1_ws_2_cr, by="crid")
head(top_1_ws_2_cr)
top_1_ws_2_cr <- top_1_ws_2_cr[,-c(4,6)]
top_1_ws_2_cr$p1_wsid <- as.numeric(top_1_ws_2_cr$p1_wsid)
head(top_1_ws_2_cr)
summary(top_1_ws_2_cr)
# Write the csv file
write.csv(top_1_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_1.csv" ,sep="_")), row.names = F)

# create single file per top watershed 2 (x, y, px_wsid)
top_2_ws_2_cr <- cr_2_ws_s2_clim0[,-c(2,4)]
head(top_2_ws_2_cr)
top_2_ws_2_cr <- cbind(crid_mar, top_2_ws_2_cr, by="crid")
head(top_2_ws_2_cr)
top_2_ws_2_cr <- top_2_ws_2_cr[,-c(4,6)]
top_2_ws_2_cr$p2_wsid <- as.numeric(top_2_ws_2_cr$p2_wsid)
head(top_2_ws_2_cr)
summary(top_2_ws_2_cr)
# Write the csv file
write.csv(top_2_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_2.csv" ,sep="_")), row.names = F)

# create single file per top watershed 3 (x, y, px_wsid)
top_3_ws_2_cr <- cr_2_ws_s2_clim0[,-c(2:3)]
head(top_3_ws_2_cr)
top_3_ws_2_cr <- cbind(crid_mar, top_3_ws_2_cr, by="crid")
head(top_3_ws_2_cr)
top_3_ws_2_cr <- top_3_ws_2_cr[,-c(4,6)]
top_3_ws_2_cr$p3_wsid <- as.numeric(top_3_ws_2_cr$p3_wsid)
head(top_3_ws_2_cr)
summary(top_3_ws_2_cr)
# Write the csv file
write.csv(top_3_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_3.csv" ,sep="_")), row.names = F)


##### 7/ Convert the dataframe with the linked crid to wsid into rasters  #####
# set the scenario
scenario <- "s2_clim0"

## Watershed # 1
# import priority wsid by crid table
top_1_ws_2_cr <- read.csv("06_wsid_crid_link/table/s2_clim0_top_1.csv", header = TRUE, sep = ",")
# remove the crid column
top_1_ws_2_cr_grid <- top_1_ws_2_cr[,-c(1)]
top_1_ws_2_cr_grid$p1_wsid <- as.numeric(top_1_ws_2_cr_grid$p1_wsid)
# set coordinates
coordinates(top_1_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_1_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_1_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_1", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)

## Watershed # 2
# import priority wsid by crid table
top_2_ws_2_cr <- read.csv("06_wsid_crid_link/table/s2_clim0_top_2.csv", header = TRUE, sep = ",")
# remove the crid column
top_2_ws_2_cr_grid <- top_2_ws_2_cr[,-c(1)]
top_2_ws_2_cr_grid$p2_wsid <- as.numeric(top_2_ws_2_cr_grid$p2_wsid)
# set coordinates
coordinates(top_2_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_2_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_2_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs" 
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_2", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)


## Watershed # 3
# import priority wsid by crid table
top_3_ws_2_cr <- read.csv("06_wsid_crid_link/table/s2_clim0_top_3.csv", header = TRUE, sep = ",")
# remove the crid column
top_3_ws_2_cr_grid <- top_3_ws_2_cr[,-c(1)]
top_3_ws_2_cr_grid$p3_wsid <- as.numeric(top_3_ws_2_cr_grid$p3_wsid)
# set coordinates
coordinates(top_3_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_3_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_3_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs" 
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_3", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 8/ Convert the marine rasters identifying the top 3 watersheds in a polygon (zones)  #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# create a working directory
dir.create(file.path("06_wsid_crid_link/polyg/dissolve"), showWarning=TRUE, recursive=TRUE)

# convert raster to polygon, dissolved neighboring same values
# watershed P1
# import rasters linking crid to wsid
r_s2_clim0_top_1 <- raster("06_wsid_crid_link/raster/s2_clim0_top_1.tif")
s2_clim0_top_1 <- rasterToPolygons(r_s2_clim0_top_1, na.rm=TRUE, dissolve = T)
# rename atribute
names(s2_clim0_top_1@data) <- "wsid"
plot(s2_clim0_top_1)
names(s2_clim0_top_1)
# export shp
writeOGR(obj=s2_clim0_top_1, dsn="06_wsid_crid_link/polyg/dissolve", layer="s2_clim0_top_1", driver="ESRI Shapefile", overwrite_layer=TRUE)

# watershed P2
# import rasters linking crid to wsid
r_s2_clim0_top_2 <- raster("06_wsid_crid_link/raster/s2_clim0_top_2.tif")
s2_clim0_top_2 <- rasterToPolygons(r_s2_clim0_top_2, na.rm=TRUE, dissolve = T)
names(s2_clim0_top_2@data) <- "wsid"
plot(s2_clim0_top_2)
names(s2_clim0_top_2)
# export shp
writeOGR(obj=s2_clim0_top_2, dsn="06_wsid_crid_link/polyg/dissolve", layer="s2_clim0_top_2", driver="ESRI Shapefile", overwrite_layer=TRUE)

# watershed P3
# import rasters linking crid to wsid
r_s2_clim0_top_3 <- raster("06_wsid_crid_link/raster/s2_clim0_top_3.tif")
s2_clim0_top_3 <- rasterToPolygons(r_s2_clim0_top_3, na.rm=TRUE, dissolve = T)
names(s2_clim0_top_3@data) <- "wsid"
plot(s2_clim0_top_3)
names(s2_clim0_top_3)
# export shp
writeOGR(obj=s2_clim0_top_3, dsn="06_wsid_crid_link/polyg/dissolve", layer="s2_clim0_top_3", driver="ESRI Shapefile", overwrite_layer=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

# in ArcGIS project the shapefiles in NAD83
# in ArcGIS manually edit to remove the polygons where the change in WQ is 0

##### 9/ Calculate the sediment loads & fractions contributed by the top 3 watersheds #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# import the mar table
r2r_key_mar <- read.csv("05_plume_delta_mar_table/s2_clim0_r2r_key_mar_cor.csv", header = TRUE, sep = ",")
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))
names(r2r_key_mar)

# calculate the total sediment discharge per pixel
sum <- as.data.frame(rowSums(r2r_key_mar[,4:163], na.rm = T))
colnames(sum)[1] <- "sum"
# find the max value for each row
sed_1 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(-x)[1]*(-1)))) 
colnames(sed_1)[1] <- "sed_1"
# find the second max value for each row
sed_2 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(-x)[2]*(-1)))) 
colnames(sed_2)[1] <- "sed_2"
# find the third max value for each row
sed_3 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(-x)[3]*(-1)))) 
colnames(sed_3)[1] <- "sed_3"
# combine all
sed_load <- cbind(sum,sed_1,sed_2, sed_3)

# Combine the watershed names and sediment fractions
# import priority wsid by crid table
top_1_ws_2_cr <- read.csv("06_wsid_crid_link/table/s2_clim0_top_1.csv", header = TRUE, sep = ",")
top_2_ws_2_cr <- read.csv("06_wsid_crid_link/table/s2_clim0_top_2.csv", header = TRUE, sep = ",")
top_3_ws_2_cr <- read.csv("06_wsid_crid_link/table/s2_clim0_top_3.csv", header = TRUE, sep = ",")

# add the wsid names
sed_load <- cbind(top_1_ws_2_cr, sed_load[2], 
                  top_2_ws_2_cr[4], sed_load[3], 
                  top_3_ws_2_cr[4], sed_load[4],sed_load[1])
# Write the csv file
write.csv(sed_load, file.path("09_wsid_prioritization/0_sed_load_by_crid", paste("s2_clim0_r2r_key_mar.csv")), row.names = F)

# convert the sediment loads into fractions
sed_load$p1_frac_sed <- sed_load$sed_1 / sed_load$sum
sed_load$p2_frac_sed <- sed_load$sed_2 / sed_load$sum
sed_load$p3_frac_sed <- sed_load$sed_3 / sed_load$sum
names(sed_load)

# bind all the wsid, sed loads, and sed fractions
crid_wsid_sed_frac <- cbind(top_1_ws_2_cr, sed_load[5], sed_load[11], 
                            top_2_ws_2_cr[4], sed_load[7], sed_load[12],
                            top_3_ws_2_cr[4], sed_load[9],sed_load[13],sed_load[10])
crid_wsid_sed_frac$tot_frac <- crid_wsid_sed_frac$p1_frac_sed+crid_wsid_sed_frac$p2_frac_sed+crid_wsid_sed_frac$p3_frac_sed

# Write the csv file
write.csv(crid_wsid_sed_frac, file.path("09_wsid_prioritization/1_sed_frac_by_crid", paste("s2_frac_sed_by_crid.csv")), row.names = F)

# in Excel delete the WSID 100 & 101 when fraction = 0

##### 10/ Calculate the average sediment fraction from coral pixel to the watershed zone #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# create a working directory
dir.create(file.path("09_wsid_prioritization/2_sed_frac_by_wsid"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s2_clim0"
# import the sediment fraction by crid
s2_sed_frac_by_crid <- read.csv("09_wsid_prioritization/1_sed_frac_by_crid/s2_frac_sed_by_crid.csv", header = TRUE, sep = ",")

# priority wsid # 1
s2_frac_sed_by_wsid_1 <- aggregate(s2_sed_frac_by_crid$p1_frac_sed ~ s2_sed_frac_by_crid$p1_wsid, s2_sed_frac_by_crid, mean )
names(s2_frac_sed_by_wsid_1)[1] <- "wsid"
names(s2_frac_sed_by_wsid_1)[2] <- "frac_sed"
summary(s2_frac_sed_by_wsid_1)
# Write the csv file
write.csv(s2_frac_sed_by_wsid_1, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_1.csv" ,sep="_")), row.names = F)

# priority wsid # 2
s2_frac_sed_by_wsid_2 <- aggregate(s2_sed_frac_by_crid$p2_frac_sed ~ s2_sed_frac_by_crid$p2_wsid, s2_sed_frac_by_crid, mean )
names(s2_frac_sed_by_wsid_2)[1] <- "wsid"
names(s2_frac_sed_by_wsid_2)[2] <- "frac_sed"
summary(s2_frac_sed_by_wsid_2)
# Write the csv file
write.csv(s2_frac_sed_by_wsid_2, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_2.csv" ,sep="_")), row.names = F)

# priority wsid # 3
s2_frac_sed_by_wsid_3 <- aggregate(s2_sed_frac_by_crid$p3_frac_sed ~ s2_sed_frac_by_crid$p3_wsid, s2_sed_frac_by_crid, mean )
names(s2_frac_sed_by_wsid_3)[1] <- "wsid"
names(s2_frac_sed_by_wsid_3)[2] <- "frac_sed"
summary(s2_frac_sed_by_wsid_3)
# Write the csv file
write.csv(s2_frac_sed_by_wsid_3, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_3.csv" ,sep="_")), row.names = F)

##### 11/ Calculate the change in marine ES  by watershed linked to reefs #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# set the scenario
scenario <- "s2_clim0"

# import marine ES change outputs
# targeted fish biomass change
s2_targ_clim0 <- raster("07_marine_es_outputs/02_targ_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s2_targ_clim0_pos.tif")
# recreation opportunity change
s2_rec_clim0 <- raster("07_marine_es_outputs/03_rec_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s2_rec_clim0_pos.tif")
# coastal protection change
s2_cv_clim0 <- raster("07_marine_es_outputs/04_cv_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s2_cv_clim0.tif")

# Count of ES changing by crid
mes_count <- raster("07_marine_es_outputs/05_mes_delta_masks/1_raster_sum/s2_marine_es_sum.tif")

# function to rescale cell values between 0 and 1
raster_scale = function(r){
  # get the min max values
  minmax_r = range(values(r), na.rm=TRUE) 
  # rescale 
  return((r-minmax_r[1])/(diff(minmax_r)))}

# function to rescale between 0-1 
s2_targ_clim0_rs <- raster_scale(s2_targ_clim0)
writeRaster(s2_targ_clim0_rs, "08_marine_es_accouting/targ/mar_r2r_s2_targ_clim0_rs.tif", overwrite=TRUE)
s2_rec_clim0_rs <- raster_scale(s2_rec_clim0)
writeRaster(s2_rec_clim0_rs, "08_marine_es_accouting/rec/mar_r2r_s2_rec_clim0_rs.tif", overwrite=TRUE)
s2_cv_clim0_rs <- raster_scale(s2_cv_clim0)
writeRaster(s2_cv_clim0_rs, "08_marine_es_accouting/cv/mar_r2r_s2_cv_clim0_rs.tif", overwrite=TRUE)

# import marine ES change RESCALED outputs
# targeted fish biomass change
s2_targ_clim0_rs <- raster("08_marine_es_accouting/targ/mar_r2r_s2_targ_clim0_rs.tif")
# recreation opportunity change
s2_rec_clim0_rs <- raster("08_marine_es_accouting/rec/mar_r2r_s2_rec_clim0_rs.tif")
# coastal protection change
s2_cv_clim0_rs <- raster("08_marine_es_accouting/cv/mar_r2r_s2_cv_clim0_rs.tif")

# combine the 3 services weighted by change in ES
es_index <- (s2_targ_clim0_rs+s2_rec_clim0_rs+s2_cv_clim0_rs)*mes_count
writeRaster(es_index, filename=file.path("08_marine_es_accouting/mes_weighted_sum", paste(scenario, "mes_index", sep="_")), format="GTiff", overwrite=TRUE)

# import crid <-> wsid zones
wsid_marine_zone_1 <- st_read("06_wsid_crid_link/polyg/clean/s2_clim0_top_1.shp")
# run zonal stat - watershed # 1
# targeted fish biomass change
wsid_marine_zone_1$targ <- exact_extract(s2_targ_clim0, wsid_marine_zone_1, 'sum')
wsid_marine_zone_1$targ <- as.integer(wsid_marine_zone_1$targ) 
# recreation opportunity change
wsid_marine_zone_1$rec <- exact_extract(s2_rec_clim0, wsid_marine_zone_1, 'sum')
# coastal protection change
wsid_marine_zone_1$cv <- exact_extract(s2_cv_clim0, wsid_marine_zone_1, 'sum')
# cumulative change weighted
wsid_marine_zone_1$es_index <- exact_extract(es_index, wsid_marine_zone_1, 'sum')
# ES count change
wsid_marine_zone_1$es_count <- exact_extract(mes_count, wsid_marine_zone_1, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_1, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s2_clim0_mes_wsid_1", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_1.csv" ,sep="_")), row.names = F)

# run zonal stat - watershed # 2
# import crid <-> wsid zones
wsid_marine_zone_2 <- st_read("06_wsid_crid_link/polyg/clean/s2_clim0_top_2.shp")
# targeted fish biomass change
wsid_marine_zone_2$targ <- exact_extract(s2_targ_clim0, wsid_marine_zone_2, 'sum')
wsid_marine_zone_2$targ <- as.integer(wsid_marine_zone_2$targ) 
# recreation opportunity change
wsid_marine_zone_2$rec <- exact_extract(s2_rec_clim0, wsid_marine_zone_2, 'sum')
# coastal protection change
wsid_marine_zone_2$cv <- exact_extract(s2_cv_clim0, wsid_marine_zone_2, 'sum')
# cumulative change weighted
wsid_marine_zone_2$es_index <- exact_extract(es_index, wsid_marine_zone_2, 'sum')
# ES count change
wsid_marine_zone_2$es_count <- exact_extract(mes_count, wsid_marine_zone_2, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_2, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s2_clim0_mes_wsid_2", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_2.csv" ,sep="_")), row.names = F)

# run zonal stat - watershed # 3
# import crid <-> wsid zones
wsid_marine_zone_3 <- st_read("06_wsid_crid_link/polyg/clean/s2_clim0_top_3.shp")
# targeted fish biomass change
wsid_marine_zone_3$targ <- exact_extract(s2_targ_clim0, wsid_marine_zone_3, 'sum')
wsid_marine_zone_3$targ <- as.integer(wsid_marine_zone_3$targ) 
# recreation opportunity change
wsid_marine_zone_3$rec <- exact_extract(s2_rec_clim0, wsid_marine_zone_3, 'sum')
# coastal protection change
wsid_marine_zone_3$cv <- exact_extract(s2_cv_clim0, wsid_marine_zone_3, 'sum')
# cumulative change weighted
wsid_marine_zone_3$es_index <- exact_extract(es_index, wsid_marine_zone_3, 'sum')
# ES count change
wsid_marine_zone_3$es_count <- exact_extract(mes_count, wsid_marine_zone_3, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_3, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s2_clim0_mes_wsid_3", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_3.csv" ,sep="_")), row.names = F)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 12/ Rescale the change in marine ES  by watershed linked to reefs from 0-100 #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# create a working directory
dir.create(file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale"), showWarning=TRUE, recursive=TRUE)

# function to rescale every column of your data frame between 0 and 100
## for every column of your data frame
rescale_pos <- function(x) (x-min(x))/(max(x) - min(x))*100
rescale_neg <- function(x) (x-max(x))/(min(x) - max(x))*100

# set the scenario
scenario <- "s2_clim0"

# wsid top #1
# import the mes accounting for the top 3 watersheds
s2_clim0_mes_account_top_1 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s2_clim0_mes_wsid_1.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s2_clim0_mes_account_top_1$wsid)
names(wsid)[1] <- "wsid"
# targ
s2_clim0_targ_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_1$targ))
names(s2_clim0_targ_rs)[1] <- "targ_rs"
summary(s2_clim0_targ_rs)
# rec
s2_clim0_rec_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_1$rec))
names(s2_clim0_rec_rs)[1] <- "rec_rs"
summary(s2_clim0_rec_rs)
# cv
s2_clim0_cv_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_1$cv))
names(s2_clim0_cv_rs)[1] <- "cv_rs"
summary(s2_clim0_cv_rs)
# mes index
s2_clim0_mes_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_1$es_index))
names(s2_clim0_mes_rs)[1] <- "mes_rs"
summary(s2_clim0_mes_rs)
# mes count
s2_clim0_mes_count <- as.data.frame(s2_clim0_mes_account_top_1$es_count)
names(s2_clim0_mes_count)[1] <- "mes_count"
# combine
s2_clim0_mes_top_1_rs <- cbind(s2_clim0_mes_account_top_1, s2_clim0_targ_rs, s2_clim0_rec_rs, s2_clim0_cv_rs,s2_clim0_mes_rs,s2_clim0_mes_count)
# Write the csv file
write.csv(s2_clim0_mes_top_1_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_1.csv" ,sep="_")), row.names = F)

# wsid top #2
# import the mes accounting for the top 3 watersheds
s2_clim0_mes_account_top_2 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s2_clim0_mes_wsid_2.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s2_clim0_mes_account_top_2$wsid)
names(wsid)[1] <- "wsid"
# targ
s2_clim0_targ_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_2$targ))
names(s2_clim0_targ_rs)[1] <- "targ_rs"
summary(s2_clim0_targ_rs)
# rec
s2_clim0_rec_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_2$rec))
names(s2_clim0_rec_rs)[1] <- "rec_rs"
summary(s2_clim0_rec_rs)
# cv
s2_clim0_cv_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_2$cv))
names(s2_clim0_cv_rs)[1] <- "cv_rs"
summary(s2_clim0_cv_rs)
# mes index
s2_clim0_mes_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_2$es_index))
names(s2_clim0_mes_rs)[1] <- "mes_rs"
summary(s2_clim0_mes_rs)
# mes count
s2_clim0_mes_count <- as.data.frame(s2_clim0_mes_account_top_2$es_count)
names(s2_clim0_mes_count)[1] <- "mes_count"
# combine
s2_clim0_mes_top_2_rs <- cbind(s2_clim0_mes_account_top_2, s2_clim0_targ_rs, s2_clim0_rec_rs, s2_clim0_cv_rs,s2_clim0_mes_rs,s2_clim0_mes_count)
# Write the csv file
write.csv(s2_clim0_mes_top_2_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_2.csv" ,sep="_")), row.names = F)

# wsid top #3
# import the mes accounting for the top 3 watersheds
s2_clim0_mes_account_top_3 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s2_clim0_mes_wsid_3.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s2_clim0_mes_account_top_3$wsid)
names(wsid)[1] <- "wsid"
# targ
s2_clim0_targ_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_3$targ))
names(s2_clim0_targ_rs)[1] <- "targ_rs"
summary(s2_clim0_targ_rs)
# rec
s2_clim0_rec_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_3$rec))
names(s2_clim0_rec_rs)[1] <- "rec_rs"
summary(s2_clim0_rec_rs)
# cv
s2_clim0_cv_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_3$cv))
names(s2_clim0_cv_rs)[1] <- "cv_rs"
summary(s2_clim0_cv_rs)
# mes index
s2_clim0_mes_rs <- as.data.frame(rescale_pos(s2_clim0_mes_account_top_3$es_index))
names(s2_clim0_mes_rs)[1] <- "mes_rs"
summary(s2_clim0_mes_rs)
# mes count
s2_clim0_mes_count <- as.data.frame(s2_clim0_mes_account_top_3$es_count)
names(s2_clim0_mes_count)[1] <- "mes_count"
# combine
s2_clim0_mes_top_3_rs <- cbind(s2_clim0_mes_account_top_3, s2_clim0_targ_rs, s2_clim0_rec_rs, s2_clim0_cv_rs,s2_clim0_mes_rs,s2_clim0_mes_count)
# Write the csv file
write.csv(s2_clim0_mes_top_3_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_3.csv" ,sep="_")), row.names = F)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 13/ Combine the fraction for sediment with the change in MES by WSID #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# create a working directory
dir.create(file.path("09_wsid_prioritization/5_wsid_weight_raw"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s2_clim0"

# import the sediment fraction csv files 
s2_clim0_frac_sed_1 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s2_clim0_frac_sed_1.csv", header = TRUE, sep = ",")
s2_clim0_frac_sed_2 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s2_clim0_frac_sed_2.csv", header = TRUE, sep = ",")
s2_clim0_frac_sed_3 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s2_clim0_frac_sed_3.csv", header = TRUE, sep = ",")

# add a column for WSID priority (1,2,3)
s2_clim0_frac_sed_1$priority <- "1"
s2_clim0_frac_sed_2$priority <- "2"
s2_clim0_frac_sed_3$priority <- "3"

# import the MES change csv files 
s2_clim0_mes_1 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s2_clim0_mes_1.csv", header = TRUE, sep = ",")
s2_clim0_mes_2 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s2_clim0_mes_2.csv", header = TRUE, sep = ",")
s2_clim0_mes_3 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s2_clim0_mes_3.csv", header = TRUE, sep = ",")

# combine the 3 MES change by averaging the rescaled values 0-100 = MES change index
s2_clim0_mes_1$mes_index <- ((s2_clim0_mes_1$targ + s2_clim0_mes_1$rec + s2_clim0_mes_1$cv)/3)
s2_clim0_mes_2$mes_index <- ((s2_clim0_mes_2$targ + s2_clim0_mes_2$rec + s2_clim0_mes_2$cv)/3)
s2_clim0_mes_3$mes_index <- ((s2_clim0_mes_3$targ + s2_clim0_mes_3$rec + s2_clim0_mes_3$cv)/3)

# join the sediment fraction and change in MES for priority watershed
s2_clim0_wsid_1 <- full_join(s2_clim0_frac_sed_1, s2_clim0_mes_1, by = "wsid")
s2_clim0_wsid_2 <- full_join(s2_clim0_frac_sed_2, s2_clim0_mes_2, by = "wsid")
s2_clim0_wsid_3 <- full_join(s2_clim0_frac_sed_3, s2_clim0_mes_3, by = "wsid")

# Calculate the WSID weight = MES change index x Sediment fraction x mean # of ES change
s2_clim0_wsid_1$s2 <- s2_clim0_wsid_1$mes_index*s2_clim0_wsid_1$frac_sed*s2_clim0_mes_1$mes_count
s2_clim0_wsid_2$s2 <- s2_clim0_wsid_2$mes_index*s2_clim0_wsid_2$frac_sed*s2_clim0_mes_2$mes_count
s2_clim0_wsid_3$s2 <- s2_clim0_wsid_3$mes_index*s2_clim0_wsid_3$frac_sed*s2_clim0_mes_3$mes_count

# Combine the dataframes with sediment fraction & MES change for the 3 priority watersheds by stacking 
s2_clim0_wsid_all <- rbind(s2_clim0_wsid_1, s2_clim0_wsid_2, s2_clim0_wsid_3)
# Write the csv file
write.csv(s2_clim0_wsid_all, file.path("09_wsid_prioritization/5_wsid_weight_raw", paste(scenario, "wsid_weight.csv" ,sep="_")), row.names = F)

# Average the WSID weight by WSID to assign one final WSID weight to each watershed
s2_clim0_wsid <- aggregate(s2_clim0_wsid_all[,14:15], list(s2_clim0_wsid_all$wsid), mean)
names(s2_clim0_wsid)[1] <- "wsid"
# Write the csv file
write.csv(s2_clim0_wsid, file.path("09_wsid_prioritization/5_wsid_weight_raw", paste(scenario, "wsid_weight_mn.csv" ,sep="_")), row.names = F)

##### 14/ Rescale the WSID weight at the country scale for ROOT (SWM - 0-1) #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

### IT NEEDS TO BE BETWEEN 0 & 1 FOR ROOT!!

# create a working directory
dir.create(file.path("09_wsid_prioritization/6_wsid_weight_rescale"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s2_clim0"

# import the MES change csv files 
s2_clim0_wsid <- read.csv("09_wsid_prioritization/5_wsid_weight_raw/s2_clim0_wsid_weight_mn.csv", header = TRUE, sep = ",")

# convert wsid at character
s2_clim0_wsid$wsid <- as.character(s2_clim0_wsid$wsid)

# function to rescale between 0-1 
rescale_pos <- function(x) (x-min(x))/(max(x) - min(x))
rescale_neg <- function(x) (x-max(x))/(min(x) - max(x))

# BZ watershed weights
# set the country
country <- "bz"
# import watershed shapefile
wsid_bz <- st_read("00_aoi_watersheds/bz_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_bz$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s2_clim0_wsid_bz <- inner_join(wsid, s2_clim0_wsid, by ="wsid")
# rescale wsid weight
s2_rs <- as.data.frame(rescale_pos(s2_clim0_wsid_bz$s2))
names(s2_rs)[1] <- "s2_rs"
summary(s2_rs)
# join the rescale wsid weight to gt wsid
s2_clim0_wsid_rs <- cbind(s2_clim0_wsid_bz, s2_rs)
# Write the csv file
write.csv(s2_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
bz_wsid_shp <- left_join(wsid_bz, s2_clim0_wsid_rs, 'wsid', 'wsid')
names(bz_wsid_shp)
# export as shp
wsid_sp <- as(bz_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="bz_s2_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# GT watershed weights
# set the country
country <- "gt"
# import watershed shapefile
wsid_gt <- st_read("00_aoi_watersheds/gt_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_gt$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s2_clim0_wsid_gt <- inner_join(wsid, s2_clim0_wsid, by ="wsid")
# rescale wsid weight
s2_rs <- as.data.frame(rescale_pos(s2_clim0_wsid_gt$s2))
names(s2_rs)[1] <- "s2_rs"
summary(s2_rs)
# join the rescale wsid weight to gt wsid
s2_clim0_wsid_rs <- cbind(s2_clim0_wsid_gt, s2_rs)
# Write the csv file
write.csv(s2_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
gt_wsid_shp <- left_join(wsid_gt, s2_clim0_wsid_rs, 'wsid', 'wsid')
names(gt_wsid_shp)
# export as shp
wsid_sp <- as(gt_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="gt_s2_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# HN watershed weights
# set the country
country <- "hn"
# import watershed shapefile
wsid_hn <- st_read("00_aoi_watersheds/hn_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_hn$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s2_clim0_wsid_hn <- inner_join(wsid, s2_clim0_wsid, by ="wsid")
# rescale wsid weight
s2_rs <- as.data.frame(rescale_pos(s2_clim0_wsid_hn$s2))
names(s2_rs)[1] <- "s2_rs"
summary(s2_rs)
# join the rescale wsid weight to hn wsid
s2_clim0_wsid_rs <- cbind(s2_clim0_wsid_hn, s2_rs)
# Write the csv file
write.csv(s2_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
hn_wsid_shp <- left_join(wsid_hn, s2_clim0_wsid_rs, 'wsid', 'wsid')
names(hn_wsid_shp)
# export as shp
wsid_sp <- as(hn_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="hn_s2_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))


##### D/ S3_CLIM0 - WATERSHED IMPORTANCE #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

#### 1/ Convert coral change mask to dataframe ##### 
# import the coral reef impact area aoi - s3
s3_marine_es <- raster("01_coral_reef_key/coral_footprint_tif/cor_s3_clim0_delta_footprint_wqm_na.tif")

# convert to coral footprint to point data
plot(s3_marine_es)
marine_es_sp <- rasterToPoints(s3_marine_es)
str(marine_es_sp)
# create a "crid" key to link coral reefs to watersheds
marine_es_df <-as.data.frame(marine_es_sp, row.names = T)
head(marine_es_df)
row.names(marine_es_df)
marine_es_df$crid = row.names(marine_es_df)
head(marine_es_df)
crid_mar <- marine_es_df[,c(4,1:2)]
head(crid_mar)
colnames(crid_mar)[1] <- "crid"
head(crid_mar)
crid_mar$crid <- as.integer(crid_mar$crid)
write.table(crid_mar, file.path("01_coral_reef_key", paste("crid_s3_clim0_cor.csv")), row.names=FALSE, col.names=TRUE, sep=",")

#### 2/ BZ - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s3_clim0"

# Create directory
dir.create(file.path("02_plume_delta_bz/s3_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_bz <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0"), pattern='.tif$')
plume_list_bz
wsid <- substr(plume_list_bz, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0", plume_list_bz[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s3_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s3_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("02_plume_delta_bz/s3_clim0", paste(plume_list_bz[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_bz_clip <- list.files(file.path("02_plume_delta_bz/s3_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_bz_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s3_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("02_plume_delta_bz/s3_clim0", plume_list_bz_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_bz <- crid_mar
# Join the clipped plume values to the coral reef aoi
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("02_plume_delta_bz/s3_clim0", plume_list_bz[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
summary(plum_delta_df_all)
head(r2r_key_bz)
r2r_key_bz_cor <- cbind(r2r_key_bz, plum_delta_df_all)
head(r2r_key_bz_cor)
r2r_key_bz_cor <- r2r_key_bz_cor [,-c(4:6)]
summary(r2r_key_bz_cor)
write.table(r2r_key_bz_cor, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_bz_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 3/ GT - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s3_clim0"

# Create directory
dir.create(file.path("03_plume_delta_gt/s3_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_gt <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/gt/s3_sust_agri_clim0"), pattern='.tif$')
plume_list_gt
wsid <- substr(plume_list_gt, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/gt/s3_sust_agri_clim0", plume_list_gt[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s3_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s3_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("03_plume_delta_gt/s3_clim0", paste(plume_list_gt[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_gt_clip <- list.files(file.path("03_plume_delta_gt/s3_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_gt_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s3_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("03_plume_delta_gt/s3_clim0", plume_list_gt_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_gt <- crid_mar
# Join the clipped plume values to the coral reef aoi
setwd("E:/Smart_Coast_project/5_watershed_importance/")
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("03_plume_delta_gt/s3_clim0", plume_list_gt_clip[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
summary(plum_delta_df_all)
head(r2r_key_gt)
r2r_key_gt_cor <- cbind(r2r_key_gt, plum_delta_df_all)
head(r2r_key_gt_cor)
r2r_key_gt_cor <- r2r_key_gt_cor [,-c(4:6)]
summary(r2r_key_gt_cor)
write.table(r2r_key_gt_cor, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_gt_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 4/ HN - Clip & convert clipped plumes delta to dataframe ##### 
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# set the scenario
scenario <- "s3_clim0"

# Create directory
dir.create(file.path("04_plume_delta_hn/s3_clim0"), showWarning=TRUE, recursive=TRUE)

#Create list of file names
plume_list_hn <- list.files(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/hn/s3_sust_agri_clim0"), pattern='.tif$')
plume_list_hn
wsid <- substr(plume_list_hn, 13, 15)
wsid

# loop clipping
for(i in 1:length(wsid)){
  plume_dlt <- raster(file.path("E:/Smart_Coast_project/2_wq_mar/05_sed_ppt_decay/1_scenarios/hn/s3_sust_agri_clim0", plume_list_hn[i]))
  plume_dlt_clip <- crop(plume_dlt, extent(s3_marine_es))
  plume_dlt_clip <- raster::mask(plume_dlt_clip, s3_marine_es)
  writeRaster(plume_dlt_clip, filename=file.path("04_plume_delta_hn/s3_clim0", paste(plume_list_hn[i], sep="_")), format="GTiff", overwrite=TRUE)
  rm(plume_dlt,plume_dlt_clip)
}

# import list of plumes
plume_list_hn_clip <- list.files(file.path("04_plume_delta_hn/s3_clim0"), pattern='.tif$', recursive=TRUE)
plume_list_hn_clip

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s3_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# prepare the plume delta dataframe
plum_delta <- raster(file.path("04_plume_delta_hn/s3_clim0", plume_list_hn_clip[1]))
plum_delta_sp <- rasterToPoints(plum_delta)
str(plum_delta_sp)
plum_delta_df_all<-as.data.frame(plum_delta_sp, row.names = F)
head(plum_delta_df_all)

# rename the coral reef area
r2r_key_hn <- crid_mar
# Join the clipped plume values to the coral reef aoi
setwd("E:/Smart_Coast_project/5_watershed_importance/")
for(i in 1:length(wsid)) {
  plum_delta <- raster(file.path("04_plume_delta_hn/s3_clim0", plume_list_hn_clip[i]))
  plum_delta_sp <- rasterToPoints(plum_delta)
  str(plum_delta_sp)
  plum_delta_df<-as.data.frame(plum_delta_sp, row.names = F)
  head(plum_delta_df)
  names(plum_delta_df)[3] <- wsid[i]
  plum_delta_df_all <- cbind(plum_delta_df_all, plum_delta_df[3]) # code line testing to rm NA
  head(plum_delta_df_all)
}
head(r2r_key_hn)
r2r_key_hn <- cbind(r2r_key_hn, plum_delta_df_all)
head(r2r_key_hn)
r2r_key_hn <- r2r_key_hn [,-c(4:6)]
summary(r2r_key_hn)
write.table(r2r_key_hn, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_hn_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

#### 5/ MAR - Combined clipped plumes delta dataframes #####
# import the coral reef area key data frame
r2r_key_bz <- read.csv("05_plume_delta_mar_table/s3_clim0_r2r_key_bz_cor.csv", header = TRUE, sep = ",")
r2r_key_gt <- read.csv("05_plume_delta_mar_table/s3_clim0_r2r_key_gt_cor.csv", header = TRUE, sep = ",")
r2r_key_hn <- read.csv("05_plume_delta_mar_table/s3_clim0_r2r_key_hn_cor.csv", header = TRUE, sep = ",")

# combine the 3 countries data frames into 1 dataframe to ases transbounadry impacts
r2r_key_mar <- cbind(r2r_key_hn, r2r_key_gt, r2r_key_bz)
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))
names(r2r_key_mar)
r2r_key_mar <- r2r_key_mar [,-c(79:81,98:100)] # Remove all the columns duplicates
names(r2r_key_mar)
summary(r2r_key_mar)
write.table(r2r_key_mar, file.path("05_plume_delta_mar_table", paste(scenario, "r2r_key_mar_cor.csv", sep="_")), row.names=FALSE, col.names=TRUE, sep=",")

# clear working environment
rm(list = ls(all.names=TRUE))

##### 6/ Identify the top 3 watersheds driving the coral habitat change at each grid cell (key = wsid) #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# import the coral reef area key data frame
crid_mar <- read.csv("01_coral_reef_key/crid_s3_clim0_cor.csv", header = TRUE, sep = ",")
str(crid_mar)

# create a working directory
dir.create(file.path("06_wsid_crid_link/table"), showWarning=TRUE, recursive=TRUE)

# create a working directory
dir.create(file.path("06_wsid_crid_link/raster"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s3_clim0"

# import the mar table
r2r_key_mar <- read.csv("05_plume_delta_mar_table/s3_clim0_r2r_key_mar_cor.csv", header = TRUE, sep = ",")
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))

# create a copy
wq_2_cr_s3_clim0 <- r2r_key_mar

# create 3 data frame for each priority watershed
wsid_top_1 <- as.data.frame(1)
wsid_top_2 <- as.data.frame(1)
wsid_top_3 <- as.data.frame(1)

# Create a list of wsid
crid <- as.integer(r2r_key_mar$crid)
# Create a data frame to store top watersheds by crid
cr_2_ws_s3_clim0 <- data.frame(crid=rep(NA,nrow(r2r_key_mar)),
                               p1_wsid=rep(NA,nrow(wsid_top_1)),
                               p2_wsid=rep(NA,nrow(wsid_top_2)),
                               p3_wsid=rep(NA,nrow(wsid_top_3)))

# Start the clock!
ptm <- proc.time()
# identify the top 3 watersheds driving the reef pixel
for(i in 1:length(crid)) {
  cr_2_ws <- wq_2_cr_s3_clim0 %>%
    slice(i:i) # extract one row from the dataframe
  df <- as.data.frame(apply(cr_2_ws[-c(1:3)], 1, function(x) names(sort(x)))) # sort in descending way the dataframe and return the watershed name
  wsid_top_1 <- df %>%
    slice(1:1) # extract row 1 from the dataframe
  wsid_top_2 <- df %>%
    slice(2:2) # extract row 2 from the dataframe
  wsid_top_3 <- df %>%
    slice(3:3) # extract row 3 from the dataframe
  cr_2_ws_s3_clim0[i,1] <- crid[i]	  # crid
  cr_2_ws_s3_clim0[i,2] <- wsid_top_1[1,1]  # priority watershed 1
  cr_2_ws_s3_clim0[i,3] <- wsid_top_2[1,1]  # priority watershed 2
  cr_2_ws_s3_clim0[i,4] <- wsid_top_3[1,1] 	# priority watershed 3
}
# Stop the clock
proc.time() - ptm

# check the structure
head(cr_2_ws_s3_clim0)

# create single file per top watershed 1 (x, y, px_wsid)
top_1_ws_2_cr <- cr_2_ws_s3_clim0[,-c(3:4)]
head(top_1_ws_2_cr)
top_1_ws_2_cr <- cbind(crid_mar, top_1_ws_2_cr, by="crid")
head(top_1_ws_2_cr)
top_1_ws_2_cr <- top_1_ws_2_cr[,-c(4,6)]
top_1_ws_2_cr$p1_wsid <- as.numeric(top_1_ws_2_cr$p1_wsid)
head(top_1_ws_2_cr)
summary(top_1_ws_2_cr)
# Write the csv file
write.csv(top_1_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_1.csv" ,sep="_")), row.names = F)

# create single file per top watershed 2 (x, y, px_wsid)
top_2_ws_2_cr <- cr_2_ws_s3_clim0[,-c(2,4)]
head(top_2_ws_2_cr)
top_2_ws_2_cr <- cbind(crid_mar, top_2_ws_2_cr, by="crid")
head(top_2_ws_2_cr)
top_2_ws_2_cr <- top_2_ws_2_cr[,-c(4,6)]
top_2_ws_2_cr$p2_wsid <- as.numeric(top_2_ws_2_cr$p2_wsid)
head(top_2_ws_2_cr)
summary(top_2_ws_2_cr)
# Write the csv file
write.csv(top_2_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_2.csv" ,sep="_")), row.names = F)

# create single file per top watershed 3 (x, y, px_wsid)
top_3_ws_2_cr <- cr_2_ws_s3_clim0[,-c(2:3)]
head(top_3_ws_2_cr)
top_3_ws_2_cr <- cbind(crid_mar, top_3_ws_2_cr, by="crid")
head(top_3_ws_2_cr)
top_3_ws_2_cr <- top_3_ws_2_cr[,-c(4,6)]
top_3_ws_2_cr$p3_wsid <- as.numeric(top_3_ws_2_cr$p3_wsid)
head(top_3_ws_2_cr)
summary(top_3_ws_2_cr)
# Write the csv file
write.csv(top_3_ws_2_cr, file.path("06_wsid_crid_link/table", paste(scenario, "top_3.csv" ,sep="_")), row.names = F)


##### 7/ Convert the dataframe with the linked crid to wsid into rasters  #####
# set the scenario
scenario <- "s3_clim0"

## Watershed # 1
# import priority wsid by crid table
top_1_ws_2_cr <- read.csv("06_wsid_crid_link/table/s3_clim0_top_1.csv", header = TRUE, sep = ",")
# remove the crid column
top_1_ws_2_cr_grid <- top_1_ws_2_cr[,-c(1)]
top_1_ws_2_cr_grid$p1_wsid <- as.numeric(top_1_ws_2_cr_grid$p1_wsid)
# set coordinates
coordinates(top_1_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_1_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_1_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_1", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)

## Watershed # 2
# import priority wsid by crid table
top_2_ws_2_cr <- read.csv("06_wsid_crid_link/table/s3_clim0_top_2.csv", header = TRUE, sep = ",")
# remove the crid column
top_2_ws_2_cr_grid <- top_2_ws_2_cr[,-c(1)]
top_2_ws_2_cr_grid$p2_wsid <- as.numeric(top_2_ws_2_cr_grid$p2_wsid)
# set coordinates
coordinates(top_2_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_2_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_2_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs" 
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_2", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)

## Watershed # 3
# import priority wsid by crid table
top_3_ws_2_cr <- read.csv("06_wsid_crid_link/table/s3_clim0_top_3.csv", header = TRUE, sep = ",")
# remove the crid column
top_3_ws_2_cr_grid <- top_3_ws_2_cr[,-c(1)]
top_3_ws_2_cr_grid$p3_wsid <- as.numeric(top_3_ws_2_cr_grid$p3_wsid)
# set coordinates
coordinates(top_3_ws_2_cr_grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(top_3_ws_2_cr_grid) <- TRUE
# coerce to raster
rasterDF <- raster(top_3_ws_2_cr_grid)
rasterDF
plot(rasterDF)
crs(rasterDF) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs" 
writeRaster(rasterDF, filename=file.path("06_wsid_crid_link/raster", paste(scenario, "top_3", sep="_")), 
            datatype='INT2U', format="GTiff", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

##### 8/ Convert the marine rasters identifying the top 3 watersheds in a polygon (zones)  #####
setwd("E:/Smart_Coast_project/5_watershed_importance/")

# create a working directory
dir.create(file.path("06_wsid_crid_link/polyg/dissolve"), showWarning=TRUE, recursive=TRUE)

# convert raster to polygon, dissolved neighboring same values
# watershed P1
# import rasters linking crid to wsid
r_s3_clim0_top_1 <- raster("E:/Smart_Coast_project/5_watershed_importance/06_wsid_crid_link/raster/s3_clim0_top_1.tif")
s3_clim0_top_1 <- rasterToPolygons(r_s3_clim0_top_1, na.rm=TRUE, dissolve = T)
# rename atribute
names(s3_clim0_top_1@data) <- "wsid"
plot(s3_clim0_top_1)
names(s3_clim0_top_1)
# export shp
writeOGR(obj=s3_clim0_top_1, dsn="06_wsid_crid_link/polyg/dissolve", layer="s3_clim0_top_1", driver="ESRI Shapefile", overwrite_layer=TRUE)

# watershed P2
# import rasters linking crid to wsid
r_s3_clim0_top_2 <- raster("E:/Smart_Coast_project/5_watershed_importance/06_wsid_crid_link/raster/s3_clim0_top_2.tif")
s3_clim0_top_2 <- rasterToPolygons(r_s3_clim0_top_2, na.rm=TRUE, dissolve = T)
names(s3_clim0_top_2@data) <- "wsid"
plot(s3_clim0_top_2)
names(s3_clim0_top_2)
# export shp
writeOGR(obj=s3_clim0_top_2, dsn="06_wsid_crid_link/polyg/dissolve", layer="s3_clim0_top_2", driver="ESRI Shapefile", overwrite_layer=TRUE)

# watershed P3
# import rasters linking crid to wsid
r_s3_clim0_top_3 <- raster("E:/Smart_Coast_project/5_watershed_importance/06_wsid_crid_link/raster/s3_clim0_top_3.tif")
s3_clim0_top_3 <- rasterToPolygons(r_s3_clim0_top_3, na.rm=TRUE, dissolve = T)
names(s3_clim0_top_3@data) <- "wsid"
plot(s3_clim0_top_3)
names(s3_clim0_top_3)
# export shp
writeOGR(obj=s3_clim0_top_3, dsn="06_wsid_crid_link/polyg/dissolve", layer="s3_clim0_top_3", driver="ESRI Shapefile", overwrite_layer=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

# in ArcGIS project the shapefiles in NAD83
# in ArcGIS manually edit to remove the polygons where the change in WQ is 0

##### 9/ Calculate the sediment loads & fractions contributed by the top 3 watersheds #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# import the mar table
r2r_key_mar <- read.csv("05_plume_delta_mar_table/s3_clim0_r2r_key_mar_cor.csv", header = TRUE, sep = ",")
names(r2r_key_mar) <- sub("^X", "", names(r2r_key_mar))
names(r2r_key_mar)

# calculate the total sediment discharge per pixel
sum <- as.data.frame(rowSums(r2r_key_mar[,4:163], na.rm = T))
colnames(sum)[1] <- "sum"
# find the min value for each row
sed_1 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(x)[1]))) 
colnames(sed_1)[1] <- "sed_1"
# find the second min value for each row
sed_2 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(x)[2]))) 
colnames(sed_2)[1] <- "sed_2"
# find the third min value for each row
sed_3 <- as.data.frame(apply(r2r_key_mar[,4:163], 1, function(x) (sort(x)[3]))) 
colnames(sed_3)[1] <- "sed_3"
# combine all
sed_load <- cbind(sum,sed_1,sed_2, sed_3)

# Combine the watershed names and sediment fractions
# import priority wsid by crid table
top_1_ws_2_cr <- read.csv("06_wsid_crid_link/table/s3_clim0_top_1.csv", header = TRUE, sep = ",")
top_2_ws_2_cr <- read.csv("06_wsid_crid_link/table/s3_clim0_top_2.csv", header = TRUE, sep = ",")
top_3_ws_2_cr <- read.csv("06_wsid_crid_link/table/s3_clim0_top_3.csv", header = TRUE, sep = ",")

# add the wsid names
sed_load <- cbind(top_1_ws_2_cr, sed_load[2], 
                  top_2_ws_2_cr[4], sed_load[3], 
                  top_3_ws_2_cr[4], sed_load[4],sed_load[1])
# Write the csv file
write.csv(sed_load, file.path("09_wsid_prioritization/0_sed_load_by_crid", paste("s3_clim0_r2r_key_mar.csv")), row.names = F)

# convert the sediment loads into fractions
sed_load$p1_frac_sed <- sed_load$sed_1 / sed_load$sum
sed_load$p2_frac_sed <- sed_load$sed_2 / sed_load$sum
sed_load$p3_frac_sed <- sed_load$sed_3 / sed_load$sum
names(sed_load)

# bind all the wsid, sed loads, and sed fractions
crid_wsid_sed_frac <- cbind(top_1_ws_2_cr, sed_load[5], sed_load[11], 
                            top_2_ws_2_cr[4], sed_load[7], sed_load[12],
                            top_3_ws_2_cr[4], sed_load[9],sed_load[13],sed_load[10])
crid_wsid_sed_frac$tot_frac <- crid_wsid_sed_frac$p1_frac_sed+crid_wsid_sed_frac$p2_frac_sed+crid_wsid_sed_frac$p3_frac_sed

# Write the csv file
write.csv(crid_wsid_sed_frac, file.path("09_wsid_prioritization/1_sed_frac_by_crid", paste("s3_frac_sed_by_crid.csv")), row.names = F)

# in Excel delete the WSID 100 & 101 when fraction = 0

##### 10/ Calculate the average sediment fraction from coral pixel to the watershed zone #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# create a working directory
dir.create(file.path("09_wsid_prioritization/2_sed_frac_by_wsid"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s3_clim0"
# import the sediment fraction by crid
s3_sed_frac_by_crid <- read.csv("09_wsid_prioritization/1_sed_frac_by_crid/s3_frac_sed_by_crid.csv", header = TRUE, sep = ",")

# priority wsid # 1
s3_frac_sed_by_wsid_1 <- aggregate(s3_sed_frac_by_crid$p1_frac_sed ~ s3_sed_frac_by_crid$p1_wsid, s3_sed_frac_by_crid, mean )
names(s3_frac_sed_by_wsid_1)[1] <- "wsid"
names(s3_frac_sed_by_wsid_1)[2] <- "frac_sed"
summary(s3_frac_sed_by_wsid_1)
# Write the csv file
write.csv(s3_frac_sed_by_wsid_1, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_1.csv" ,sep="_")), row.names = F)

# priority wsid # 2
s3_frac_sed_by_wsid_2 <- aggregate(s3_sed_frac_by_crid$p2_frac_sed ~ s3_sed_frac_by_crid$p2_wsid, s3_sed_frac_by_crid, mean )
names(s3_frac_sed_by_wsid_2)[1] <- "wsid"
names(s3_frac_sed_by_wsid_2)[2] <- "frac_sed"
summary(s3_frac_sed_by_wsid_2)
# Write the csv file
write.csv(s3_frac_sed_by_wsid_2, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_2.csv" ,sep="_")), row.names = F)

# priority wsid # 3
s3_frac_sed_by_wsid_3 <- aggregate(s3_sed_frac_by_crid$p3_frac_sed ~ s3_sed_frac_by_crid$p3_wsid, s3_sed_frac_by_crid, mean )
names(s3_frac_sed_by_wsid_3)[1] <- "wsid"
names(s3_frac_sed_by_wsid_3)[2] <- "frac_sed"
summary(s3_frac_sed_by_wsid_3)
# Write the csv file
write.csv(s3_frac_sed_by_wsid_3, file.path("09_wsid_prioritization/2_sed_frac_by_wsid", paste(scenario, "frac_sed_3.csv" ,sep="_")), row.names = F)

##### 11/ Calculate the change in marine ES  by watershed linked to reefs #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# set the scenario
scenario <- "s3_clim0"

# import marine ES change outputs
# targeted fish biomass change
s3_targ_clim0 <- raster("07_marine_es_outputs/02_targ_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s3_targ_clim0.tif")
# recreation opportunity change
s3_rec_clim0 <- raster("07_marine_es_outputs/03_rec_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s3_rec_clim0.tif")
# coastal protection change
s3_cv_clim0 <- raster("07_marine_es_outputs/04_cv_outputs/3_raster_coral_footprint_clip/cor_mar_r2r_s3_cv_clim0.tif")

# Count of ES changing by crid
mes_count <- raster("07_marine_es_outputs/05_mes_delta_masks/1_raster_sum/s3_marine_es_sum.tif")

# function to rescale cell values between 0 and 1
raster_scale = function(r){
  # get the min max values
  minmax_r = range(values(r), na.rm=TRUE) 
  # rescale 
  return((r-minmax_r[1])/(diff(minmax_r)))}

# function to rescale between 0-1 
s3_targ_clim0_rs <- raster_scale(s3_targ_clim0)
writeRaster(s3_targ_clim0_rs, "08_marine_es_accouting/targ/mar_r2r_s3_targ_clim0_rs.tif", overwrite=TRUE)
s3_rec_clim0_rs <- raster_scale(s3_rec_clim0)
writeRaster(s3_rec_clim0_rs, "08_marine_es_accouting/rec/mar_r2r_s3_rec_clim0_rs.tif", overwrite=TRUE)
s3_cv_clim0_rs <- raster_scale(s3_cv_clim0)
writeRaster(s3_cv_clim0_rs, "08_marine_es_accouting/cv/mar_r2r_s3_cv_clim0_rs.tif", overwrite=TRUE)

# import marine ES change RESCALED outputs
# targeted fish biomass change
s3_targ_clim0_rs <- raster("08_marine_es_accouting/targ/mar_r2r_s3_targ_clim0_rs.tif")
# recreation opportunity change
s3_rec_clim0_rs <- raster("08_marine_es_accouting/rec/mar_r2r_s3_rec_clim0_rs.tif")
# coastal protection change
s3_cv_clim0_rs <- raster("08_marine_es_accouting/cv/mar_r2r_s3_cv_clim0_rs.tif")

# combine the 3 services weighted by change in ES
es_index <- (s3_targ_clim0_rs+s3_rec_clim0_rs+s3_cv_clim0_rs)*mes_count
writeRaster(es_index, filename=file.path("08_marine_es_accouting/mes_weighted_sum", paste(scenario, "mes_index", sep="_")), format="GTiff", overwrite=TRUE)

# import crid <-> wsid zones
wsid_marine_zone_1 <- st_read("06_wsid_crid_link/polyg/clean/s3_clim0_top_1.shp")
# run zonal stat - watershed # 1
# targeted fish biomass change
wsid_marine_zone_1$targ <- exact_extract(s3_targ_clim0, wsid_marine_zone_1, 'sum')
wsid_marine_zone_1$targ <- as.integer(wsid_marine_zone_1$targ) 
# recreation opportunity change
wsid_marine_zone_1$rec <- exact_extract(s3_rec_clim0, wsid_marine_zone_1, 'sum')
# coastal protection change
wsid_marine_zone_1$cv <- exact_extract(s3_cv_clim0, wsid_marine_zone_1, 'sum')
# cumulative change weighted
wsid_marine_zone_1$es_index <- exact_extract(es_index, wsid_marine_zone_1, 'sum')
# ES count change
wsid_marine_zone_1$es_count <- exact_extract(mes_count, wsid_marine_zone_1, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_1, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s3_clim0_mes_wsid_1", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_1.csv" ,sep="_")), row.names = F)

# run zonal stat - watershed # 2
# import crid <-> wsid zones
wsid_marine_zone_2 <- st_read("06_wsid_crid_link/polyg/clean/s3_clim0_top_2.shp")
# targeted fish biomass change
wsid_marine_zone_2$targ <- exact_extract(s3_targ_clim0, wsid_marine_zone_2, 'sum')
wsid_marine_zone_2$targ <- as.integer(wsid_marine_zone_2$targ) 
# recreation opportunity change
wsid_marine_zone_2$rec <- exact_extract(s3_rec_clim0, wsid_marine_zone_2, 'sum')
# coastal protection change
wsid_marine_zone_2$cv <- exact_extract(s3_cv_clim0, wsid_marine_zone_2, 'sum')
# cumulative change weighted
wsid_marine_zone_2$es_index <- exact_extract(es_index, wsid_marine_zone_2, 'sum')
# ES count change
wsid_marine_zone_2$es_count <- exact_extract(mes_count, wsid_marine_zone_2, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_2, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s3_clim0_mes_wsid_2", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_2.csv" ,sep="_")), row.names = F)

# run zonal stat - watershed # 3
# import crid <-> wsid zones
wsid_marine_zone_3 <- st_read("06_wsid_crid_link/polyg/clean/s3_clim0_top_3.shp")
# targeted fish biomass change
wsid_marine_zone_3$targ <- exact_extract(s3_targ_clim0, wsid_marine_zone_3, 'sum')
wsid_marine_zone_3$targ <- as.integer(wsid_marine_zone_3$targ) 
# recreation opportunity change
wsid_marine_zone_3$rec <- exact_extract(s3_rec_clim0, wsid_marine_zone_3, 'sum')
# coastal protection change
wsid_marine_zone_3$cv <- exact_extract(s3_cv_clim0, wsid_marine_zone_3, 'sum')
# cumulative change weighted
wsid_marine_zone_3$es_index <- exact_extract(es_index, wsid_marine_zone_3, 'sum')
# ES count change
wsid_marine_zone_3$es_count <- exact_extract(mes_count, wsid_marine_zone_3, 'mean')
# export shp
wsid_sp <- as(wsid_marine_zone_3, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/3_mes_accounting_by_wsid_shp", layer="s3_clim0_mes_wsid_3", driver="ESRI Shapefile", overwrite=TRUE)
# convert to dataframe
wsid_marine_zone <- as.data.frame(wsid_sp)
names(wsid_marine_zone)
# Write the csv file
write.csv(wsid_marine_zone, file.path("09_wsid_prioritization/3_mes_accounting_by_wsid_csv", paste(scenario, "mes_wsid_3.csv" ,sep="_")), row.names = F)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 12/ Rescale the change in marine ES  by watershed linked to reefs from 0-100 #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# create a working directory
dir.create(file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale"), showWarning=TRUE, recursive=TRUE)

# function to rescale every column of your data frame between 0 and 100
## for every column of your data frame
rescale <- function(x) (x-min(x))/(max(x) - min(x))*100

# set the scenario
scenario <- "s3_clim0"

# wsid top #1
# import the mes accounting for the top 3 watersheds
s3_clim0_mes_account_top_1 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s3_clim0_mes_wsid_1.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s3_clim0_mes_account_top_1$wsid)
names(wsid)[1] <- "wsid"
# targ
s3_clim0_targ_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_1$targ))
names(s3_clim0_targ_rs)[1] <- "targ_rs"
summary(s3_clim0_targ_rs)
# rec
s3_clim0_rec_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_1$rec))
names(s3_clim0_rec_rs)[1] <- "rec_rs"
summary(s3_clim0_rec_rs)
# cv
s3_clim0_cv_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_1$cv))
names(s3_clim0_cv_rs)[1] <- "cv_rs"
summary(s3_clim0_cv_rs)
# mes index
s3_clim0_mes_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_1$es_index))
names(s3_clim0_mes_rs)[1] <- "mes_rs"
summary(s3_clim0_mes_rs)
# mes count
s3_clim0_mes_count <- as.data.frame(s3_clim0_mes_account_top_1$es_count)
names(s3_clim0_mes_count)[1] <- "mes_count"
# combine
s3_clim0_mes_top_1_rs <- cbind(s3_clim0_mes_account_top_1, s3_clim0_targ_rs, s3_clim0_rec_rs, s3_clim0_cv_rs,s3_clim0_mes_rs,s3_clim0_mes_count)
# Write the csv file
write.csv(s3_clim0_mes_top_1_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_1.csv" ,sep="_")), row.names = F)

# wsid top #2
# import the mes accounting for the top 3 watersheds
s3_clim0_mes_account_top_2 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s3_clim0_mes_wsid_2.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s3_clim0_mes_account_top_2$wsid)
names(wsid)[1] <- "wsid"
# targ
s3_clim0_targ_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_2$targ))
names(s3_clim0_targ_rs)[1] <- "targ_rs"
summary(s3_clim0_targ_rs)
# rec
s3_clim0_rec_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_2$rec))
names(s3_clim0_rec_rs)[1] <- "rec_rs"
summary(s3_clim0_rec_rs)
# cv
s3_clim0_cv_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_2$cv))
names(s3_clim0_cv_rs)[1] <- "cv_rs"
summary(s3_clim0_cv_rs)
# mes index
s3_clim0_mes_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_2$es_index))
names(s3_clim0_mes_rs)[1] <- "mes_rs"
summary(s3_clim0_mes_rs)
# mes count
s3_clim0_mes_count <- as.data.frame(s3_clim0_mes_account_top_2$es_count)
names(s3_clim0_mes_count)[1] <- "mes_count"
# combine
s3_clim0_mes_top_2_rs <- cbind(s3_clim0_mes_account_top_2, s3_clim0_targ_rs, s3_clim0_rec_rs, s3_clim0_cv_rs, s3_clim0_mes_rs, s3_clim0_mes_count)
# Write the csv file
write.csv(s3_clim0_mes_top_2_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_2.csv" ,sep="_")), row.names = F)

# wsid top #3
# import the mes accounting for the top 3 watersheds
s3_clim0_mes_account_top_3 <- read.csv("09_wsid_prioritization/3_mes_accounting_by_wsid_csv/s3_clim0_mes_wsid_3.csv", header = TRUE, sep = ",")
# get the wsid
wsid <- as.data.frame(s3_clim0_mes_account_top_3$wsid)
names(wsid)[1] <- "wsid"
# targ
s3_clim0_targ_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_3$targ))
names(s3_clim0_targ_rs)[1] <- "targ_rs"
summary(s3_clim0_targ_rs)
# rec
s3_clim0_rec_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_3$rec))
names(s3_clim0_rec_rs)[1] <- "rec_rs"
summary(s3_clim0_rec_rs)
# cv
s3_clim0_cv_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_3$cv))
names(s3_clim0_cv_rs)[1] <- "cv_rs"
summary(s3_clim0_cv_rs)
# mes index
s3_clim0_mes_rs <- as.data.frame(rescale(s3_clim0_mes_account_top_3$es_index))
names(s3_clim0_mes_rs)[1] <- "mes_rs"
summary(s3_clim0_mes_rs)
# mes count
s3_clim0_mes_count <- as.data.frame(s3_clim0_mes_account_top_3$es_count)
names(s3_clim0_mes_count)[1] <- "mes_count"
# combine
s3_clim0_mes_top_3_rs <- cbind(s3_clim0_mes_account_top_3, s3_clim0_targ_rs, s3_clim0_rec_rs, s3_clim0_cv_rs,s3_clim0_mes_rs,s3_clim0_mes_count)
# Write the csv file
write.csv(s3_clim0_mes_top_3_rs, file.path("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale", paste(scenario, "mes_3.csv" ,sep="_")), row.names = F)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 13/ Combine the fraction for sediment with the change in MES by WSID #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

# create a working directory
dir.create(file.path("09_wsid_prioritization/5_wsid_weight_raw"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s3_clim0"

# import the sediment fraction csv files 
s3_clim0_frac_sed_1 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s3_clim0_frac_sed_1.csv", header = TRUE, sep = ",")
s3_clim0_frac_sed_2 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s3_clim0_frac_sed_2.csv", header = TRUE, sep = ",")
s3_clim0_frac_sed_3 <- read.csv("09_wsid_prioritization/2_sed_frac_by_wsid/s3_clim0_frac_sed_3.csv", header = TRUE, sep = ",")

# add a column for WSID priority (1,2,3)
s3_clim0_frac_sed_1$priority <- "1"
s3_clim0_frac_sed_2$priority <- "2"
s3_clim0_frac_sed_3$priority <- "3"

# import the MES change csv files 
s3_clim0_mes_1 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s3_clim0_mes_1.csv", header = TRUE, sep = ",")
s3_clim0_mes_2 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s3_clim0_mes_2.csv", header = TRUE, sep = ",")
s3_clim0_mes_3 <- read.csv("09_wsid_prioritization/4_mes_accouting_by_wsid_rescale/s3_clim0_mes_3.csv", header = TRUE, sep = ",")

# combine the 3 MES change by averaging the rescaled values 0-100 = MES change index
s3_clim0_mes_1$mes_index <- ((s3_clim0_mes_1$targ + s3_clim0_mes_1$rec + s3_clim0_mes_1$cv)/3)
s3_clim0_mes_2$mes_index <- ((s3_clim0_mes_2$targ + s3_clim0_mes_2$rec + s3_clim0_mes_2$cv)/3)
s3_clim0_mes_3$mes_index <- ((s3_clim0_mes_3$targ + s3_clim0_mes_3$rec + s3_clim0_mes_3$cv)/3)

# join the sediment fraction and change in MES for priority watershed
s3_clim0_wsid_1 <- full_join(s3_clim0_frac_sed_1, s3_clim0_mes_1, by = "wsid")
s3_clim0_wsid_2 <- full_join(s3_clim0_frac_sed_2, s3_clim0_mes_2, by = "wsid")
s3_clim0_wsid_3 <- full_join(s3_clim0_frac_sed_3, s3_clim0_mes_3, by = "wsid")

# Calculate the WSID weight = MES change index x Sediment fraction x mean # of ES change
s3_clim0_wsid_1$s3 <- s3_clim0_wsid_1$mes_index*s3_clim0_wsid_1$frac_sed*s3_clim0_mes_1$mes_count
s3_clim0_wsid_2$s3 <- s3_clim0_wsid_2$mes_index*s3_clim0_wsid_2$frac_sed*s3_clim0_mes_2$mes_count
s3_clim0_wsid_3$s3 <- s3_clim0_wsid_3$mes_index*s3_clim0_wsid_3$frac_sed*s3_clim0_mes_3$mes_count

# Combine the dataframes with sediment fraction & MES change for the 3 priority watersheds by stacking 
s3_clim0_wsid_all <- rbind(s3_clim0_wsid_1, s3_clim0_wsid_2, s3_clim0_wsid_3)
# Write the csv file
write.csv(s3_clim0_wsid_all, file.path("09_wsid_prioritization/5_wsid_weight_raw", paste(scenario, "wsid_weight.csv" ,sep="_")), row.names = F)

# Average the WSID weight by WSID to assign one final WSID weight to each watershed
s3_clim0_wsid <- aggregate(s3_clim0_wsid_all[,14:15], list(s3_clim0_wsid_all$wsid), mean)
names(s3_clim0_wsid)[1] <- "wsid"
names(s3_clim0_wsid)
# Write the csv file
write.csv(s3_clim0_wsid, file.path("09_wsid_prioritization/5_wsid_weight_raw", paste(scenario, "wsid_weight_mn.csv" ,sep="_")), row.names = F)

# clear working environment
rm(list = ls(all.names=TRUE))


##### 14/ Rescale the WSID weight at the country scale for ROOT (SWM - 0-1) #####
setwd("E:/Smart_Coast_project/5_watershed_importance")

### IT NEEDS TO BE BETWEEN 0 & 1 FOR ROOT!!

# create a working directory
dir.create(file.path("09_wsid_prioritization/6_wsid_weight_rescale"), showWarning=TRUE, recursive=TRUE)

# set the scenario
scenario <- "s3_clim0"

# import the MES change csv files 
s3_clim0_wsid <- read.csv("09_wsid_prioritization/5_wsid_weight_raw/s3_clim0_wsid_weight_mn.csv", header = TRUE, sep = ",")

# convert wsid at character
s3_clim0_wsid$wsid <- as.character(s3_clim0_wsid$wsid)

# function to rescale between 0-1 
rescale <- function(x) (x-min(x))/(max(x) - min(x))

# BZ watershed weights
# set the country
country <- "bz"
# import watershed shapefile
wsid_bz <- st_read("00_aoi_watersheds/bz_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_bz$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s3_clim0_wsid_bz <- inner_join(wsid, s3_clim0_wsid, by ="wsid")
# rescale wsid weight
s3_rs <- as.data.frame(rescale(s3_clim0_wsid_bz$s3))
names(s3_rs)[1] <- "s3_rs"
summary(s3_rs)
# join the rescale wsid weight to bz wsid
s3_clim0_wsid_rs <- cbind(s3_clim0_wsid_bz, s3_rs)
# Write the csv file
write.csv(s3_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
bz_wsid_shp <- left_join(wsid_bz, s3_clim0_wsid_rs, 'wsid', 'wsid')
names(bz_wsid_shp)
# export as shp
wsid_sp <- as(bz_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="bz_s3_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# GT watershed weights
# set the country
country <- "gt"
# import watershed shapefile
wsid_gt <- st_read("00_aoi_watersheds/gt_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_gt$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s3_clim0_wsid_gt <- inner_join(wsid, s3_clim0_wsid, by ="wsid")
# rescale wsid weight
s3_rs <- as.data.frame(rescale(s3_clim0_wsid_gt$s3))
names(s3_rs)[1] <- "s3_rs"
summary(s3_rs)
# join the rescale wsid weight to gt wsid
s3_clim0_wsid_rs <- cbind(s3_clim0_wsid_gt, s3_rs)
# Write the csv file
write.csv(s3_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
gt_wsid_shp <- left_join(wsid_gt, s3_clim0_wsid_rs, 'wsid', 'wsid')
names(gt_wsid_shp)
# export as shp
wsid_sp <- as(gt_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="gt_s3_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# HN watershed weights
# set the country
country <- "hn"
# import watershed shapefile
wsid_hn <- st_read("00_aoi_watersheds/hn_wsid.shp")
# get the wsid
wsid <- as.data.frame(wsid_hn$wsid)
names(wsid)[1] <- "wsid"
# join by wsid to only select the country watersheds with the wsid weights
s3_clim0_wsid_hn <- inner_join(wsid, s3_clim0_wsid, by ="wsid")
# rescale wsid weight
s3_rs <- as.data.frame(rescale(s3_clim0_wsid_hn$s3))
names(s3_rs)[1] <- "s3_rs"
summary(s3_rs)
# join the rescale wsid weight to hn wsid
s3_clim0_wsid_rs <- cbind(s3_clim0_wsid_hn, s3_rs)
# Write the csv file
write.csv(s3_clim0_wsid_rs, file.path("09_wsid_prioritization/6_wsid_weight_rescale", paste(country, scenario, "wsid_weight.csv" ,sep="_")), row.names = F)
# add wsid_weight to shapefile
hn_wsid_shp <- left_join(wsid_hn, s3_clim0_wsid_rs, 'wsid', 'wsid')
names(hn_wsid_shp)
# export as shp
wsid_sp <- as(hn_wsid_shp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="hn_s3_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))


##### E/ MERGE LAND-SEA INDEX FOR ALL SCENARIOS #####

# import the watershed weights
s1_bz <- st_read("09_wsid_prioritization/7_wsid_weight_shp/bz_s1_clim0_wsid_weight.shp")
s1_gt <- st_read("09_wsid_prioritization/7_wsid_weight_shp/gt_s1_clim0_wsid_weight.shp")
s1_hn <- st_read("09_wsid_prioritization/7_wsid_weight_shp/hn_s1_clim0_wsid_weight.shp")
s2_bz <- st_read("09_wsid_prioritization/7_wsid_weight_shp/bz_s2_clim0_wsid_weight.shp")
s2_gt <- st_read("09_wsid_prioritization/7_wsid_weight_shp/gt_s2_clim0_wsid_weight.shp")
s2_hn <- st_read("09_wsid_prioritization/7_wsid_weight_shp/hn_s2_clim0_wsid_weight.shp")
s3_bz <- st_read("09_wsid_prioritization/7_wsid_weight_shp/bz_s3_clim0_wsid_weight.shp")
s3_gt <- st_read("09_wsid_prioritization/7_wsid_weight_shp/gt_s3_clim0_wsid_weight.shp")
s3_hn <- st_read("09_wsid_prioritization/7_wsid_weight_shp/hn_s3_clim0_wsid_weight.shp")

sX_mar <- st_read("00_aoi_watersheds/mar_wsid.shp")

# combine BZ, GT & HN
mar_s1 <- rbind(s1_bz, s1_gt, s1_hn)
mar_s1_sp <- as(mar_s1, "Spatial")
class(mar_s1_sp)
writeOGR(obj=mar_s1_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="mar_s1_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

mar_s2 <- rbind(s2_bz, s2_gt, s2_hn)
mar_s2_sp <- as(mar_s2, "Spatial")
class(mar_s2_sp)
writeOGR(obj=mar_s2_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="mar_s2_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

mar_s3 <- rbind(s3_bz, s3_gt, s3_hn)
mar_s3_sp <- as(mar_s3, "Spatial")
class(mar_s3_sp)
writeOGR(obj=mar_s3_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="mar_s3_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

mar_sx <- cbind (mar_s1, mar_s2, mar_s3)
names(mar_sx)
mar_sx <- mar_sx[,c(1:5,7:8,15:16,23:25)]
names(mar_sx)
mar_sx_sp <- as(mar_sx, "Spatial")
class(mar_sx_sp)
writeOGR(obj=mar_sx_sp, dsn="E:/Smart_Coast_project/0_paper/2_R2R_paper/Data/watershed_links", layer="mar_sx_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)
writeOGR(obj=mar_sx_sp, dsn="09_wsid_prioritization/7_wsid_weight_shp", layer="mar_sx_clim0_wsid_weight", driver="ESRI Shapefile", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

#### 7/ BZ - rescale the marine services ####
setwd("E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex")

# function to rescale between 0-1 
rescale_pos <- function(x) (x-min(x))/(max(x) - min(x))
rescale_neg <- function(x) (x-max(x))/(min(x) - max(x))

# import marine SDU w ES values
bz_root_hex_mes <- st_read("mar_bz_root_hex_mes_dlt.shp")

# convert to dataframe
bz_root_hex_mes_df <- as.data.frame(bz_root_hex_mes)
names(bz_root_hex_mes_df)
# Write the csv file
write.csv(bz_root_hex_mes_df, file.path("E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex", paste("bz_root_hex_mes.csv")), row.names = F)

# import the table with the aggregated values
bz_root_hex_mes_df <- read.csv("bz_root_hex_mes.csv", header = TRUE, sep = ",")

# rescale targ
bz_root_hex_mes_df$s1_targ_rs <- rescale_pos(bz_root_hex_mes_df$s1_targ)
names(bz_root_hex_mes_df)[12] <- "s1_targ_rs"
summary(bz_root_hex_mes_df)
bz_root_hex_mes_df$s2_targ_rs <- rescale_pos(bz_root_hex_mes_df$s2_targ)
names(bz_root_hex_mes_df)[13] <- "s2_targ_rs"
summary(bz_root_hex_mes_df)
bz_root_hex_mes_df$s3_targ_rs <- rescale_pos(bz_root_hex_mes_df$s3_targ)
names(bz_root_hex_mes_df)[14] <- "s3_targ_rs"
summary(bz_root_hex_mes_df)

# rescale cv
bz_root_hex_mes_df$s1_cv_rs <- rescale_pos(bz_root_hex_mes_df$s1_cv)
names(bz_root_hex_mes_df)[15] <- "s1_cv_rs"
summary(bz_root_hex_mes_df)
bz_root_hex_mes_df$s2_cv_rs <- rescale_pos(bz_root_hex_mes_df$s2_cv)
names(bz_root_hex_mes_df)[16] <- "s2_cv_rs"
summary(bz_root_hex_mes_df)
bz_root_hex_mes_df$s3_cv_rs <- rescale_pos(bz_root_hex_mes_df$s3_cv)
names(bz_root_hex_mes_df)[17] <- "s3_cv_rs"
summary(bz_root_hex_mes_df)

# rescale rec
bz_root_hex_mes_df$s1_rec_rs <- rescale_pos(bz_root_hex_mes_df$s1_rec)
names(bz_root_hex_mes_df)[18] <- "s1_rec_rs"
summary(bz_root_hex_mes_df)
bz_root_hex_mes_df$s2_rec_rs <- rescale_pos(bz_root_hex_mes_df$s2_rec)
names(bz_root_hex_mes_df)[19] <- "s2_rec_rs"
summary(bz_root_hex_mes_df)
bz_root_hex_mes_df$s3_rec_rs <- rescale_pos(bz_root_hex_mes_df$s3_rec)
names(bz_root_hex_mes_df)[20] <- "s3_rec_rs"
summary(bz_root_hex_mes_df)

# delete first column
bz_root_hex_mes_rs <- bz_root_hex_mes_df[ -c(2:11) ]

# Write the csv file
write.csv(bz_root_hex_mes_rs, file.path(paste("bz_root_hex_mes_rs.csv")), row.names = F)

# import the table with the rescaled values
bz_root_hex_mes_rs <- read.csv("bz_root_hex_mes_rs.csv", header = TRUE, sep = ",")

# join sum_table & agreement map shapefile
bz_root_hex_mes <- geo_join(bz_root_hex_mes, bz_root_hex_mes_rs, 'SDU_ID', 'SDU_ID', how = 'left')
names(bz_root_hex_mes)

# export as shp
bz_root_hex_mes_sp <- as(bz_root_hex_mes, "Spatial")
class(bz_root_hex_mes_sp)
writeOGR(obj=bz_root_hex_mes_sp, dsn="E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex", layer="bz_root_hex_mes_rs", driver="ESRI Shapefile", overwrite=TRUE)


#### 7/ GT - rescale the marine services ####
setwd("E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex")

# function to rescale between 0-1 
rescale_pos <- function(x) (x-min(x))/(max(x) - min(x))
rescale_neg <- function(x) (x-max(x))/(min(x) - max(x))

# import marine SDU w ES values
gt_root_hex_mes <- st_read("mar_gt_root_hex_mes_dlt.shp")

# convert to dataframe
gt_root_hex_mes_df <- as.data.frame(gt_root_hex_mes)
names(gt_root_hex_mes_df)
# Write the csv file
write.csv(gt_root_hex_mes_df, file.path("E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex", paste("gt_root_hex_mes.csv")), row.names = F)

# import the table with the aggregated values
gt_root_hex_mes_df <- read.csv("gt_root_hex_mes.csv", header = TRUE, sep = ",")

# rescale targ
gt_root_hex_mes_df$s1_targ_rs <- rescale_pos(gt_root_hex_mes_df$s1_targ)
names(gt_root_hex_mes_df)[12] <- "s1_targ_rs"
summary(gt_root_hex_mes_df)
gt_root_hex_mes_df$s2_targ_rs <- rescale_pos(gt_root_hex_mes_df$s2_targ)
names(gt_root_hex_mes_df)[13] <- "s2_targ_rs"
summary(gt_root_hex_mes_df)
gt_root_hex_mes_df$s3_targ_rs <- rescale_pos(gt_root_hex_mes_df$s3_targ)
names(gt_root_hex_mes_df)[14] <- "s3_targ_rs"
summary(gt_root_hex_mes_df)

# rescale cv
gt_root_hex_mes_df$s1_cv_rs <- rescale_pos(gt_root_hex_mes_df$s1_cv)
names(gt_root_hex_mes_df)[15] <- "s1_cv_rs"
summary(gt_root_hex_mes_df)
gt_root_hex_mes_df$s2_cv_rs <- rescale_pos(gt_root_hex_mes_df$s2_cv)
names(gt_root_hex_mes_df)[16] <- "s2_cv_rs"
summary(gt_root_hex_mes_df)
gt_root_hex_mes_df$s3_cv_rs <- rescale_pos(gt_root_hex_mes_df$s3_cv)
names(gt_root_hex_mes_df)[17] <- "s3_cv_rs"
summary(gt_root_hex_mes_df)

# rescale rec
gt_root_hex_mes_df$s1_rec_rs <- rescale_pos(gt_root_hex_mes_df$s1_rec)
names(gt_root_hex_mes_df)[18] <- "s1_rec_rs"
summary(gt_root_hex_mes_df)
gt_root_hex_mes_df$s2_rec_rs <- rescale_pos(gt_root_hex_mes_df$s2_rec)
names(gt_root_hex_mes_df)[19] <- "s2_rec_rs"
summary(gt_root_hex_mes_df)
gt_root_hex_mes_df$s3_rec_rs <- rescale_pos(gt_root_hex_mes_df$s3_rec)
names(gt_root_hex_mes_df)[20] <- "s3_rec_rs"
summary(gt_root_hex_mes_df)

# delete first column
gt_root_hex_mes_rs <- gt_root_hex_mes_df[ -c(2:11) ]

# Write the csv file
write.csv(gt_root_hex_mes_rs, file.path(paste("gt_root_hex_mes_rs.csv")), row.names = F)

# import the table with the rescaled values
gt_root_hex_mes_rs <- read.csv("gt_root_hex_mes_rs.csv", header = TRUE, sep = ",")

# join sum_table & agreement map shapefile
gt_root_hex_mes <- geo_join(gt_root_hex_mes, gt_root_hex_mes_rs, 'SDU_ID', 'SDU_ID', how = 'left')
names(gt_root_hex_mes)

# export as shp
gt_root_hex_mes_sp <- as(gt_root_hex_mes, "Spatial")
class(gt_root_hex_mes_sp)
writeOGR(obj=gt_root_hex_mes_sp, dsn="E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex", layer="gt_root_hex_mes_rs", driver="ESRI Shapefile", overwrite=TRUE)


#### 8/ HN - rescale the marine services ####
setwd("E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex")

# function to rescale between 0-1 
rescale_pos <- function(x) (x-min(x))/(max(x) - min(x))
rescale_neg <- function(x) (x-max(x))/(min(x) - max(x))

# import marine SDU w ES values
hn_root_hex_mes <- st_read("mar_hn_root_hex_mes_dlt.shp")

# convert to dataframe
hn_root_hex_mes_df <- as.data.frame(hn_root_hex_mes)
names(hn_root_hex_mes_df)
# Write the csv file
write.csv(hn_root_hex_mes_df, file.path("E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex", paste("hn_root_hex_mes.csv")), row.names = F)

# import the table with the aggregated values
hn_root_hex_mes_df <- read.csv("hn_root_hex_mes.csv", header = TRUE, sep = ",")

# rescale targ
hn_root_hex_mes_df$s1_targ_rs <- rescale_pos(hn_root_hex_mes_df$s1_targ)
names(hn_root_hex_mes_df)[14] <- "s1_targ_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s2_targ_rs <- rescale_pos(hn_root_hex_mes_df$s2_targ)
names(hn_root_hex_mes_df)[15] <- "s2_targ_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s3_targ_rs <- rescale_pos(hn_root_hex_mes_df$s3_targ)
names(hn_root_hex_mes_df)[16] <- "s3_targ_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s4_targ_rs <- rescale_pos(hn_root_hex_mes_df$s4_targ)
names(hn_root_hex_mes_df)[17] <- "s4_targ_rs"
summary(hn_root_hex_mes_df)

# rescale cv
hn_root_hex_mes_df$s1_cv_rs <- rescale_pos(hn_root_hex_mes_df$s1_cv)
names(hn_root_hex_mes_df)[18] <- "s1_cv_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s2_cv_rs <- rescale_pos(hn_root_hex_mes_df$s2_cv)
names(hn_root_hex_mes_df)[19] <- "s2_cv_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s3_cv_rs <- rescale_pos(hn_root_hex_mes_df$s3_cv)
names(hn_root_hex_mes_df)[20] <- "s3_cv_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s4_cv_rs <- rescale_pos(hn_root_hex_mes_df$s4_cv)
names(hn_root_hex_mes_df)[21] <- "s4_cv_rs"
summary(hn_root_hex_mes_df)

# rescale rec
hn_root_hex_mes_df$s1_rec_rs <- rescale_pos(hn_root_hex_mes_df$s1_rec)
names(hn_root_hex_mes_df)[22] <- "s1_rec_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s2_rec_rs <- rescale_pos(hn_root_hex_mes_df$s2_rec)
names(hn_root_hex_mes_df)[23] <- "s2_rec_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s3_rec_rs <- rescale_pos(hn_root_hex_mes_df$s3_rec)
names(hn_root_hex_mes_df)[24] <- "s3_rec_rs"
summary(hn_root_hex_mes_df)
hn_root_hex_mes_df$s4_rec_rs <- rescale_pos(hn_root_hex_mes_df$s4_rec)
names(hn_root_hex_mes_df)[25] <- "s4_rec_rs"
summary(hn_root_hex_mes_df)

# delete first column
hn_root_hex_mes_rs <- hn_root_hex_mes_df[ -c(2:13) ]

# Write the csv file
write.csv(hn_root_hex_mes_rs, file.path(paste("hn_root_hex_mes_rs.csv")), row.names = F)

# import the table with the rescaled values
hn_root_hex_mes_rs <- read.csv("hn_root_hex_mes_rs.csv", header = TRUE, sep = ",")

# join sum_table & agreement map shapefile
hn_root_hex_mes <- geo_join(hn_root_hex_mes, hn_root_hex_mes_rs, 'SDU_ID', 'SDU_ID', how = 'left')
names(hn_root_hex_mes)

# export as shp
hn_root_hex_mes_sp <- as(hn_root_hex_mes, "Spatial")
class(hn_root_hex_mes_sp)
writeOGR(obj=hn_root_hex_mes_sp, dsn="E:/Smart_Coast_project/5_watershed_importance/08_marine_es_accouting/2_root_hex", layer="hn_root_hex_mes_rs", driver="ESRI Shapefile", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))
