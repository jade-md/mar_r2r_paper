# Created by Jade D. 
# 2020APR03

##### Packages #####
library(sf)
library(GISTools)
library(maptools)
library(foreign)
library(raster)
library(rgdal)
library(dplyr)
library(exactextractr) # quick zonal stat
library(gdalUtils)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(rgeos)
library(sp)


##### A/ SDR INPUTS SECTION #####

###### 1/ Link the wsid between pourpoints & watersheds shp ####
setwd("E:/0_NATCAP_MAR/1_SDR_MAR/0_base_data")

# import watershed unique ID
gt_ws_id <- read.table("ws_id_gt.csv",  stringsAsFactors = T, header = T)
# add leading 000
gt_ws_id$wsid <- sprintf("%04d", gt_ws_id$ws_id)
# import pourpoints
gt_ppt_srtm_30m <- st_read("gt_wsid_ppt_srtm_30m.shp")
# merge pourpoints with watershed unique id in text format
wsid_ppt_srtm_30m_belize <- merge(gt_ppt_srtm_30m, gt_ws_id, by='ws_id')
# export ppt shp
st_write(wsid_ppt_srtm_30m_belize, "gt_wsid_ppt_srtm_30m.shp", driver = "ESRI Shapefile")
# import watersheds
gt_ws_srtm_30m <- st_read("gt_wsid_srtm_30m.shp")
# merge watersheds with watershed unique id in text format
gt_ws_srtm_30m <- merge(gt_ws_srtm_30m, ws_id_belize, by='ws_id')
# export shp
st_write(gt_ws_srtm_30m, "gt_wsid_srtm_30m.shp", driver = "ESRI Shapefile")


##### 2/ Calculate rainfall erosivity (30m) #####
# Make sure the DEM and annual rainfall rasters have the same number of rows and columns
dem <- raster("E:/Smart_Coast_project/1_sdr_gt/0_base_data/1_dem/gt_dem_srtm_30m.tif")
NAvalue(dem) <- -9999
rain_clim0 <- raster("E:/Smart_Coast_project/1_sdr_gt/0_base_data/3_rainfall/rainfall_30m_annual/rainfall_30m_annual_clim0.tif")
NAvalue(rain_clim0) <- -9999
rain_clim1 <- raster("E:/Smart_Coast_project/1_sdr_gt/0_base_data/3_rainfall/rainfall_30m_annual/rainfall_30m_annual_clim1.tif")
NAvalue(rain_clim1) <- -9999
rain_clim2 <- raster("E:/Smart_Coast_project/1_sdr_gt/0_base_data/3_rainfall/rainfall_30m_annual/rainfall_30m_annual_clim2.tif")
NAvalue(rain_clim2) <- -9999
setwd("E:/Smart_Coast_project/1_sdr_gt/0_base_data/3_rainfall/rainfall_30m_erosivity")

# Current climate
rain_erosivity_clim0 <- ((3786.6+(1.5679*rain_clim0))-(1.9809*dem))
writeRaster(rain_erosivity_clim0, paste0('rain_erosivity_clim0.tif'), format="GTiff", overwrite=T)
# Climate RCP8.5 25pct
rain_erosivity_clim1 <- ((3786.6+(1.5679*rain_clim1))-(1.9809*dem))
writeRaster(rain_erosivity_clim1, paste0('rain_erosivity_clim1.tif'), format="GTiff", overwrite=T)
# Climate RCP8.5 75pct
rain_erosivity_clim2 <- ((3786.6+(1.5679*rain_clim2))-(1.9809*dem))
writeRaster(rain_erosivity_clim2, paste0('rain_erosivity_clim2.tif'), format="GTiff", overwrite=T)

# clear working environment
rm(list = ls(all.names=TRUE))


##### B/ SDR POST-PROCESSING - BASELINE LUC ~ CURRENT CLIMATE #####
setwd("E:/Smart_Coast_project/1_sdr_gt")

#### 0/ Import SDR results ####
# Create directory
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim"), showWarning=TRUE, recursive=TRUE)

# baseline 
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/3_SDR_outputs_raw/s0_clim0/sed_export.tif")
crs(sxp_s0_clim0_yr) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(sxp_s0_clim0_yr, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/sxp_s0_clim0_yr.tif", overwrite=TRUE)

#### 1/ Aggregate the sxp by watershed ####
# import wsid zones
wsid_sxp <- st_read("E:/Smart_Coast_project/1_sdr_gt/0_base_data/2_watersheds/gt_wsid_srtm_30m.shp")
# run zonal stat on watershed boundaries for annual results
wsid_sxp$clim0_yr <- exact_extract(sxp_s0_clim0_yr, wsid_sxp, 'sum')
wsid_sxp$clim0_yr <- as.integer(wsid_sxp$clim0_yr) 
# export shp
setwd("E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol")
wsid_sp <- as(wsid_sxp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol", layer="gt_s0_clim0_sxp_yr", driver="ESRI Shapefile")

# clear working environment
rm(list = ls(all.names=TRUE))


##### B/ SDR POST-PROCESSING - BASELINE LUC ~ CLIMATE CHANGE SCENARIOS #####
setwd("E:/Smart_Coast_project/1_sdr_gt")

#### 0/ Import SDR results ####
# Create directory
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim"), showWarning=TRUE, recursive=TRUE)

# climate 1
sxp_s0_clim1_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/3_SDR_outputs_raw/s0_clim1/sed_export.tif")
crs(sxp_s0_clim1_yr) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(sxp_s0_clim1_yr, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/sxp_s0_clim1_yr.tif", overwrite=TRUE)

# climate 2
sxp_s0_clim2_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/3_SDR_outputs_raw/s0_clim2/sed_export.tif")
crs(sxp_s0_clim2_yr) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(sxp_s0_clim2_yr, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/sxp_s0_clim2_yr.tif", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

#### 1/ Calculate the sxp change by pixel ####
# import SDR outputs (annual and seasonal for 3 climate change scenarios)
# Annual current luc + current climate
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/sxp_s0_clim0_yr.tif")
# Annual current luc + RCP8.5 25pct climate
sxp_s0_clim1_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/sxp_s0_clim1_yr.tif")
# Annual current luc + RCP8.5 75pct climate
sxp_s0_clim2_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/sxp_s0_clim2_yr.tif")

# Create directory for annual delta
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta"), showWarning=TRUE, recursive=TRUE)

# Calculate the annual delta
sxp_s0_clim1_yr_delta <- sxp_s0_clim1_yr - sxp_s0_clim0_yr
writeRaster(sxp_s0_clim1_yr_delta, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta/sxp_s0_clim1_yr_delta.tif", overwrite=TRUE)
sxp_s0_clim2_yr_delta <- sxp_s0_clim2_yr - sxp_s0_clim0_yr
writeRaster(sxp_s0_clim2_yr_delta, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta/sxp_s0_clim2_yr_delta.tif", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))


#### 2/ Format & save the raster as the impact potential raster ####
# import delta rasters per year
setwd("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta")
rlist=list.files(getwd(), pattern="tif$", full.names=FALSE)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

# Create directory for annual delta
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/9_ROOT_ipm/s0_clim"), showWarning=TRUE, recursive=TRUE)
# set wd
setwd("E:/Smart_Coast_project/1_sdr_gt/9_ROOT_ipm/s0_clim")

# annual
writeRaster(sxp_s0_clim1_yr_delta, filename="ipm_00_sxp_clim1_yr.tif", overwrite=TRUE)
writeRaster(sxp_s0_clim2_yr_delta, filename="ipm_00_sxp_clim2_yr.tif", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

#### 3/ Calculate the sxp % change by pixel ####
# import SDR outputs (annual and seasonal for 3 climate change scenarios)
# Annual current luc + current climate
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim0_yr.tif")
# Annual current luc + RCP8.5 25pct climate
sxp_s0_clim1_yr_delta <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta/gt_sxp_s0_clim1_yr_delta.tif")
# Annual current luc + RCP8.5 75pct climate
sxp_s0_clim2_yr_delta <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta/gt_sxp_s0_clim2_yr_delta.tif")

# Create directory for annual delta
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta_pct"), showWarning=TRUE, recursive=TRUE)

# Calculate the annual delta
sxp_s0_clim1_yr_delta_pct <- (sxp_s0_clim1_yr_delta/abs(sxp_s0_clim0_yr))*100
writeRaster(sxp_s0_clim1_yr_delta_pct, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta_pct/gt_sxp_s0_clim1_yr_delta_pct.tif", overwrite=TRUE)
sxp_s0_clim2_yr_delta_pct <- (sxp_s0_clim2_yr_delta/abs(sxp_s0_clim0_yr))*100
writeRaster(sxp_s0_clim2_yr_delta_pct, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim_delta_pct/gt_sxp_s0_clim2_yr_delta_pct.tif", overwrite=TRUE)

#### 4/ Aggregate the sxp absolute, delta, and pct change by watershed ####
# import SDR outputs (annual and seasonal for 3 climate change scenarios)
# Annual current luc + current climate
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim0_yr.tif")
# Annual current luc + RCP8.5 25pct climate
sxp_s0_clim1_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim1_yr.tif")
# Annual current luc + RCP8.5 75pct climate
sxp_s0_clim2_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim2_yr.tif")

# import wsid zones
wsid_sxp <- st_read("E:/Smart_Coast_project/1_sdr_gt/0_base_data/2_watersheds/gt_wsid_srtm_30m.shp")

# run zonal stat on watershed boundaries for annual results
# current luc + current climate
wsid_sxp$clim0_yr <- exact_extract(sxp_s0_clim0_yr, wsid_sxp, 'sum')
wsid_sxp$clim0_yr <- as.integer(wsid_sxp$clim0_yr) 
# current luc + RCP8.5 25pct climate
wsid_sxp$clim1_yr <- exact_extract(sxp_s0_clim1_yr, wsid_sxp, 'sum')
wsid_sxp$clim1_yr <- as.integer(wsid_sxp$clim1_yr) 
# current luc + RCP8.5 75pct climate
wsid_sxp$clim2_yr <- exact_extract(sxp_s0_clim2_yr, wsid_sxp, 'sum')
wsid_sxp$clim2_yr <- as.integer(wsid_sxp$clim2_yr) 

# run zonal stat on watershed boundaries for annual delta
# current luc + RCP8.5 25pct climate
wsid_sxp$clim1_dlt <- exact_extract(sxp_s0_clim1_yr_delta, wsid_sxp, 'sum')
wsid_sxp$clim1_dlt <- as.integer(wsid_sxp$clim1_dlt) 
# current luc + RCP8.5 75pct climate
wsid_sxp$clim2_dlt <- exact_extract(sxp_s0_clim2_yr_delta, wsid_sxp, 'sum')
wsid_sxp$clim2_dlt <- as.integer(wsid_sxp$clim2_dlt) 

# calculate pct change per year
wsid_sxp$clim1_pct <- (wsid_sxp$clim1_dlt/abs(wsid_sxp$clim0_yr))*100
wsid_sxp$clim1_pct[is.na(wsid_sxp$clim1)] <- 0
wsid_sxp$clim2_pct <- (wsid_sxp$clim2_dlt/abs(wsid_sxp$clim0_yr))*100
wsid_sxp$clim2_pct[is.na(wsid_sxp$clim2)] <- 0

# export shp
setwd("E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol")
wsid_sp <- as(wsid_sxp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol", layer="gt_s0_clim_sxp_delta_pct", driver="ESRI Shapefile", overwrite_layer = T)

# clear working environment
rm(list = ls(all.names=TRUE))


##### C/ SDR OUTPUTS POST-PROCESSING - BASELINE ~ S1 RESTORE WATERSHED #####
setwd("E:/Smart_Coast_project/1_sdr_gt")

# Create directory
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s1_rest_fors_clim"), showWarning=TRUE, recursive=TRUE)
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s1_rest_fors_clim_delta"), showWarning=TRUE, recursive=TRUE)

#### 0/ Import SDR results ####
# import SDR outputs 
# Baseline
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim0_yr.tif")
# Management n LSI
sxp_s1_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/3_SDR_outputs_raw/s1_rest_fors_clim0/sed_export.tif")
crs(sxp_s1_clim0_yr) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(sxp_s1_clim0_yr, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s1_rest_fors_clim/gt_sxp_s1_rest_fors_clim0_yr.tif", overwrite=TRUE)

#### 1/ Calculate the sxp change by pixel ####
# conditional statement
sxp_s1_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s1_rest_fors_clim/gt_sxp_s1_rest_fors_clim0_yr.tif")
sxp_s1_clim0_yr_delta <- sxp_s1_clim0_yr - sxp_s0_clim0_yr
sxp_s1_clim0_yr_e <- sxp_s1_clim0_yr
sxp_s1_clim0_yr_e[] = ifelse(sxp_s1_clim0_yr_delta[]>0, sxp_s0_clim0_yr[], sxp_s1_clim0_yr[])
# check
sxp_s1_clim0_yr_delta <- sxp_s1_clim0_yr_e-sxp_s0_clim0_yr
plot(sxp_s1_clim0_yr_delta)
writeRaster(sxp_s1_clim0_yr_delta, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/gt_s1_rest_fors_clim_delta/gt_sxp_s1_rest_fors_clim0_yr_delta.tif", overwrite=TRUE)

#### 2/ Aggregate the sxp absolute, delta, and pct change by watershed ####
# import wsid zones
wsid_sxp <- st_read("E:/Smart_Coast_project/1_sdr_gt/0_base_data/2_watersheds/gt_wsid_srtm_30m.shp")

# run zonal stat on watershed boundaries for annual results
wsid_sxp$s0 <- exact_extract(sxp_s0_clim0_yr, wsid_sxp, 'sum')
wsid_sxp$s0 <- as.integer(wsid_sxp$s0) 
wsid_sxp$s1 <- exact_extract(sxp_s1_clim0_yr, wsid_sxp, 'sum')
wsid_sxp$s1 <- as.integer(wsid_sxp$s1) 
# run zonal stat on watershed boundaries for annual delta
wsid_sxp$s1_dlt <- exact_extract(sxp_s1_clim0_yr_delta, wsid_sxp, 'sum')
wsid_sxp$s1_dlt <- as.integer(wsid_sxp$s1_dlt) 
# export shp
setwd("E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol")
wsid_sp <- as(wsid_sxp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol", layer="gt_s1_clim_sxp_delta_pct", driver="ESRI Shapefile", overwrite_layer = T)

# clear working environment
rm(list = ls(all.names=TRUE))

#### 3/ Save the raster as the impact potential raster ####
# import SDR outputs (annual for 3 climate change scenarios)
# Annual current luc + current climate
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim0_yr.tif")
# Annual current luc + current climate
sxp_s1_rest_fors_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s1_rest_fors_clim/gt_sxp_s1_rest_fors_clim0_yr.tif")
# Calculate the annual delta
sxp_s1_rest_fors_clim0_yr_delta <- sxp_s1_rest_fors_clim0_yr - sxp_s0_clim0_yr
# annual
writeRaster(sxp_s1_rest_fors_clim0_yr_delta, filename="ipm_01_rest_fors_sdr_clim0.tif", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

##### D/ SDR OUTPUTS POST-PROCESSING - BASELINE ~ S2 PROTECT WATERSHED #####
setwd("E:/Smart_Coast_project/1_sdr_gt")

# Create directory
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s2_prot_fors_clim"), showWarning=TRUE, recursive=TRUE)
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s2_prot_fors_clim_delta"), showWarning=TRUE, recursive=TRUE)

#### 0/ Import SDR results ####
# import SDR outputs 
# Baseline
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim0_yr.tif")
sxp_s2_clim_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/3_SDR_outputs_raw/s2_prot_fors_clim0/sed_export.tif")
crs(sxp_s2_clim_yr) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(sxp_s2_clim_yr, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s2_prot_fors_clim/gt_sxp_s2_prot_fors_clim0_yr.tif", overwrite=TRUE)

#### 1/ Calculate the sxp change by pixel ####
# conditional statement
sxp_s2_clim_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s2_prot_fors_clim/gt_sxp_s2_prot_fors_clim0_yr.tif")
sxp_s2_clim_yr_delta <- sxp_s2_clim_yr - sxp_s0_clim0_yr
sxp_s2_clim_yr_e <- sxp_s2_clim_yr
sxp_s2_clim_yr_e[] = ifelse(sxp_s2_clim_yr_delta[]<0, sxp_s0_clim0_yr[], sxp_s2_clim_yr[])
# check
sxp_s2_clim_yr_delta <- sxp_s2_clim_yr_e-sxp_s0_clim0_yr
plot(sxp_s2_clim_yr_delta)
writeRaster(sxp_s2_clim_yr_delta, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s2_prot_fors_clim_delta/gt_sxp_s2_prot_fors_clim0_yr_delta.tif", overwrite=TRUE)

#### 2/ Aggregate the sxp absolute, delta, and pct change by watershed ####
# import wsid zones
wsid_sxp <- st_read("E:/Smart_Coast_project/1_sdr_gt/0_base_data/2_watersheds/gt_wsid_srtm_30m.shp")

# run zonal stat on watershed boundaries for annual results
wsid_sxp$s0 <- exact_extract(sxp_s0_clim0_yr, wsid_sxp, 'sum')
wsid_sxp$s0 <- as.integer(wsid_sxp$s0) 
wsid_sxp$s2 <- exact_extract(sxp_s2_clim_yr, wsid_sxp, 'sum')
wsid_sxp$s2 <- as.integer(wsid_sxp$s2) 
# run zonal stat on watershed boundaries for annual delta
wsid_sxp$s2_dlt <- exact_extract(sxp_s2_clim_yr_delta, wsid_sxp, 'sum')
wsid_sxp$s2_dlt <- as.integer(wsid_sxp$s2_dlt) 
# export shp
setwd("E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol")
wsid_sp <- as(wsid_sxp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol", layer="gt_s2_clim_sxp_delta_pct", driver="ESRI Shapefile", overwrite_layer = T)

# clear working environment
rm(list = ls(all.names=TRUE))


##### E/ SDR OUTPUTS POST-PROCESSING - BASELINE ~ S3 SUSTAINABLE AGRICULTURE #####
setwd("E:/Smart_Coast_project/1_sdr_gt")

# Create directory
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s3_sust_agri_clim"), showWarning=TRUE, recursive=TRUE)
dir.create(file.path("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s3_sust_agri_clim_delta"), showWarning=TRUE, recursive=TRUE)

#### 0/ Import SDR results ####
# import SDR outputs 
# Baseline
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim0_yr.tif")
# Management n LSI
sxp_s3_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/3_SDR_outputs_raw/s3_sust_agri_clim0/sed_export.tif")
crs(sxp_s3_clim0_yr) <- "+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs"
writeRaster(sxp_s3_clim0_yr, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s3_sust_agri_clim/gt_sxp_s3_sust_agri_clim0_yr.tif", overwrite=TRUE)

#### 1/ Calculate the sxp change by pixel ####
# conditional statement
sxp_s3_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s3_sust_agri_clim/gt_sxp_s3_sust_agri_clim0_yr.tif")
sxp_s3_clim0_yr_delta <- sxp_s3_clim0_yr - sxp_s0_clim0_yr
sxp_s3_clim0_yr_e <- sxp_s3_clim0_yr
sxp_s3_clim0_yr_e[] = ifelse(sxp_s3_clim0_yr_delta[]>0, sxp_s0_clim0_yr[], sxp_s3_clim0_yr[])
# check
sxp_s3_clim0_yr_delta <- sxp_s3_clim0_yr_e-sxp_s0_clim0_yr
plot(sxp_s3_clim0_yr_delta)
writeRaster(sxp_s3_clim0_yr_delta, filename="E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s3_sust_agri_clim_delta/gt_sxp_s3_sust_agri_clim0_yr_delta.tif", overwrite=TRUE)

#### 2/ Aggregate the sxp absolute, delta, and pct change by watershed ####
# import wsid zones
wsid_sxp <- st_read("E:/Smart_Coast_project/1_sdr_gt/0_base_data/2_watersheds/gt_wsid_srtm_30m.shp")

# run zonal stat on watershed boundaries for annual results
wsid_sxp$s0 <- exact_extract(sxp_s0_clim0_yr, wsid_sxp, 'sum')
wsid_sxp$s0 <- as.integer(wsid_sxp$s0) 
wsid_sxp$s3 <- exact_extract(sxp_s3_clim0_yr, wsid_sxp, 'sum')
wsid_sxp$s3 <- as.integer(wsid_sxp$s3) 
# run zonal stat on watershed boundaries for annual delta
wsid_sxp$s3_dlt <- exact_extract(sxp_s3_clim0_yr_delta, wsid_sxp, 'sum')
wsid_sxp$s3_dlt <- as.integer(wsid_sxp$s3_dlt) 
# export shp
setwd("E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol")
wsid_sp <- as(wsid_sxp, "Spatial")
class(wsid_sp)
writeOGR(obj=wsid_sp, dsn="E:/Smart_Coast_project/1_sdr_gt/8_SDR_outputs_ws_pol", layer="gt_s3_clim_sxp_delta_pct", driver="ESRI Shapefile", overwrite_layer = T)

# clear working environment
rm(list = ls(all.names=TRUE))

#### 3/ Save the raster as the impact potential raster ####
# import SDR outputs (annual for 3 climate change scenarios)
# Annual current luc + current climate
sxp_s0_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s0_clim/gt_sxp_s0_clim0_yr.tif")
# Annual current luc + current climate
sxp_s3_sust_agri_clim0_yr <- raster("E:/Smart_Coast_project/1_sdr_gt/5_SDR_outputs_annual/s3_sust_agri_clim/gt_sxp_s3_sust_agri_clim0_yr.tif")
# Calculate the annual delta
sxp_s3_sust_agri_clim0_yr_delta <- sxp_s3_sust_agri_clim0_yr - sxp_s0_clim0_yr
# annual
writeRaster(sxp_s3_sust_agri_clim0_yr_delta, filename="ipm_01_rest_fors_sdr_clim0.tif", overwrite=TRUE)

# clear working environment
rm(list = ls(all.names=TRUE))

