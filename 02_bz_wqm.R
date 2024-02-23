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
library(MASS)
#library(parallel)
#library(foreach)
# library(doParallel)
#library(doFuture)
#registerDoFuture()
#plan(multiprocess)

##### A/ WQ MODEL INPUTS PREPARATION #####

##### 1/ Import pourpoints #####
setwd("E:/0_NATCAP_MAR/2_WQ_MAR")
# Import ppt
ppt_bz <- st_read("E:/0_NATCAP_MAR/2_WQ_MAR/00_base_data/01_pourpoints/bz_ppt_srtm_30m.shp")

#### 2/ Split the pourpoints by watershed ##### 
setwd("E:/0_NATCAP_MAR/2_WQ_MAR/01_source/bz")
for (i in seq_len (nrow (ppt_bz)))
{
  fname <- paste0 (ppt_bz$wsid[i], ".shp")
  write_sf (ppt_bz [i, ], fname)
}


##### B/ WQ MODEL - BASELINE ######
setwd("E:/0_NATCAP_MAR/2_WQ_MAR")

# create the directory links
dir.create(file.path("05_sed_ppt_decay/0_baseline/bz/"), showWarning=TRUE, recursive=TRUE)
tmp_dir<- "C:/Users/jademd/AppData/Local/Temp/Rtmpshzte8"

# AOI raster
wq_model_aoi <- raster('00_base_data/00_wq_model_aoi/wq_model_aoi.tif')
plot(wq_model_aoi)

# import SDR results by watershed
bz_wsid_sxp <- st_read("E:/0_NATCAP_MAR/1_SDR_BZ/8_SDR_outputs_ws_pol/bz_s0_clim0_sxp_yr.shp")
# remove the non-watershed rows
wsid_sxp <- bz_wsid_sxp[bz_wsid_sxp$type == "watershed",]
names(wsid_sxp)

#Create list of cost distance raster file names
cd_list <- list.files(file.path("03_distance_rasters/bz/"), pattern='.tif$')
cd_list

# Create a list of wsid
wsid <- as.character(wsid_sxp$wsid)

# Diffuse the sediment in the marine environment based on decay function & cost rasters
# Apply a non linear decay function to the pourpoints sdx
setwd("E:/0_NATCAP_MAR/2_WQ_MAR/")
for(i in 1:length(wsid)) {
  load_name <- wq_model_aoi*wsid_sxp$clim0_yr[i]
  cd <- raster(file.path("E:/0_NATCAP_MAR/2_WQ_MAR/03_distance_rasters/bz/", cd_list[i]))
  plume_name <- load_name*(exp(-1*(cd^2)/5000000))
  plume_name [plume_name < 1] = 0
  plot(plume_name, main=wsid[i])
  writeRaster(plume_name, filename=file.path("05_sed_ppt_decay/0_baseline/bz", paste("plume", wsid[i], sep="_")), format="GTiff", overwrite=TRUE)
  file.remove(dir(tmp_dir, pattern = ".grd$", full.names = TRUE, recursive=TRUE))
  file.remove(dir(tmp_dir, pattern = ".gri$", full.names = TRUE, recursive=TRUE))
}

#Create list of file names
plume_list <- list.files(file.path("05_sed_ppt_decay/0_baseline/bz/"), pattern='.tif$')
plume_list

# AOI to build the plume 
plume_sum <- wq_model_aoi*0
plot(plume_sum)

# Sum all the plumes to obtain the final plume raster
for (i in 1:length(plume_list)){
  assign(plume_list[i], raster(file.path("05_sed_ppt_decay/0_baseline/bz/", plume_list[i])))
  plume_sum <- plume_sum + get(plume_list[i])
  rm(list=plume_list[i])
}
plot(plume_sum)
# trim plume ends
plume_sum [plume_sum < 1] = 0
writeRaster(plume_sum, filename="06_sed_plume/0_baseline_by_country/bz_plume_s0_clim0.tif", overwrite=T)

# clear working environment
rm(list = ls(all.names=TRUE))


##### D/ WQ MODEL - S1 RESTORE WATERSHED SCENARIOS ######

#### 1a/ Plume model - Restore watershed x Climate 0 (absolute)######
setwd("E:/0_NATCAP_MAR/2_WQ_MAR/")

# create the directory links
dir.create(file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0_abs"), showWarning=TRUE, recursive=TRUE)
tmp_dir<- "C:/Users/jademd/AppData/Local/Temp/RtmpMBlBS1"

# AOI raster
wq_model_aoi <- raster('00_base_data/00_wq_model_aoi/wq_model_aoi.tif')
plot(wq_model_aoi)

# import SDR results by watershed
bz_wsid_sxp <- st_read("E:/0_NATCAP_MAR/1_SDR_BZ/8_SDR_outputs_ws_pol/bz_s1_rest_fors_sxp_yr.shp")
# remove the non-watershed rows
wsid_sxp <- bz_wsid_sxp[bz_wsid_sxp$type == "watershed",]
names(wsid_sxp)

#Create list of cost distance raster file names
cd_list <- list.files(file.path("03_distance_rasters/bz/"), pattern='.tif$')
cd_list

# Create a list of wsid
wsid <- as.character(wsid_sxp$wsid)

# Apply a non linear decay function to the pourpoints sdx and calculate the delta
for(i in 1:length(wsid)) {
  # get the sxp load from SDR
  load <- wq_model_aoi*wsid_sxp$clim0_yr[i]
  # get the cost distance raster for the given watershed/pourpoint
  cd <- raster(file.path("03_distance_rasters/bz/", cd_list[i]))
  # model the watershed-scale plume
  plume_1 <- load*(exp(-1*(cd^2)/5000000))
  # trim plume ends
  plume_1 [plume_1 < 1] = 0
  writeRaster(plume_1, filename=file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0_abs", 
                                          paste("plume", wsid[i], sep="_")), format="GTiff", overwrite=TRUE)
  file.remove(dir(tmp_dir, pattern = ".grd$", full.names = TRUE))
  file.remove(dir(tmp_dir, pattern = ".gri$", full.names = TRUE))
}

# set delta plume AOI
plume_sum <- wq_model_aoi*0
plot(plume_sum)

#Create list of baseline plume raster file names
plume_1_name <- list.files(file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0_abs"), pattern='.tif$')
plume_1_name

# sum the delta plumes
for(i in 1:length(wsid)) {
  # calculate the delta from baseline 
  plume_1 <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0_abs", plume_1_name[i]))
  plume_sum <- plume_sum + plume_1
}
plot(plume_sum)
writeRaster(plume_sum, filename="06_sed_plume/1_scenarios_by_country/bz_plume_s1_clim0.tif", overwrite=T)

# clear working environment
rm(list = ls(all.names=TRUE))


#### 1b/ Plume model - Baseline x Climate 0 ~ Restore watershed x Climate 0  (delta) ######
setwd("E:/0_NATCAP_MAR/2_WQ_MAR/")

# create the directory links
dir.create(file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0"), showWarning=TRUE, recursive=TRUE)
tmp_dir<- "C:/Users/jademd/AppData/Local/Temp/RtmpisGcNJ"

# AOI raster
wq_model_aoi <- raster('00_base_data/00_wq_model_aoi/wq_model_aoi.tif')
plot(wq_model_aoi)

# import SDR results by watershed
bz_wsid_sxp <- st_read("E:/0_NATCAP_MAR/1_SDR_BZ/8_SDR_outputs_ws_pol/bz_s1_rest_fors_sxp_yr.shp")
# remove the non-watershed rows
wsid_sxp <- bz_wsid_sxp[bz_wsid_sxp$type == "watershed",]
names(wsid_sxp)

# Create a list of wsid
wsid <- as.character(wsid_sxp$wsid)

# Create list of baseline plume raster file names
plume_list <- list.files(file.path("05_sed_ppt_decay/0_baseline/bz/"), pattern='.tif$')
plume_list

# Apply a non linear decay function to the pourpoints sdx and calculate the delta
for(i in 1:length(wsid)) {
  # get the baseline plume for the given watershed/pourpoint
  plume_0 <- raster(file.path("05_sed_ppt_decay/0_baseline/bz", plume_list[i]))
  # get the scenario plume for the given watershed/pourpoint
  plume_1 <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0_abs", plume_list[i]))
  # calculate the delta from baseline 
  plume_delta <- plume_1 - plume_0 
  # write raster
  writeRaster(plume_delta, filename=file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0", 
                                              paste("plume_delta", wsid[i], sep="_")), format="GTiff", overwrite=TRUE)
  file.remove(dir(tmp_dir, pattern = ".grd$", full.names = TRUE))
  file.remove(dir(tmp_dir, pattern = ".gri$", full.names = TRUE))
}

# set delta plume AOI
plume_delta_sum <- wq_model_aoi*0
plot(plume_delta_sum)

#Create list of baseline plume raster file names
plume_delta_name <- list.files(file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0"), pattern='.tif$')
plume_delta_name

# sum the delta plumes
for(i in 1:length(wsid)) {
  # calculate the delta from baseline 
  plume_delta <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s1_rest_fors_clim0", plume_delta_name[i]))
  plume_delta_sum <- plume_delta_sum + plume_delta
}
plot(plume_delta_sum)
writeRaster(plume_delta_sum, filename="07_sed_plume_delta/bz/bz_plume_delta_s1_clim0.tif", overwrite=T)

# clear working environment
rm(list = ls(all.names=TRUE))


##### E/ WQ MODEL - S2 PROTECT WATERSHED SCENARIOS ######

#### 1a/ Plume model - Protect watershed x Climate 0 (absolute)######
setwd("E:/0_NATCAP_MAR/2_WQ_MAR/")

# create the directory links
dir.create(file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0_abs"), showWarning=TRUE, recursive=TRUE)
tmp_dir<- "C:/Users/jademd/AppData/Local/Temp/RtmpWyw0LI"

# AOI raster
wq_model_aoi <- raster('00_base_data/00_wq_model_aoi/wq_model_aoi.tif')
plot(wq_model_aoi)

# import SDR results by watershed
bz_wsid_sxp <- st_read("E:/0_NATCAP_MAR/1_SDR_BZ/8_SDR_outputs_ws_pol/bz_s2_prot_fors_sxp_yr.shp")
# remove the non-watershed rows
wsid_sxp <- bz_wsid_sxp[bz_wsid_sxp$type == "watershed",]
names(wsid_sxp)

#Create list of cost distance raster file names
cd_list <- list.files(file.path("03_distance_rasters/bz/"), pattern='.tif$')
cd_list

# Create a list of wsid
wsid <- as.character(wsid_sxp$wsid)

# Apply a non linear decay function to the pourpoints sdx and calculate the delta
for(i in 1:length(wsid)) {
  # get the sxp load from SDR
  load <- wq_model_aoi*wsid_sxp$clim0_yr[i]
  # get the cost distance raster for the given watershed/pourpoint
  cd <- raster(file.path("03_distance_rasters/bz/", cd_list[i]))
  # model the watershed-scale plume
  plume_1 <- load*(exp(-1*(cd^2)/5000000))
  # trim plume ends
  plume_1 [plume_1 < 1] = 0
  writeRaster(plume_1, filename=file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0_abs", 
                                          paste("plume", wsid[i], sep="_")), format="GTiff", overwrite=TRUE)
  file.remove(dir(tmp_dir, pattern = ".grd$", full.names = TRUE))
  file.remove(dir(tmp_dir, pattern = ".gri$", full.names = TRUE))
}

# set delta plume AOI
plume_sum <- wq_model_aoi*0
plot(plume_sum)

#Create list of baseline plume raster file names
plume_1_name <- list.files(file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0_abs"), pattern='.tif$')
plume_1_name

# sum the delta plumes
for(i in 1:length(wsid)) {
  # calculate the delta from baseline 
  plume_1 <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0_abs", plume_1_name[i]))
  plume_sum <- plume_sum + plume_1
}
plot(plume_sum)
writeRaster(plume_sum, filename="06_sed_plume/1_scenarios_by_country/bz_plume_s2_clim0.tif", overwrite=T)

# clear working environment
rm(list = ls(all.names=TRUE))


#### 1b/ Plume model - Baseline x Climate 0 ~ Protect watershed x Climate 0 (delta) ######
setwd("E:/0_NATCAP_MAR/2_WQ_MAR/")

# create the directory links
dir.create(file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0"), showWarning=TRUE, recursive=TRUE)
tmp_dir<- "C:/Users/jademd/AppData/Local/Temp/RtmpisGcNJ"

# AOI raster
wq_model_aoi <- raster('00_base_data/00_wq_model_aoi/wq_model_aoi.tif')
plot(wq_model_aoi)

# import SDR results by watershed
bz_wsid_sxp <- st_read("E:/0_NATCAP_MAR/1_SDR_BZ/8_SDR_outputs_ws_pol/bz_s2_prot_fors_sxp_yr.shp")
# remove the non-watershed rows
wsid_sxp <- bz_wsid_sxp[bz_wsid_sxp$type == "watershed",]
names(wsid_sxp)

# Create a list of wsid
wsid <- as.character(wsid_sxp$wsid)

# Create list of baseline plume raster file names
plume_list <- list.files(file.path("05_sed_ppt_decay/0_baseline/bz/"), pattern='.tif$')
plume_list

# Apply a non linear decay function to the pourpoints sdx and calculate the delta
for(i in 1:length(wsid)) {
  # get the baseline plume for the given watershed/pourpoint
  plume_0 <- raster(file.path("05_sed_ppt_decay/0_baseline/bz", plume_list[i]))
  # get the scenario plume for the given watershed/pourpoint
  plume_1 <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0_abs", plume_list[i]))
  # calculate the delta from baseline 
  plume_delta <- plume_1 - plume_0 
  # write raster
  writeRaster(plume_delta, filename=file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0", 
                                              paste("plume_delta", wsid[i], sep="_")), format="GTiff", overwrite=TRUE)
  file.remove(dir(tmp_dir, pattern = ".grd$", full.names = TRUE))
  file.remove(dir(tmp_dir, pattern = ".gri$", full.names = TRUE))
}

# set delta plume AOI
plume_delta_sum <- wq_model_aoi*0
plot(plume_delta_sum)

#Create list of baseline plume raster file names
plume_delta_name <- list.files(file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0"), pattern='.tif$')
plume_delta_name

# sum the delta plumes
for(i in 1:length(wsid)) {
  # calculate the delta from baseline 
  plume_delta <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s2_prot_fors_clim0", plume_delta_name[i]))
  plume_delta_sum <- plume_delta_sum + plume_delta
}
plot(plume_delta_sum)
writeRaster(plume_delta_sum, filename="07_sed_plume_delta/bz/bz_plume_delta_s2_clim0.tif", overwrite=T)

# clear working environment
rm(list = ls(all.names=TRUE))


##### F/ WQ MODEL - S3 SUSTAINABLE AGRICULTURE SCENARIOS ######

#### 1a/ Plume model - Sustainable agriculture x Climate 0 (absolute)######
setwd("E:/0_NATCAP_MAR/2_WQ_MAR/")

# create the directory links
dir.create(file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0_abs"), showWarning=TRUE, recursive=TRUE)
tmp_dir<- "C:/Users/jademd/AppData/Local/Temp/RtmpisGcNJ"

# AOI raster
wq_model_aoi <- raster('00_base_data/00_wq_model_aoi/wq_model_aoi.tif')
plot(wq_model_aoi)

# import SDR results by watershed
bz_wsid_sxp <- st_read("E:/0_NATCAP_MAR/1_SDR_BZ/8_SDR_outputs_ws_pol/bz_s3_sust_agri_sxp_yr.shp")
# remove the non-watershed rows
wsid_sxp <- bz_wsid_sxp[bz_wsid_sxp$type == "watershed",]
names(wsid_sxp)

#Create list of cost distance raster file names
cd_list <- list.files(file.path("03_distance_rasters/bz/"), pattern='.tif$')
cd_list

# Create a list of wsid
wsid <- as.character(wsid_sxp$wsid)

# Apply a non linear decay function to the pourpoints sdx and calculate the delta
for(i in 1:length(wsid)) {
  # get the sxp load from SDR
  load <- wq_model_aoi*wsid_sxp$clim0_yr[i]
  # get the cost distance raster for the given watershed/pourpoint
  cd <- raster(file.path("03_distance_rasters/bz/", cd_list[i]))
  # model the watershed-scale plume
  plume_1 <- load*(exp(-1*(cd^2)/5000000))
  # trim plume ends
  plume_1 [plume_1 < 1] = 0
  writeRaster(plume_1, filename=file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0_abs", 
                                          paste("plume", wsid[i], sep="_")), format="GTiff", overwrite=TRUE)
  file.remove(dir(tmp_dir, pattern = ".grd$", full.names = TRUE))
  file.remove(dir(tmp_dir, pattern = ".gri$", full.names = TRUE))
}

# set delta plume AOI
plume_sum <- wq_model_aoi*0
plot(plume_sum)

#Create list of baseline plume raster file names
plume_1_name <- list.files(file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0_abs"), pattern='.tif$')
plume_1_name

# sum the delta plumes
for(i in 1:length(wsid)) {
  # calculate the delta from baseline 
  plume_1 <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0_abs", plume_1_name[i]))
  plume_sum <- plume_sum + plume_1
}
plot(plume_sum)
writeRaster(plume_sum, filename="06_sed_plume/1_scenarios_by_country/bz_plume_s3_clim0.tif", overwrite=T)

# clear working environment
rm(list = ls(all.names=TRUE))


#### 1b/ Plume model - Baseline x Climate 0 ~ Sustainable agriculture x Climate 0 (delta) ######
setwd("E:/0_NATCAP_MAR/2_WQ_MAR/")

# create the directory links
dir.create(file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0"), showWarning=TRUE, recursive=TRUE)
tmp_dir<- "C:/Users/jademd/AppData/Local/Temp/RtmpisGcNJ"

# AOI raster
wq_model_aoi <- raster('00_base_data/00_wq_model_aoi/wq_model_aoi.tif')
plot(wq_model_aoi)

# import SDR results by watershed
bz_wsid_sxp <- st_read("E:/0_NATCAP_MAR/1_SDR_BZ/8_SDR_outputs_ws_pol/bz_s3_sust_agri_sxp_yr.shp")
# remove the non-watershed rows
wsid_sxp <- bz_wsid_sxp[bz_wsid_sxp$type == "watershed",]
names(wsid_sxp)

# Create a list of wsid
wsid <- as.character(wsid_sxp$wsid)

# Create list of baseline plume raster file names
plume_list <- list.files(file.path("05_sed_ppt_decay/0_baseline/bz/"), pattern='.tif$')
plume_list

# Apply a non linear decay function to the pourpoints sdx and calculate the delta
for(i in 1:length(wsid)) {
  # get the baseline plume for the given watershed/pourpoint
  plume_0 <- raster(file.path("05_sed_ppt_decay/0_baseline/bz", plume_list[i]))
  # get the scenario plume for the given watershed/pourpoint
  plume_1 <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0_abs", plume_list[i]))
  # calculate the delta from baseline 
  plume_delta <- plume_1 - plume_0 
  # write raster
  writeRaster(plume_delta, filename=file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0", 
                                              paste("plume_delta", wsid[i], sep="_")), format="GTiff", overwrite=TRUE)
  file.remove(dir(tmp_dir, pattern = ".grd$", full.names = TRUE))
  file.remove(dir(tmp_dir, pattern = ".gri$", full.names = TRUE))
}

# set delta plume AOI
plume_delta_sum <- wq_model_aoi*0
plot(plume_delta_sum)

#Create list of baseline plume raster file names
plume_delta_name <- list.files(file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0"), pattern='.tif$')
plume_delta_name

# sum the delta plumes
for(i in 1:length(wsid)) {
  # calculate the delta from baseline 
  plume_delta <- raster(file.path("05_sed_ppt_decay/1_scenarios/bz/s3_sust_agri_clim0", plume_delta_name[i]))
  plume_delta_sum <- plume_delta_sum + plume_delta
}
plot(plume_delta_sum)
writeRaster(plume_delta_sum, filename="07_sed_plume_delta/bz/bz_plume_delta_s3_clim0.tif", overwrite=T)

# clear working environment
rm(list = ls(all.names=TRUE))

