# Retrieve and Clean Manuscript Datasets and Save as Local Copy


# This manuscript relies on a combination of publicly available (and
# downloadable) datasets and datasets that must be stored in this 
# repository for access. 


# Types of bit decay that can cause this script to throw an error:
# 1) May run into public datasets whose naming conventions have changed (i.e.,
# a shapefile that was called California_Counties.shp and is subsequently 
# called i03_CaliforniaCounties.shp). 
# 2) Alternatively, an agency might have changed its hosting service and the
# download link may have changed. In this case, finding the new link should be 
# easy by checking the "accessed from" website.


# If desired, connect to Data Management System (DMS) Archive folder to
# save copies of each individual dataset (rather than one combined .Rdata environment). 
# When downloading this set of spatial and tabular datasets for the first 
# time, or updating them all, expect the script to take about 10-20 minutes to run,
# depending on your download speed.


# One workaround to avoid the long runtime, if updating only one dataset is desired: 
# 1) In a new (empty-environment) session, load the manuscript_data.Rdata environment.
# 2) In this script, overwrite one of the dataset variables by running only the lines
# of code in which that dataset is downloaded or updated.
# 3) Run the final line of code in this script, in which the working environment is 
# saved as manuscript_data.Rdata.

library(rgdal)
library(raster)
library(rpostgis)
library(rgeos)
library(postGIStools)
library(here)
library(httr)
library(stringr)
library(rstudioapi)
library(readxl)
library(tidyr)
library(soilDB) #ssurgo
library(aqp) #ssurgo
library(plyr) #ssurgo
library(data.table) #fread function
library(dataRetrieval) # USGS flow data
library(readr) # for parsing downloaded NOAA data
# library(devtools); devtools::install_github("flowwest/CDECRetrieve") # This line only needs to be run once to install the CDECRetrieve package (which is not available on CRAN)
library(CDECRetrieve) # for CDEC data
library(tidyr)
library(readxl)
library(geodata)
library(sf)


#Directories and local file name update
if(!exists("ms_dir")){ms_dir = ms_dir}
local_data_dir = file.path(ms_dir, "Data")
wl_dir = file.path(local_data_dir, "Wells and water levels")
scratch_dir = file.path(ms_dir, "scratch_work")

save_data_to_archive = F
# If saving data to an archive (such as cloud storage), 
# set save_data_to_archive = TRUE, and populate archive directory name here
dms_archive_dir = "" #[INSERT ARCHIVE DIRECTORY]
latest_vmp_wl_data_filename = "ScottValleyGWMonitProg_2022-11.csv"

# svihm_scenarios_dir = NA 
read_in_model_results = F # no need in this manuscript for multiple management scenarios
# Directory for postprocessed, tabulated results
svihm_results_dir = file.path(local_data_dir, "SVIHM Model Results")

#_SPATIAL DATA ------------------------------------------------------------

# US States ---------------------------------------------------------------

us_states = gadm(country = "USA", level = 1, path = scratch_dir)
file.remove(file.path(scratch_dir,"gadm41_USA_1_pk.rds"))
ca_or = us_states[us_states$NAME_1 %in% c("California", "Oregon"),]
ca_or = st_transform(x = st_as_sf(ca_or), crs = crs("+init=epsg:3310"))

# California Counties -----------------------------------------------------
#Accessed from https://data.cnra.ca.gov/dataset/california-counties
counties_url = "http://atlas-dwr.opendata.arcgis.com/datasets/f26ff0ccda6e48a8b923927cfdb2a451_0.zip"

zipname = strsplit(counties_url, "/")[[1]][length(strsplit(counties_url, "/")[[1]])]
counties_dl = GET(counties_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
#Read shapefile into R
county_shp_name ="i03_CaliforniaCounties"
counties_all = st_read( file.path(scratch_dir, paste0(county_shp_name,".shp")))
county = counties_all[counties_all$COUNTY_NAM=="Siskiyou",]
county = st_transform(x = county, crs = crs("+init=epsg:3310"))

#Copy to the box DMS archive
if(save_data_to_archive==T & 
   !file.exists(file.path(dms_archive_dir, "siskiyou_county_boundary.shp"))){
  st_write(obj = county, dsn = dms_archive_dir, layer = "siskiyou_county_boundary", driver = "ESRI Shapefile")
}
#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml")
file.remove(file.path(scratch_dir,paste(county_shp_name, extension_list, sep = ".")))

# California Cities -------------------------------------------------------
# Accessed from https://data.ca.gov/dataset/ca-geographic-boundaries
# cities_url = "https://data.ca.gov/sites/default/files/CA_Places.zip" # old
cities_url = "https://data.ca.gov/dataset/e212e397-1277-4df3-8c22-40721b095f33/resource/436fc714-831c-4070-b44b-b06dcde6bf18/download/ca-places-boundaries.zip"

zipname = strsplit(cities_url, "/")[[1]][length(strsplit(cities_url, "/")[[1]])]
cities_dl = GET(cities_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir))#, list = TRUE) # just lists files, does not unzip
#Read shapefile into R
## cities_all = st_read(file.path(scratch_dir, "CA_Places_TIGER2016.shp"))
cities_all = st_read(file.path(scratch_dir, "CA_Places_TIGER2016.shp"))
## cities_all = st_transform(cities_all, crs("+init=epsg:3310"))
cities_all = st_transform(cities_all,  crs("+init=epsg:3310"))
cities = cities_all[county,] # clip to county


#Copy to the box DMS archive
if(save_data_to_archive==T & 
   !file.exists(file.path(dms_archive_dir, "siskiyou_city_boundaries.shp"))){
  st_write(obj = cities, dsn = dms_archive_dir, layer = "siskiyou_city_boundaries", driver = "ESRI Shapefile")
}
#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml", "sbn", "sbx", "shp.xml")
file.remove(file.path(scratch_dir,paste("CA_Places_TIGER2016", extension_list, sep = ".")))


# California Boundary ------------------------------------------------------
california = st_union(counties_all)
california = st_transform(california, crs("+init=epsg:3310"))

# Parcels -----------------------------------------------------------------

par_url = "https://www.co.siskiyou.ca.us/sites/default/files/fileattachments/community_development/page/2461/siskiyouparcelsnov2017.zip"

zipname = strsplit(par_url, "/")[[1]][length(strsplit(par_url, "/")[[1]])]
par_dl = GET(par_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
#Read shapefile into R
par_all = st_read(file.path(scratch_dir, "SiskiyouParcelsNov2017.shp"))
par = st_transform(par_all, crs("+init=epsg:3310"))

#Copy to the box DMS archive
# TO DO: when this wrote to file, it threw a bunch of errors saying: 
#Warning 1: Value 490894360.912 of field Shape_Area of feature 37797 not successfully written. Possibly due to too larger number with respect to field width
# Figure out why it's throwing that error
if(save_data_to_archive==T & 
   !file.exists(file.path(dms_archive_dir, "parcels.shp"))){
  st_write(obj = par, dsn = dms_archive_dir, layer = "parcels", driver = "ESRI Shapefile")
}
#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml", "sbn", "sbx", "shp.xml")
file.remove(file.path(scratch_dir,paste("SiskiyouParcelsNov2017", extension_list, sep = ".")))


# Roads (TIGER) -----------------------------------------------------------
# Accessed from https://catalog.data.gov/dataset/tiger-line-shapefile-2018-county-siskiyou-county-ca-all-roads-county-based-shapefile
roads_url = "https://www2.census.gov/geo/tiger/TIGER2018/ROADS/tl_2018_06093_roads.zip"
zipname = strsplit(roads_url, "/")[[1]][length(strsplit(roads_url, "/")[[1]])]
roads_dl = GET(roads_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir))
#Read shapefiles into R
roads_all = st_read( file.path(scratch_dir, "tl_2018_06093_roads.shp"))
roads_all = st_transform(roads_all, crs("+init=epsg:3310"))

# #Copy to the box DMS archive
if(save_data_to_archive==T & 
   !file.exists(file.path(dms_archive_dir, "tiger_roads_siskiyou_county.shp"))){
  st_write(obj = roads_all, dsn = dms_archive_dir, layer = "tiger_roads_siskiyou_county", driver = "ESRI Shapefile")
}
#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp")
file.remove(file.path(scratch_dir,paste("tl_2018_06093_roads", extension_list, sep = ".")))


# DWR Basin Boundaries ----------------------------------------------------
#Accessed at https://data.cnra.ca.gov/dataset/ca-bulletin-118-groundwater-basins
# basins_url = "http://atlas-dwr.opendata.arcgis.com/datasets/b5325164abf94d5cbeb48bb542fa616e_0.zip"
basins_url = "https://gis.data.cnra.ca.gov/datasets/bdfc6550b4f3401a83ad1e2f468140ca_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
zipname1 = strsplit(basins_url, "/")[[1]][length(strsplit(basins_url, "/")[[1]])]
zipname = paste0(strsplit(zipname1, ".zip")[[1]][[1]], ".zip")
#retrieve zip file from url
basins_dl = GET(basins_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
#unzip in the scratch work directory
unzip(file.path(scratch_dir,zipname), exdir = file.path(scratch_dir))#, list = TRUE)
# Read shapefile
gw_basin_name = "i08_B118_CA_GroundwaterBasins"
basins_all = st_read(file.path(scratch_dir, paste0(gw_basin_name,".shp")))
#subset the Scott GSP basin and reproject to 3310
basin = basins_all[basins_all$Basin_Numb %in% c("1-005"),]
basin = st_transform(basin, crs("+init=epsg:3310"))
# Write to Dropbox
if(save_data_to_archive==T & 
   !file.exists(file.path(dms_archive_dir, "scott_basin_boundary_2018.shp"))){
  st_write(obj = basin, dsn = dms_archive_dir, layer = "scott_basin_boundary_2018", driver = "ESRI Shapefile")
}
# bnd3_from_db = get_postgis_query(siskiyou_spatial, "SELECT * FROM three_basin_boundaries_2018", geom_name = "geom")
#Remove scratch work
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml")
file.remove(file.path(scratch_dir,paste(gw_basin_name, extension_list, sep = ".")))


# USGS Watersheds, waterbodies, and streams -------------------------------

# _Scott River watershed --------------------------------------------------
# Accessed at https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/HU8/Shape/
# Which was accessed via https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
HUC8_num = c(18010208)#, 18010207, 18010205) # scott, shasta, butte
valley_name = ("scott")#, "shasta", "butte")
# huc_table = data.frame(huc8 = HUC8_nums, basin = valley_names)

#Download the zipped files for each watershed, unzip, load into R, write to database (overwriting for each new basin)
# for(i in 1:3){
  huc = HUC8_num # huc_table$huc8[i]
  basin_name = valley_name #huc_table$basin[i]
  
  # wsh_url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_",huc,"_HU8_Shape.zip")
  wsh_url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_",huc,"_HU8_Shape.zip")
  zipname = paste0("NHD_H_",huc,"_HU8_Shape.zip")
  # Download from USGS site and write to the working drive (scratch_dir)
  wsh_dl = GET(wsh_url, write_disk(file.path(scratch_dir, zipname), overwrite = TRUE))
  # Unzip file and save in the working directory (defaults to Documents folder)
  unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
  #Read watershed and named stream shapefiles into R
  wbdhu8 = st_read(file.path(scratch_dir, "Shape", "WBDHU8.shp"))
  nhdwaterbody = st_read(file.path(scratch_dir, "Shape", "NHDWaterbody.shp"))
  flowlines = st_zm(st_read(file.path(scratch_dir, "Shape", "NHDFlowline.shp")))
  
  #Subset flowlines
  #Note: keep named streams and unnamed ReachCode 18010208002394.
  #This is a channelized ditch connecting Johnson Creek to Crystal Creek
  named_streams = flowlines[(!is.na(flowlines$gnis_name)) | flowlines$reachcode == 18010208002394,]
  #And, name this reach to make it part of Johnson Creek
  named_streams$gnis_name[named_streams$reachcode == 18010208002394] = "Johnson Creek"
  
  #reproject
  watershed = st_transform(wbdhu8, crs("+init=epsg:3310"))
  nhdwaterbody = st_transform(nhdwaterbody, crs("+init=epsg:3310"))
  named_streams = st_transform(named_streams, crs("+init=epsg:3310"))
  
  # Write shapefiles to Box archive
  if(save_data_to_archive==T & 
     !file.exists(file.path(dms_archive_dir, paste0(basin_name,"_watershed_huc8.shp")))){
  st_write(obj = watershed, dsn = dms_archive_dir, layer = paste(basin_name,"watershed_huc8", sep="_"), driver = "ESRI Shapefile")
  st_write(obj = nhdwaterbody, dsn = dms_archive_dir, layer = paste(basin_name,"waterbody_nhd", sep="_"), driver = "ESRI Shapefile")
  st_write(obj = named_streams, dsn = dms_archive_dir, layer = paste(basin_name,"named_streams_nhd", sep="_"), driver = "ESRI Shapefile")
  }
# }

#Delete zipped files
file.remove(file.path(scratch_dir, "NHD_H_18010208_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "NHD_H_18010207_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "NHD_H_18010205_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "Shape", list.files(file.path(scratch_dir, "Shape")))) #deletes files in unzipped "Shape" folder
unlink(file.path(scratch_dir, "Shape"),recursive = TRUE) #Removes an empty file


# _Klamath Basin boundary -------------------------------------------------
kb_huc6 = 180102
# Accessed from https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/HU4/Shape/
# klamath_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/HighResolution/Shape/NHD_H_1801_HU4_Shape.zip"
klamath_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU4/Shape/NHD_H_1801_HU4_Shape.zip"
zipname = strsplit(klamath_url, "/")[[1]][length(strsplit(klamath_url, "/")[[1]])]
#retrieve zip file from url
klamath_dl = GET(klamath_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
#unzip in the scratch work directory
unzip(file.path(scratch_dir,zipname), exdir = file.path(scratch_dir))#, list = TRUE)
# Read shapefile
basins_1801_all = st_read(file.path(scratch_dir, "Shape","WBDHU6.shp"))
#subset the Klamath basin and reproject to 3310
klamath = basins_1801_all[basins_1801_all$huc6==kb_huc6,]
klamath = st_transform(klamath, crs("+init=epsg:3310"))
# Write to Dropbox
if(save_data_to_archive==T & 
   !file.exists(file.path(dms_archive_dir, "klamath_basin_boundary_2018.shp"))){
  st_write(obj = klamath, dsn = dms_archive_dir, layer = "klamath_basin_boundary_2018", driver = "ESRI Shapefile")
}
#Remove scratch work
file.remove(file.path(scratch_dir, zipname))
file.remove(file.path(scratch_dir, "Shape", list.files(file.path(scratch_dir, "Shape")))) #deletes files in unzipped "Shape" folder
unlink(file.path(scratch_dir, "Shape"),recursive = TRUE) #Removes an empty file

# USGS Flow gages  -------------------------------------------------------------------


#find daily surface water data in CA
pCode = c("00060") #daily avg streamflow
swCA_dl = readNWISdata(stateCd="CA", parameterCd=pCode,
                       service="site", seriesCatalogOutput=TRUE)
swCA = swCA_dl[swCA_dl$parm_cd %in% pCode,]

HUC8_nums = c(18010208) #, 18010207, 18010205) # scott, shasta, butte
sw_scott = swCA[swCA$huc_cd %in% HUC8_nums,]

sw_scott_sp = st_as_sf(sw_scott, coords = c("dec_long_va","dec_lat_va"),
                       crs = crs("+init=epsg:4326")) #assign WGS84 projection to coordinates
sw_scott_sp = st_transform(sw_scott_sp, crs("+init=epsg:3310"))
usgs_gauges = sw_scott_sp[(watershed),]

# DEM layer (topography) ------------------------------------------------------

# USGS DEMs 1/3 arc-second 
# Accessed via https://viewer.nationalmap.gov/basic/#productSearch

# dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/IMG/n42w123.zip"
# dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/IMG/n42w124.zip"
# dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/n42w123/USGS_13_n42w123_20210624.tif"
# dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/n42w124/USGS_13_n42w124_20210624.tif"
dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/historical/n42w123/USGS_13_n42w123_20210623.tif"
dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/historical/n42w124/USGS_13_n42w124_20210623.tif"
dem_urls = c(dem123_url, dem124_url)

dem_url = dem_urls[1]
# Download from USGS site and write to the working drive (scratch_dir)
tif_name = strsplit(dem_url, "/")[[1]][length(strsplit(dem_url, "/")[[1]])]
if(!file.exists(file.path(scratch_dir,tif_name))){
  dem_dl = GET(dem_url, write_disk(file.path(scratch_dir,tif_name), overwrite = TRUE))
  }
dem123 = rast(file.path(scratch_dir, tif_name))

dem_url = dem_urls[2]
# Download from USGS site and write to the working drive (scratch_dir)
tif_name = strsplit(dem_url, "/")[[1]][length(strsplit(dem_url, "/")[[1]])]
if(!file.exists(file.path(scratch_dir,tif_name))){
  dem_dl = GET(dem_url, write_disk(file.path(scratch_dir,tif_name), overwrite = TRUE))
  }
dem124 = rast(file.path(scratch_dir, tif_name))

m <- merge(dem123, dem124) # getting some weird errors but it still runs the merge.
# plot(m)
dem_3310 = terra::project(x = m, y = crs("+init=epsg:3310"))

# Generate hillshade
wsh_10km = st_buffer(watershed, 1e4) # give it a buffer for figure backgrounds
dem_watershed=terra::crop(dem_3310, wsh_10km)
dem_shade=gray(0:100 / 100)


if(!file.exists(file.path(scratch_dir, "hillshade_cropped_raster.tif"))){
  #generate hillshade (for smaller maps)
  # hill_basin = hillShade(slope = terrain(dem_basin, "slope"), aspect = terrain(dem_basin, "aspect"))
  #generate cropped hillshade
  dem_watershed_slope = terra::terrain(dem_watershed, v = "slope", unit = "radians")
  dem_watershed_asp = terra::terrain(dem_watershed, v = "aspect", unit = "radians")
  hill_wsh = shade(slope = dem_watershed_slope, aspect = dem_watershed_asp,
                   angle = 45, direction = 0)
  
  
  plot(hill_wsh, col = grey(c(0:100)/100), legend = F)
  
  # plot(hill_wsh)
  # hill_wsh_spatRaster = hill_wsh
  # hill_wsh_raster = raster(hill_wsh_spatRaster)
  # hill_wsh_wrapped = wrap(hill_wsh_spatRaster)
  terra::writeRaster(hill_wsh, filename = file.path(scratch_dir, "hillshade_cropped_raster.tif"), overwrite=T)
  
}

# American Indian Areas ----------------------------------------------------
tribal_url = "https://www2.census.gov/geo/tiger/TIGER2021/AIANNH/tl_2021_us_aiannh.zip"

zipname = strsplit(tribal_url, "/")[[1]][length(strsplit(tribal_url, "/")[[1]])]
tribal_dl = GET(tribal_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir))#, list = TRUE) # just lists files, does not unzip
#Read shapefile into R
tribal_all = st_read(file.path(scratch_dir, "tl_2021_us_aiannh.shp"))
tribal_all = st_transform(tribal_all, crs("+init=epsg:3310"))
# county = get_postgis_query(siskiyou_spatial, "SELECT * FROM siskiyou_county_boundary", geom_name = "geom")
# requires klamath loading (earlier in this script)
tribal = tribal_all[st_as_sf(klamath),] # clip to klamath

#Copy to the box DMS archive
if(save_data_to_archive==T & 
   !file.exists(file.path(dms_archive_dir, "am_indian_areas_klamath.shp"))){
  st_write(obj = tribal, dsn = dms_archive_dir, layer = "am_indian_areas_klamath", driver = "ESRI Shapefile", overwrite=T)
}
#Write to database
#remove files from scratch drive
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml", "shp.ea.iso.xml", "shp.iso.xml")
file.remove(file.path(scratch_dir,paste("tl_2021_us_aiannh", extension_list, sep = ".")))



#_TABULAR DATA ------------------------------------------------------------


# Fort Jones USGS flow ----------------------------------------------------

# Retrieve fort jones gage data
fj_num = "11519500"
fj_flow = readNWISdv(siteNumbers = fj_num, parameterCd="00060" )
fj_flow = renameNWISColumns(fj_flow)
fj_flow$wy = year(fj_flow$Date); fj_flow$wy[month(fj_flow$Date) > 9] = fj_flow$wy[month(fj_flow$Date) > 9]+1


# Groundwater level data, 2023.01.11 --------------------------------------


clean_colnames = function(array_of_colnames){
  #Get rid of characters in first column header introduced by encoding problem
  array_of_colnames = gsub("X.U.FEFF.", replacement = "", array_of_colnames)
  
  #general cleanup
  array_of_colnames = gsub("[.]", replacement = "_", array_of_colnames) #searching for a "." character requires escape with []
  array_of_colnames = gsub("__", replacement = "_", array_of_colnames) #there were some ".." characters; reduce to one _
  array_of_colnames = str_to_lower(array_of_colnames)
  array_of_colnames = gsub("_$", replacement = "", array_of_colnames) # remove trailing _
  
  #special cases
  # CK Note: We need a different way of removing the encoding character as this removes all the _ characters from all column names
  # array_of_colnames = gsub("?_", replacement = "", array_of_colnames) #remove encoding character
  array_of_colnames = gsub("X.U.FEFF", replacement = "", array_of_colnames)
  array_of_colnames = gsub("co_op", replacement = "coop", array_of_colnames) #remove extraneous _
  array_of_colnames = gsub("_nad_83", replacement = "", array_of_colnames) #remove extraneous _
  array_of_colnames = gsub("_me$", replacement = "", array_of_colnames) #remove extraneous, confusing abbreviation
  
  #rename long column names
  array_of_colnames[array_of_colnames == "groundwater_basin_subbasin_name"] = "basin"
  
  #Rename casgem names if names are confusing
  array_of_colnames[array_of_colnames == "site_code"] = "well_code"
  array_of_colnames[array_of_colnames == "msmt_date"] = "date"
  array_of_colnames[array_of_colnames == "wlm_rpe"] = "rp_elev_ft"
  array_of_colnames[array_of_colnames == "wlm_gse"] = "gp_elev_ft"
  # array_of_colnames[array_of_colnames == "rdng_ws"] = "???"
  array_of_colnames[array_of_colnames == "rdng_rp"] = "reading_rp_ft"
  array_of_colnames[array_of_colnames == "wse"] = "wse_ft"
  # array_of_colnames[array_of_colnames == "rpe_wse"] = "???"
  array_of_colnames[array_of_colnames == "gse_wse"] = "gs_to_ws_ft"
  array_of_colnames[array_of_colnames == "msmt_cmt"] = "comments"
  
  #casgem stations
  array_of_colnames[array_of_colnames == "wcr_no"] = "wcr_log_no"
  
  return(array_of_colnames)
}

psia_to_ft <- function(psia) { psia * 2.3066587368787}

read_field_data <- function(x) {
  
  cat("downloading data from provided URL...")
  
  calib_file_path = file.path(wl_dir,"calib.xlsx")
  # save file locally
  download.file(x, destfile = calib_file_path, mode = "wb") #mode specific to Windows
  
  cat("\n data was succcessfully downloaded!")
  
  # sheet 1 is calibration data
  cb <- readxl::read_excel(calib_file_path, sheet = 1)
  
  cb <- separate(cb, LatLon, c("lat", "lon"), sep =", ") %>% 
    mutate_at(.vars = c("lat","lon"), .funs = as.numeric) %>% 
    dplyr::select(Site_ID, everything())
  
  # sheet 2 is field data, importantly: depth to water & measurement time
  field <- readxl::read_excel(calib_file_path, sheet = 2) %>% 
    dplyr::select(Site_ID, `DTW-MP_ft`, Meas_Time)
  
  # discard downloaded file
  file.remove(calib_file_path)
  
  # spatial object
  # ll <- "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"
  
  cb_sp <- SpatialPointsDataFrame(coords      = cb[ , c("lat","lon")],
                                  data        = cb,
                                  proj4string = CRS("+init=epsg:4269")
  )
  
  # return data and spatial object
  return(list(data = cb, field = field, sp = cb_sp))
}

pull_table = function(table_name){
  return (data.frame(tbl(gw_continuous, table_name)))
}

pull_continuous_data <- function() { # Copies code from read_campbell function in 00_functions.R
  # get the site ids, logger dataframes, and baro dataframe in a list
  # precondition: db connection to gw_continuous (i.e. have previously sourced connect_t_db.R)
  
  # Pull from server: Campbell Sci raw data files
  continuous_tables = dbListTables(gw_continuous)
  
  # extract ids from table names (exclude meta tables and a "TEST" table)
  valley = substr(continuous_tables, start = 1, stop= 3)
  last4 = str_sub(continuous_tables, start = -4) # get the last 4 characters
  td_list = continuous_tables[valley %in% c("BUT","SCT", "SHA") & last4 != "TEST"] # fix this to not include tables with _ _
  
  
  
  # read raw continuous data from DB 
  cat("reading data for sensor ids...")
  z = td_list %>% purrr::map(pull_table)  
  
  # extract barometric pressure logger data, connected to SHA_06
  b <- z[[which(td_list == "SHA_06")]][, c("TIMESTAMP", "BP_psia_Avg", "NaN_BP_Avg")]
  
  return(list(ids = td_list, data = z, baro_df = b))
  
}

compute_dtw <- function(data, baro, calib_data, field_data, is_bubbler) {
  
  # Dozens of these tables have no ID column and no way to subset
  # Also some do not have these key columns?
  missing_cols = setdiff(c("TIMESTAMP", "Sub_psia_Avg", "ID", "NaN_Lvl_Avg"), colnames(data))
  if(length(missing_cols)>1){
    print( paste(data$ID[1], ": missing columns", paste(missing_cols, collapse = ", ")))
    return(NA)
  }else{
    # find t_0 for the Site_ID
    t_0 <- filter(field_data, Site_ID == data$ID[1]) %>% 
      pull(Meas_Time) %>% 
      sort()
    t_0 <- t_0[1]
    
    # clean baro NaNs (when NaN column == 15, data reported == 0, but is truly NA)
    baro <- mutate(baro, BP_psia_Avg = ifelse(!is.na(NaN_BP_Avg) & NaN_BP_Avg == 15, NA, BP_psia_Avg))
    
    
    # bubblers don't require barometric correction
    if(data$ID[1] %in% is_bubbler) {
      
      # cols to keep
      keep <- c("TIMESTAMP", "Lvl_ft", "ID")
      
      d <- dplyr::select(data, keep) %>% 
        left_join(dplyr::select(calib_data, Site_ID, SensorCable_ft), 
                  by = c("ID" = "Site_ID")) %>% 
        # use cable depth to compute the depth to water
        mutate(dtw_ft  = (SensorCable_ft - Lvl_ft)*-1) %>% 
        # filter out early, incorrect observations 
        filter(TIMESTAMP >= t_0)
    }
    
    # standard pressure transducers require barometric correction
    if(! data$ID[1] %in% is_bubbler) {
      
      # cols to keep
      keep <- c("TIMESTAMP", "Sub_psia_Avg", "ID", "NaN_Lvl_Avg")
      
      d <- dplyr::select(data, keep) %>% 
        # clean NaNs (when NaN column == 15, data reported == 0, but is truly NA)
        mutate(Sub_psia_Avg = ifelse(!is.na(NaN_Lvl_Avg) & NaN_Lvl_Avg == 15, NA, Sub_psia_Avg)) %>%
        
        left_join(baro, by = "TIMESTAMP") %>% 
        filter(!is.na(BP_psia_Avg)) %>% 
        left_join(dplyr::select(calib_data, Site_ID, SensorCable_ft), 
                  by = c("ID" = "Site_ID")) %>% 
        # adjust raw water level by barometric pressure,
        # use cable depth to compute the depth to water
        mutate(badj_ft = psia_to_ft(Sub_psia_Avg - BP_psia_Avg),
               dtw_ft  = (SensorCable_ft - badj_ft)*-1) %>% 
        # filter out early, incorrect observations 
        filter(TIMESTAMP >= t_0) %>% 
        dplyr::select(-NaN_Lvl_Avg)
    }
    
    return(d)
  }
}

downsample_wl = function(data, time_period = "week", find = "max_gw_elevation"){
  #Options for "find" are "max_gw_elevation", "min_gw_elevation"
  
  #Assign relevant time periods to each measurement
  data$time_per = floor_date(data$TIMESTAMP, unit = time_period)
  
  if(find == "max_gw_elevation"){
    downsampled = data %>%
      group_by(ID, time_per) %>%
      slice(which.min(dtw_ft)) # find the min. depth to water to get the highest measured gw elevation
  }
  
  if(find == "min_gw_elevation") {
    downsampled = data %>%
      group_by(ID, time_per) %>%
      slice(which.max(dtw_ft)) # find the max. depth to water to get the lowest measured gw elevation
  }
  
  # if(find == "median_gw_elevation") {
  #   downsampled = data %>%
  #     group_by(ID, time_per) %>%
  #     slice(which(dtw_ft == median(dtw_ft))) # find the max. depth to water to get the lowest measured gw elevation
  # }
  
  return(downsampled)
}

dwr_text_parser = function(dwr_text){
  dwr_lines = unlist(strsplit(dwr_text, split = "\n"))
  
  colnames_dwr = unlist(strsplit(dwr_lines[1], ","))
  ncol_dwr = length(colnames_dwr)
  nrow_dwr = length(dwr_lines) - 1
  
  dwr_data = strsplit(x = dwr_lines[2:nrow_dwr], split = ",")
  
  weird_ones = which(lengths(dwr_data)<ncol_dwr)
  dwr_data_rightsize = dwr_data[lengths(dwr_data)==ncol_dwr]
  dwr_data_other = dwr_data[lengths(dwr_data)>ncol_dwr]
  
  #Matrix of the right-size ones
  dwr_1 = matrix(data = unlist(dwr_data_rightsize), 
                 nrow = length(dwr_data_rightsize),
                 ncol = ncol_dwr, byrow = T)
  #make a secondary matrix of the data from the ones with commas in the comments
  if(length(dwr_data_other)>0){
    dwr_2 = matrix(data = NA, nrow = length(dwr_data_other),
                   ncol = ncol_dwr)
    for(i in 1:length(dwr_data_other)){
      split_data = unlist(dwr_data_other[[i]])
      comments = paste(split_data[ncol_dwr:length(split_data)], collapse = ",")
      dwr_2[i,1:(ncol_dwr-1)] = split_data[1:(ncol_dwr-1)]
      dwr_2[i,ncol_dwr] = comments
    }
    # Combine matrices and convert to dataframe
    dwr_matrix = rbind(dwr_1, dwr_2)
  } else {dwr_matrix = dwr_1}
  
  dwr_all = as.data.frame(dwr_matrix)
  colnames(dwr_all) = colnames_dwr
  # remove weird end of line character
  colnames(dwr_all)[grepl(colnames(dwr_all), pattern = "\r")] = 
    gsub(pattern = "\r", replacement = "", 
         x = colnames(dwr_all)[grepl(colnames(dwr_all), pattern = "\r")] )
  dwr_all[,ncol_dwr] = gsub(pattern = "\r", replacement = "", 
                            x = dwr_all[,ncol_dwr])
  
  return(dwr_all)
}

get_wl_data = function(VMP = TRUE, DWR = TRUE,  PSI = TRUE,
                       LWA_TD = TRUE, QV = TRUE, clean = TRUE){
  
  wl_obs_colnames = c("stn_id", "well_code","wlm_id","date", "rp_elev_ft","gp_elev_ft",
                      "rdng_ws","reading_rp_ft","wse_ft","rpe_wse","gs_to_ws_ft",
                      "wlm_qa_desc", "wlm_desc","wlm_acc_desc","wlm_org_id","wlm_org_name",
                      "comments","coop_agency_org_id","coop_org_name", "data_table_source")
  stations_colnames = c("stn_id","well_code","swn","well_name","latitude","longitude",
                        "wlm_method","wlm_acc","basin_code",
                        "basin_name","county_name", "well_depth","well_use", "well_type", "wcr_log_no", 
                        "wcr_file_no", "well_loc_source", "comments")
  perf_colnames = c("stn_id", "well_code", "top_prf", "bot_prf")
  
  if(VMP==T | QV == T){
    # pull in DEM for filling in RP elevation gaps
    wsh = watershed # assumes you've run the NHD watershed retrieval component in this same session
    wsh_10km = st_buffer(wsh, 1e4)
  }
  
  # 1) DWR wl, wells and perf data: webscrape  -----------------------------------------------
  if(DWR == TRUE){
    #Pull data from CNRA website
    stations_dwr_url = "https://data.cnra.ca.gov/dataset/dd9b15f5-6d08-4d8c-bace-37dc761a9c08/resource/af157380-fb42-4abf-b72a-6f9f98868077/download/stations.csv"
    stations_dwr_dl = GET(stations_dwr_url)
    stations_dwr_text = content(stations_dwr_dl, as = "text" )
    stations_dwr_all = dwr_text_parser(dwr_text = stations_dwr_text)
    # stations_dwr_all = as.data.frame(content(stations_dwr_dl, as = "parsed" ))
    
    wl_obs_dwr_url = "https://data.cnra.ca.gov/dataset/dd9b15f5-6d08-4d8c-bace-37dc761a9c08/resource/bfa9f262-24a1-45bd-8dc8-138bc8107266/download/measurements.csv"
    wl_obs_dwr_dl = GET(wl_obs_dwr_url)
    # wl_obs_dwr_all = as.data.frame(content(wl_obs_dwr_dl, as = "parsed"))
    # "content" no longer working; resort to home made text parser
    
    wl_obs_dwr_text = content(wl_obs_dwr_dl, as = "text")
    wl_obs_dwr_all = dwr_text_parser(dwr_text = wl_obs_dwr_text)

    wl_obs_dwr_all = wl_obs_dwr_all[order(wl_obs_dwr_all$site_code, 
                                          wl_obs_dwr_all$msmt_date),]
    
    perf_dwr_url = "https://data.cnra.ca.gov/dataset/dd9b15f5-6d08-4d8c-bace-37dc761a9c08/resource/f1deaa6d-2cb5-4052-a73f-08a69f26b750/download/perforations.csv"
    perf_dwr_dl = GET(perf_dwr_url)
    # perf_dwr_all = as.data.frame(content(perf_dwr_dl, as = "parsed"))
    perf_dwr_text = content(perf_dwr_dl, as = "text")
    perf_dwr_all = dwr_text_parser(dwr_text = perf_dwr_text)
    
    # Clean up column names and add source info, and add some columns
    colnames(wl_obs_dwr_all) =  clean_colnames(colnames(wl_obs_dwr_all))
    wl_obs_dwr_all$data_table_source = "DWR_Periodic_GWL"
    colnames(stations_dwr_all) = clean_colnames(colnames(stations_dwr_all))
    
    # Clip obs and stations to Siskiyou County
    stations_dwr_all$latitude=as.numeric(stations_dwr_all$latitude)
    stations_dwr_all$longitude=as.numeric(stations_dwr_all$longitude)
    # filter out 3 Kern county wells that have offset lat-long info
    stations_dwr_all = stations_dwr_all[!is.na(stations_dwr_all$latitude) & !is.na(stations_dwr_all$longitude),]
    # Subset spatially
    # assume already downloaded county boundary
    # county = get_postgis_query(siskiyou_spatial, "SELECT * FROM siskiyou_county_boundary", geom_name = "geom")
    stations_dwr_sp = st_as_sf(stations_dwr_all, coords = c("longitude","latitude"),
                               crs = crs("+init=epsg:4326")) #copy table to make spatial object
    stations_dwr_sp = st_transform(stations_dwr_sp, crs(county))
    stations_siskiyou = stations_dwr_sp[county,]
    
    #Subset tables based on spatial clip
    stations_dwr = stations_dwr_all[stations_dwr_all$stn_id %in% stations_siskiyou$stn_id,]
    wl_obs_dwr = wl_obs_dwr_all[wl_obs_dwr_all$well_code %in% stations_siskiyou$well_code,]
    perf_dwr = perf_dwr_all[perf_dwr_all$well_code %in% stations_siskiyou$well_code,]
    
    
    stations_dwr$wcr_file_no = NA; stations_dwr$well_loc_source = NA; 
    stations_dwr$well_loc_source = "DWR_Periodic_GWL"
    stations_dwr$comments = NA
    
    colnames(perf_dwr) = clean_colnames(colnames(perf_dwr))
    
    
    # Update 2021.07.13: DWR changed its column name scheme so now we have to cross-walk.
    # Saving this logical statement in case they... change it back?
    dwr_cols_diff_from_2019_template = length(setdiff(colnames(wl_obs_dwr),wl_obs_colnames)) > 1 |length(setdiff(wl_obs_colnames,colnames(wl_obs_dwr))) > 1 | 
      length(setdiff(colnames(stations_dwr), stations_colnames)) > 1 | length(setdiff(stations_colnames,colnames(stations_dwr))) > 1 |
      length(setdiff(colnames(perf_dwr), perf_colnames)) > 1 | length(setdiff(perf_colnames,colnames(perf_dwr))) > 1
    if(dwr_cols_diff_from_2019_template){ 
      print("DWR colnames distinct from original colnames used to create this template. Using updated colnames as of 7/13/2021.")
      
      #Crosswalk from July 2021 colnames to original 2019 template colnames
      wl_obs_dwr_colnames_2021.07 = c("well_code","wlm_id", "date",
                                      "rp_elev_ft",  "gp_elev_ft", "gwe", "gse_gwe",
                                      "wlm_qa_desc", "wlm_desc",  "wlm_acc_desc",
                                      "wlm_org_name", "coop_org_name", "monitoring_program",
                                      "comments",  "data_table_source" )
      stations_dwr_colnames_2021.07 = c( "stn_id", "well_code", "swn",  "well_name",
                                         "latitude", "longitude", "gse","gse_method", "gse_acc",
                                         "basin_code", "basin_name", "county_name",
                                         "well_depth","well_use", "well_type", "wcr_log_no",
                                         "monitoring_program", "wcr_file_no", "well_loc_source",
                                         "comments")
      perf_dwr_colnames_2021.07 = c("well_code", "top_prf_int", "bot_prf_int")
      
      dwr_cols_diff_from_2021.07_cols = length(setdiff(colnames(wl_obs_dwr),wl_obs_dwr_colnames_2021.07)) > 1 |length(setdiff(wl_obs_dwr_colnames_2021.07,colnames(wl_obs_dwr))) > 1 | 
        length(setdiff(colnames(stations_dwr), stations_dwr_colnames_2021.07)) > 1 | length(setdiff(stations_dwr_colnames_2021.07,colnames(stations_dwr))) > 1 |
        length(setdiff(colnames(perf_dwr), perf_dwr_colnames_2021.07)) > 1 | length(setdiff(perf_dwr_colnames_2021.07,colnames(perf_dwr))) > 1
      if(dwr_cols_diff_from_2021.07_cols){ 
        print("DWR colnames distinct from updated colnames as of 7/13/2021. REVISE COLNAMES; will throw an error when it tries to combine tables.")
      }
      
      # Crosswalk columns from 2021.07 columns to orig template
      
      #Initialize VMP table to add to dwr data
      # save orig table as "dwr"
      dwr = wl_obs_dwr 
      # Make new wl_obs_dwr table with orig. 2019 columns
      wl_obs_dwr = data.frame(matrix(NA, nrow(dwr), length(wl_obs_colnames)))
      colnames(wl_obs_dwr) = wl_obs_colnames
      
      # wl_obs_dwr$stn_id  # No stn_id in new table
      wl_obs_dwr$well_code = dwr$well_code
      wl_obs_dwr$wlm_id = dwr$wlm_id
      wl_obs_dwr$date = dwr$date
      wl_obs_dwr$rp_elev_ft = dwr$rp_elev_ft
      wl_obs_dwr$gp_elev_ft = dwr$gp_elev_ft
      # wl_obs_dwr$rdng_ws  # Not in this table. 
      # wl_obs_dwr$reading_rp_ft # Not in this table. (What? This is what everyone measures)
      wl_obs_dwr$wse_ft = dwr$gwe
      # wl_obs_dwr$rpe_wse # Not in this table.
      wl_obs_dwr$gs_to_ws_ft = dwr$gse_gwe # This is a guess
      wl_obs_dwr$wlm_qa_desc = dwr$wlm_qa_desc
      wl_obs_dwr$wlm_desc = dwr$wlm_desc
      wl_obs_dwr$wlm_acc_desc = dwr$wlm_acc_desc
      # wl_obs_dwr$wlm_org_id  # Not in new table
      wl_obs_dwr$wlm_org_name = dwr$wlm_org_name
      wl_obs_dwr$comments = dwr$comments
      # wl_obs_dwr$coop_agency_org_id # Not in new table
      wl_obs_dwr$coop_org_name = dwr$coop_org_name
      wl_obs_dwr$data_table_source = dwr$data_table_source
      
      #Initialize DWR table to add to Stations data
      stn_dwr_2021 = stations_dwr # Save old version of stations_dwr
      stations_dwr = data.frame(matrix(NA, nrow(stations_dwr), length(stations_colnames)))
      colnames(stations_dwr) = stations_colnames
      
      # stations_dwr$stn_id # No stn_id in new table
      stations_dwr$well_code = stn_dwr_2021$well_code
      stations_dwr$swn = stn_dwr_2021$swn
      stations_dwr$well_name = stn_dwr_2021$well_name
      stations_dwr$latitude = stn_dwr_2021$latitude
      stations_dwr$longitude = stn_dwr_2021$longitude
      stations_dwr$wlm_method = stn_dwr_2021$gse_method
      stations_dwr$wlm_acc = stn_dwr_2021$gse_acc
      stations_dwr$basin_code = stn_dwr_2021$basin_code
      stations_dwr$basin_name = stn_dwr_2021$basin_name
      stations_dwr$county_name = stn_dwr_2021$county_name
      stations_dwr$well_depth = stn_dwr_2021$well_depth
      stations_dwr$well_use = stn_dwr_2021$well_use
      stations_dwr$well_type = stn_dwr_2021$well_type
      stations_dwr$wcr_log_no = stn_dwr_2021$wcr_log_no
      stations_dwr$wcr_file_no = stn_dwr_2021$wcr_file_no
      stations_dwr$well_loc_source = stn_dwr_2021$well_loc_source
      stations_dwr$comments = stn_dwr_2021$comments
      # Does not include new columns "gse" and "monitoring_program"
      
      #Initialize DWR table to add to perf data
      perf_dwr_2021 = perf_dwr # Save old version of perf_dwr
      perf_dwr = data.frame(matrix(NA, nrow(perf_dwr), length(perf_colnames)))
      colnames(perf_dwr) = perf_colnames
      
      perf_dwr$well_code = perf_dwr_2021$well_code
      perf_dwr$top_prf = perf_dwr_2021$top_prf_int
      perf_dwr$bot_prf = perf_dwr_2021$bot_prf_int
      
    }
    
    
    
  }
  
  if(VMP == TRUE){
    #2a) VMP wells info: clean, attach to DWR  -------------------------------------------
    
    #Retrieve Scott well location shapefiles from server
    # irr_wells = get_postgis_query(siskiyou_spatial, "SELECT * FROM irrigation_wells_svihm_2018", geom_name = "geom")
    # mon_wells = get_postgis_query(siskiyou_spatial, "SELECT * FROM monitoring_wells_svihm_2018", geom_name = "geom")
    irr_wells = st_read(dsn = file.path(local_data_dir, "Wells and water levels"), layer = "irrigation_wells_svihm_2018")
    mon_wells = st_read(dsn = file.path(local_data_dir, "Wells and water levels"), layer = "monitoring_wells_svihm_2018")
    
    #add coordinates to wells table
    irr_wells = st_transform(irr_wells, crs("+init=epsg:4326"))
    mon_wells = st_transform(mon_wells, crs("+init=epsg:4326"))
    
    # st_coordinates CURRENTLY HERE
    
    #Clean up irrigation well names for adding to DWR station library 
    irr_wells$well_code = paste0("WCRfn_",irr_wells$WCR_File_N)
    #hard-coded: build new names for 3 wells with no attached WCR number 
    owners_w_missing_wcr = c("MORRIS","HANNA")
    trs_xcoord_w_missing_wcr = c(512613.5,513208.1,514334.4)
    selector = irr_wells$WellOwner %in% owners_w_missing_wcr & irr_wells$TRS_XCOORD %in% trs_xcoord_w_missing_wcr
    new_names = c("MORRIS_1", "HANNA_1", "HANNA_2")
    irr_wells$well_code[selector] = new_names
    
    irr_wells$Use = "Irrigation" #instead of "IRR", to match DWR term
    
    #Combine irrigation and minitoring wells
    # needs the same columns; add missing columns to each well set
    irr_wells_allcols = st_join(irr_wells, mon_wells)
    mon_wells_allcols = st_join(mon_wells, irr_wells)
    vmp_wells = rbind(irr_wells_allcols, mon_wells_allcols)
    vmp_wells$longitude = st_coordinates(vmp_wells)[,1]
    vmp_wells$latitude = st_coordinates(vmp_wells)[,2]
    
    #Add anonymized VMP well codes to well_code, and add a well_name column to keep owner/anonymized VMP well ID
    vmp_wells$well_code[is.na(vmp_wells$well_code)] = vmp_wells$Well_ID_2[is.na(vmp_wells$well_code)]
    vmp_wells$well_name = vmp_wells$WellOwner
    vmp_wells$well_name[is.na(vmp_wells$WellOwner)] = vmp_wells$Well_ID_2[is.na(vmp_wells$WellOwner)]
    
    #Initialize VMP table to add to dwr data
    stations_vmp = data.frame(matrix(NA, dim(vmp_wells)[1], length(stations_colnames)))
    colnames(stations_vmp) = stations_colnames
    
    #cross-walk columns and add entries to DWR columns such as "basin_name"
    stations_vmp$well_code = vmp_wells$well_code
    stations_vmp$well_name = vmp_wells$well_name
    stations_vmp$latitude = vmp_wells$latitude
    stations_vmp$longitude = vmp_wells$longitude
    stations_vmp$wlm_method = "UCDavis_Well_Review_2013"
    stations_vmp$wlm_acc = "Unknown"
    stations_vmp$basin_code = "1-005"
    stations_vmp$basin_name = "Scott River Valley"
    stations_vmp$county_name = "Siskiyou"
    stations_vmp$well_depth  = vmp_wells$Depth
    stations_vmp$well_use = vmp_wells$Use
    stations_vmp$well_type = "Single Well" #assumed for all irr and mon wells in network
    stations_vmp$wcr_log_no = vmp_wells$LogNumber
    stations_vmp$wcr_file_no = vmp_wells$WCR_File_N
    stations_vmp$well_loc_source = "UCDavis_Well_Review_2013"
    stations_vmp$well_loc_source[stations_vmp$well_name %in% new_names] = "RCD_Pumping_Test" 
    
    
    # Eliminate wells with repeat names. Some wells in the mon_wells shapefile are from DWR. 
    # Uniformly, there's more in the DWR table, so delete them from the VMP table.
    mon_wells_swn = c("42N09W02A002M", "42N09W27N001M", "42N09W27N002M", "43N09W02P002M", "43N09W23F001M", "43N09W24F001M", "44N09W25R001M", "44N09W28P001M")
    stations_vmp = stations_vmp[!(stations_vmp$well_code %in% mon_wells_swn),]
    
    
    # 2a) VMP wl obs data: read, clean, attach (Voluntary Monitoring Program in Scott Valley) -------------------------------------------------------
    
    #Note: Update this csv file name if you have an updated scott WL dataset
    vmp = read.csv(file.path(wl_dir,latest_vmp_wl_data_filename))
    colnames(vmp) = clean_colnames(colnames(vmp))
    
    # Clean Data
    #column types
    vmp$date = as.Date(vmp$date, format = "%m/%d/%Y")
    vmp$comments = as.character(vmp$comments)
    #get rid of commas, make numeric
    vmp$rp_elev_ft = gsub(",", "",vmp$rp_elev_ft)
    vmp$rp_elev_ft = as.numeric(vmp$rp_elev_ft)
    vmp$gp_elev_ft = gsub(",", "",vmp$gp_elev_ft)
    vmp$gp_elev_ft = as.numeric(vmp$gp_elev_ft)
    vmp$reading_at_rp_ft = as.numeric(vmp$reading_at_rp_ft)
    vmp$reading_at_rp_m = as.numeric(vmp$reading_at_rp_m)
    vmp$wse_ft = as.numeric(vmp$wse_ft)
    vmp$wse_m = as.numeric(vmp$wse_m)
    
    # fill in dtw readings (and RP elev, and GP elev) in ft or m
    missing_m_selector = !is.na(vmp$reading_at_rp_ft) & is.na(vmp$reading_at_rp_m)
    vmp$reading_at_rp_m[missing_m_selector] = vmp$reading_at_rp_ft[missing_m_selector] / 3.2808
    missing_ft_selector = is.na(vmp$reading_at_rp_ft) & !is.na(vmp$reading_at_rp_m)
    vmp$reading_at_rp_ft[missing_ft_selector] = vmp$reading_at_rp_m[missing_ft_selector] * 3.2808
    
    missing_rp_m_selector = !is.na(vmp$rp_elev_ft) & is.na(vmp$rp_elev_m)
    vmp$rp_elev_m[missing_rp_m_selector] = vmp$reading_at_rp_ft[missing_rp_m_selector] / 3.2808
    missing_rp_ft_selector = is.na(vmp$rp_elev_ft) & !is.na(vmp$rp_elev_m)
    vmp$rp_elev_ft[missing_rp_ft_selector] = vmp$reading_at_rp_m[missing_rp_ft_selector] * 3.2808
    
    # Fill in missing Ref Pt elevations
    missing_ref_pt_ft_selector = (is.na(vmp$rp_elev_ft) | vmp$rp_elev_ft=="") & !is.na(vmp$reading_at_rp_ft)
    all_obs_missing_rp_selector = vmp$well_code %in% unique(vmp$well_code[missing_ref_pt_ft_selector])
    missing_ref_pt_df = aggregate(vmp$rp_elev_ft[all_obs_missing_rp_selector],
                                  by = list(vmp$well_code[all_obs_missing_rp_selector]),
                                  FUN = max, na.rm = T)
    colnames(missing_ref_pt_df)=c("well_code", "rp_elev_ft")
    
    # For Ref Pt elev for wells that have not yet been surveyed, find ground elevation
    no_rp_survey = missing_ref_pt_df$well_code[!is.finite(missing_ref_pt_df$rp_elev_ft)]
    # These wells are not in the fucking shapefile, so we need to add them
    rp_missing_sp = data.frame(well_code = c("E57", "W31", "S67"), # Still not sure of whereabouts of R23
                               latitude = c(41.520370, 41.448185, 41.631569),
                               longitude = c(-122.902587, -122.854899, -122.910165),
                               lat_copy = c(41.520370, 41.448185, 41.631569),        # keep for assigning to vmp stations table
                               long_copy = c(-122.902587, -122.854899, -122.910165)) # keep for assigning to vmp stations table
    rp_missing_sp = st_as_sf(rp_missing_sp, coords = c("longitude","latitude"),
                             crs = crs("+init=epsg:4326"))
    rp_missing_sp = st_transform(rp_missing_sp, crs = crs(dem_watershed))
    rpm_elev_values = terra::extract(x = dem_watershed, y = rp_missing_sp, ID=F)
    rp_missing_sp$rp_elev_m = rpm_elev_values$USGS_13_n42w123_20210623
    
    # Now add them back into the stations table
    stations_vmp_rpmissing = stations_vmp[1:3,]
    stations_vmp_rpmissing[1:3,]=NA
    stations_vmp_rpmissing$well_code = rp_missing_sp$well_code
    stations_vmp_rpmissing$well_name = rp_missing_sp$well_code
    stations_vmp_rpmissing$latitude = rp_missing_sp$lat_copy
    stations_vmp_rpmissing$longitude = rp_missing_sp$long_copy
    stations_vmp_rpmissing$well_loc_source = "New_VMP_Wells_SGMA_2020"
    stations_vmp_rpmissing$wlm_acc = "Unknown"
    stations_vmp_rpmissing$basin_code = "1-005"
    stations_vmp_rpmissing$basin_name = "Scott River Valley"
    stations_vmp_rpmissing$county_name = "Siskiyou"
    stations_vmp_rpmissing$well_type = "Single Well" #assumed for all irr and mon wells in network
    stations_vmp = rbind(stations_vmp, stations_vmp_rpmissing)
    
    # OKAY. Back to filling in missing RPs.
    # Fill in 3 missing ref pt dfs. Convert to feet. (Currently missing lat-long info for R23. )
    inf_rp = !is.finite(missing_ref_pt_df$rp_elev_ft) & 
      missing_ref_pt_df$well_code %in% rp_missing_sp$well_code
    missing_ref_pt_df$rp_elev_ft[inf_rp] = 
      3.28084 * rp_missing_sp$rp_elev_m[match(missing_ref_pt_df$well_code[inf_rp],
                                              rp_missing_sp$well_code)]
    no_ref_pt_ft_selector = (!is.na(vmp$reading_at_rp_ft) | !is.na(vmp$reading_at_rp_ft)) &
      (is.na(vmp$rp_elev_ft) | vmp$rp_elev_ft=="")
    vmp$rp_elev_ft[no_ref_pt_ft_selector] = 
      missing_ref_pt_df$rp_elev_ft[match(vmp$well_code[no_ref_pt_ft_selector],
                                         missing_ref_pt_df$well_code)]
    vmp$rp_elev_m[no_ref_pt_ft_selector] = vmp$rp_elev_ft[no_ref_pt_ft_selector] / 3.28084
    
    # calculate GW Elev for readings with a DTW reading but no GW elev reading
    dtw_but_no_elev_selector = (!is.na(vmp$reading_at_rp_ft) | !is.na(vmp$reading_at_rp_ft)) & 
      (is.na(vmp$wse_ft) | vmp$wse_ft=="")
    vmp$wse_ft[dtw_but_no_elev_selector] = 
      vmp$rp_elev_ft[dtw_but_no_elev_selector] - vmp$reading_at_rp_ft[dtw_but_no_elev_selector]
    vmp$wse_m[dtw_but_no_elev_selector] = vmp$wse_ft[dtw_but_no_elev_selector] / 3.28084
    
    # Remove points that haven't yet been surveyed
    vmp = vmp[vmp$rp_elev_ft != "" & !is.na(vmp$rp_elev_ft),]
    
    #Initialize VMP table to add to dwr data
    wl_obs_vmp = data.frame(matrix(NA, dim(vmp)[1], length(wl_obs_colnames)))
    colnames(wl_obs_vmp) = wl_obs_colnames
    
    # recalculate elevations
    
    #cross-walk columns
    wl_obs_vmp$well_code = vmp$well_code
    wl_obs_vmp$date = vmp$date
    wl_obs_vmp$rp_elev_ft = vmp$rp_elev_ft
    wl_obs_vmp$gp_elev_ft = vmp$gp_elev_ft
    wl_obs_vmp$reading_rp_ft = vmp$reading_at_rp_ft
    wl_obs_vmp$wse_ft = vmp$wse_ft
    wl_obs_vmp$gs_to_ws_ft = vmp$gsws_ft
    wl_obs_vmp$wlm_org_name = vmp$data_source
    # array_of_colnames[array_of_colnames == "rdng_ws"] = "???"
    # array_of_colnames[array_of_colnames == "rpe_wse"] = "???"
    wl_obs_vmp$data_table_source = "Scott_VMP"
    
    #Aggregate comments and outlier notes into one column, and cross-walk
    vmp$outlier[is.na(vmp$outlier)]=0 # clean up NAs for logical statement in next line
    vmp$comments[vmp$outlier == 1 & vmp$comments != ""] = paste(vmp$comments[vmp$outlier == 1 & vmp$comments!=""], "; noted as outlier")
    vmp$comments[vmp$outlier == 1 & vmp$comments == ""] = "noted as outlier"
    wl_obs_vmp$comments = vmp$comments
    
    # Aggregate NM and QM codes into one column, and cross-walk
    vmp$meas_codes = NA
    qm = vmp$tblqm_codes_description != "" 
    nm = vmp$tblnm_codes_description != ""
    vmp$meas_code[qm] = paste("QM:", vmp$tblqm_codes_description[qm])
    vmp$meas_code[nm] = paste("NM:", vmp$tblnm_codes_description[nm])
    wl_obs_vmp$wlm_qa_desc = vmp$meas_code
    # get rid of NA rows
    wl_obs_vmp = wl_obs_vmp[!is.na(wl_obs_vmp$date),]
    
    #2c) VMP perf info: clean, attach to DWR ------------------------------------------
    # attach VMP perf info (available for irrigation wells only) to DWR perf info
    
    #select irr wells with perf info
    irr_perf = irr_wells[!is.na(irr_wells$PerfBottom) & !is.na(irr_wells$PerfTop),]
    
    perf_vmp_irr = data.frame(matrix(data=NA, nrow = dim(irr_perf)[1], ncol = length(perf_colnames)))
    colnames(perf_vmp_irr) = perf_colnames
    
    #cross-walk columns
    perf_vmp_irr$well_code = irr_perf$well_code
    perf_vmp_irr$top_prf = irr_perf$PerfTop
    perf_vmp_irr$bot_prf = irr_perf$PerfBottom
    
  }
  
  
  # 3) LWA_TD data: read, calculate WSE, attach (Larry Walker Transd --------
  
  if(LWA_TD == TRUE){
    
    ### 1)  Pull continuous transducer tables off the AWS server and combine into one table
    
    l = pull_continuous_data()
    
    # subset data tables: only include ones with an ID column for sorting. Would sure be nice if these tables had names in this list
    tables_with_ID = vector(length = 0) 
    for(i in 1:length(l$data)){
      if(is.element(el = "ID", set = colnames(l$data[[i]]))){
        tables_with_ID = append(tables_with_ID, i)
      }
    }
    
    ### 2) Read station information
    
    # read the field data used to calibrate the data (lives on Dropbox)
    calib  <- read_field_data("https://www.dropbox.com/sh/osl85d9rc9bvf2o/AADtm376mJEEMkqZuzB4_Hcva/gwdata.xlsx?dl=1")
    
    # calculate depth to water from transducer pressure reading and baro reading at SHA_06
    is_bubbler <- c("SHA_24", "BUT_185") # replace this with a calib file
    
    # Compute dtw for each sensor table. Only include ones with ID column
    ## Note, 8/15/2022: this is the more elegant way to do this, but I can't troubleshoot which ID is fucking up inside lapply.
    # lwa_all <- lapply(X = l$data[tables_with_ID], 
    #                   FUN = compute_dtw, 
    #                   baro = l$baro_df, 
    #                   calib_data = calib$data, 
    #                   field_data = calib$field, 
    #                   is_bubbler = is_bubbler) %>% 
    #   bind_rows()
    
    for(i in 1:length(tables_with_ID)){
      tab_with_ID_index = tables_with_ID[i]
      print(tab_with_ID_index)
      data_tab = l$data[[tab_with_ID_index]]
      stn_dtw = compute_dtw(data = data_tab, 
                            baro = l$baro_df, calib_data = calib$data,
                            field_data = calib$field, is_bubbler = is_bubbler)
      if(i==1 & sum(!is.na(stn_dtw)) > 0){lwa_all = stn_dtw}
      if(i>1 & sum(!is.na(stn_dtw)) > 0){lwa_all = bind_rows(lwa_all, stn_dtw)}
    }
    
    # Currently (3/24/2021) this is fucking up on line 433. Somehow the compute_dtw function has a needs 155 or 1 but gets 0 problem.
    # Today (7/12/2021) I figured this out. July Claire is smug about this. March Claire is irate it took so long.
    # Once again (8/12/2022) I meet my old enemy, this lapply() function. Sigh.
    # (8/15/2022) I have fixed it! But I had to unwrap it from lapply() to trouble-shoot it. 
    
    ### 3) Reduce dtw measurements to 1 measurement per well per week
    # find the highest water level in each well in each week, to best represent static water level and avoid transient pumping impacts
    
    lwa = downsample_wl(data = lwa_all, time_period = "week", find = "max_gw_elevation")
    
    ### 4) Reformat lwa wl_obs data
    wl_obs_lwa = data.frame(matrix(NA, nrow = dim(lwa)[1], ncol = length(wl_obs_colnames)))
    colnames(wl_obs_lwa) = wl_obs_colnames
    
    # Cross-walk columns
    wl_obs_lwa$well_code = lwa$ID
    wl_obs_lwa$date = as.Date(lwa$TIMESTAMP)
    wl_obs_lwa$reading_rp_ft = lwa$dtw_ft * -1 #convert from negative DTW values in lwa
    wl_obs_lwa$wlm_org_name = "LWA for Siskiyou GSA"
    wl_obs_lwa$data_table_source = "LWA GWO"
    wl_obs_lwa$comments = "highest weekly transducer measurement"
    wl_obs_lwa$wlm_qa_desc = "transducer data; reference point accuracy TBD"
    
    #Assign reference points
    wl_obs_lwa$gp_elev_ft = calib$data$Elevation_ft[match(lwa$ID, calib$data$Site_ID)]
    wl_obs_lwa$rp_elev_ft = wl_obs_lwa$gp_elev_ft # get from a survey someday
    
    # Calculate water surface elevation
    # NOTE: Currently the groundwater surface elevation is calculated w.r.t.
    # the estimated ground surface, not to a surveyed reference point.
    # We are keeping 2 decimal places, because the transducer can achieve this accuracy, 
    # but the accuracy of elevations relative to other WLs should be + or - 10 feet or so.
    wl_obs_lwa$wse_ft = round(wl_obs_lwa$rp_elev_ft - wl_obs_lwa$reading_rp_ft, 2) #Calculated from GP (Ground Point, or ground surface estimate)
    # A reference point survey would improve the LWA elevation data accuracy.    
    
    ### 5) Reformat lwa station data
    #Initialize lwa table
    stations_lwa = data.frame(matrix(NA, dim(calib$data)[1], length(stations_colnames)))
    colnames(stations_lwa) = stations_colnames
    
    #cross-walk columns
    stations_lwa$well_code = calib$data$Site_ID
    # stations_lwa$well_name = calib$data$Description # Keep this Description confidential in hydrographs!
    stations_lwa$latitude = calib$data$lat
    stations_lwa$longitude = calib$data$lon
    stations_lwa$wlm_method = "Cell phone GPS (lat-long) and Google Earth pinpoint (elev)"
    stations_lwa$wlm_acc = "5 ft" # assumed for standard cell phone GPS accuracy
    stations_lwa$county_name = "Siskiyou"
    stations_lwa$well_use = calib$data$WellType
    stations_lwa$well_type = "Single Well"
    # stations_lwa$wcr_log_no # future goal: add this info
    # stations_lwa$wcr_file_no # future goal: add this info
    stations_lwa$well_loc_source = "LWA GWO"
    
    #Assign and clean well depth
    well_depths = as.numeric(calib$data$TotalDepth_ft) # this coerces to NA two values with "?"
    stations_lwa$well_depth = well_depths
    filtered_out = is.na(well_depths) & !is.na(calib$data$TotalDepth_ft)
    stations_lwa$comments[filtered_out] = paste("Tot. Depth estimated",calib$data$TotalDepth_ft[filtered_out], "ft")
    
    # Assign basin names and codes
    basin_codes = data.frame("basin_ID_lwa" = c("BUT", "SHA", "SCT", "SCO"),
                             "basin_ID_dwr" = c("Butte Valley", "Shasta Valley", "Scott River Valley", "Scott River Valley"),
                             "basin_num_dwr" = c("1-003", "1-004", "1-005", "1-005"))
    stations_lwa$basin_code = basin_codes$basin_num_dwr[match(calib$data$Basin, basin_codes$basin_ID_lwa)]
    stations_lwa$basin_name = basin_codes$basin_ID_dwr[match(calib$data$Basin, basin_codes$basin_ID_lwa)]
    
    
    ### 6) When available, reformat perforation info for LWA wells 
  }
  
  #4) PSI data: read, clean, attach (private Plant Sciences data from Butte Valley) -------
  
  if(PSI == TRUE){
    wl_obs_psi = read.csv(file.path(wl_dir,"psi_wl_obs.csv"))
    stations_psi =  read.csv(file.path(wl_dir,"psi_wells.csv"))
    
    #wl observations: clean up columns and data types
    wl_obs_psi$date = as.Date(wl_obs_psi$date, format = "%m/%d/%Y")
    wl_obs_psi$well_code = stations_psi$well_code[match(wl_obs_psi$well_code, stations_psi$well_name)]
    wl_obs_psi$stn_id = NA
    wl_obs_psi$rdng_ws = NA
    wl_obs_psi$gs_to_ws_ft = NA
    wl_obs_psi$wlm_id = NA
    wl_obs_psi$wlm_org_id = NA
    wl_obs_psi$wlm_org_name = "Butte Valley Private Monitoring Network"
    wl_obs_psi$coop_agency_org_id = NA
    wl_obs_psi$data_table_source = "Butte Valley Private Monitoring Network"
    
    #Assign reference points. currently using ground point elevation estimated from Google Earth. 
    wl_obs_psi$rp_elev_ft = wl_obs_psi$gp_elev_ft # get from a survey someday
    
    # wells (stations): 
    stations_psi$comments = NA
    
    
  }
  
  
  # #5) Quartz Valley data: read, clean, attach -----------------------------
  
  if(QV == TRUE){
    wl_years = 2007:2020
    
    for(i in 1:length(wl_years)){
      sheet=read_xlsx(path = file.path(wl_dir, "2020.10.07_Quartz Valley Drinking Water Well WLs_07-20.xlsx"),
                      sheet=i)
      colnames(sheet)[1] = "well_name"
      sheet_long = pivot_longer(data = sheet, cols = colnames(sheet)[-1], 
                                names_to = "date", values_to = "dtw_ft_bgs")
      sheet_long$date = as.Date(as.numeric(sheet_long$date), origin = as.Date("1899-12-30"))
      sheet_long = sheet_long[!is.na(sheet_long$dtw_ft_bgs),]
      
      if(i==1){
        qv_dtw = sheet_long
      } else {
        qv_dtw = rbind(qv_dtw, sheet_long)
      }
      
    }
    
    latlong = read_xlsx(path = file.path(wl_dir, "2020.10.07_Quartz Valley Drinking Water Well WLs_07-20.xlsx"),
                        sheet=15)
    # Process lat long info
    latlong$Lon_decimal = latlong$Lon_decimal*-1
    latlong$Lat_decimal2 = latlong$Lat_decimal  #store these columns for crosswalking later
    latlong$Lon_decimal2 = latlong$Lon_decimal  #store these columns for crosswalking later
    # Process names (order by road name)
    # Add a rd for Gymnasium
    latlong$`Well Name`[latlong$`Well Name` == "Gymnasium"] = "Gymnasium_QuartzValleyRd"
    latlong$road_name = matrix(unlist(strsplit(latlong$`Well Name`, "_")), 
                               ncol = 2, byrow=T)[,2]
    latlong$road_address_num = matrix(unlist(strsplit(latlong$`Well Name`, "_")), 
                                      ncol = 2, byrow=T)[,1]
    latlong = latlong[order(latlong$road_name, latlong$road_address_num), ]
    
    
    # Pull RPs from ground elevation
    qv_wells = latlong
    qv_wells = st_as_sf(qv_wells, coords = c("Lon_decimal", "Lat_decimal"),
                        crs = crs("+init=epsg:4326"))
    qv_wells = st_transform(st_as_sf(qv_wells), crs(dem_watershed))
    qv_ground_elev = terra::extract(x = dem_watershed, y = qv_wells, ID=F)
    qv_wells$ground_elev_mamsl = qv_ground_elev$USGS_13_n42w123_20210623
    
    #add RP to qv_dtw data
    qv = qv_dtw
    qv$gp_elev_ft = qv_wells$ground_elev_mamsl[match(qv$well_name, qv_wells$`Well Name`)] * 3.28084 #meters to feet amsl
    qv$wse = qv$gp_elev_ft - qv$dtw_ft_bgs
    
    # Crosswalk columns
    #Initialize QV table
    wl_obs_qv = data.frame(matrix(NA, nrow(qv), length(wl_obs_colnames)))
    colnames(wl_obs_qv) = wl_obs_colnames
    
    #cross-walk columns
    wl_obs_qv$well_code = qv$well_name # Overwrite this below, to avoid showing confidential addresses
    wl_obs_qv$date = qv$date
    wl_obs_qv$gp_elev_ft = qv$gp_elev_ft
    wl_obs_qv$gs_to_ws_ft = qv$dtw_ft_bgs
    wl_obs_qv$wse_ft = qv$wse
    # AS OF JAN. 2021, NO RP INFORMATION, ONLY GP INFO FROM DEM
    # For convenience, going to set "rp_elev_ft" to "gp_elev_ft" and "reading_rp_ft" to "gs_to_ws_ft"
    # This will let us make contour maps with the other data
    # BUT no RP info has been measured
    wl_obs_qv$rp_elev_ft = qv$gp_elev_ft
    wl_obs_qv$reading_rp_ft = qv$dtw_ft_bgs
    wl_obs_qv$wlm_org_name = "Quartz Valley IR"
    wl_obs_qv$data_table_source = "Quartz Valley IR"
    
    #Initialize stations table
    stations_qv = data.frame(matrix(NA, nrow(qv_wells), length(stations_colnames)))
    colnames(stations_qv) = stations_colnames
    
    #cross-walk columns
    stations_qv$well_code = c(paste0("QV0",1:9), paste0("QV",10:nrow(qv_wells))) # Anonymous well code
    stations_qv$well_name = qv_wells$`Well Name` # Includes address of well
    stations_qv$latitude = qv_wells$Lat_decimal2
    stations_qv$longitude = qv_wells$Lon_decimal2
    # stations_qv$wlm_method = "Cell phone GPS (lat-long) and Google Earth pinpoint (elev)"
    # stations_qv$wlm_acc = "5 ft" # assumed for standard cell phone GPS accuracy
    stations_qv$basin_code = "1-005"
    stations_qv$basin_name = "Scott River Valley"
    stations_qv$county_name = "Siskiyou"
    stations_qv$well_use = "Domestic"
    stations_qv$well_type = "Single Well"
    # stations_qv$wcr_log_no # future goal: add this info
    # stations_qv$wcr_file_no # future goal: add this info
    stations_qv$well_loc_source = "Quartz Valley IR"
    
    
    #Overwrite wl obs with QVIR code
    
    wl_obs_qv$well_code = stations_qv$well_code[match(wl_obs_qv$well_code,stations_qv$well_name)]
    
    # add to wl_obs_qv and stations_qv
    
  }  
  
  # Combine WL datasets and clean -------------------------------------------
  
  
  #Figure out what to return as the WL dataframe
  
  #Initialize combo tables
  wl_obs = data.frame(matrix(NA, nrow = 0, ncol = length(wl_obs_colnames)));   colnames(wl_obs) = wl_obs_colnames
  stations = data.frame(matrix(NA, nrow =0, ncol = length(stations_colnames)));   colnames(stations) = stations_colnames
  perf = data.frame(matrix(NA, nrow = 0, ncol = length(perf_colnames)));  colnames(perf) = perf_colnames
  
  
  if(VMP == TRUE){
    wl_obs = rbind(wl_obs, wl_obs_vmp)
    stations = rbind(stations, stations_vmp)
    perf = rbind(perf, perf_vmp_irr)
  }
  if(DWR == TRUE){
    wl_obs = rbind(wl_obs, wl_obs_dwr)
    stations = rbind(stations, stations_dwr)
    perf = rbind(perf, perf_dwr)
  }
  if(LWA_TD == TRUE){
    wl_obs = rbind(wl_obs, wl_obs_lwa)
    stations = rbind(stations, stations_lwa)
    # perf = rbind(perf, perf_lwa) # don't currently have any perf info for these wells
  }
  if(QV == TRUE){
    wl_obs = rbind(wl_obs, wl_obs_qv)
    stations = rbind(stations, stations_qv)
    # perf = rbind(perf, perf_qv) # don't currently have any perf info for these wells
  }
  if(PSI == TRUE){
    wl_obs = rbind(wl_obs, wl_obs_psi)
    stations = rbind(stations, stations_psi)
    # perf = rbind(perf, perf_psi) # don't currently have any perf info for these wells
  }
  
  #Cleaning
  if(clean==TRUE){
    # column conversions
    wl_obs$date = as.Date(wl_obs$date)
    wl_obs$wse_ft = as.numeric(wl_obs$wse_ft)
    wl_obs$rp_elev_ft = as.numeric(wl_obs$rp_elev_ft)
    wl_obs$gp_elev_ft = as.numeric(wl_obs$gp_elev_ft)
    wl_obs$reading_rp_ft = as.numeric(wl_obs$reading_rp_ft)
    
    # Fill in the gs_to_ws column with APPROX values from reading_rp_feet
    missing_gs_to_ws_val = is.na(wl_obs$gs_to_ws_ft)
    wl_obs$gs_to_ws_ft[missing_gs_to_ws_val] = wl_obs$reading_rp_ft[missing_gs_to_ws_val]
    
    # Fill in the missing gp_elev_ft  values with APPROX values from rp_elev_ft
    missing_gp_ft = is.na(wl_obs$gp_elev_ft)
    wl_obs$gp_elev_ft[missing_gp_ft] = wl_obs$rp_elev_ft[missing_gp_ft]
    
    #Duplicate removal using local IDs
    id_date_elev = paste0(wl_obs$well_code, wl_obs$date, wl_obs$wse_ft)
    wl_obs = wl_obs[!duplicated(id_date_elev),]
    
    # Remove records with no WL measurement
    # VMP note: For some reason PH is putting "NM" in the reading_rp column, and some records are just included as blanks with no notes.
    # Probably an artifact of data collection for VMP. My guess is he just copies all the wells each month and leaves blank whichever don't get measured.
    
    # Exclude measurements with "NM" as the reference point reading. or "MN," a typo, and one with just "QM" with no measurement
    wl_obs = wl_obs[!(wl_obs$reading_rp_ft %in% c("NM", "MN", "QM")),]
    # Exclude measurements with no reading (from reference point or ground surface)
    wl_obs$gs_to_ws_ft[wl_obs$gs_to_ws_ft %in% c("", "#VALUE!")] = NA
    wl_obs = wl_obs[!(is.na(wl_obs$reading_rp_ft) & is.na(wl_obs$gs_to_ws_ft)),]
    wl_obs = wl_obs[-grep(wl_obs$wlm_qa_desc, pattern = "NM:"),] # Remove ones for which there is a documented no measurement
    
    #Try to fill in NA values with depth to water (rp) readings, if presesnt
    wse_na = is.na(wl_obs$wse_ft) | wl_obs$wse_ft == ""
    wl_obs$wse_ft[wse_na] = wl_obs$rp_elev_ft[wse_na] - wl_obs$reading_rp_ft[wse_na]
    #then exclude 
    wl_obs = wl_obs[!(is.na(wl_obs$wse_ft)),] # Remove records with no reading and no WSE entry
    
  }
  
  #Return all variables
  return(list(wl_obs, stations, perf))
  
}


archive_wl_data = function(wl_obs, wells, perf){
  #Update the live "wl_observations" and "wells" tables, and make an archive datestamped copy
  #Archive
  wl_archive_name = paste0("wl_observations_",format(Sys.Date(), "%Y.%m.%d"),".csv" )
  wells_archive_name = paste0("wells_",format(Sys.Date(), "%Y.%m.%d"),".csv" )
  perf_archive_name = paste0("perf_",format(Sys.Date(), "%Y.%m.%d"),".csv")
  
  if(save_data_to_archive==T & 
     !file.exists(file.path(dms_archive_dir, wl_archive_name))){
    write.csv(wl_obs, file.path(dms_archive_dir,wl_archive_name))
    write.csv(wells, file.path(dms_archive_dir,wells_archive_name))
    write.csv(perf, file.path(dms_archive_dir,perf_archive_name))
  }
  
}

# Get an updated table of water levels and associated stations

# The acronyms VMP, DWR, etc. refer to various water level sources in the 
# local context of Scott Valley. The PSI data source is for an area of Siskiyou
# County not overlapping with the Scott. The LWA_TD data source contains
# 15-minute transducer data, which adds another layer of complexity and is not
# needed for this manuscript.
wl_info = get_wl_data(VMP = TRUE, DWR = TRUE, 
                      PSI = FALSE, LWA_TD = FALSE,
                      QV = TRUE, clean = TRUE)
wl_obs = wl_info[[1]]
wells = wl_info[[2]]
perf = wl_info[[3]]

# save updated wl data in archive
# will only archive if save_data_to_archive == TRUE
archive_wl_data(wl_obs, wells, perf)



# NOAA NCDC ---------------------------------------------------------------

#Subfunctions
get_bbox_string = function(poly, return_half = 0){
  poly = st_transform(poly, crs("+init=epsg:4326")) #convert to WGS84)
  north = st_bbox(poly)$ymax; west = st_bbox(poly)$xmin; south = st_bbox(poly)$ymin; east = st_bbox(poly)$xmax
  bbox_poly = paste(c(south, west, north, east), collapse = ",")
  if(return_half>0){ ns_midpoint = south + 0.5*(north-south)}
  if(return_half == 1){bbox_poly = paste(c(ns_midpoint, west, north, east), collapse = ",")}
  if(return_half == 2){bbox_poly = paste(c(south, west, ns_midpoint, east), collapse = ",")}
  return(bbox_poly)
}

get_noaa_stations = function(cmk_token = "scKXkYaFjbbLtxmSnNYjWKBKXDvOCoeU", #received from NOAA on 2019-08-09: https://www.ncdc.noaa.gov/cdo-web/token
                             list_of_bboxes){

  #Initialize station table
  colnames_station_table = c("results.elevation","results.mindate",
                             "results.maxdate","results.latitude","results.name",
  
                                                        "results.datacoverage","results.id",
                             "results.elevationUnit","results.longitude")
  station_table = data.frame(matrix(NA, nrow = 0, ncol = 9))
  colnames(station_table) = colnames_station_table
  
  for(i in 1:length(list_of_bboxes)){
    bbox_string = list_of_bboxes[[i]]
    base_url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/"
    stations_query_url = paste0(base_url, "stations?extent=", bbox_string)
    header_token <- structure(cmk_token , names = "token")
    # header_limit <- structure(200 , names = "limit")
    stations_dl = GET(stations_query_url, httr::add_headers(header_token))#, header_limit))
    
    #Parse download
    stations_dl_unlisted = unlist(content(stations_dl, "parsed" ))
    metadata=stations_dl_unlisted[1:3]
    stations_dl_unlisted = stations_dl_unlisted[-(1:3)] # scrape off 3 metatata arguments
    stations_colnames = names(stations_dl_unlisted[1:9])
    noaa_stations = data.frame(matrix(stations_dl_unlisted, ncol = 9, byrow = T))
    colnames(noaa_stations) = stations_colnames
    
    station_table = rbind(station_table, noaa_stations)
  }
  #Eliminate duplicates
  # sum(duplicated(station_table))
  station_table = station_table[!duplicated(station_table),]
  #Split up results ID into dataset type and station ID
  results_id_split=matrix(unlist(strsplit(as.character(station_table$results.id), split=":")), ncol=2, byrow=T)
  station_table$results.type = results_id_split[,1]
  station_table$station.id = results_id_split[,2]
  
  #Make stations table spatial
  stations_sp = station_table
  stations_sp$results.latitude = as.numeric(as.character(stations_sp$results.latitude))
  stations_sp$results.longitude = as.numeric(as.character(stations_sp$results.longitude))
  
  stations_sp = st_as_sf(stations_sp, 
                         coords = c("results.longitude", "results.latitude"), 
                         crs = 4326)

  return(list(stations_sp, station_table))
}

get_noaa_data = function(station_list){
  # More info: https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily
  
  base_url = "https://www.ncei.noaa.gov/access/services/data/v1"
  dataset = "daily-summaries"
  # station_list = c("USC00041316","USC00043182","USC00042899", "USC00043614", "USC00049866", "US1CASK0005")
  stations = paste(station_list,collapse = ",") # Callahan, Ft Jones, Etna, Greenview, Yreka, Yreka NW
  start_date = "1800-01-01"
  end_date = Sys.Date()
  data_types = paste(c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"), collapse=",")
  
  noaa_url = paste0(base_url, "?dataset=", dataset, 
                    "&stations=", stations,
                    "&startDate=", start_date,
                    "&endDate=", end_date,
                    "&dataTypes=", data_types,
                    "&format=csv",
                    "&includeAttributes=0&includeStationName=true&includeStationLocation=true",
                    "&units=metric")
  
  noaa_dl = GET(noaa_url)
  noaa = as.data.frame(content(noaa_dl, "parsed", 
                               col_types = cols("STATION" = col_character(),
                                                "DATE" = col_date(format = ""),
                                                "PRCP" = col_double(),
                                                "SNOW" = col_double(),
                                                "SNWD" = col_double(),
                                                "TMAX" = col_double(),
                                                "TMIN" = col_double()) ))
  return(noaa)
}

archive_noaa_data = function(noaa_data){
  
  #Update the live tables, and make an archive datestamped copy
  #Archive
  noaa_archive_name = paste0("noaa_daily_data_ghcnd_",format(Sys.Date(), "%Y.%m.%d"),".csv" )
  if(save_data_to_archive==T & 
     !file.exists(file.path(dms_archive_dir, noaa_archive_name))){
    write.csv(noaa_data, file.path(dms_archive_dir,noaa_archive_name))
  }
}


#Pull station list from noaa website
bbox_scott = get_bbox_string(watershed)
station_info = get_noaa_stations(list_of_bboxes = bbox_scott)


station_sp = station_info[[1]]; station_sp = st_transform(station_sp, CRS("+init=epsg:3310"))
station_table = station_info[[2]]
#Only include GHCND stations. These are historical daily records.
ghcnd_stations = station_table$station.id[station_table$results.type == "GHCND"]
noaa_updated_dataset = get_noaa_data(station_list = ghcnd_stations)

archive_noaa_data(noaa_data = noaa_updated_dataset) # only archives if save_data_to_archive == TRUE

noaa = noaa_updated_dataset
noaa_stations = station_table[station_table$results.type == "GHCND",]
noaa_station_sp = station_sp

rm(list = c("noaa_updated_dataset", "station_table", "station_sp")) # remove older variable names

# Notes: optional extra weather datasets (for scott; could be others in other basins)
# TMAX 	Maximum temperature 
# TMIN 	Minimum temperature 
# TOBS 	Temperature at the time of observation
# DAPR 	Number of days included in the multiday precipitation total (MDPR) 	1949-12-19 	2018-11-29 	
# MDPR 	Multiday precipitation total (use with DAPR and DWPR, if available) 	1949-12-19 	2018-11-29 	
# PRCP 	Precipitation #tenths of a mm
# SNOW 	Snowfall 
# SNWD 	Snow depth
# WT01 	Fog, ice fog, or freezing fog (may include heavy fog)
# WT03 	Thunder
# WT04 	Ice pellets, sleet, snow pellets, or small hail" 
# WT05 	Hail (may include small hail) 
# WT06 	Glaze or rime 
# WT08 	Smoke or haze 
# WT09 	Blowing or drifting snow
# WT11 	High or damaging winds 
# WT14 	Drizzle 
# WT16  Rain (may include freezing rain, drizzle, and freezing drizzle)" 



# CDEC Snow ---------------------------------------------------------------

# Note, cdec snow retrieval data has been extremely buggy; current approach is to manually download 
# cdec snow data files and load from local (see section below)


# scott_stations = cdec_stations(river_basin = "scott r") #can also call using county = "siskiyou"
# 
# # #visualize whole county
# # county_stations = cdec_stations(county = "siskiyou") #can also call using county = "siskiyou"
# # cdec_sp=county_stations
# # coordinates(cdec_sp) = ~longitude + latitude
# # proj4string(cdec_sp) <- CRS("+init=epsg:4326") #assign WGS84 projection to coordinates
# # cdec_sp = st_transform(cdec_sp, crs("+init=epsg:3310"))
# #
# # plot(watershed)
# # plot(riv, add=T, col = "blue")
# # plot(cdec_sp, add=T)
# 
# #initialize table of stations and available sensors
# datasets = cdec_datasets(scott_stations$station_id[1])
# datasets$station = scott_stations$station_id[1]
# 
# for(i in 2:dim(scott_stations)[1]){
#   dataset = cdec_datasets(scott_stations$station_id[i])
#   dataset$station = scott_stations$station_id[i]
#   datasets = rbind(datasets, dataset)
# }
# 
# snow_sensors = c(3,18,82,73) # snow water content, snow depth, snow water content revised, snow auxiliary reading
# scott_snow_stations = unique(datasets$station[datasets$sensor_number %in% snow_sensors])
# #Scott snow stations with old monthly records:
# snow_stn_M = sort(c("mbl", "bxc", "mbv", "mb3", "log", "dym", "etn", "SWJ")) #evaluated in the tabular_data_upload script
# 
# # View(datasets[datasets$station %in% scott_snow_stations,])
# 
# # currently, this cdec retrieval method does not work
# 
# snow_records = data.frame(matrix(NA,0,5)); colnames(snow_records)=c("agency_cd", "location_id", "datetime","parameter_cd","parameter_value")
# snow_record_annual_max = data.frame(matrix(NA,0,4)); colnames(snow_record_annual_max)=c("water_year", "parameter_value_max","parameter_cd", "location_id")
# for(i in 1:length(snow_sensors)){
#   for(j in 1:length(snow_stn_M)){
#     record = cdec_query(station = as.character(snow_stn_M[j]),
#                         sensor_num = as.character(snow_sensors[i]),
#                         dur_code = "M",
#                         start_date = "1946-01-01", end_date = Sys.Date())
#     #combine into full snow record
#     record$parameter_cd=snow_sensors[i]
#     snow_records=rbind(snow_records,record)
#     print(paste("sensor" , snow_sensors[i], "; station", snow_stn_M[j],";", dim(record)[1], "records"))
#     # plot(record$datetime,record$parameter_value, pch=15,
#     #      xlab = "Date", type = "l",
#     #      main = paste("sensor" , snow_sensors[i], "; station", scott_snow_stations[j]))
# 
#     #calculate annual max for each record (except for 3, since that's not returning any data)
#     if(sum(is.na(record$datetime)) == nrow(record)){break}
#     if(snow_sensors[i]!=3){
#       record$wy = year(record$datetime)
#       record$wy[record$wy>9]=1+record$wy[record$wy>9]
#       record_annual_max = aggregate(record$parameter_value, by=list(record$wy), FUN="max", na.rm=T)
#       colnames(record_annual_max) = c("water_year", "parameter_value_max")
#       record_annual_max$location_id=snow_stn_M[j]; record_annual_max$parameter_cd=snow_sensors[i]
#       snow_record_annual_max=rbind(snow_record_annual_max, record_annual_max)
#     }
#   }
# }


#_LOAD FROM LOCAL ---------------------------------------------------------



# CDEC snow ---------------------------------------------------------------

# downloaded from: https://cdec.water.ca.gov/dynamicapp/snowQuery

snow_dir = file.path(local_data_dir,"CDEC Snow Data")
snow_record_csvs = list.files(path = snow_dir)


for(i in 1:length(snow_record_csvs)){
  snow_csv = snow_record_csvs[i]
  # read csv
  snowtab = read.csv(file = file.path(snow_dir, snow_csv))

  #get the station code
  name_pieces = unlist(strsplit(snow_csv," "))
  code_selector = grep(pattern = ")", x = name_pieces)
  stn_code = name_pieces[code_selector]
  stn_code = substr(x = stn_code, start = 2, stop = 4) # remove parens
  snowtab$stn_code = stn_code # add this column to the table
  #append them together
  if(i==1){snow_cdec = snowtab} else {
    snow_cdec = rbind(snow_cdec, snowtab)
  }
}

colnames(snow_cdec) = c("Month", "Date", "Depth_in", "na_col","Water_Content_in",
                        "na_col2","Density","Station_code")
snow_cdec=snow_cdec[,c("Month", "Date", "Depth_in", "Water_Content_in","Density","Station_code")]
snow_stns = unique(snow_cdec$Station_code)
# 
# process snow stations for spatial layer
scott_stations = cdec_stations(river_basin = "scott r") #can also call using county = "siskiyou"

snow_stns = toupper(unique(scott_stations$station_id))
snow_stn_sp = cdec_stations(river_basin = "scott r")
snow_stn_sp$station_id=toupper(snow_stn_sp$station_id)
snow_stn_sp = snow_stn_sp[snow_stn_sp$station_id %in% snow_stns,]

snow_stn_sp = st_as_sf(snow_stn_sp, 
                       coords = c("longitude", "latitude"), 
                       crs = 4326)
snow_stn_sp = st_transform(snow_stn_sp,  crs("+init=epsg:3310"))

# CIMIS, ET ref -----------------------------------------------------------

# Update process: Log in to https://cimis.water.ca.gov, go to Data, request a daily CSV report, 1/1/2015-present day (record starts april 2015)
cimis = read.csv(file.path(local_data_dir, "CIMIS Stn 225 Daily 2015.04.19 to 2022.02.27.csv"))
cimis$Date=as.Date(cimis$Date, format = "%m/%d/%Y")
et_0 = cimis[,c("Date", "ETo..in.")]
colnames(et_0)=c("Date","ET_ref_in")

# Land Use DWR 2016 ----------------------------------------------------------------

# This is only for the Scott watershed

#Accessed from email sent from Flackus, Todd@DWR <Todd.Flackus@water.ca.gov>
lu_zip = file.path(local_data_dir,"Siskiyou2017_Final_WaterSourceDAU003Clip.shp.zip")

zipname = strsplit(lu_zip, "/")[[1]][length(strsplit(lu_zip, "/")[[1]])]
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(lu_zip, exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
#Read adjudicated shapefiles into R
lu_all = st_read( file.path(scratch_dir, "Siskiyou2017_Final_WaterSourceDAU003Clip.shp"))
landuse_dwr_2016 = st_transform(lu_all, crs("+init=epsg:3310"))
# lu = lu_all[county,] # not necessary, this is already clipped to the watershed

#Copy to the box DMS archive
if(save_data_to_archive==T & 
   !file.exists(file.path(dms_archive_dir, "landuse_siskiyou_county_dwr2016.shp"))){
  st_write(obj = landuse_dwr_2016, dsn = dms_archive_dir, layer = "landuse_siskiyou_county_dwr2016", driver = "ESRI Shapefile")
}
  #remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "sbn", "sbx", "shp.xml")
file.remove(file.path(scratch_dir,paste("Siskiyou2017_Final_WaterSourceDAU003Clip", extension_list, sep = ".")))


# SVIHM fields: MAR, ILR, Adjudicated Zone layers -----------------------------------------------------------------------

svihm_ref_dir = file.path(local_data_dir,"SVIHM Reference Data")
svihm_fields = st_read(dsn = svihm_ref_dir, layer = "Landuse_20190219")
adj_zone = st_read(dsn = svihm_ref_dir, layer = "Adjudicated Area")
svihm_fields = st_transform(svihm_fields, crs(adj_zone))


# Read in the Fields Attribute text file. Process SVIHM fields spatial layer
fields_column_classes = c(rep("integer",4),
                        "numeric","integer","numeric","numeric",
                        "integer","integer","character",
                        rep("NULL",16)) # get rid of empty columns in the text file
fields_tab = read.table(file.path(svihm_ref_dir,"polygons_table.txt"),
                      header = T, comment.char = "!", 
                      fill = T, sep = "\t", colClasses = fields_column_classes)
colnames(fields_tab) = c("Field_ID",colnames(fields_tab)[2:11])
# MAR fields table
mar_fields = read.table(file.path(local_data_dir,"SVIHM Reference Data","MAR_Fields.txt"), 
                        comment.char = "!", skip = 1, header = F)
names(mar_fields) = c("Field_poly_num", "Max_infil_rate_m_day")

# Process SVIHM fields - water source, land use color, overlap with adjudicated zone

# 1. Calculate the fraction of each polygon *inside* the adjudicated zone.
svihm_fields$fraction_in_adj = 0 # initialize new column

for(i in 1:max(svihm_fields$Polynmbr)){
  selector = svihm_fields$Polynmbr==i
  field = svihm_fields[selector,]
  if(!st_is_valid(field)){
    field = st_buffer(field, dist = 0) # fix invalid geoms and warn about it
    print(paste("polygon number",i,"invalid"))}
  
  if(st_overlaps(adj_zone, field, sparse = F)){
    overlap_poly = st_intersection(field, adj_zone)
    svihm_fields$fraction_in_adj[selector] = round(st_area(overlap_poly) / st_area(field), digits = 3) # otherwise get leftover digit junk
  }
}

# 2. Assign status as inside or outside adjudicated zone (based on overlap threshold)
in_adj_threshold = 0.05 # lower numbers mean, just a sliver overlapping are included
fields_inside_adj = svihm_fields$Polynmbr[svihm_fields$fraction_in_adj > in_adj_threshold]
fields_outside_adj = svihm_fields$Polynmbr[!(svihm_fields$Polynmbr %in% fields_inside_adj)]

fields_inside_adj = svihm_fields[svihm_fields$fraction_in_adj > in_adj_threshold,]
fields_outside_adj =  svihm_fields[svihm_fields$fraction_in_adj <= in_adj_threshold,]

# 3. Process water source
# Color by water source - initialize columns
svihm_fields$wat_source_from_svihm = NA
svihm_fields$wat_source_from_svihm_color = NA

# make water source color table
wat_source = c(1,2,3,4,5,999)
wat_source_descrip = c("SW","GW","Mixed", "Sub-irrigated","Dry","Unknown")
wat_source_color = c("dodgerblue","firebrick2","darkorchid1","green","yellow","gray")
wat_source_df = data.frame(ws_code = wat_source,
                           descrip = wat_source_descrip,
                           color = wat_source_color)
#match water source codes and colors
svihm_fields$wat_source_from_svihm = fields_tab$Water_Source[match(svihm_fields$Polynmbr, fields_tab$Field_ID)]
svihm_fields$wat_source_desc_from_svihm = wat_source_df$descrip[match(svihm_fields$wat_source_from_svihm, wat_source_df$ws_code)]
svihm_fields$wat_source_from_svihm_color = wat_source_df$color[match(svihm_fields$wat_source_from_svihm, wat_source_df$ws_code)]


# 4. Process land use
alf_col = "forestgreen"; pasture_col = "darkolivegreen2"
natveg_col = "khaki"; noet_noirr_col = "red" # CURRENTLY HERE
lu_descrip_in_shp = c("Alfalfa/Grain","Pasture",
                      "ET/No Irrigation","No ET/No Irrigation")
lu_color = c(alf_col, pasture_col, natveg_col,  noet_noirr_col)
# svihm_fields$landuse_color1 = lu_color[match(svihm_fields$LNDU_SIM1, lu_descrip_in_shp)] # only difference is more NAs

# Landuse key for fields table
# # alfalfa = 25; palture = 2; ET_noIrr = 3 (native veg, assumes kc of 0.6);  noET_noIrr = 4; water = 6
lu = c(25,2,3,4,6)
lu_descrip = c("Alfalfa","Pasture","ET_noIrr","noET_noIrr", "Water")
lu_descrip_in_shp = c("Alfalfa/Grain","Pasture",
                      "ET/No Irrigation","No ET/No Irrigation", "Water")
lu_color = c(alf_col, pasture_col, natveg_col, noet_noirr_col,"dodgerblue")
lu_df = data.frame(lu_code = lu,
                   descrip = lu_descrip,
                   descrip_shp = lu_descrip_in_shp,
                   color = lu_color)
# assign color based on land use in the shapefile
svihm_fields$landuse_color2b = lu_color[match(svihm_fields$LNDU_SIM2b, lu_descrip_in_shp)]

# assign the landuse in the fields table, check match to shapefile, and assign color to the fields table landuse
svihm_fields$lu_from_svihm = NA
svihm_fields$lu_from_svihm = fields_tab$Landuse[match(svihm_fields$Polynmbr, fields_tab$Field_ID)]
svihm_fields$lu_desc_from_svihm = lu_df$descrip_shp[match(svihm_fields$lu_from_svihm, lu_df$lu_code)]
# View(svihm_fields@data[svihm_fields$lu_desc_from_svihm != svihm_fields$LNDU_SIM2b,]) # mostly reassigning no et/no irr fields to be water surface
svihm_fields$lu_from_svihm_color = lu_df$color[match(svihm_fields$lu_from_svihm, lu_df$lu_code)]

# clean up columns to keep only ones that will get used
keep_cols = c("Polynmbr","Acres","fraction_in_adj", 
              "wat_source_from_svihm","wat_source_desc_from_svihm","wat_source_from_svihm_color",
              "lu_from_svihm","lu_desc_from_svihm","lu_from_svihm_color", "geometry")
svihm_fields = svihm_fields[,keep_cols]
colnames(svihm_fields) = c("Field_ID", "Acres","fraction_in_adj",
                                "wat_source","wat_source_desc","wat_source_color",
                                "landuse","landuse_desc","landuse_color", "geometry")

#5. Add MAR status
svihm_fields$mar_field = "No"
svihm_fields$mar_field[svihm_fields$Field_ID %in% mar_fields$Field_poly_num] = "Yes"



# SVIHM grid and Discharge Zone ----------------------------------------------------

svihm_ref_dir = file.path(local_data_dir,"SVIHM Reference Data")
discharge_zone = st_read(dsn = svihm_ref_dir, layer = "Discharge_Zone")
discharge_zone = st_transform(discharge_zone, crs(watershed))

# Model raster

# discharge_zone_cells = raster(file.path(data_dir,"SVIHM Reference Data","ET_Extinction_Depth_raster"))
# discharge_zone_cells = projectRaster(discharge_zone_cells, crs = crs(watershed))
# svihm_raster = discharge_zone_cells
# rm(list = "svihm_raster")

grid_shp = st_read(dsn = svihm_ref_dir, layer = "100m_grid_UTM_20180126")
grid_shp2 = st_transform(grid_shp, crs = crs(watershed)) 
# ^ need to transform the .shp first to conserve the number of gridcells in the raster. rather than transforming the raster
n_row = max(grid_shp2$row)
n_col = max(grid_shp2$column)
svihm_raster = raster(nrows = n_row, ncols = n_col,  crs = crs(grid_shp2),
                     xmx = st_bbox(grid_shp2)["xmax"], xmn = st_bbox(grid_shp2)["xmin"],
                     ymx = st_bbox(grid_shp2)["ymax"], ymn = st_bbox(grid_shp2)["ymin"])
values(svihm_raster) = NA
rm(list = "grid_shp")
rm(list = "grid_shp2")

# Well table
hob_info = read.table(file.path(svihm_ref_dir,"hob_wells.txt"), header = F, skip = 4)
colnames(hob_info) = c('OBSNAM', 'LAYER', 'ROW', 'COLUMN', 'IREFSP', 'TOFFSET', 'ROFF', 'COFF', 'HOBS', 'STATISTIC', 'STAT-FLAG', 'PLOT-SYMBOL')
# read in longer names to match DWR_1 well abbrevs to wl_obs data
mon_info = read.csv( file.path(svihm_ref_dir, "Monitoring_Wells_Names.csv"))
dwr_in_model_short_names = c("DWR_1","DWR_2","DWR_3","DWR_4","DWR_5") 

long_name_wells = mon_info[mon_info$Well_ID %in% dwr_in_model_short_names,]
# match 1: OBSNAM to list of short names (irrelevant unless they ever get reordered)
hob_info$longname = ""
longname_matcher = match(hob_info$OBSNAM, long_name_wells$Well_ID)
hob_info$longname[hob_info$OBSNAM %in% dwr_in_model_short_names] =
  long_name_wells$Well_ID_2[longname_matcher[!is.na(longname_matcher)]]
# match 2: short names to long names
# match 3: long names to well codes

hob_info$well_code = #[hob_info$OBSNAM %in% dwr_in_model_short_names] =
  wells$well_code[match(hob_info$longname, wells$well_name)]
hob_info$well_code[is.na(hob_info$well_code)] = hob_info$OBSNAM[is.na(hob_info$well_code)]
hob_info$well_code[hob_info$well_code == "A4_1"] = "A41"

# # this matching exercise is somehow wildly frustrating. Going to hardcode this bullshit.
# # actually, I was getting the right answer after all. DWR_1 and 3 have no match in the wells table.
# # hob_info$well_code = hob_info$OBSNAM
# # hob_info$well_code[hob_info$OBSNAM == "DWR_1"] = wells$well_code[wells$well_name == "43N09W02P002M" & !is.na(wells$well_name)]
# hob_info$well_code[hob_info$OBSNAM == "DWR_2"] = wells$well_code[wells$well_name == "44N09W25R001M" & !is.na(wells$well_name)]
# # hob_info$well_code[hob_info$OBSNAM == "DWR_3"] = wells$well_code[wells$well_name == "44N09W28P001M" & !is.na(wells$well_name)]
# hob_info$well_code[hob_info$OBSNAM == "DWR_4"] = wells$well_code[wells$well_name == "43N09W23F001M" & !is.na(wells$well_name)]
# hob_info$well_code[hob_info$OBSNAM == "DWR_5"] = wells$well_code[wells$well_name == "43N09W24F001M" & !is.na(wells$well_name)]

  

# other SVIHM results -----------------------------------------------------

# these are declared in Ch. 3 if they don't exist
# observed hydraulic gradient table, grad
# stream-aquifer exchange table, stream_aq_tab

# Flow regimes -------------------------------------------------------------------------

# CDFW 2017 interim instream flows
cdfw_tab = read.csv(file.path(local_data_dir,"cdfw_2017_instream_flows.csv"))
colnames(cdfw_tab) = c("start_date_month","start_date_day","end_date_month","end_date_day","rec_flow_cfs")
# Forest Service water right
fs_tab = read.csv(file.path(local_data_dir,"USFS Scott Water Right.csv"))
colnames(fs_tab) = c("start_date_month","start_date_day","end_date_month","end_date_day","rec_flow_cfs")
# CDFW 2021 emergency drought minimum flows
cdfw_2021 = read.csv(file.path(local_data_dir, "cdfw_2021c_emergency_drought_flows.csv"))


# Fish data ---------------------------------------------------------------

spawners = read.csv(file.path(local_data_dir,"cdfw_2021a_tab5_Klamath and Scott Chinook Natural Spawner escapment.csv"))
smolt_per_fem = read.csv(file.path(local_data_dir, "cdfw_2021a_tab7_Coho_smolt_production_per_female_2008-2018.csv"))
chinook_abun = read.csv(file.path(local_data_dir,"cdfw_2021a_tab4_Chinook abundance estimates 2008-2020.csv"))
chinook_spawn_and_juv = read.csv(file.path(local_data_dir,"massie_2020_Scott River Chinook Adult_juv_data_BY_1999_2020.csv"))
coho_abun = read.csv(file.path(local_data_dir,"cdfw_2021a_fig18_Coho abundance estimates 2007-2020.csv"))
redds = read.csv(file.path(local_data_dir, "rcd_2020_coho_spawning_surveys.csv"))
outmigs = read.csv(file.path(local_data_dir,"cdfw_2021a_tab6_Coho smolt outmigrant survival 2004-2017.csv"))
outmigs$conditions_year = (outmigs$Brood.Year + outmigs$Smolt.Year)/2 # take middle year
outmigs$Percent.smolt.survival = as.numeric(outmigs$Percent.smolt.survival)

#Clean
# Clean colnames, remove commas, convert from characters to numbers
colnames(spawners) = c("year", "chinook_klamath", "chinook_scott")
spawners$chinook_klamath = as.numeric(gsub(pattern = ",", replacement = "", x = spawners$chinook_klamath))
spawners$chinook_scott = as.numeric(gsub(pattern = ",", replacement = "", x = spawners$chinook_scott))


# Functional flow metrics
fflows = read.csv(file.path(local_data_dir, "ScottR_FJ_wy1942_2021.08.04_annual_flow_result.csv"))
colnames(fflows)[1] = "Water_Year"
fflows[colnames(fflows[2:ncol(fflows)])] = sapply(fflows[colnames(fflows[2:ncol(fflows)])],as.numeric)


# River Connectivity data ------------------------------------------------------------------
connect_dir = file.path(local_data_dir, "Scott R Connectivity Data")
### 1. Voigt and Yokel river flowing or dry observations
ground_obs = read.csv(file.path(connect_dir, "Scott_R_Connectivity_Ground_Obs.csv"))
ground_obs$Obs_Date = as.Date(ground_obs$Obs_Date, format = "%m/%d/%Y")

ground_obs_rch = ground_obs[!is.na(ground_obs$Latitude_pt2),]
ground_obs_pts = ground_obs[is.na(ground_obs$Latitude_pt2) & 
                              !is.na(ground_obs$Latitude_pt1),] # clean out 1 missing value
#### convert observations to spatial object
ground_obs_pts = st_as_sf(ground_obs_pts, coords = c("Longitude_pt1", "Latitude_pt1"), crs = crs("+init=epsg:3310"))
ground_obs_pts = st_transform(ground_obs_pts, crs("+init=epsg:3310"))

### 2. Draft chokepoints and salmon reach access, derived from above obs.
hab_access = st_read(file.path(connect_dir, "habitat access by flow_draft_2022.01.12.shp"))
chokepts = st_read(file.path(connect_dir, "Scott River habitat access chokepoints_draft_2022.01.12.shp"))

### 3. Sophie connectivity tables and kml
sp_sat = read.csv(file.path(connect_dir,"Scott River - Sentinel Playground_cleaned.csv"))
ge_sat = read.csv(file.path(connect_dir,"Scott River - Google Earth.csv"))
sat_pts = st_read(file.path(connect_dir,
                            "Streamflow connectivity points - UPDATED.kml"),
                  "Streamflow connectivity points")
sat_pts = st_transform(sat_pts, crs("+init=epsg:3310"))
# classify
sat_pts$Description[grepl(pattern = "Sentinel", x = sat_pts$Name)]="Sentinel"
reach_info_pts = c("Scott Reach 14", "Scott Reach 13", "Scott Reach 12", 
                   "Scott Reach 15", "Scott Reach 16 N", "Scott Reach 16 S (Tailings)",
                   "Scott River S of Ft Jones", "Scott River S of Hamlin Gulch",
                   "Scott River W of Moffett")
sat_pts$Description[grepl(pattern = "Sentinel", x = sat_pts$Name)]="Sentinel"
sat_pts$Description[sat_pts$Name %in% reach_info_pts]="Reach Info"
sat_pts$Description[is.na(sat_pts$Description)] = "Google Earth"



# _MODEL RESULTS ----------------------------------------------------------



read_modflow_heads = function(scenario_directory, num_stress_periods = 336){
  start_time <- Sys.time()
  
  # User Inputs --
  NLAY = 2                 # Number of layers in model
  NSP = num_stress_periods                # Number of stress peridos for which heads are printed
  filename = file.path(scenario_directory,'SVIHM.hds')   #Name of binary head file
  No_Flow_Val = 9999       #Value of no flow cells
  # output_dir = 'Results/'  #Output directory
  
  # Read Heads --
  H_by_SP = list()
  fid = file(filename, "rb")
  bytes = 0                                     #Bytes counter
  p=1
  
  for(k in 1:NSP){
    print(paste0('Reading Heads for Stress Period ', k))
    KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
    KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
    PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
    TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
    DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                    "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
    NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
    NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
    ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number
    H = array(data = NA, dim = c(NROW, NCOL, NLAY))
    H1 = matrix(readBin(fid, numeric(), n=92400, size = 4), nrow = NROW, ncol = NCOL, byrow = T)  #Read in head matrix
    H[,,1] = H1
    for (i in 2:NLAY){ # Read in data for remaining layers
      KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
      KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
      PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
      TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
      DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                      "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
      NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
      NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
      ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number  
      H_temp = matrix(readBin(fid, numeric(), n=92400, size = 4), nrow = NROW, ncol = NCOL, byrow = T)  #Read in head matrix
      eval(parse(text = paste0('H[,,NLAY] = H_temp')))
    }
    H[H==No_Flow_Val] = NaN
    
    #add to list
    H_by_SP[[k]] = H
  }
  
  closeAllConnections()
  end_time <- Sys.time()
  print(paste0('Total Run Time was ', round(end_time - start_time, digits = 1), ' seconds'))
  
  return(H_by_SP)
}


read_ground_surface_elev = function(scenario_directory){
  filename = file.path(scenario_directory,'SVIHM.dis')   #Name of discretization file
  
  disLines = readLines(filename)
  top_line = grep(pattern = "TOP of Model", x = disLines)+1
  bottom_line = grep(pattern = "BOTTOM of Layer   1", x = disLines)-1
  
  gs_elev_lines = disLines[top_line:bottom_line]
  
  gs_elev_values = as.numeric(unlist(strsplit(trimws(gs_elev_lines), split = "  ")))
  
  gs_elev_matrix = matrix(gs_elev_values, nrow = 440, byrow=T)
  # image(t(gs_elev_matrix)[,440:1])
  return(gs_elev_matrix)
}

if(read_in_model_results==T){
  modflow_heads_bc = read_modflow_heads(scenario_directory = file.path(svihm_scenarios_dir, "basecase"))
  ground_elev = read_ground_surface_elev(scenario_directory = file.path(svihm_scenarios_dir, "basecase"))
  
}

# Save workspace as .RData ------------------------------------------------

save.image(file = file.path(ms_dir, "manuscript_data.RData"))
