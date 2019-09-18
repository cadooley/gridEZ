
#######################################################################################################
#######################################################################################################
###                                                                                                 ###
###   R script to create a sampling frame of gridded enumeration zones for Saint Kitts and Nevis    ###
###                                                                                                 ###
#######################################################################################################
#######################################################################################################


# This R script contains the following sections:
# - Instructions for downloading Saint Kitts and Nevis data
# - Load R libraries
# - Prepare input data for running gridEZ
# - Run gridEZ function to create a sampling frame of gridded enumeration zones
# - Explore results



#########################
###   DOWNLOAD DATA   ###
#########################

# Saint Kitts and Nevis population 2020 from here: https://www.worldpop.org/geodata/summary?id=6492
# Saint Kitts and Nevis GADM administrative boundaries by selecting the country 'Saint Kitts and Nevis'
# and clicking on 'Shapefile', from here: https://gadm.org/download_country_v3.html
# Create a folder called 'KNA'
# Add the population file (kna_ppp_2020.tif) to the KNA folder 
# Extract the files from the GADM shapefile (gadm36_KNA_shp.zip) so that you have a folder called 
# gadm36_KNA_shp containing the files. Add this gadm36_KNA_shp folder to the KNA folder



############################
###   LOAD R LIBRARIES   ###
############################

rm(list=ls())

# install and load R packages (data.table package loaded later to produce summary statistics, but not needed for running gridEZ and producing sampling frames)
library(sp)           #version: sp_1.2-7
library(raster)       #version: raster_2.6-7
library(rgdal)        #version: rgdal_1.2-18
library(parallel)     #base package



##############################
###   PREPARE INPUT DATA   ###
##############################

# user to set their working directory: this is the location of your KNA folder and the gridEZ code
setwd('//filestore.soton.ac.uk/users/cad1c14/mydocuments/gridEZ')

# read in population and admin boundaries data
KNApop_raster <- raster("KNA/kna_ppp_2020.tif")
plot(KNApop_raster)
KNAad_poly <- readOGR("KNA/gadm36_KNA_shp/gadm36_KNA_1.shp")
plot(KNAad_poly)


# gridEZ_rasterize function converts GADM boundaries SpatialPolygonsDataFrame object into raster 

# run the following to save the function in R memory
gridEZ_rasterize <- function(strata_shp, population_raster, field){
  r1 <- raster()
  crs(r1) <- crs(population_raster); extent(r1) <- extent(population_raster); res(r1) <- res(population_raster)
  r2 <- rasterize(strata_shp, r1, field)
  return(r2)
}

# identify the field that provides unique IDs for the admin units of the boundaries data 
KNAad_poly@data  # 'GID_1' looks appropriate
field_ID <- "GID_1" 

# apply the gridEZ_rasterize function to create an admin boundaries raster
KNAad_raster <- gridEZ_rasterize(KNAad_poly, KNApop_raster, field = field_ID)
plot(KNAad_raster)

# create a settlement layer (this step is simply for this example, there are a number of publicly available settlement datasets that can be used)

# define urban and rural regions
KNAsettagg_raster <- aggregate(KNApop_raster, 10, fun=max) # this finds the max pop (per 100m x 100m pixel) in each 1km x 1km square
KNAsettagg_raster[KNAsettagg_raster < 8] <- 1              # squares with max pop (per 100m x 100m pixel) smaller than than 8 are classified as '1' representing rural
KNAsettagg_raster[KNAsettagg_raster >= 8] <- 2             # squares with max pop (per 100m x 100m pixel) large than or equal to 8 are classified as '2' representing urban

# match the extent of the settlement layer to that of the pop layer
KNAsettagg_raster <- projectRaster(KNAsettagg_raster, crs=crs(KNApop_raster), res=res(KNApop_raster), method='ngb')
new_extent <- extent(c(max(extent(KNAsettagg_raster)[1], extent(KNApop_raster)[1]),
                       min(extent(KNAsettagg_raster)[2], extent(KNApop_raster)[2]),
                       max(extent(KNAsettagg_raster)[3], extent(KNApop_raster)[3]),
                       min(extent(KNAsettagg_raster)[4], extent(KNApop_raster)[4])))
KNAsettagg_raster <- crop(KNAsettagg_raster, new_extent)


# plot the three input rasters
par(mfrow=c(1,3))
plot(KNApop_raster, main='population')
plot(KNAad_raster, main='admin units')
plot(KNAsettagg_raster, main='settlement type (1=rural, 2=urban)')



###############################
###   RUN gridEZ FUNCTION   ###
###############################


# read in gridEZ function
source('gridEZ_fn_public_release_v1.R')

# specify memory
memory.limit(5000000)

# specify number of cores to use during parallel processing and type of parallel processing
total_ncores <- detectCores()  # this gives the number of cores for your computing system
ncores <- total_ncores - 1     # want to use all but 1 core for the parallel processing
if(ncores == 0) ncores <- 1
par_type <- "PSOCK"            # parallel processing type - "PSOCK" for windows, "FORK" for linux, unix, mac 


# run grid EZ code (this should take ~3mins with 7 cores and longer if you have fewer cores)

             gridEZ(population_raster = KNApop_raster, 
                    settlement_raster = KNAsettagg_raster, 
                    strata_raster = KNAad_raster, 
                    exclude_unsettled = FALSE, 
                    using_ghs_smod_pop2015 = FALSE,
                    predefined_EZ_size = TRUE, EZ_target_size = "small", 
                    output_path = "KNA/", run_ID = "_KNA_smallEZs_2020")

             
             
             
###########################
###   EXPLORE RESULTS   ###
###########################             
             
# gridEZ saves the output rasters to file. Open them in GIS software or you can read them into R:

EZ_pops <- raster("KNA/EZ_pop_raster_master_KNA_smallEZs_2020.tif")
EZ_IDs <- raster("KNA/EZ_raster_master_KNA_smallEZs_2020.tif")

# install and load data.table package
library(data.table)

# create results EZ data table 
EZdt <- data.table(EZ_ID = EZ_IDs[],
                 pop = EZ_pops[],
                 sett = KNAsettagg_raster[],
                 strata = KNAad_raster[])
cell_count_dt <- EZdt[,.N,by=EZ_ID]
res_dt <- merge(EZdt, cell_count_dt, by='EZ_ID')
res_dt <- res_dt[!is.na(EZ_ID),] # delete all NA raster cell locations, i.e. outside of country boundary
res_EZ_dt <- unique(res_dt)

# total number of EZs in sampling frame
nrow(res_EZ_dt)

# total EZs by sett type
res_EZ_dt[, .N, by = 'sett']     # sett = 1 for rural and 2 for urban

# total EZs per admin unit
res_EZ_dt[, .N, by = 'strata'] 

# for all EZs, plot histograms for a)number of cells per EZ, and b) pop count per EZ
par(mfrow=c(1,2))
hist(res_EZ_dt[,N], breaks=100, xlab = "number of cells per EZ", main = paste("All EZs, n =",nrow(res_EZ_dt)))
hist(res_EZ_dt[,pop], breaks=100, xlab = "population count per EZ", main = paste("All EZs, n =",nrow(res_EZ_dt)))

# our target pop per EZ was 75 so we expect most EZs to have between 56 and 94 people (+/- 25%  of 75)
# our max cells per EZ (i.e. max geographic size) was 100 cells so we expect most EZs to not exceed 1.5 x the max number of cells which is 150 here
# the spike at 100 in the number of cells per EZ histogram show this restriction being played out in the results

# for rural EZs, plot histograms for a)number of cells per EZ, and b) pop count per EZ
par(mfrow=c(1,2))
hist(res_EZ_dt[sett==1,N], breaks=100, xlab = "number of cells per EZ", main = paste("Rural EZs, n =",nrow(res_EZ_dt[sett==1])))
hist(res_EZ_dt[sett==1,pop], breaks=100, xlab = "population count per EZ", main = paste("Rural EZs, n =",nrow(res_EZ_dt[sett==1])))

# for rural EZs, plot histograms for a)number of cells per EZ, and b) pop count per EZ
par(mfrow=c(1,2))
hist(res_EZ_dt[sett==2,N], breaks=100, xlab = "number of cells per EZ", main = paste("Urban EZs, n =",nrow(res_EZ_dt[sett==2])))
hist(res_EZ_dt[sett==2,pop], breaks=100, xlab = "population count per EZ", main = paste("Urban EZs, n =",nrow(res_EZ_dt[sett==2])))

# from the urban and rural plots you can see that most EZs with pop count below 45 are found in rural areas
# in the very low pop density areas the EZs have the maximum geographic size per EZ and therefore low EZ population counts

par(mfrow=c(1,2))
plot(KNApop_raster, main='population per 100m x 100m pixel',cex.main=0.7)
plot(EZ_pops, main='EZ population count',cex.main=0.7)  




