

#session info:
#R version 3.4.2 (2017-09-28)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows >= 8 x64 (build 9200)

library(sp)           #version: sp_1.2-7
library(raster)       #version: raster_2.6-7
library(rgdal)        #version: rgdal_1.2-18
library(parallel)     #base package

# specify memory
memory.limit(5000000)

# specify number of cores to use during parallel processing and type of parallel processing
ncores <- 7             # instead of specifying, may want to do: ncores <- detectCores() - 1
par_type <- "PSOCK"     # parallel processing type - "PSOCK" for windows, "FORK" for linux, unix, mac 


# read in gridEZ function
source('gridEZ_fn_public_release_v1.R')

# set input files

country_population_raster <- raster("~/gridEZ/X/X_pop.tif")
country_strata_raster <- raster("~/gridEZ/X/X_admin.tif")
country_settlement_raster <- raster("~/gridEZ/X/X_ghs_smod.tif")


# run grid EZ code
gridEZ(population_raster = country_population_raster, 
                     settlement_raster = country_settlement_raster, 
                     strata_raster = country_strata_raster, 
                     exclude_unsettled = FALSE, unsettled_ID = 0, 
                     using_ghs_smod_pop2015 = TRUE,
                     predefined_EZ_size = TRUE, EZ_target_size = "medium", 
                     output_path = "~/gridEZ/X/", run_ID = "_X")


