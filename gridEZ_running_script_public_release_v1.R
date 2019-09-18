

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


# run grid EZ code (see below for input specifications details)
gridEZ(population_raster = country_population_raster, 
                     settlement_raster = country_settlement_raster, 
                     strata_raster = country_strata_raster, 
                     exclude_unsettled = FALSE, 
                     using_ghs_smod_pop2015 = TRUE,
                     predefined_EZ_size = TRUE, EZ_target_size = "medium", 
                     output_path = "~/gridEZ/X/", run_ID = "_X")


# REQUIRED #

#population_raster:      Raster* layer. Each pixel/cell should contain a population count. NAs allowed. Cells containing NAs are excluded from the enumeration zones/sampling frame.
#settlement_raster:      Raster* layer. Each pixel/cell should contain a number that represents a settlment type. Ideally the study region should be made up of contiguous sections with the same settlement type as each EZ will only contain a single settlement type.  NAs allowed. Cells containing NAs are excluded from the enumeration zones/sampling frame 
#strata_raster:          Raster* layer. Each pixel/cell should contain a number that represents a stratum, e.g. administrative unit. NAs allowed. Cells containing NAs are excluded from the enumeration zones/sampling frame.
#output_path:            Character. An existing directory where final EZ rasters will be saved and where a temporary folder will be placed while the gridEz code is running
#run_ID: character.      Default = "_run1". This will appear at the end of output files. Allows easy distinction between resultant sampling frames from different runs of the code, e.g. for different countries or for different initial parameters


# OPTIONAL #

#exclude_unsettled:      Logical. Default is FALSE. If TRUE all cells in the settlement_raster with value equal to that specified for 'unsettled_ID'. Alternatively, unsettled cells could already be NAs in the settlement raster, in which case exclude_unsettled should be set to FALSE 
#unsettled_ID:           Numeric. Default is NA. Needs to be defined if 'exclude_unsettled' = TRUE. A single value corresponding to the value for unsettled cells in the settlement_raster. 
#using_ghs_smod_pop2015: Logical. Default is FALSE. This specification has been included because ghs_smod settlement layer has good characteristics for creating gridded EZs and has been used for generating EZ datasets with gridEZ. This specification modifies a ghs_smod country level settlement layer such that smod classifications of 0, 1 or 2 are joined together to form one single class. Using ghs_smod and this specification leads to sensible EZs that cover the whole extent of the user's study region
#predefined_EZ_size:     Logical. Default is TRUE. If TRUE, 'EZ_by_hh' and 'EZ_by_pop' must not be TRUE. Set to FALSE if you want to specify your own target population per EZ and maximum number of cells per EZ.
#EZ_target_size:         Character. Default is "medium". Either "small", "medium" (or "med") or "large"; other specified entries will be ignored and values specified (or default values) for 'target_pop_per_EZ' and 'max_cells_per_EA' will be used instead. If 'predefined_EZ_size' is TRUE this will be used. For "small" target_pop_per_EZ = 75 and max_cells_per_EZ = 100; for "medium" target_pop_per_EZ = 500 and max_cells_per_EZ = 900; for "large" target_pop_per_EZ = 1200 and max_cells_per_EZ = 2500
#EZ_by_hh:               Logical. Default is FALSE. If TRUE, 'predefined_EZ_size' and 'EZ_by_pop' must not be TRUE.
#target_hh_per_EZ:       Numeric or dataframe. Default is 100. Used when 'EZ_by_hh' is TRUE. For a single target number of households per EZ is to be applied across the whole study region, specify a single number. If different target number of households per EZ are needed for different settlement types and/or strata, specify a dataframe with 4 columns named 'strata_ID_number', 'settlement_type_ID_number', 'target_number_hh_per_EZ' and 'pop_number_per_hh', and numeric values for each entry
#pop_per_hh:             Numeric. Default is 5. Used when 'EZ_by_hh' is TRUE and 'target_hh_per_EZ' is a single number. If 'target_hh_per_EZ' is a dataframe this is not used.
#EZ_by_pop:              Logical. Default is FALSE. If TRUE, 'predefined_EZ_size' and 'EZ_by_hh' must not be TRUE.
#target_pop_per_EZ:      Numeric or dataframe. Default is 500. Used when 'EZ_by_pop' is TRUE. For a single target population per EZ is to be applied across the whole study region, specify a single number. If different target populations per EZ are needed for different settlement types and/or strata, specify a dataframe with 3 columns named 'strata_ID_number', 'settlement_type_ID_number' and 'target_pop_number_per_EZ', and numeric values for each entry 
#max_cells_per_EA:       Numeric. Default is 900 which is approx. 3km by 3 km when using worldpop 100m x 100m population rasters, depending on location. Must be a single number. Value specified will be ignored if 'predefined_EZ_size' is TRUE.

 

