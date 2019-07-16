# gridEZ

gridEZ algorithm for generating enumeration zones (EZs) with user-defined target population and geographic size. 

EZs are generated from three input rasters that cover the study region. These are 1. population counts or densities; 2. strata IDs; 3. settlement type IDs 


# Instructions

1. Open and run gridEZ_fn_public_release_v1.R
2. Open gridEZ_running_script_public_release_v1.R 
3. Edit memory.limit(#), if needed
4. Edit ncores and par_type to suit your computing system's parallel processing 
4. Load in your input population, strata and settlement rasters by editing the pathways and filenames under'set input files' section
5. Edit gridEZ() specifications for your EZ generations under 'run grid EZ code' section
6. Run whole gridEZ_running_script_public_release_v1.R script


# Recommendations (as of 16/07/2019)

As the predefined_EZ_size functionality has been thoroughly tested, it is recommended that users take advantage of the predefined_EZ_size specification by setting this to = "small", "medium" or "large". 

Testing of user defined EZ_by_hh, target_hh_per_EZ, pop_per_hh, EZ_by_pop, target_pop_per_EZ and max_cells_per_EZ is ongoing. 
