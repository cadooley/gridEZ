

# gridEZ function
gridEZ <- function(population_raster, settlement_raster, strata_raster, 
                   exclude_unsettled = FALSE, unsettled_ID = NA, 
                   using_ghs_smod_pop2015 = FALSE,
                   predefined_EZ_size = TRUE, EZ_target_size = "medium", 
                   EZ_by_hh = FALSE, target_hh_per_EZ = 100, pop_per_hh = 5,
                   EZ_by_pop = FALSE, target_pop_per_EZ = 500, max_cells_per_EZ = 900,
                   output_path, run_ID = "_run1"){
  
  ############################################
  # initial raster and specifications checks #
  ############################################
  
  if(file.exists(paste(output_path,"/EZ_raster_master", run_ID, ".tif",sep=""))){
    stop(paste("Desired output filename already exists; delete file or change run_ID so that output file is given a different name. File location and name is:", output_path,"/EZ_raster_master", run_ID, ".tif",sep=""))
  }
  if(file.exists(paste(output_path,"/EZ_pop_raster_master", run_ID, ".tif",sep=""))){
    stop(paste("Desired output filename already exists; delete file or change run_ID so that output file is given a different name. File location and name is:", output_path,"/EZ_pop_raster_master", run_ID, ".tif",sep=""))
  }
  if (sp::proj4string(population_raster) != sp::proj4string(settlement_raster) | sp::proj4string(population_raster) != sp::proj4string(strata_raster)) {
    stop(paste("Projection system of rasters differs. You can use sp::proj4string() to confirm raster projections."))
  }
  new_extent <- extent(c(max(extent(population_raster)[1], extent(settlement_raster)[1], extent(strata_raster)[1]),
                         min(extent(population_raster)[2], extent(settlement_raster)[2], extent(strata_raster)[2]),
                         max(extent(population_raster)[3], extent(settlement_raster)[3], extent(strata_raster)[3]),
                         min(extent(population_raster)[4], extent(settlement_raster)[4], extent(strata_raster)[4])))
  population_raster <- crop(population_raster, new_extent)
  settlement_raster <- crop(settlement_raster, new_extent)
  strata_raster <- crop(strata_raster, new_extent)
  if(!dim(population_raster)[1] == dim(strata_raster)[1] | !dim(population_raster)[1] == dim(settlement_raster)[1] | !dim(population_raster)[2] == dim(strata_raster)[2] | !dim(population_raster)[2] == dim(settlement_raster)[2]){
    stop("Unable to crop input rasters to same extent. Edit input rasters so that they have matching dimensions then re-run gridEZ code")
  }
  if(exclude_unsettled == TRUE){
    if(is.numeric(unsettled_ID) == FALSE | !length(unsettled_ID) == 1){
      stop("unsettled_ID needs to be a single number")
    }
    settlement_raster[settlement_raster[] == unsettled_ID] <- NA
  }  
  if(using_ghs_smod_pop2015 == TRUE){
    if(exclude_unsettled == FALSE){
      settlement_raster[settlement_raster[] == 0] <- 1
    }
    settlement_raster[settlement_raster[] == 2] <- 1
  }
  settlement_raster[which(is.na(population_raster[]))] <- NA
  strata_raster[which(is.na(population_raster[]))] <- NA
  settlement_raster[which(is.na(strata_raster[]))] <- NA
  strata_raster[which(is.na(settlement_raster[]))] <- NA
  if(predefined_EZ_size == TRUE){
    if(EZ_by_hh == TRUE | EZ_by_pop == TRUE){
      stop("Need to specify predefined_EZ_size = FALSE if you want to specify a target number of households or people per Enumeration Zone")
    }
    if(EZ_target_size == "small"){
      target_pop_per_EZ <- 75
      max_cells_per_EZ <- 100
    }
    if(EZ_target_size == "medium"){
      target_pop_per_EZ <- 500
      max_cells_per_EZ <- 900
    }
    if(EZ_target_size == "med"){
      target_pop_per_EZ <- 500
      max_cells_per_EZ <- 900
    }
    if(EZ_target_size == "large"){
      target_pop_per_EZ <- 1200
      max_cells_per_EZ <- 2500
    }
    print(paste("Creating EZs based on predefined EZ size criteria: target population per EZ = ", target_pop_per_EZ, " & max number of cells per EZ = ", max_cells_per_EZ))
  }else{
    if(EZ_by_hh == TRUE & EZ_by_pop == TRUE){
      stop("Both EZ_by_hh & EZ_by_pop can't be TRUE. Set just one to be TRUE")
    }
    if(EZ_by_hh == TRUE){ 
      if(class(target_hh_per_EZ) == "numeric"){
        ### add check in here for pop_per_hh being numeric too
        target_pop_per_EZ <- target_hh_per_EZ * pop_per_hh
      }else{
        if(!class(target_hh_per_EZ) == "data.frame"){
          stop(paste("target_hh_per_EZ has class", class(target_hh_per_EZ), "; target_hh_per_EZ needs to be a single number or a data.frame object, see ?class"))
        }
        if(!ncol(target_hh_per_EZ) == 4){
          stop(paste("target_hh_per_EZ has", ncol(target_hh_per_EZ), " columns; target_hh_per_EZ should have 4 columns - 'strata_ID_number', 'settlement_type_ID_number', 'target_number_hh_per_EZ', 'pop_number_per_hh'"))
        }
        if(FALSE %in% (c("strata_ID_number", "settlement_type_ID_number", "target_number_hh_per_EZ", "pop_number_per_hh") %in% names(target_hh_per_EZ))){
          stop("target_hh_per_EZ column names need to be 'strata_ID_number', 'settlement_type_ID_number', 'target_number_hh_per_EZ', 'pop_number_per_hh', see names(target_hh_per_EZ) to check")
        }
        if(exclude_unsettled == TRUE){     # leave here rather than further down ifs in case user puts NAs for the unsettled entries
          target_hh_per_EZ <- target_hh_per_EZ[-(target_hh_per_EZ$settlement_type_ID_number == unsettled_ID),]
        } 
        if(TRUE %in% is.na(unlist(target_hh_per_EZ))){
          stop("target_hh_per_EZ contains NAs, all entries should be numeric with 'strata_ID_number' and 'settlement_type_ID_number' entries corresponding to the values in strata_raster and settlement_raster, respectively")
        }   
        if(!is.numeric(unlist(target_hh_per_EZ))){
          stop("target_hh_per_EZ contains non-numerics, see is.numeric(unlist(target_hh_per_EZ)). All entries should be numeric with 'strata_ID_number' and 'settlement_type_ID_number' entries corresponding to the values in strata_raster and settlement_raster, respectively")
        }   
        if(FALSE %in% (sort(unique(getValues(strata_raster))) %in% target_hh_per_EZ$strata_ID_number)){
          stop("incorrect strata_ID_number values in target_hh_per_EZ; all values in strata_raster need to be included in target_hh_per_EZ")
        }
        if(FALSE %in% (sort(unique(getValues(settlement_raster))) %in% target_hh_per_EZ$settlement_type_ID_number)){
          stop("incorrect settlement_type_ID_number values in target_hh_per_EZ; all values settlement_raster need to be included in target_hh_per_EZ")
        }
        # because not all sett types may be in a stratum, user may not specify (number of strata x number of settlement) rows, if there are missing combos this is picked up later in code
        target_pop_number_per_EZ <- target_hh_per_EZ$target_number_hh_per_EZ * target_hh_per_EZ$pop_number_per_hh
        target_pop_per_EZ <- as.data.frame(cbind(target_hh_per_EZ$strata_ID_number,target_hh_per_EZ$settlement_type_ID_number, target_pop_number_per_EZ))
      }
      print("EZs will be created based on user's specified number of households per EZ")
    }
    if(EZ_by_pop == TRUE){
      if(!class(target_pop_per_EZ) == "numeric"){
        if(!class(target_pop_per_EZ) == "data.frame"){
          stop(paste("target_pop_per_EZ has class", class(target_pop_per_EZ), "; target_pop_per_EZ needs to be a single number or a data.frame object, see ?class"))
        }
        if(!ncol(target_hh_per_EZ) == 3){
          stop(paste("target_pop_per_EZ has", ncol(target_pop_per_EZ), " columns; target_pop_per_EZ should have 3 columns - 'strata_ID_number', 'settlement_type_ID_number', 'target_pop_number_per_EZ'"))
        }
        if(FALSE %in% (c("strata_ID_number", "settlement_type_ID_number", "target_pop_number_per_EZ") %in% names(target_pop_per_EZ))){
          stop("target_pop_per_EZ column names need to be 'strata_ID_number', 'settlement_type_ID_number', 'target_pop_number_per_EZ', see names(target_pop_per_EZ) to check")
        }
        if(exclude_unsettled == TRUE){     # leave here rather than further down ifs in case user puts NAs for the unsettled entries
          target_pop_per_EZ <- target_pop_per_EZ[-(target_pop_per_EZ$settlement_type_ID_number == unsettled_ID),]
        } 
        if(TRUE %in% is.na(unlist(target_pop_per_EZ))){
          stop("target_pop_per_EZ contains NAs, all entries should be numeric with 'strata_ID_number' and 'settlement_type_ID_number' entries corresponding to the values in strata_raster and settlement_raster, respectively")
        }   
        if(!is.numeric(unlist(target_pop_per_EZ))){
          stop("target_pop_per_EZ contains non-numerics, see is.numeric(unlist(target_pop_per_EZ)). All entries should be numeric with 'strata_ID_number' and 'settlement_type_ID_number' entries corresponding to the values in strata_raster and settlement_raster, respectively")
        }   
        if(FALSE %in% (sort(unique(getValues(strata_raster))) %in% target_pop_per_EZ$strata_ID_number)){
          stop("incorrect strata_ID_number values in target_pop_per_EZ; all values in strata_raster need to be included in target_pop_per_EZ")
        }
        if(FALSE %in% (sort(unique(getValues(settlement_raster))) %in% target_pop_per_EZ$settlement_type_ID_number)){
          stop("incorrect settlement_type_ID_number values in target_pop_per_EZ; all values settlement_raster need to be included in target_pop_per_EZ")
        }
        # because not all sett types may be in a stratum, user may not specify (number of strata x number of settlement) rows, if there are missing combos this is picked up later in code
      }
      print("EZs will be created based on user's specified population per EZ")
    }    
    if(is.numeric(max_cells_per_EZ) == FALSE | !length(max_cells_per_EZ) == 1){
      stop("max_cells_per_EZ needs to be a single number")
    }
  }
  
  ###########################
  # create temporary folder #
  ###########################
  
  print(paste("creating", " /temp_folder", run_ID, sep=""))       
  dir.create(paste(output_path,"/temp_folder", run_ID, sep="")) 
  
  ###############################################################
  # keep raster specs then delete rasters to reduce memory used #
  ###############################################################
  
  r_xmn <- extent(population_raster)[1]
  r_xmx <- extent(population_raster)[2]
  r_ymn <- extent(population_raster)[3]
  r_ymx <- extent(population_raster)[4]
  r_crs <- sp::proj4string(population_raster)
  n_rows <- dim(population_raster)[1]
  n_cols <- dim(population_raster)[2]
  population_mat <- as.matrix(population_raster)
  sett_mat <- as.matrix(settlement_raster)
  strata_mat <- as.matrix(strata_raster)
  rm(population_raster); rm(settlement_raster); rm(strata_raster)
  
  ###############################################################
  # cut up study region in preparation for parallel processing  #
  ###############################################################
  
  sett_vals <- sort(unique(round(sett_mat[!is.na(sett_mat)])))
  strat_vals <- sort(unique(round(strata_mat[!is.na(strata_mat)])))
  strat_sett_df <- cbind(rep(strat_vals, each = length(sett_vals)), rep(sett_vals, length(strat_vals))) 
  clump_info <- list()
  seq_max <- round(sqrt(max_cells_per_EZ))
  if(seq_max <= 50){
    clump_side_min <- 50
  }else{
    clump_side_min <- seq_max
  }
  if(seq_max <= 3){block_pop_cutoffs <- unique(1:seq_max)}
  if(seq_max > 3){block_pop_cutoffs <- unique(c(1,2,3,seq(4,min(seq_max,(clump_side_min/2)),2),seq_max))}
  # a for loop used because memory limits are often reached with large population matrices when data is copied across multiple cores
  for(ssID in 1:nrow(strat_sett_df)){  
    strat_sett_mat <- matrix(NA, nrow = n_rows, ncol = n_cols)
    i <- strat_sett_df[ssID, 1] #stratum ID
    j <- strat_sett_df[ssID, 2] #sett_type ID
    if(is.data.frame(target_pop_per_EZ) == TRUE){
      if(!length(target_pop_per_EZ$target_pop_number_per_EZ[target_pop_per_EZ$strata_ID_number == i & target_pop_per_EZ$settlement_type_ID_number == j] == 1)){
        stop(paste("no row in user specifications for stratum", i, "and settlement type", j, "in either target_hh_per_EZ or target_pop_per_EZ data frame"))
      }
      strat_sett_target_pop_per_EZ <- target_pop_per_EZ$target_pop_number_per_EZ[target_pop_per_EZ$strata_ID_number == i & target_pop_per_EZ$settlement_type_ID_number == j] 
    }else{
      strat_sett_target_pop_per_EZ <- target_pop_per_EZ
    }
    strat_sett_mat[strata_mat[] == i & sett_mat[] == j] <- 1
    if(length(sort(unique(strat_sett_mat[!is.na(strat_sett_mat)]))) == 0) next   # if no cells of j sett type are in i state
    cell_IDs <- which(strat_sett_mat[] == 1, arr.ind = TRUE)
    min_strat_sett_row <- min(cell_IDs[,1])
    max_strat_sett_row <- max(cell_IDs[,1])
    min_strat_sett_col <- min(cell_IDs[,2])
    max_strat_sett_col <- max(cell_IDs[,2])  
    sub_strat_sett_mat <- strat_sett_mat[min_strat_sett_row:max_strat_sett_row, min_strat_sett_col:max_strat_sett_col]
    sub_strat_sett_pop_mat <- population_mat[min_strat_sett_row:max_strat_sett_row, min_strat_sett_col:max_strat_sett_col]
    if(!class(sub_strat_sett_mat)[1] == "matrix"){sub_strat_sett_mat <- matrix(sub_strat_sett_mat, nrow=(max_strat_sett_row - min_strat_sett_row + 1))}
    if(!class(sub_strat_sett_pop_mat)[1] == "matrix"){sub_strat_sett_pop_mat <- matrix(sub_strat_sett_pop_mat, nrow=(max_strat_sett_row - min_strat_sett_row + 1))}
    sub_strat_sett_pop_mat[which(is.na(sub_strat_sett_mat))] <- NA
    if(nrow(cell_IDs) > ((clump_side_min^2) * 4)){
      blocks <- block_pop_cutoffs
    }else{
      blocks <- seq_max
    }
    corners_rows <- seq(1, nrow(sub_strat_sett_mat), clump_side_min)
    corners_cols <- seq(1, ncol(sub_strat_sett_mat), clump_side_min)
    corner_IDs <- cbind(rep(corners_rows,each=length(corners_cols)), rep(corners_cols, length(corners_rows)))
    for(block_size in blocks){
      clump_IDs <- NULL
      if(!block_size == seq_max){
        pop_strat_mat <- matrix(NA, nrow = nrow(sub_strat_sett_mat), ncol = ncol(sub_strat_sett_mat))
        pop_class_cell_IDs <- which((sub_strat_sett_pop_mat * (block_size^2))[] >= strat_sett_target_pop_per_EZ , arr.ind=TRUE)
        if(nrow(pop_class_cell_IDs)==0) next
        pop_class_block_IDs <- sapply(split(pop_class_cell_IDs,1:nrow(pop_class_cell_IDs)), function(pop_class_cell){
          block_corner_row <- corners_rows[length(which(corners_rows <= pop_class_cell[1]))]
          block_corner_col <- corners_cols[length(which(corners_cols <= pop_class_cell[2]))]
          which(corner_IDs[,1] == block_corner_row & corner_IDs[,2] == block_corner_col)
        })
        pop_class_block_corners <- matrix(corner_IDs[unique(pop_class_block_IDs),], ncol=2)
        pop_class_IDs <- lapply(split(pop_class_block_corners,1:nrow(pop_class_block_corners)), function(corner){
          cbind(rep(corner[1]:(corner[1] + clump_side_min - 1),each=clump_side_min),
                rep(corner[2]:(corner[2] + clump_side_min - 1), clump_side_min))
        })
        pop_class_IDs <-	do.call(rbind, pop_class_IDs)
        if(length(which(pop_class_IDs[,1] > nrow(sub_strat_sett_mat)))>0){pop_class_IDs <- pop_class_IDs[-which(pop_class_IDs[,1] > nrow(sub_strat_sett_mat)),]}
        if(length(which(pop_class_IDs[,2] > ncol(sub_strat_sett_mat)))>0){pop_class_IDs <- pop_class_IDs[-which(pop_class_IDs[,2] > ncol(sub_strat_sett_mat)),]}
        pop_strat_mat[pop_class_IDs] <- 1
        pop_strat_mat[which(is.na(sub_strat_sett_pop_mat))] <- NA
        sub_strat_sett_mat[pop_class_IDs] <- sub_strat_sett_pop_mat[pop_class_IDs] <- NA
      }else{
        pop_strat_mat <- sub_strat_sett_mat
      }
      clump_raster <- raster::clump(raster::raster(pop_strat_mat), directions = 4, gaps = FALSE)
      rm(pop_strat_mat)
      clump_mat <- as.matrix(clump_raster)
      rm(clump_raster)
      clump_IDs <- sort(unique(clump_mat[!is.na(clump_mat)]))
      clump_ncells <- sapply(clump_IDs,function(x){length(which(clump_mat[] == x))})
      if(length(which(clump_ncells > 125000)) > 0){
        big_clumps <- clump_IDs[which(clump_ncells > 125000)]
        for(big_clump_ID in big_clumps){
          splits <- ceiling(clump_ncells[which(clump_IDs==big_clump_ID)]/125000)
          clump_cell_IDs <- which(clump_mat[] == big_clump_ID, arr.ind = T)
          row_IDs <- sort(unique(clump_cell_IDs[,1]))
          col_IDs <- sort(unique(clump_cell_IDs[,2]))
          if(length(row_IDs) >= length(col_IDs)){
            min_side_IDs <- min(row_IDs)
            row_or_col <- 1
          }else{
            min_side_IDs <- min(col_IDs)
            row_or_col <- 2
          }
          if(row_or_col == 1){
            new_length <- length(row_IDs)/splits 
            temp_clump_row_IDs <- row_IDs[round(seq(new_length,(length(row_IDs) - new_length),new_length))]
            new_clump_row_IDs <- c(corners_rows[sapply(temp_clump_row_IDs,function(x){which.min(abs(corners_rows-x))})],row_IDs[length(row_IDs)])
            for(k_row in 1:(length(new_clump_row_IDs)-1)){
              k_rows <- (new_clump_row_IDs[k_row] + 1):new_clump_row_IDs[k_row + 1]
              clump_IDs <- c(clump_IDs, max(clump_IDs) + 1)
              sub_clump_cell_IDs <- matrix(clump_cell_IDs[which(clump_cell_IDs[,1] %in% k_rows),],ncol=2)
              clump_mat[sub_clump_cell_IDs] <- max(clump_IDs)
              temp_mat <- clump_mat; temp_mat[!temp_mat == max(clump_IDs)] <- NA
              temp_raster <- raster::clump(raster::raster(temp_mat), directions = 4, gaps = FALSE)
              temp_mat <- as.matrix(temp_raster); rm(temp_raster)
              temp_IDs <- sort(unique(temp_mat[!is.na(temp_mat)]))
              if(length(temp_IDs) > 1){
                new_IDs <- max(clump_IDs):(max(clump_IDs) + length(temp_IDs) - 1)
                for(z in 1:length(temp_IDs)){clump_mat[temp_mat == temp_IDs[z]] <- new_IDs[z]}
                clump_IDs <- unique(c(clump_IDs,new_IDs))
              }
            }
          } 
          if(row_or_col == 2){
            new_length <- length(col_IDs)/splits
            temp_clump_col_IDs <- col_IDs[round(seq(new_length,(length(col_IDs) - new_length),new_length))]
            new_clump_col_IDs <- c(corners_cols[sapply(temp_clump_col_IDs,function(x){which.min(abs(corners_cols-x))})],col_IDs[length(col_IDs)])
            for(k_col in 1:(length(new_clump_col_IDs)-1)){
              k_cols <- (new_clump_col_IDs[k_col] + 1):new_clump_col_IDs[k_col + 1]
              clump_IDs <- c(clump_IDs, max(clump_IDs) + 1)
              sub_clump_cell_IDs <- matrix(clump_cell_IDs[which(clump_cell_IDs[,2] %in% k_cols),],ncol=2)
              clump_mat[sub_clump_cell_IDs] <- max(clump_IDs)
              temp_mat <- clump_mat; temp_mat[!temp_mat == max(clump_IDs)] <- NA
              temp_raster <- raster::clump(raster::raster(temp_mat), directions = 4, gaps = FALSE)
              temp_mat <- as.matrix(temp_raster); rm(temp_raster)
              temp_IDs <- sort(unique(temp_mat[!is.na(temp_mat)]))
              if(length(temp_IDs) > 1){
                new_IDs <- max(clump_IDs):(max(clump_IDs) + length(temp_IDs) - 1)
                for(z in 1:length(temp_IDs)){clump_mat[temp_mat == temp_IDs[z]] <- new_IDs[z]}
                clump_IDs <- unique(c(clump_IDs,new_IDs))
              }
            }
          } 
          temp_mat <- clump_mat; temp_mat[!temp_mat == big_clump_ID] <- NA
          temp_raster <- raster::clump(raster::raster(temp_mat), directions = 4, gaps = FALSE)
          temp_mat <- as.matrix(temp_raster); rm(temp_raster)
          temp_IDs <- sort(unique(temp_mat[!is.na(temp_mat)]))
          if(length(temp_IDs) > 1){
            new_IDs <- c(big_clump_ID,(max(clump_IDs) + 1):(max(clump_IDs) + length(temp_IDs) - 1))
            for(z in 1:length(temp_IDs)){clump_mat[temp_mat == temp_IDs[z]] <- new_IDs[z]}
            clump_IDs <- unique(c(clump_IDs,new_IDs))
          }
        }
      }
      for(clump_ID in clump_IDs){
        cell_IDs <- which(clump_mat[] == clump_ID, arr.ind = TRUE)
        min_clump_row <- min(cell_IDs[,1])
        max_clump_row <- max(cell_IDs[,1])
        min_clump_col <- min(cell_IDs[,2])
        max_clump_col <- max(cell_IDs[,2])
        subclump_mat <- clump_mat[min_clump_row:max_clump_row, min_clump_col:max_clump_col]
        if(!class(subclump_mat)[1] == "matrix"){subclump_mat <- matrix(subclump_mat, nrow=(max_clump_row - min_clump_row + 1))}
        subclump_mat[which(!subclump_mat[] == clump_ID)] <- NA
        subpop_mat <- population_mat[((min_strat_sett_row - 1) + min_clump_row):((min_strat_sett_row - 1) + max_clump_row), ((min_strat_sett_col - 1) + min_clump_col):((min_strat_sett_col - 1) + max_clump_col)]
        if(!class(subpop_mat)[1] == "matrix"){subpop_mat <- matrix(subpop_mat, nrow=(max_clump_row - min_clump_row + 1))}
        subpop_mat[which(is.na(subclump_mat))] <- NA
        listID <- length(clump_info) + 1
        clump_info[[listID]] <- list(subpop_mat, c(((min_strat_sett_row - 1) + min_clump_row), ((min_strat_sett_row - 1) + max_clump_row), ((min_strat_sett_col - 1) + min_clump_col), ((min_strat_sett_col - 1) + max_clump_col)), strat_sett_target_pop_per_EZ)
      }
    }
  }
  print("number of clumps = ")
  print(length(clump_info))
  save(clump_info, file = paste(output_path,"/clump_info",run_ID, ".RData",sep=""))
  rm(strat_sett_mat);rm(sett_mat);rm(strata_mat);rm(clump_mat)

  
  ###################################
  # specify EZ generation function  #
  ###################################
  
  EZ_gen_fn <- function(clump_x, max_cells_per_EZ, output_path, run_ID){ 
    
    # save a temporary file for clump currently being processed 
    fake <- list(1)
    tempfileID <- paste(clump_x[[2]][1], clump_x[[2]][2], clump_x[[2]][3], clump_x[[2]][4], sep="")
    save(fake, file = paste(output_path,"/temp_folder", run_ID, "/temp_", tempfileID, ".RData",sep=""))
    
    ###################################
    # prep & check clump information  #
    ###################################
    
    EZ_cell_IDs <- list()
    ss_target_pop_per_EZ <- clump_x[[3]]
    min_pop_per_EZ <- ss_target_pop_per_EZ * 0.75  # 25%
    max_pop_per_EZ <- ss_target_pop_per_EZ * 1.25  # 25%
    min_clump_row <- clump_x[[2]][1]
    max_clump_row <- clump_x[[2]][2]
    min_clump_col <- clump_x[[2]][3]
    max_clump_col <- clump_x[[2]][4]
    subpop_mat <- clump_x[[1]]
    clump_x[[1]] <- 0 # to reduce memory use
    if(class(subpop_mat)[1] == "matrix"){                       #i.e. more than 1 cell (almost always)
      cell_IDs <- which(!is.na(subpop_mat), arr.ind = TRUE)
    }else{                                                      #i.e. just 1 cell (occasionally, if other cells of smod block have corresponding pop or strata = NA)
      cell_IDs <- matrix(c(1,1), nrow=1)
    }
    clump_pop_size <- sum(subpop_mat[cell_IDs])
    
    ##########################################################
    # if clump constitutes a single EZ, identify EZ cell IDs #
    ##########################################################
    
    #if total pop of clump is smaller than min pop per EZ AND total number of cells in clump (given by nrow(cell_IDs)) is smaller than max cells per EZ, then don't split into multiple EZs & save clump as single EZ
    if(((clump_pop_size <= max_pop_per_EZ) & (nrow(cell_IDs) <= max_cells_per_EZ * 1.5)) | nrow(cell_IDs)==1){
      cell_IDs[,1] <- cell_IDs[,1] + min_clump_row - 1
      cell_IDs[,2] <- cell_IDs[,2] + min_clump_col - 1
      EZ_cell_IDs <- list(cell_IDs) 
    }else{
    
    ###########################################################
    # if clump does not constitutes a single EZ, generate EZs #
    ###########################################################
      
      #################################################
      # create initial blocked mesh of potential EZs  #
      #################################################
      
      max_cell_pop_size <- max(subpop_mat[!(is.na(subpop_mat))]) 
      med_cell_pop_size <- median(subpop_mat[!(is.na(subpop_mat))]) 
      min_sett_row <- min_sett_col <- 1
      max_sett_row <- max_clump_row - (min_clump_row - 1)   
      max_sett_col <- max_clump_col - (min_clump_col - 1)   
      if((max_cell_pop_size * max_cells_per_EZ) < min_pop_per_EZ){
        block_horiz <- block_vert <- round(sqrt(max_cells_per_EZ))
      }else{
        val1 <- round(sqrt(ss_target_pop_per_EZ/med_cell_pop_size))
        if(val1 <= 0){val1 <- 1}
        if(val1 > round(sqrt(max_cells_per_EZ))){
          block_horiz <- block_vert <- round(sqrt(max_cells_per_EZ))
        }else{
          if(val1 %in% 1:2){ 
            block_vert <- block_horiz <- val1
          }else{
            if(val1 < 5){val2 <- val1 - 1}
            if(val1 >= 5){val2 <- val1 - 2}
            row_range <- max_sett_row - min_sett_row 
            col_range <- max_sett_col - min_sett_col 
            if(row_range >= col_range){
              block_vert <- val1
              block_horiz <- val2
            }else{
              block_vert <- val2
              block_horiz <- val1
            }
          }
        }
      }
      # ID location of max pop cell so that we can put in centre of a block and build block frame from there
      ID_max_pop_cell <- (which(subpop_mat == max_cell_pop_size, arr.ind = TRUE))[1,] 
      y_start <- min((ID_max_pop_cell[1] + floor(block_vert/2)), max_sett_row) #/2 so that max cell is middle of block
      x_start <- min((ID_max_pop_cell[2] + floor(block_horiz/2)), max_sett_col)
      seq_vert <- y_start; seq_horiz <- x_start   
      # build grid for blocks. Lots of ifs so that use alternate values if close to the edge of the raster section
      if((y_start - block_vert) > min_sett_row){seq_vert <- rev(seq(y_start, min_sett_row, -block_vert))}
      if((y_start + block_vert) < max_sett_row){seq_vert <- c(seq_vert, seq((y_start + block_vert), max_sett_row, block_vert))}
      if(!seq_vert[1] == min_sett_row){seq_vert <- c(min_sett_row, seq_vert)}
      if(!seq_vert[length(seq_vert)] == max_sett_row){seq_vert <- c(seq_vert, max_sett_row)}
      if((x_start - block_horiz) > min_sett_col){seq_horiz <- rev(seq(x_start, min_sett_col, -block_horiz))}
      if((x_start + block_horiz) < max_sett_col){seq_horiz <- c(seq_horiz, seq((x_start + block_horiz), max_sett_col, block_horiz))}
      if(!seq_horiz[1] == min_sett_col){seq_horiz <- c(min_sett_col, seq_horiz)}
      if(!seq_horiz[length(seq_horiz)] == max_sett_col){seq_horiz <- c(seq_horiz, max_sett_col)}
      if(block_vert == 1 | length(seq_vert) == 1){block_ref_vert <- cbind(seq_vert,seq_vert)}else{
        block_ref_vert <- cbind(seq_vert[1:(length(seq_vert)-1)], c((seq_vert[2:(length(seq_vert)-1)]-1), max_sett_row))
      }
      if(block_horiz == 1 | length(seq_horiz) == 1){block_ref_horiz <- cbind(seq_horiz,seq_horiz)}else{
        block_ref_horiz <- cbind(seq_horiz[1:(length(seq_horiz)-1)], c((seq_horiz[2:(length(seq_horiz)-1)]-1), max_sett_col))
      }
      block_ref_df <- cbind(block_ref_vert[rep(seq_len(nrow(block_ref_vert)), nrow(block_ref_horiz)),], block_ref_horiz[rep(seq_len(nrow(block_ref_horiz)), each = nrow(block_ref_vert)),])
      # assign temporary EZ IDs to all blocks within extent of clump. Extent defined by min/max_sett_row/col
      EZ_mat <- matrix(NA, nrow = max_sett_row, ncol = max_sett_col)  
      for(block_ID in 1:nrow(block_ref_df)){    # loop instead of function or apply because want vals in one matrix to be updated
        EZ_mat[unique(block_ref_df[block_ID,1]:block_ref_df[block_ID,2]), unique(block_ref_df[block_ID,3]:block_ref_df[block_ID,4])] <- block_ID
      }
      EZ_mat[which(is.na(subpop_mat))] <- NA    
      EZ_ID_vec <- unique(EZ_mat[which(!is.na(EZ_mat))])  
      EZ_pop <- sapply(EZ_ID_vec, function(EZ_index){sum(subpop_mat[which(EZ_mat == EZ_index)])})
      
      ###############################################################################
      # add blocks (EZs) with pop below min pop per EZ to neighbouring blocks (EZs) #
      ###############################################################################
      
      small_pop_EZs <- EZ_ID_vec[which(EZ_pop < min_pop_per_EZ)]
      # of these small EZs, exclude from small EZ list those that meet geog size 
      cells_count_test <- table(EZ_mat[EZ_mat %in% small_pop_EZs]) > (max_cells_per_EZ * 0.8) #have  80% of max limit here, if an EZ is almost at the lim don't want to add to another block 
      if("TRUE" %in% cells_count_test){
        max_geog_in_small_pop_EZs <- as.numeric(names(cells_count_test[cells_count_test == "TRUE"]))
        small_pop_EZs <- small_pop_EZs[!small_pop_EZs %in% max_geog_in_small_pop_EZs]
      }
      small_pop_EZs <- as.numeric(names(sort(table(EZ_mat[EZ_mat %in% small_pop_EZs])))) # re-order small_pop_EZs by geog size
      print(length(small_pop_EZs))
      if(length(small_pop_EZs)>0){
        repeat{
          EZ_ID <- small_pop_EZs[1]
          # neighbour EZs
          EZ_cell_IDs <- which(EZ_mat == EZ_ID, arr.ind = TRUE)
          nb_EZs <- cell_nbs <- NULL
          for(df_row in 1:nrow(EZ_cell_IDs)){
            loc_vert <- EZ_cell_IDs[df_row,1]
            loc_horiz <- EZ_cell_IDs[df_row,2]
            adj_cells <- cbind(c((loc_vert + 1), (loc_vert - 1), loc_vert, loc_vert), c(loc_horiz, loc_horiz, (loc_horiz + 1), (loc_horiz - 1)))
            if(TRUE %in% (!adj_cells[,1] %in% 1:max_sett_row)){adj_cells <- adj_cells[-which((!adj_cells[,1] %in% 1:max_sett_row)),]}
            if(TRUE %in% (!adj_cells[,2] %in% 1:max_sett_col)){adj_cells <- adj_cells[-which((!adj_cells[,2] %in% 1:max_sett_col)),]}
            cell_nbs <- EZ_mat[adj_cells]
            nb_EZs <- c(nb_EZs, cell_nbs)
          }
          nb_EZs <- unique(nb_EZs)
          nb_EZs <- nb_EZs[!is.na(nb_EZs) & !nb_EZs == EZ_ID]
          # measure compactness for each of the possible combinations of blocks/EZs
          compact_vals <- sapply(nb_EZs,function(new_EZ){
            test_EZ_locs <- which(EZ_mat == EZ_ID | EZ_mat == new_EZ ,arr.ind = TRUE)
            edge_peri <- sum(sapply(split(test_EZ_locs, 1:nrow(test_EZ_locs)),function(mat_cell){
              row <- c((mat_cell[1] + 1), (mat_cell[1] - 1), mat_cell[1], mat_cell[1])
              col <- c(mat_cell[2], mat_cell[2], (mat_cell[2] + 1), (mat_cell[2] - 1))
              4 - nrow(merge(test_EZ_locs,cbind(row,col),by=c("row","col")))
            }))
            (edge_peri^2)/nrow(test_EZ_locs)
          })
          EZ_ID_to_add <- (nb_EZs[compact_vals == min(compact_vals)])[1] 
          EZ_mat[EZ_mat == EZ_ID_to_add] <- EZ_ID   
          if(EZ_ID_to_add %in% small_pop_EZs){small_pop_EZs <- small_pop_EZs[-which(small_pop_EZs == EZ_ID_to_add)]} 
          if(sum(subpop_mat[which(EZ_mat == EZ_ID)]) > min_pop_per_EZ){ 
            small_pop_EZs <- small_pop_EZs[-which(small_pop_EZs == EZ_ID)]
          }else{  
            if(length(which(EZ_mat[] == EZ_ID)) > (max_cells_per_EZ * 0.8)){
              small_pop_EZs <- small_pop_EZs[-which(small_pop_EZs == EZ_ID)]
            }
          }  
          if(!length(small_pop_EZs)>0) break
          small_pop_EZs <- as.numeric(names(sort(table(EZ_mat[EZ_mat %in% small_pop_EZs])))) 
        }
        EZ_ID_vec <- unique(EZ_mat[which(!is.na(EZ_mat))]) 
        EZ_pop <- sapply(EZ_ID_vec, function(EZ_index){sum(subpop_mat[which(EZ_mat == EZ_index)])})
      }
      
      ###############################################################
      # split blocks (EZs) with pop greater than 2 x max pop per EZ #
      ###############################################################
      
      vbig_pop_EZs <- EZ_ID_vec[which(EZ_pop >= (2 * ss_target_pop_per_EZ))] #identify very big pop EZs,if over 2*target pop (very big) then can split in two, along axis with most cells
      if(length(vbig_pop_EZs)>0){
        EZ_IDs_new <- (max(EZ_ID_vec) + 1):(max(EZ_ID_vec) + 2)
        repeat{
          EZ_ID <- vbig_pop_EZs[1]
          EZ_cell_IDs <- which(EZ_mat == EZ_ID, arr.ind = TRUE)
          tot_pop_size <- sum(subpop_mat[EZ_cell_IDs])
          #create list, an element for each potential split, element contains matrix of cell row & col ids one EZ of the potential split, do this because can't assume EZs are only four edged shapes
          row_IDs <- sort(unique(EZ_cell_IDs[,1]))
          col_IDs <- sort(unique(EZ_cell_IDs[,2]))
          if(length(row_IDs) >= length(col_IDs)){
            side_IDs <- row_IDs
            min_side_IDs <- min(row_IDs)
            row_or_col <- 1
          }else{
            side_IDs <- col_IDs
            min_side_IDs <- min(col_IDs)
            row_or_col <- 2
          }
          cell_IDs_ls <- lapply(side_IDs[1:(length(side_IDs) - 1)], function(x){matrix(EZ_cell_IDs[EZ_cell_IDs[,row_or_col] %in% (min_side_IDs:x),], ncol=2)})    
          pop_sizes <- sapply(cell_IDs_ls, function(x){sum(subpop_mat[x])})
          pop_sizes_second <- tot_pop_size - pop_sizes
          keepers1 <- which(pop_sizes >= min_pop_per_EZ)
          keepers2 <- which(pop_sizes_second >= min_pop_per_EZ)
          test_splits <- keepers1[which(keepers1 %in% keepers2)]
          if(!length(test_splits) > 0){
            vbig_pop_EZs <- vbig_pop_EZs[-(which(vbig_pop_EZs == EZ_ID))]
            if(!length(vbig_pop_EZs)>0){ 
              break
            }else{
              next    
            }
          }
          if(length(test_splits) > 10){
            test_splits <- test_splits[seq(1,length(test_splits),ceiling(length(test_splits)/10))]
          }
          compact_vals <-  lapply(test_splits,function(test_EZ_split){
            compacts <- NULL
            for(x in 1:2){
              if(x==1){test_EZ_locs <- cell_IDs_ls[[test_EZ_split]]}
              if(x==2){
                d1 <- duplicated(rbind(cell_IDs_ls[[test_EZ_split]],EZ_cell_IDs))
                d1 <- d1[(nrow(cell_IDs_ls[[test_EZ_split]])+1):length(d1)] #FALSES for EZ_cell_IDs that are not duplicated in cell_IDs for one side of split
                test_EZ_locs <- EZ_cell_IDs[which(d1 == FALSE),]
              }
              if(!class(test_EZ_locs)[1] == "matrix"){test_EZ_locs <- matrix(test_EZ_locs, nrow=1)}
              colnames(test_EZ_locs) <- c("row","col")
              edge_peri <- sum(sapply(split(test_EZ_locs, 1:nrow(test_EZ_locs)),function(mat_cell){
                row <- c((mat_cell[1] + 1), (mat_cell[1] - 1), mat_cell[1], mat_cell[1])
                col <- c(mat_cell[2], mat_cell[2], (mat_cell[2] + 1), (mat_cell[2] - 1))
                4 - nrow(merge(test_EZ_locs,cbind(row,col),by=c("row","col")))
              }))
              compacts <- c(compacts, (edge_peri^2)/nrow(test_EZ_locs)) 
            }
            compacts
          })
          compact_vals <- do.call(rbind, compact_vals)
          compact_sum <- compact_vals[,1] + compact_vals[,2]
          compact_threshold <- min(compact_sum) * 1.25
          selected_split_ID <- test_splits[which(compact_sum <= compact_threshold)[1]]
          EZ_mat[cell_IDs_ls[[selected_split_ID]]] <- EZ_IDs_new[1]  # use a brand new EZ_ID, taken from beginning of vec of new IDs
          EZ_ID_pop <- sum(subpop_mat[which(EZ_mat == EZ_ID)])
          if(EZ_ID_pop < (2 * ss_target_pop_per_EZ)){vbig_pop_EZs <- vbig_pop_EZs[-(which(vbig_pop_EZs == EZ_ID))]}
          EZ_ID_new_pop <- sum(subpop_mat[which(EZ_mat == EZ_IDs_new[1])])
          if(EZ_ID_new_pop >= (2 * ss_target_pop_per_EZ)){vbig_pop_EZs <- c(vbig_pop_EZs, EZ_IDs_new[1])}
          EZ_IDs_new <- EZ_IDs_new[-1]; EZ_IDs_new <- c(EZ_IDs_new, (max(EZ_IDs_new) + 1))
          if(!length(vbig_pop_EZs)>0) break
        }
        EZ_ID_vec <- unique(EZ_mat[which(!is.na(EZ_mat))]) 
        EZ_pop <- sapply(EZ_ID_vec, function(EZ_index){sum(subpop_mat[which(EZ_mat == EZ_index)])})
      }
      
      #################################################################################
      # add blocks (EZs) with pop greater than max pop per EZ, then split into 2 or 3 #
      #################################################################################
      
      big_pop_EZs <- EZ_ID_vec[which(EZ_pop > max_pop_per_EZ)] #remaining big pop EZs (exceeding max pop limit), add to near block, then divide by 2 or 3
      cells_count_test <- table(EZ_mat[EZ_mat %in% big_pop_EZs]) >= 2  #at least 2 cells needed for splitting
      if("TRUE" %in% cells_count_test){
        big_pop_EZs <- as.numeric(names(cells_count_test[cells_count_test == "TRUE"]))
      }
      if(length(big_pop_EZs)>0){
        EZ_IDs_new <- (max(EZ_ID_vec) + 1):(max(EZ_ID_vec) + 3)   
        repeat{
          EZ_ID <- big_pop_EZs[1]
          EZ_cell_IDs <- which(EZ_mat == EZ_ID, arr.ind = TRUE)
          #neighbour EZs
          nb_EZs <- cell_nbs <- NULL
          for(df_row in 1:nrow(EZ_cell_IDs)){
            loc_vert <- EZ_cell_IDs[df_row,1]
            loc_horiz <- EZ_cell_IDs[df_row,2]
            adj_cells <- cbind(c((loc_vert + 1), (loc_vert - 1), loc_vert, loc_vert), c(loc_horiz, loc_horiz, (loc_horiz + 1), (loc_horiz - 1)))
            if(TRUE %in% (!adj_cells[,1] %in% 1:max_sett_row)){adj_cells <- adj_cells[-which((!adj_cells[,1] %in% 1:max_sett_row)),]}
            if(TRUE %in% (!adj_cells[,2] %in% 1:max_sett_col)){adj_cells <- adj_cells[-which((!adj_cells[,2] %in% 1:max_sett_col)),]}
            cell_nbs <- EZ_mat[adj_cells]
            nb_EZs <- c(nb_EZs, cell_nbs)
          }
          nb_EZs <- unique(nb_EZs)
          nb_EZs <- nb_EZs[!is.na(nb_EZs) & !nb_EZs == EZ_ID]
          if(length(nb_EZs) > 0){         #i.e. add to neighbour only if neighbour exists
            EZ_pop_size <- sum(subpop_mat[EZ_cell_IDs])
            nb_EZ_pop_sizes <- NULL
            for(nb_EZ in nb_EZs){
              nb_EZ_cell_IDs <- which(EZ_mat == nb_EZ, arr.ind = TRUE)
              nb_EZ_pop_size <- sum(subpop_mat[nb_EZ_cell_IDs])
              nb_EZ_pop_sizes <- c(nb_EZ_pop_sizes, nb_EZ_pop_size)
            }
            new_pop_sizes <- (nb_EZ_pop_sizes + EZ_pop_size)/2
            if(max(new_pop_sizes) < min_pop_per_EZ){
              big_pop_EZs <- big_pop_EZs[-which(big_pop_EZs == EZ_ID)]
              if(!length(big_pop_EZs)>0){ 
                break
              }else{
                next   #don't do anything if splitting will result in new EZs with too small pop 
              }
            }
            #combine with appropriate neighbouring EZ 
            nb_EZ_ID <- which(new_pop_sizes >= min_pop_per_EZ)
            if(length(nb_EZ_ID) > 1){
              nb_EZs <- nb_EZs[nb_EZ_ID]
              compact_vals <- sapply(nb_EZs,function(new_EZ){
                test_EZ_locs <- which(EZ_mat == EZ_ID | EZ_mat == new_EZ ,arr.ind = TRUE)
                edge_peri <- sum(sapply(split(test_EZ_locs, 1:nrow(test_EZ_locs)),function(mat_cell){
                  row <- c((mat_cell[1] + 1), (mat_cell[1] - 1), mat_cell[1], mat_cell[1])
                  col <- c(mat_cell[2], mat_cell[2], (mat_cell[2] + 1), (mat_cell[2] - 1))
                  4 - nrow(merge(test_EZ_locs,cbind(row,col),by=c("row","col")))
                }))
                (edge_peri^2)/nrow(test_EZ_locs)
              })
              nb_EZ <- (nb_EZs[compact_vals == min(compact_vals)])[1] 
            }else{
              nb_EZ <- nb_EZs[nb_EZ_ID]
            }
            #create single big EZ to then split into three or two
            EZ_mat[EZ_mat == nb_EZ] <- EZ_ID
            if(nb_EZ %in% big_pop_EZs){big_pop_EZs <- big_pop_EZs[-which(big_pop_EZs == nb_EZ)]}
          }
          EZ_cell_IDs <- which(EZ_mat == EZ_ID, arr.ind = TRUE)
          tot_pop_size <- sum(subpop_mat[EZ_cell_IDs])
          if(((tot_pop_size/3) > min_pop_per_EZ) & (nrow(EZ_cell_IDs) >= 3)){ 
            split_types <- c(1,2)     #splitting twice
          }else{
            split_types <- 2          #splitting once (this split is same as second split when splitting twice)
          }
          #split shape into most compact shapes
          for(split_type in split_types){
            if(split_type == 2 & length(split_types) == 2){     #need to update if doing 2nd split
              EZ_cell_IDs <- which(EZ_mat == EZ_ID, arr.ind = TRUE)
              tot_pop_size <- sum(subpop_mat[EZ_cell_IDs])
              if(nrow(EZ_cell_IDs) == 1) break
            }
            row_IDs <- sort(unique(EZ_cell_IDs[,1]))
            col_IDs <- sort(unique(EZ_cell_IDs[,2]))
            if(length(row_IDs) >= length(col_IDs)){
              side_IDs <- row_IDs
              min_side_IDs <- min(row_IDs)
              row_or_col <- 1
            }else{
              side_IDs <- col_IDs
              min_side_IDs <- min(col_IDs)
              row_or_col <- 2
            }
            cell_IDs_ls <- lapply(side_IDs[1:(length(side_IDs) - 1)], function(x){matrix(EZ_cell_IDs[EZ_cell_IDs[,row_or_col] %in% (min_side_IDs:x),], ncol=2)})    
            pop_sizes <- sapply(cell_IDs_ls, function(x){sum(subpop_mat[x])})
            if(split_type == 1){
              diff_to_ideal_pop <- abs((tot_pop_size/3) - pop_sizes)
            }
            if(split_type == 2){
              diff_to_ideal_pop <- abs(tot_pop_size - (2*pop_sizes))  # A = B + C, this gives is |C - B| i.e. diff in the two potential EZs' pop sizes
            }
            test_splits <- order(diff_to_ideal_pop)
            if(length(test_splits) >= 10){test_splits <- test_splits[1:10]}
            compact_vals <-  lapply(test_splits,function(test_EZ_split){
              compacts <- NULL
              for(x in 1:2){
                if(x==1){test_EZ_locs <- cell_IDs_ls[[test_EZ_split]]}
                if(x==2){
                  d1 <- duplicated(rbind(cell_IDs_ls[[test_EZ_split]],EZ_cell_IDs))
                  d1 <- d1[(nrow(cell_IDs_ls[[test_EZ_split]])+1):length(d1)] #FALSES for EZ_cell_IDs that are not duplicated in cell_IDs for one side of split
                  test_EZ_locs <- EZ_cell_IDs[which(d1 == FALSE),]
                }
                if(!class(test_EZ_locs)[1] == "matrix"){test_EZ_locs <- matrix(test_EZ_locs, nrow=1)}
                colnames(test_EZ_locs) <- c("row","col")
                edge_peri <- sum(sapply(split(test_EZ_locs, 1:nrow(test_EZ_locs)),function(mat_cell){
                  row <- c((mat_cell[1] + 1), (mat_cell[1] - 1), mat_cell[1], mat_cell[1])
                  col <- c(mat_cell[2], mat_cell[2], (mat_cell[2] + 1), (mat_cell[2] - 1))
                  4 - nrow(merge(test_EZ_locs,cbind(row,col),by=c("row","col")))
                }))
                compacts <- c(compacts, (edge_peri^2)/nrow(test_EZ_locs)) 
              }
              compacts
            })
            compact_vals <- do.call(rbind, compact_vals)
            compact_sum <- compact_vals[,1] + compact_vals[,2]
            compact_threshold <- min(compact_sum) * 1.25
            selected_split_ID <- test_splits[which(compact_sum <= compact_threshold)[1]]
            EZ_mat[cell_IDs_ls[[selected_split_ID]]] <- EZ_IDs_new[1]  
            EZ_IDs_new <- EZ_IDs_new[-1]; EZ_IDs_new <- c(EZ_IDs_new, (max(EZ_IDs_new) + 1))
          }
          big_pop_EZs <- big_pop_EZs[-which(big_pop_EZs == EZ_ID)]
          if(!length(big_pop_EZs)>0) break
        } 
        EZ_ID_vec <- unique(EZ_mat[which(!is.na(EZ_mat))]) 
      }
      
      #########################################################################
      # split blocks (EZs) with geog. size greater than max geog. size per EZ #
      #########################################################################
      
      EZ_geog <- sapply(EZ_ID_vec, function(EZ_index){length(which(EZ_mat == EZ_index))})
      big_geog_EZs <- EZ_ID_vec[which(EZ_geog > (1.5 * max_cells_per_EZ))]
      if(length(big_geog_EZs)>0){
        print("sorting big geog EZs")
        print(Sys.time())
        EZ_IDs_new <- (max(EZ_ID_vec) + 1):(max(EZ_ID_vec) + length(big_geog_EZs))
        for(EZ_ID in big_geog_EZs){  
          EZ_cell_IDs <- which(EZ_mat == EZ_ID, arr.ind = TRUE)
          row_IDs <- sort(unique(EZ_cell_IDs[,1]))
          col_IDs <- sort(unique(EZ_cell_IDs[,2]))
          if(length(row_IDs) >= length(col_IDs)){
            side_IDs <- row_IDs
            min_side_IDs <- min(row_IDs)
            row_or_col <- 1
          }else{
            side_IDs <- col_IDs
            min_side_IDs <- min(col_IDs)
            row_or_col <- 2
          }
          cell_IDs_ls <- lapply(side_IDs[1:(length(side_IDs) - 1)], function(x){matrix(EZ_cell_IDs[EZ_cell_IDs[,row_or_col] %in% (min_side_IDs:x),], ncol=2)})    
          cell_counts <- sapply(cell_IDs_ls, function(x){nrow(x)})
          target_geog <- EZ_geog[which(EZ_ID_vec == EZ_ID)]/2
          selected_split_ID <- which(abs(target_geog - cell_counts) == min(abs(target_geog - cell_counts)))[1]
          EZ_mat[cell_IDs_ls[[selected_split_ID]]] <- EZ_IDs_new[1]
          EZ_IDs_new <- EZ_IDs_new[-1]
        }
        EZ_ID_vec <- unique(EZ_mat[which(!is.na(EZ_mat))]) 
      }
      
      ################################
      # check all EZs are contiguous #
      ################################
      
      EZ_ID_max <- max(EZ_ID_vec)
      for(EZ_ID in EZ_ID_vec){  
        cell_IDs <- which(EZ_mat[] == EZ_ID, arr.ind = TRUE)
        min_EZ_row <- min(cell_IDs[,1])
        max_EZ_row <- max(cell_IDs[,1])
        min_EZ_col <- min(cell_IDs[,2])
        max_EZ_col <- max(cell_IDs[,2])
        subEZ_IDs_mat <- EZ_mat[min_EZ_row:max_EZ_row, min_EZ_col:max_EZ_col]
        subEZ_IDs_mat[which(!subEZ_IDs_mat[] == EZ_ID)] <- NA
        if(class(subEZ_IDs_mat)[1] == "matrix"){
          EZ_clump_raster <- raster::clump(raster::raster(subEZ_IDs_mat), directions = 4, gaps = FALSE)
          clump_ls <- sort(unique(getValues(EZ_clump_raster)))
          if(length(clump_ls) > 1){
            new_EZ_IDs_vec <- c(EZ_ID, seq((EZ_ID_max + 1), (EZ_ID_max + length(clump_ls) - 1), length.out = (length(clump_ls) - 1)))
            EZ_clump_mat <- as.matrix(EZ_clump_raster)
            rm(EZ_clump_raster)
            for(new_clump in clump_ls[-1]){   
              new_clump_sub_IDs <- which(EZ_clump_mat[] == new_clump, arr.ind = TRUE)
              new_clump_IDs <- cbind(new_clump_sub_IDs[,1] + min_EZ_row - 1, new_clump_sub_IDs[,2] + min_EZ_col - 1)
              EZ_mat[new_clump_IDs] <- new_EZ_IDs_vec[which(clump_ls == new_clump)]
            }
            EZ_ID_max <- max(new_EZ_IDs_vec) + 1
            new_EZ_pop <- sapply(new_EZ_IDs_vec, function(EZ_index){sum(subpop_mat[which(EZ_mat == EZ_index)])})
            new_EZ_size <- sapply(new_EZ_IDs_vec, function(EZ_index){length(which(EZ_mat == EZ_index))})
            small_new_EZs <- new_EZ_IDs_vec[which(new_EZ_size < (0.75 * max_cells_per_EZ) & (new_EZ_pop < min_pop_per_EZ))]
            for(small_EZ in small_new_EZs){
              # neighbour EZs
              EZ_cell_IDs <- which(EZ_mat == small_EZ, arr.ind = TRUE)
              nb_EZs <- cell_nbs <- NULL
              for(df_row in 1:nrow(EZ_cell_IDs)){
                loc_vert <- EZ_cell_IDs[df_row,1]
                loc_horiz <- EZ_cell_IDs[df_row,2]
                adj_cells <- cbind(c((loc_vert + 1), (loc_vert - 1), loc_vert, loc_vert), c(loc_horiz, loc_horiz, (loc_horiz + 1), (loc_horiz - 1)))
                if(TRUE %in% (!adj_cells[,1] %in% 1:max_sett_row)){adj_cells <- adj_cells[-which((!adj_cells[,1] %in% 1:max_sett_row)),]}
                if(TRUE %in% (!adj_cells[,2] %in% 1:max_sett_col)){adj_cells <- adj_cells[-which((!adj_cells[,2] %in% 1:max_sett_col)),]}
                cell_nbs <- EZ_mat[adj_cells]
                nb_EZs <- c(nb_EZs, cell_nbs)
              }
              nb_EZs <- unique(nb_EZs)
              nb_EZs <- nb_EZs[!is.na(nb_EZs) & !nb_EZs == small_EZ]
              if(length(nb_EZs) > 0){
                # measure compactness for each of the possible combinations of blocks/EZs
                compact_vals <- sapply(nb_EZs,function(new_EZ){
                  test_EZ_locs <- which(EZ_mat == EZ_ID | EZ_mat == new_EZ ,arr.ind = TRUE)
                  edge_peri <- sum(sapply(split(test_EZ_locs, 1:nrow(test_EZ_locs)),function(mat_cell){
                    row <- c((mat_cell[1] + 1), (mat_cell[1] - 1), mat_cell[1], mat_cell[1])
                    col <- c(mat_cell[2], mat_cell[2], (mat_cell[2] + 1), (mat_cell[2] - 1))
                    4 - nrow(merge(test_EZ_locs,cbind(row,col),by=c("row","col")))
                  }))
                  (edge_peri^2)/nrow(test_EZ_locs)
                })
                EZ_ID_to_add <- (nb_EZs[compact_vals == min(compact_vals)])[1]   # compactness is likely most important when adding on smaller chunks, so don't consider pop size here
                EZ_mat[EZ_mat == small_EZ] <- EZ_ID_to_add
              }
            }
          }
        }
      } 

      #########################
      # identify EZs cell IDs #
      #########################
      
      EZ_ID_vec <- sort(unique(as.vector(EZ_mat))) 
      EZ_cell_IDs <- lapply(1:length(EZ_ID_vec), function(EZ){
        subEZ_cell_IDs <- which(EZ_mat == EZ_ID_vec[EZ], arr.ind = T)
        subEZ_cell_IDs[,1] <- subEZ_cell_IDs[,1] + min_clump_row - 1
        subEZ_cell_IDs[,2] <- subEZ_cell_IDs[,2] + min_clump_col - 1
        subEZ_cell_IDs
      })
    } 
    
    #################
    # save EZs data #
    #################
    
    unlink(paste(output_path,"/temp_folder", run_ID, "/temp_", tempfileID, ".RData",sep=""), recursive = TRUE)
    fileID <- paste(clump_x[[2]][1], clump_x[[2]][2], clump_x[[2]][3], clump_x[[2]][4], sep="")
    save(EZ_cell_IDs, file = paste(output_path,"/temp_folder", run_ID, "/EZ_cell_IDs_list", fileID, ".RData",sep=""))
  } 
  # end of EZ generation function
  
  ###############################################################
  # run EZ generation function across specified number of cores #
  ###############################################################
  
  cl <- makeCluster(ncores, par_type)
  print("starting cluster")
  clusterExport(cl, split(ls("package:raster"),1:length(ls("package:raster"))))   
  invisible(parLapply(cl, clump_info, EZ_gen_fn, max_cells_per_EZ=max_cells_per_EZ, output_path = output_path, run_ID = run_ID))
  stopCluster(cl)
  print("EZ generation complete. Compiling EZ files and generating EZ rasters next")
  
  ###############################################################
  #  load EZ files, and create EZ ID and EZ population rasters  #
  ###############################################################
  
  current_list <- list.files(path = paste(output_path,"/temp_folder", run_ID, sep=""), pattern =".RData$", full.names=TRUE)
  EZ_pop_master_mat <- EZ_master_mat <- matrix(NA, nrow = n_rows, ncol = n_cols)
  EZ_index <- 0
  # a for loop used so that the EZ IDs are consecutive
  for(filenumber in 1:length(current_list)){
    load(current_list[filenumber])
    for(file_listID in 1:length(EZ_cell_IDs)){   #in case code changes to have multiple clumps per file
      EZ_cells_IDs_temp <- EZ_cell_IDs[[file_listID]]
      EZ_index <- EZ_index + 1
      EZ_master_mat[EZ_cells_IDs_temp] <- EZ_index
      EZ_pop_master_mat[EZ_cells_IDs_temp] <- sum(population_mat[EZ_cells_IDs_temp])
    }
    EZ_cell_IDs <- NULL
  }  

  ############################################
  #  saving EZ ID and EZ population rasters  #
  ############################################
  
  EZ_raster_master <- raster(EZ_master_mat,crs=r_crs, xmn=r_xmn, xmx=r_xmx, ymn=r_ymn, ymx=r_ymx)
  EZ_pop_raster_master <- raster(EZ_pop_master_mat,crs=r_crs, xmn=r_xmn, xmx=r_xmx, ymn=r_ymn, ymx=r_ymx)
  writeRaster(EZ_raster_master, paste(output_path,"/EZ_raster_master", run_ID, ".tif",sep=""), datatype = 'INT4U', format = "GTiff")
  writeRaster(EZ_pop_raster_master, paste(output_path,"/EZ_pop_raster_master", run_ID, ".tif",sep=""), format = "GTiff")
  unlink(paste(output_path,"/clump_info",run_ID, ".RData",sep=""), recursive = TRUE)
  unlink(paste(output_path,"/temp_folder", run_ID, sep=""), recursive = TRUE)
  return(NA)
}
