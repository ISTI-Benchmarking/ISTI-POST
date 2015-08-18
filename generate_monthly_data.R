generate_monthly_data <- function() {
  # This script generates monthly data files for the level 4 directories 
  # from the standard format daily files in the level 3 directories. Later 
  # this function should also copy level 2 monthly data to the level 4 directories.
  # 
  # Written by Victor Venema
  # First version 11 Sept 2014
  
  # Read the name of the conversion functions to be used from the settings 
  settings = read_software_settings()
  conversion = settings$conversion  
  
  # Loop over all data sources
  noSources = length(conversion$dirName)
  for( iSource in 1:noSources) {      
    dataDirectory = as.character(conversion$dirName[iSource])      # Name of directory with native data
#     Level2DataDir = paste(settings$baseDir, "data", dataDirectory, "level2", sep=.Platform$file.sep)
    Level3DataDir = paste(settings$baseDir, "data", dataDirectory, "level3", sep=.Platform$file.sep)
    Level4DataDir = paste(settings$baseDir, "data", dataDirectory, "level4", sep=.Platform$file.sep)
    stationSubDirs = dir(Level3DataDir, no..=TRUE)
    stationSubDirsFull = dir(Level3DataDir, full.names=TRUE, no..=TRUE)
    noStations = length(stationSubDirs)
    # if there are stations directory in the level 3 directory of the source iSource loop over them
    if( noStations > 0 ) {
      for(iStat in 1:noStations) {
        # read al data in the subdirectory into the lists for every parallel measurement and one for the co-variates
        # read_standard_data
        tmp = read_standard_data_directory(stationSubDirsFull[iStat])
        data = tmp[[1]]
        dirFileNames = tmp[[2]]
        rm(tmp)
        
        # Force the measurement to start at the beginning of the month (day=1) and end at the end of the month                
        noVar = length(data)
        for( iVar in 1:noVar ) {
    #         indexBegin = which.max(data[[iStat]]$month == 1)
    #         indexEnd   = which(data[[iStat]]$month == 1)
          index      = which(data[[iVar]]$day == 1)
          indexBegin = index[1]
          indexEnd   = index[length(index)-1] # If the dataset already end exactly at the end of the month, this code would remove a month for nothing. Consider rewriting.        

          noElements = length(data[[iVar]])
          data2 = vector("list", length=noElements) # Do not ask me why it is necessary to introduce data2, but otherwise R complains: Error in `[[<-.data.frame`(`*tmp*`, i, value = c(2008, 2008, 2008, 2008,  : replacement has 1645 rows, data has 1736                      
          for( iList in 1:noElements) {
            data2[[iList]] = data[[iVar]][[iList]][indexBegin:indexEnd]
#             data2[[i]] =  dat
#             data2[[iList]] = data2[[iList]] # [[iList]]            
          }
          names(data2) = c("year", "month", "day", "var", "flags")    
          data[[iVar]] = data2
        }

        # Average data to monthly scales
        noVars = length(data)
        for( iVar in 1:noVars  ) {
            indexFirst  = which(data[[iVar]]$day==1)            
            dataOut = vector("list", length=4)
            dataOut[[1]] = data[[iVar]][[1]][indexFirst] # Year
            dataOut[[2]] = data[[iVar]][[2]][indexFirst] # Month
#             dataOut[[3]] = data[[iVar]][[3]][indexFirst]
            dateFactor = factor(100*data[[iVar]]$year+data[[iVar]]$month) # Give every date (year and month combination) a unique number to be used as averaging factor in tapply
            dataOut[[3]] = tapply(data[[iVar]]$var,   dateFactor, mean, na.rm = FALSE) # If one value is missing, the mean is missing            
            dataOut[[4]] = tapply(data[[iVar]]$flags, dateFactor, min)  
            names(dataOut) = c("year", "month", "var", "flags")          
          
            # Save data and flags of one variable in one file.
            Level4DataDir = paste(settings$baseDir, "data", dataDirectory, "level4", sep=.Platform$file.sep)      
            fileName  = unlist(strsplit(dirFileNames[[iVar]], split=.Platform$file.sep))
            fileName  = fileName[length(fileName)]
            fileName = paste(substr(fileName,1,7), "m", substr(fileName, 9, nchar(fileName)), sep="") # The old filename was at a daily scale, this data is a a month ("m") scale
            save_standard_data(Level4DataDir, stationSubDirs[iStat], fileName, dataOut$var, dataOut$flags, list(dataOut$year, dataOut$month))         

            # Compute further indices, this could be limited to the parallel measurements
            # Compute monthly standard deviation
            dataOut[[3]] = tapply(data[[iVar]]$var,   dateFactor, sd, na.rm = FALSE) # If one value is missing, the mean is missing                 
            
            # Save data and flags of one variable in one file.
            Level4DataDir = paste(settings$baseDir, "data", dataDirectory, "level4", sep=.Platform$file.sep)      
            fileName  = unlist(strsplit(dirFileNames[[iVar]], split=.Platform$file.sep))
            fileName  = unlist(fileName[length(fileName)]) # Only use the filename , not the directories
            fileName  = unlist(strsplit(fileName, split=".", fixed = TRUE)) # Split in the three parts by the .
            fileName[2] = paste("std_", fileName[2], sep="")  # Add _std to the index
            fileName = paste(fileName, collapse=".") # Put filename back together            
            fileName = paste(substr(fileName,1,7), "m", substr(fileName, 9, nchar(fileName)), collapse="", sep="") # The old filename was at a daily scale, this data is a a month ("m") scale
            save_standard_data(Level4DataDir, stationSubDirs[iStat], fileName, dataOut$var, dataOut$flags, list(dataOut$year, dataOut$month))         

            rm(dataOut)        
        } # for iVar
      } # for iStat
    } # if there is data in level 3 directory
  } # for iSource, loop over all data sources
} # End of function



## Wastebin

