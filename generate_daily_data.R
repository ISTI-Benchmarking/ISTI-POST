generate_daily_data <- function() {
  # This script generates daily data files for the level 3 directories 
  # from the standard format daily and sub-daily files in the level 2 directories.
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
    Level2DataDir = paste(settings$baseDir, "data", dataDirectory, "level2", sep=.Platform$file.sep)
    Level3DataDir = paste(settings$baseDir, "data", dataDirectory, "level3", sep=.Platform$file.sep)     
    stationSubDirs = dir(Level2DataDir, no..=TRUE)
    stationSubDirsFull = dir(Level2DataDir, full.names=TRUE, no..=TRUE)
    noStations = length(stationSubDirs)
    # if there are stations directory in the level 2 directory of the source iSource loop over them
    if( noStations > 0 ) {
      for(iStat in 1:noStations) {
        # read al data in the subdirectory into the lists for every parallel measurement and one for the co-variates
                    # read_standard_data
        tmp = read_standard_data_directory(stationSubDirsFull[iStat])
        data = tmp[[1]]
        dirFileNames = tmp[[2]]
        rm(tmp)
        
        # Determine kind of data (and thus conversion function needed)        
        noVars = length(data)
        averagingMethod = vector("character", length=noVars)
        for( iVar in 1:noVars  ) {              
          if( length(data[[iVar]]) == 5 ) averagingMethod[iVar] = "copy"
          if( length(data[[iVar]]) == 6 ) averagingMethod[iVar] = "h1h1h2"
          if( length(data[[iVar]]) == 7 ) averagingMethod[iVar] = "sub-daily"
        }
          
        # Convert data to daily scales (if necessary)
        for( iVar in 1:noVars  ) {
          converted = 0
          if( averagingMethod[iVar] == "copy") {    
            dataOut = data[[iVar]]
            converted = 1
          }
          if( averagingMethod[iVar] == "h1h1h2") {
            if( names(data)[iVar] == "dd_read" ) {
              dataOut = data[[iVar]]
              dataOut$var = NA # If wind direction, do not average. File with only NA are not saved.
            } else {     
              index6  = which(data[[iVar]]$hour==6)
              index13 = which(data[[iVar]]$hour==13)
              index20 = which(data[[iVar]]$hour==20) # The evening value get a double weight, is used twice in next line
              var2D = cbind(data[[iVar]]$var[index6], data[[iVar]]$var[index13], data[[iVar]]$var[index20], data[[iVar]]$var[index20])
              dailyMean = rowMeans(var2D) 
              
              noRowsHourly = length(data[[iVar]]$var)  
              dataOut = vector("list", length=5)
              dataOut[[1]] = data[[iVar]][[1]][seq.int(1, noRowsHourly, 3)]
              dataOut[[2]] = data[[iVar]][[2]][seq.int(1, noRowsHourly, 3)]
              dataOut[[3]] = data[[iVar]][[3]][seq.int(1, noRowsHourly, 3)]
              dataOut[[4]] = dailyMean
              # The flag of the daily mean is given the lowest value of the hourly values, might need some more thought (sum all unique flags?)
              dateFactor = factor(10000*data[[iVar]]$year+100*data[[iVar]]$month+data[[iVar]]$day) # Give every date a unique number to be used as averaging factor in tapply
#               dateFactor = factor(10000*data[[iVar]]$year+100*data[[iVar]]$month+data[[iVar]]$day) # Give every date a unique number to be used as averaging factor in tapply
              dataOut[[5]] = tapply(data[[iVar]]$flags, dateFactor, min)
              names(dataOut) = c("year", "month", "day", "var", "flags")
            }
            converted = 1
          }
          if( averagingMethod[iVar] == "sub-daily") {
            indexFirst  = which(data[[iVar]]$hour==0 & data[[iVar]]$min==0)            
            dataOut = vector("list", length=5)
            dataOut[[1]] = data[[iVar]][[1]][indexFirst]
            dataOut[[2]] = data[[iVar]][[2]][indexFirst]
            dataOut[[3]] = data[[iVar]][[3]][indexFirst]
            dateFactor = factor(10000*data[[iVar]]$year+100*data[[iVar]]$month+data[[iVar]]$day) # Give every date a unique number to be used as averaging factor in tapply
            dataOut[[4]] = tapply(data[[iVar]]$var,   dateFactor, mean, na.rm = FALSE) # If one value is missing, the mean is missing            
            dataOut[[5]] = tapply(data[[iVar]]$flags, dateFactor, min)  
            names(dataOut) = c("year", "month", "day", "var", "flags")          
            converted = 1
            # TODO: change index name from read to mean.
          }
          if( converted == 0 ) {
            stop("No conversion to daily data performed.")
          }
          
          # Save data and flags of one variable in one file.
          Level3DataDir = paste(settings$baseDir, "data", dataDirectory, "level3", sep=.Platform$file.sep)      
          fileNameList  = unlist(strsplit(dirFileNames[[iVar]], split=.Platform$file.sep))
          fileNameList  = fileNameList[length(fileNameList)]
          fileName = paste(substr(fileName,1,7), "d", substr(fileName, 9, nchar(fileName)), sep="")  # The old filename may have been at a sub-daily scale, this data is on a daily ("d") scale
          save_standard_data(Level3DataDir, stationSubDirs[iStat], fileNameList, dataOut$var, dataOut$flags, list(dataOut$year, dataOut$month, dataOut$day))         
          rm(dataOut)
        } # for iVar
      } # for iStat
    } # if there is data in level 2 directory
  } # for iSource, loop over all data sources
} # End of function



## Wastebin


#                   var2D = array(c(4, length(index6)))
#                   var2D[1,] = data[[iVar]]$var[index6]
#                   var2D[2,] = data[[iVar]]$var[index13]
#                   var2D[3,] = data[[iVar]]$var[index20]
#                   var2D[4,] = data[[iVar]]$var[index20]

#                   dailyMean = colMeans(c(data[[iVar]]$var[index6], data[[iVar]]$var[index13], data[[iVar]]$var[index20], data[[iVar]]$var[index20]))


#                   temp = data[[iVar]]$var
#                   dim(temp) = c(3, length(temp)/3)                
#                   dailyMeans = colMeans(temp)


#                   dataOut[[5]] = data[[iVar]][[6]][seq.int(1, noRowsHourly, 3)]
#                   noRows = length(dailyMean)                


# for( iCol in c(1:3,6) ) { # Copy the date from the first measurement of the day
#   dataOut[[iCol]] = data[[iVar]][[iCol]][seq.int(1, noRowsHourly, 3)]
# }


#                   monthlyMeanStevenson[,i] = tapply(stevensonT[,i], monthf, mean, na.rm = TRUE)



