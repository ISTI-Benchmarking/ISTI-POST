plot_monthly_data <- function() {

  # Written by Victor Venema
  # First version 11 Sept 2014
  
  # Read the name of the conversion functions to be used from the settings 
  settings = read_software_settings()
  conversion = settings$conversion  
  
  # Loop over all data sources
  noSources = length(conversion$dirName)
  for( iSource in 2) {      
    dataDirectory = as.character(conversion$dirName[iSource])      # Name of directory with native data
    Level4DataDir = paste(settings$baseDir, "data", dataDirectory, "level4", sep=.Platform$file.sep)
    stationSubDirs = dir(Level4DataDir, no..=TRUE)
    stationSubDirsFull = dir(Level4DataDir, full.names=TRUE, no..=TRUE)
    noStations = length(stationSubDirs)
    # if there are stations directory in the level 3 directory of the source iSource loop over them
    if( noStations > 0 ) {
      fileNameStdP1 = c("pratr01m001400038.std_read1.txt", "pratr01m00140076.std_read1.txt", "pratr01m00140301.std_read1.txt")
      fileNameStdP2 = c("pratr02m001400038.std_read1.txt", "pratr02m00140076.std_read1.txt", "pratr02m00140301.std_read1.txt")
      fileNameMeanP1 = c("pratr01m001400038.read1.txt", "pratr01m00140076.read1.txt", "pratr01m00140301.read1.txt")
      fileNameMeanP2 = c("pratr02m001400038.read1.txt", "pratr02m00140076.read1.txt", "pratr02m00140301.read1.txt")
      seasonalCycleStdP1   = array(0, c(3, 12))
      seasonalCycleStdP2R1 = array(0, c(3, 12))
      seasonalCycleMeanP1     = array(0, c(3, 12))
      seasonalCycleMeanP2R1   = array(0, c(3, 12))
      meanTemp = array(NA, c(2,3,100))
      stdTemp = array(NA, c(2,3,100))
      for(iStat in 1:3) {
        # read al data in the subdirectory into the lists for every parallel measurement and one for the co-variates
        # read_standard_data
        dirFileNameP1   = paste(stationSubDirsFull[iStat], fileNameMeanP1[iStat], sep=.Platform$file.sep)
        dirFileNameP2R1 = paste(stationSubDirsFull[iStat], fileNameMeanP2[iStat], sep=.Platform$file.sep)
#         dirFileNameP1   = paste(stationSubDirsFull[iStat], "pratr01m001400038.read1.txt", sep=.Platform$file.sep)
#         dirFileNameP1R1 = paste(stationSubDirsFull[iStat], "pratr02m001400038.read1.txt", sep=.Platform$file.sep)
                
#         dirFileNameP1R1 = paste(stationSubDirsFull[iStat], "pratr02m001400038.read2.txt", sep=.Platform$file.sep)
        meanP1 = read_standard_data_file(dirFileNameP1)
        meanP2R1 = read_standard_data_file(dirFileNameP2R1)
#         meanP1R2 = read_standard_data_file(dirFileNameP1R1)
        meanTemp[1, iStat, 1:length(meanP1$var)] = meanP1$var
        meanTemp[2, iStat, 1:length(meanP2R1$var)] = meanP2R1$var

        seasonalCycleMeanP1[iStat,]   = tapply(meanP1$var,     meanP1$month, mean, na.rm = TRUE) # If one value is missing, the mean is missing            
        seasonalCycleMeanP2R1[iStat,] = tapply(meanP2R1$var,   meanP2R1$month, mean, na.rm = TRUE) # If one value is missing, the mean is missing            
#         seasonalCycleP2R2 = tapply(meanP1R2$var,   meanP1R2$month, mean, na.rm = TRUE) # If one value is missing, the mean is missing            
        
        X11()
        plot(seasonalCycleMeanP1[iStat,], type="l", col="black", lwd="6", xlab="Month", ylab="Mean temperature °C") 
        lines(seasonalCycleMeanP2R1[iStat,], col="red", lwd="4")
#         lines(seasonalCycleP2R2, col="red", lwd="2", lty="longdash")
        title(main="Seasonal cycle mean -Slovenia") 
        legend("topleft", inset=.05, legend=c("Manual", "Automatic"), fill=c("black", "red"))
#         legend("topleft", inset=.05, legend=c("Manual", "Automatic full day", "Automatic 3h"), fill=c("black", "blue", "red"))
#         dirFileName = paste(stationSubDirsFull, "seasonal_cycle_mean_temp.png", sep="/")
#         savePlot(filename = dirFileName)
        dev.off()
        
#         dirFileNameP1   = paste(stationSubDirsFull[iStat], "pratr01m001400038.std_read1.txt", sep=.Platform$file.sep)
#         dirFileNameP1R1 = paste(stationSubDirsFull[iStat], "pratr02m001400038.std_read1.txt", sep=.Platform$file.sep)
        dirFileNameP1   = paste(stationSubDirsFull[iStat], fileNameStdP1[iStat], sep=.Platform$file.sep)
        dirFileNameP2R1 = paste(stationSubDirsFull[iStat], fileNameStdP2[iStat], sep=.Platform$file.sep)

#         dirFileNameP1R1 = paste(stationSubDirsFull[iStat], "pratr02m001400038.std_read2.txt", sep=.Platform$file.sep)
        stdP1 = read_standard_data_file(dirFileNameP1)
        stdP2R1 = read_standard_data_file(dirFileNameP2R1)
#         stdP1R2 = read_standard_data_file(dirFileNameP1R1)
        stdTemp[1, iStat, 1:length(stdP1$var)] = stdP1$var
        stdTemp[2, iStat, 1:length(stdP2R1$var)] = stdP2R1$var
        
        seasonalCycleStdP1[iStat,]   = tapply(stdP1$var,     stdP1$month, mean, na.rm = TRUE) # If one value is missing, the mean is missing            
        seasonalCycleStdP2R1[iStat,] = tapply(stdP2R1$var,   stdP2R1$month, mean, na.rm = TRUE) # If one value is missing, the mean is missing            
#         stdSeasonalCycleP2R2 = tapply(stdP1R2$var,   stdP1R2$month, mean, na.rm = TRUE) # If one value is missing, the mean is missing            

        X11()
        plot(seasonalCycleStdP1[iStat,], type="l", col="black", lwd="6", xlab="Month", ylab="Mean temperature °C") 
        lines(seasonalCycleStdP2R1[iStat,], col="red", lwd="4")
#         lines(stdSeasonalCycleP2R2, col="red", lwd="2", lty="longdash")
        title(main="Seasonal cycle standard deviation - Slovenia") 
        legend("top", inset=.05, legend=c("Manual", "Automatic"), fill=c("black", "red"))
#         dirFileName = paste(stationSubDirsFull, "seasonal_cycle_std_temp.png", sep="/")
#         savePlot(filename = dirFileName)
        dev.off()        
        
        a=0
      }
    }

    X11()
    plot(meanTemp[1,,], meanTemp[2,,], xlab="Mean temp conv", ylab="Mean temp automatic", cex.axis = 1.5, cex.lab = 1.5)
    title("Scatterplot monthly means, conv vs automatic")
    lines(c(-10,30), c(-10,30), col="red")
    dirFileName = paste(stationSubDirsFull[1], "scatterplot_monthly_mean_temp.png", sep="/")
    savePlot(filename = dirFileName)
    dev.off()

    X11()    
    plot(stdTemp[1,,], stdTemp[2,,], xlab="Std temp conv", ylab="Std temp automatic", cex.axis = 1.5, cex.lab = 1.5)
    title("Scatterplot monthly standard deviation, conv vs automatic")
    lines(c(-10,30), c(-10,30), col="red")
    dirFileName = paste(stationSubDirsFull[1], "scatterplot_monthly_std_temp.png", sep="/")
    savePlot(filename = dirFileName)
    dev.off()


    seasonalCycleMeanP1   = colMeans(seasonalCycleMeanP1)
    seasonalCycleMeanP2R1 = colMeans(seasonalCycleMeanP2R1)
    X11()
    plot(seasonalCycleMeanP1, type="l", col="black", lwd="6", xlab="Month", ylab="Mean temperature °C", cex.axis = 1.5, cex.lab = 1.5) 
    lines(seasonalCycleMeanP2R1, col="red", lwd="4")
    title(main="Seasonal cycle mean, Slovenia") 
    legend("topleft", inset=.05, legend=c("Manual", "Automatic"), fill=c("black", "red"))
    dirFileName = paste(stationSubDirsFull[1], "seasonal_cycle_mean_temp.png", sep="/")
    savePlot(filename = dirFileName)
    dev.off()
  
    X11()
    plot(seasonalCycleMeanP1-seasonalCycleMeanP2R1, type="l", col="black", lwd="6", xlab="Month", ylab="Mean temperature °C", cex.axis = 1.5, cex.lab = 1.5)     
    title(main="Difference Conv - AWS, seasonal cycle mean, Slovenia")     
    dirFileName = paste(stationSubDirsFull[1], "seasonal_cycle_diff_mean_temp.png", sep="/")
    savePlot(filename = dirFileName)
    dev.off()
  
    seasonalCycleStdP1   = colMeans(seasonalCycleStdP1)
    seasonalCycleStdP2R1 = colMeans(seasonalCycleStdP2R1)
    X11()
    plot(seasonalCycleStdP1, type="l", col="black", lwd="6", xlab="Month", ylab="standard deviation temperature °C", cex.axis = 1.5, cex.lab = 1.5) 
    lines(seasonalCycleStdP2R1, col="red", lwd="4")
    title(main="Seasonal cycle standard deviation, Slovenia") 
    legend("top", inset=.05, legend=c("Manual", "Automatic"), fill=c("black", "red"))
    dirFileName = paste(stationSubDirsFull[1], "seasonal_cycle_std_temp.png", sep="/")
    savePlot(filename = dirFileName)
    dev.off()    

    X11()
    plot(seasonalCycleStdP1-seasonalCycleStdP2R1, type="l", col="black", lwd="6", xlab="Month", ylab="Standard deviation temperature °C", cex.axis = 1.5, cex.lab = 1.5)     
    title(main="Difference Conv - AWS, seasonal cycle standard deviation, Slovenia")     
    dirFileName = paste(stationSubDirsFull[1], "seasonal_cycle_diff_std_temp.png", sep="/")
    savePlot(filename = dirFileName)
    dev.off()
  }

}



        
#         print(stationSubDirsFull[iStat])
#         tmp = read_standard_data_directory(stationSubDirsFull[iStat])
#         data = tmp[[1]]
#         dirFileNames = tmp[[2]]
#         rm(tmp)
#         
#         # Force the measurement to start at the beginning of the month (day=1) and end at the end of the month                
#         noVars = length(data)
#         for( iVar in 1:noVars  ) {
#           # Compute annual cycle
#           seasonalCycle = tapply(data[[iVar]]$var,   data[[iVar]]$month, mean, na.rm = TRUE) # If one value is missing, the mean is missing            
#           plot(dataOut)          
#           
#           
#           indexFirst  = which(data[[iVar]]$day==1)            
#           dataOut = vector("list", length=4)
#           dataOut[[1]] = data[[iVar]][[1]][indexFirst] # Year
#           dataOut[[2]] = data[[iVar]][[2]][indexFirst] # Month
#           #             dataOut[[3]] = data[[iVar]][[3]][indexFirst]
#           dateFactor = factor(100*data[[iVar]]$year+data[[iVar]]$month) # Give every date (year and month combination) a unique number to be used as averaging factor in tapply
#           dataOut[[3]] = tapply(data[[iVar]]$var,   dateFactor, mean, na.rm = FALSE) # If one value is missing, the mean is missing            
#           dataOut[[4]] = tapply(data[[iVar]]$flags, dateFactor, min)  
#           names(dataOut) = c("year", "month", "var", "flags")          
#           
#           # Save data and flags of one variable in one file.
#           Level4DataDir = paste(settings$baseDir, "data", dataDirectory, "level4", sep=.Platform$file.sep)      
#           fileName  = unlist(strsplit(dirFileNames[[iVar]], split=.Platform$file.sep))
#           fileName  = fileName[length(fileName)]
#           fileName = paste(substr(fileName,1,7), "m", substr(fileName, 9, nchar(fileName)), sep="") # The old filename was at a daily scale, this data is a a month ("m") scale
#           save_standard_data(Level4DataDir, stationSubDirs[iStat], fileName, dataOut$var, dataOut$flags, list(dataOut$year, dataOut$month))         
#           
#           # Compute further indices, this could be limited to the parallel measurements
#           # Compute monthly standard deviation
#           dataOut[[3]] = tapply(data[[iVar]]$var,   dateFactor, sd, na.rm = FALSE) # If one value is missing, the mean is missing                 
#           
#           # Save data and flags of one variable in one file.
#           Level4DataDir = paste(settings$baseDir, "data", dataDirectory, "level4", sep=.Platform$file.sep)      
#           fileName  = unlist(strsplit(dirFileNames[[iVar]], split=.Platform$file.sep))
#           fileName  = unlist(fileName[length(fileName)]) # Only use the filename , not the directories
#           fileName  = unlist(strsplit(fileName, split=".", fixed = TRUE)) # Split in the three parts by the .
#           fileName[2] = paste(fileName[2], "_", "std", sep="")  # Add _std to the index
#           fileName = paste(fileName, collapse=".") # Put filename back together            
#           fileName = paste(substr(fileName,1,7), "m", substr(fileName, 9, nchar(fileName)), collapse="", sep="") # The old filename was at a daily scale, this data is a a month ("m") scale
#           save_standard_data(Level4DataDir, stationSubDirs[iStat], fileName, dataOut$var, dataOut$flags, list(dataOut$year, dataOut$month))         
# #           
# #           rm(dataOut)        
#         } # for iVar
#       } # for iStat
#     } # if there is data in level 3 directory
#   } # for iSource, loop over all data sources
# } # End of function



## Wastebin
