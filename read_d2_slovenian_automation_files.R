read_d2_slovenian_automation_files <-
function(dataDirectory, dataFile) {
  # This function read the data of Slovenian stations with three pairs of thermometers and 
  # automatic probes in the same Stevenson screen.
  # These pairs can be used to study the influence of automation without change of the screen.
  # And one pair that also has a relocation.
  #
  # Written by Victor Venema
  # First version 31 August 2014
  
  # Settings
  settings = read_software_settings()
  baseDirData = paste(settings$baseDir, "data", dataDirectory, "level1", sep=.Platform$file.sep)
  manualDataDirFileName = paste(baseDirData, "manual-measurements.txt",        sep=.Platform$file.sep)
  automaticDataDirFileName = vector(mode="character", length=4)
  automaticDataDirFileName[1] = paste(baseDirData, "iButton-measurements_ID38.txt",  sep=.Platform$file.sep)
  automaticDataDirFileName[2] = paste(baseDirData, "iButton-measurements_ID76.txt",  sep=.Platform$file.sep)
  automaticDataDirFileName[3] = paste(baseDirData, "iButton-measurements_ID301.txt", sep=.Platform$file.sep)
  automaticDataDirFileName[4] = paste(baseDirData, "iButton-measurements_ID348.txt", sep=.Platform$file.sep)
  
  # Read manual measurements, all statons are all in one file
  # For pressure, T, Tmin, Tmax and RR the missing data indicator is  -999
  # For the other variables it is -9 (or 9) and we have to send this later
  # File has 41 columns
  
  # The manual data has 5 station IDs, the manual station 348 was relocated are given a new number 786
  # In the automatic measurements stayed. Thus this pair represents both a change of sensor and location.
  # As 348 has only 1 month of data and is thus not used, manual station 786 thus corresponds to 
  # automatic station 348.
  # 38  Planina pod Golico
  # 76  Vojsko
  # 301  Slovenske Konjice
  # 348  Jeruzalem; this station only contains one month of manual data this part is not used further
  # 786  Ivanjkovci  
  manualMeasurements = read.csv(manualDataDirFileName, header=TRUE, sep = "\t", skip=94, na.strings="-999")
  # Set the missing data values marked with a -9 (or 9) to missing
  # ice-status  9, # RH	-9, # wind-dir	-9, # wind-speed	-9, # ground-state	-9, # visibility	-9, # sun-duration	-9,# CC	-9, # RR-type  -9, # total-snow, new-snow	-9, 
#   manualMeasurements$ice.status7 [manualMeasurements$ice.status7 ==9] = NA  
#   manualMeasurements$ice.status14[manualMeasurements$ice.status14==9] = NA  
#   manualMeasurements$ice.status21[manualMeasurements$ice.status21==9] = NA  
  manualMeasurements$ice.status7  = NULL # Complicated non-standard variable, not used
  manualMeasurements$ice.status14 = NULL 
  manualMeasurements$ice.status21 = NULL 
  manualMeasurements$RH7 [manualMeasurements$RH7 ==-9] = NA
  manualMeasurements$RH14[manualMeasurements$RH14==-9] = NA
  manualMeasurements$RH21[manualMeasurements$RH21==-9] = NA  
  manualMeasurements$wind.dir7 [manualMeasurements$wind.dir7 ==-9] = NA
  manualMeasurements$wind.dir14[manualMeasurements$wind.dir14==-9] = NA
  manualMeasurements$wind.dir21[manualMeasurements$wind.dir21==-9] = NA
  manualMeasurements$wind.speed7 [manualMeasurements$wind.speed7 <=-9] = NA
  manualMeasurements$wind.speed14[manualMeasurements$wind.speed14<=-9] = NA
  manualMeasurements$wind.speed21[manualMeasurements$wind.speed21<=-9] = NA
  manualMeasurements$ground.state7  = NULL # Complicated non-standard variable, not used
  manualMeasurements$ground.state14 = NULL 
  manualMeasurements$ground.state21 = NULL 
#   manualMeasurements$ground.state7 [manualMeasurements$ground.state7 ==-9] = NA
#   manualMeasurements$ground.state14[manualMeasurements$ground.state14==-9] = NA
#   manualMeasurements$ground.state21[manualMeasurements$ground.state21==-9] = NA  
  manualMeasurements$visibility7  = NULL # a code found in metadata
  manualMeasurements$visibility14 = NULL
  manualMeasurements$visibility21 = NULL
  manualMeasurements$pressure7  = NULL # station air pressure in 0,1 mm Hg, 1 bar = 750.06 mmHg, 1 mbar = 0.75006 mmHg
  manualMeasurements$pressure14 = NULL
  manualMeasurements$pressure21 = NULL  
  manualMeasurements$sun.duration = NULL # in 0.1h
  manualMeasurements$CC7 [manualMeasurements$CC7 ==-9] = NA
  manualMeasurements$CC14[manualMeasurements$CC14==-9] = NA
  manualMeasurements$CC21[manualMeasurements$CC21==-9] = NA  
  manualMeasurements$total.snow[manualMeasurements$total.snow ==-9] = NA # snow depth in cm
  manualMeasurements$new.snow  [manualMeasurements$new.snow ==-9] = NA   # snow depth in cm
#   manualMeasurements$RR_type[manualMeasurements$RR_type ==-9] = NA
  manualMeasurements$RR_type = NULL # Complicated non-standard variable, not used
  
  ## Convert data to standard units
  # -1 for RR and snow (fall) means no precipitation (completely dry day), whereas a zero may also mean too little precip to observe.
  # As this -1 may lead to mistakes in the analysis it is set to zero.
  manualMeasurements$RR        [manualMeasurements$RR         ==-1] = 0
  manualMeasurements$new.snow  [manualMeasurements$new.snow   ==-1] = 0
  manualMeasurements$total.snow[manualMeasurements$total.snow ==-1] = 0
  
  manualMeasurements$RR     = manualMeasurements$RR     / 10 # RR reported in 0.1 mm
  manualMeasurements$T7     = manualMeasurements$T7     / 10 # All temps reported in 0.1 °C
  manualMeasurements$T14    = manualMeasurements$T14    / 10
  manualMeasurements$T21    = manualMeasurements$T21    / 10    
  manualMeasurements$Tmax   = manualMeasurements$Tmax   / 10
  manualMeasurements$Tmin   = manualMeasurements$Tmin   / 10
  manualMeasurements$Twet7  = manualMeasurements$Twet7  / 10  
  manualMeasurements$Twet14 = manualMeasurements$Twet14 / 10  
  manualMeasurements$Twet21 = manualMeasurements$Twet21 / 10  
  manualMeasurements$CC7    = manualMeasurements$CC7    * 10  # Convert from 1/10 to percent
  manualMeasurements$CC14   = manualMeasurements$CC14   * 10  
  manualMeasurements$CC21   = manualMeasurements$CC21   * 10  
  manualMeasurements$total.snow   = manualMeasurements$total.snow   * 10 # converted to snow depth in mm
  manualMeasurements$new.snow     = manualMeasurements$new.snow     * 10
  manualMeasurements$wind.speed7  = manualMeasurements$wind.speed7  / 10 # convert wind speed from 0,1 m/s (usually calculated from Beaufort-scale estimation)
  manualMeasurements$wind.speed14 = manualMeasurements$wind.speed14 / 10 # to meter per second
  manualMeasurements$wind.speed21 = manualMeasurements$wind.speed21 / 10
  # Convert wind-direction categories to degrees (calms with no wind direction are set to -1)
  manualMeasurements$wind.dir7  = convert_winddir(manualMeasurements$wind.dir7) # see function below
  manualMeasurements$wind.dir14 = convert_winddir(manualMeasurements$wind.dir14)
  manualMeasurements$wind.dir21 = convert_winddir(manualMeasurements$wind.dir21)
  
  # Split the data into the four stations
  ID = c(38, 76, 301, 786)
  manual=vector("list", 4)
  for(i in 1:4) {
    manual[[i]]$year  = manualMeasurements$year [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$month = manualMeasurements$month[manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$day   = manualMeasurements$day  [manualMeasurements$station.ID == ID[i]]    
    
    manual[[i]]$CC7            = manualMeasurements$CC7           [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$CC14           = manualMeasurements$CC14          [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$CC21           = manualMeasurements$CC21          [manualMeasurements$station.ID == ID[i]]     
    manual[[i]]$ground.state7  = manualMeasurements$ground.state7 [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$ground.state14 = manualMeasurements$ground.state14[manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$ground.state21 = manualMeasurements$ground.state21[manualMeasurements$station.ID == ID[i]]      
    manual[[i]]$ice.status7    = manualMeasurements$ice.status7   [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$ice.status14   = manualMeasurements$ice.status14  [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$ice.status21   = manualMeasurements$ice.status21  [manualMeasurements$station.ID == ID[i]]
    manual[[i]]$pressure7      = manualMeasurements$pressure7     [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$pressure14     = manualMeasurements$pressure14    [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$pressure21     = manualMeasurements$pressure21    [manualMeasurements$station.ID == ID[i]]          
    manual[[i]]$RH7            = manualMeasurements$RH7           [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$RH14           = manualMeasurements$RH14          [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$RH21           = manualMeasurements$RH21          [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$RR             = manualMeasurements$RR            [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$RR_type        = manualMeasurements$RR_type       [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$station.ID     = manualMeasurements$station.ID    [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$T7             = manualMeasurements$T7            [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$T14            = manualMeasurements$T14           [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$T21            = manualMeasurements$T21           [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$Tmax           = manualMeasurements$Tmax          [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$Tmin           = manualMeasurements$Tmin          [manualMeasurements$station.ID == ID[i]]  
    manual[[i]]$new.snow       = manualMeasurements$new.snow      [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$total.snow     = manualMeasurements$total.snow    [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$Twet7          = manualMeasurements$Twet7         [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$Twet14         = manualMeasurements$Twet14        [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$Twet21         = manualMeasurements$Twet21        [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$wind.dir7      = manualMeasurements$wind.dir7     [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$wind.dir14     = manualMeasurements$wind.dir14    [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$wind.dir21     = manualMeasurements$wind.dir21    [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$wind.speed7    = manualMeasurements$wind.speed7   [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$wind.speed14   = manualMeasurements$wind.speed14  [manualMeasurements$station.ID == ID[i]] 
    manual[[i]]$wind.speed21   = manualMeasurements$wind.speed21  [manualMeasurements$station.ID == ID[i]] 
  }
  
  # TODO
  # - DONE, Convert wind direction to degree
  # - DONE, Convert cloud cover to a standard (fraction, percent?)
  # - DONE, Plausibility check of the variables with -999 as missing value
  
  # Read automatic measurements, one station per file
  # Missing values are simply missing in these files
  # R cannot handle the header line somehow, thus also skipped
  # All automatic data files have a different length
  auto=vector("list", 4)
  for( i in 1:4  ) {    
    automaticMeasurements  = read.csv(automaticDataDirFileName[i], header=FALSE, sep = "\t", skip=13)
    dateStat =as.numeric(unlist(strsplit(as.character(automaticMeasurements[,1]), "[.]")))
    dim(dateStat) =c(3,length(dateStat )/3) # reshape vector to matrix. date[,1] is first date, date[1,] the days of the month
    auto[[i]]$year  = dateStat[3,]
    auto[[i]]$month = dateStat[2,]
    auto[[i]]$day   = dateStat[1,]
    rm(dateStat)
    timeStat =as.numeric(unlist(strsplit(as.character(automaticMeasurements [,2]), "[:]")))
    dim(timeStat) =c(2,length(timeStat )/2) # reshape vector to matrix. time[,1] is first time, time[1,] the hour of the reading
    auto[[i]]$hour    = timeStat[1,]
    auto[[i]]$minutes = timeStat[2,]
    rm(timeStat)
    auto[[i]]$temp = as.numeric(automaticMeasurements[,3])
  }
  
  list(manual, auto)
}

convert_winddir = function(winddir) {
#   0   calm  NaN (no NA, which means missing)
#   2	  NNE
#   4	  NE
#   6	  ENE
#   8	  E     90
#   10	ESE
#   12	SE
#   14	SSE
#   16	S     180
#   18	SSW
#   20	SW
#   22	WSW
#   24	W     270
#   26	WNW
#   28	NW
#   30	NNW
#   32	N     360
    
  winddir = 180 * winddir / 16 
  winddir[winddir==0] = -1
  
  winddir
}
