convert_d2_slovenian_automation_files = function(dataDirectory, dataFile) {
  # This function reads the level1 data of Slovenian stations and converts it to level2 standard format data. 
  # This dataset contains 3 pairs of thermometers and automatic probes in the same Stevenson screen.
  # The last, fourth) pair also includes a different location.
  # These pairs can be used to study the influence of automation without change of the screen.
  #
  # Written by Victor Venema
  # First version 01 September 2014
  
  # Read the data in its native format
  dataVals = read_d2_slovenian_automation_files(dataDirectory, dataFile)
  manual=dataVals[[1]] # Manual observation at 7, 14 and 21 h
  auto = dataVals[[2]] # Automatic probe with 15-minute data

  # Plot data as first check
  if( FALSE ) {
    for( j in 1:4 ) {
      for( i in 1:length(auto[[j]]) ) {
        plot(auto[[j]][[i]])
      }
    }
    for( j in 1:4 ) {
      for( i in 1:length(manual[[j]]) ) {
        plot(manual[[j]][[i]])
      }
    }
    # Looks like there is a break in relative humidty after about 3 years in the (manual) second and fourth station
  }

  # Force the automatic measurement to start at the beginning of the day (00:00) and end at the end of the day (23:45)
  for( iStat in 1:4 ) {
    noVar = length(auto[[iStat]])
    indexBegin = which.max(auto[[iStat]]$hour == 0 & auto[[iStat]]$minutes == 0)
    # Is there an equivalent to which.max for the last which that fits the criterion, rather than the first?
    indexEnd   = which(auto[[iStat]]$hour == 23 & auto[[iStat]]$minutes == 45)
    indexEnd   = indexEnd[length(indexEnd)]
    for( i in 1:noVar ) {
      auto[[iStat]][[i]] =  auto[[iStat]][[i]][indexBegin:indexEnd]
    }
  }

  ## Derive fixed hour measurements at 7, 14, and 21 h from the automatic full times ones (for comparison with manual measurements at this time)
  auto3 = vector( mode="list", length=4) # Make new list for the automatic measurements at the time of the manual ones  
  for( iStat in 1:4 ) {
    index7  = auto[[iStat]]$hour == 7  & auto[[iStat]]$minutes == 0
    index14 = auto[[iStat]]$hour == 14 & auto[[iStat]]$minutes == 0
    index21 = auto[[iStat]]$hour == 21 & auto[[iStat]]$minutes == 0

    tmp7  = auto[[iStat]]$temp[index7] # tmp means temporary, in contrast to temp, which means temperature
    tmp14 = auto[[iStat]]$temp[index14]
    tmp21 = auto[[iStat]]$temp[index21]
    auto3[[iStat]]$temp  = combine_fixed_hour_measurements(list(tmp7, tmp14, tmp21)) 
    
    tmp7  = auto[[iStat]]$year[index7]
    tmp14 = auto[[iStat]]$year[index14]
    tmp21 = auto[[iStat]]$year[index21]
    auto3[[iStat]]$year  = combine_fixed_hour_measurements(list(tmp7, tmp14, tmp21)) 
    
    tmp7  = auto[[iStat]]$month[index7]
    tmp14 = auto[[iStat]]$month[index14]
    tmp21 = auto[[iStat]]$month[index21]
    auto3[[iStat]]$month = combine_fixed_hour_measurements(list(tmp7, tmp14, tmp21)) 
    
    tmp7  = auto[[iStat]]$day[index7]
    tmp14 = auto[[iStat]]$day[index14]
    tmp21 = auto[[iStat]]$day[index21]
    auto3[[iStat]]$day   = combine_fixed_hour_measurements(list(tmp7, tmp14, tmp21)) 
    
    tmp7  = auto[[iStat]]$hour[index7]
    tmp14 = auto[[iStat]]$hour[index14]
    tmp21 = auto[[iStat]]$hour[index21]
    auto3[[iStat]]$hour  = combine_fixed_hour_measurements(list(tmp7, tmp14, tmp21))     
  }
  rm(tmp7, tmp14, tmp21, index7, index14, index21)

  ## Convert time zone from CET to UTC  
  # For the 15-minute measurement this can also change the day, month or year, thus the use of the full date is necessary
  # Is there a more elegant way to do this? POSIXct does not know $year, $month, etc., thus a print converted back to number was necessary.
  for( iStat in 1:4) {
    dateTime = ISOdatetime(auto[[iStat]]$year, auto[[iStat]]$month, auto[[iStat]]$day, auto[[iStat]]$hour, auto[[iStat]]$minutes, 0, tz="GMT")
    dateTime2 = dateTime - 3600 # Subtract 1 hour (3600 s) to go from CET to UTC, cannot use build in functions for time zone conversion because they see CET as having summer time (CEST) and GMT-1 does not work on some Linux systems.    
    auto[[iStat]]$year    = as.numeric(strftime(dateTime2, format="%Y")) # Year with century.
    auto[[iStat]]$month   = as.numeric(strftime(dateTime2, format="%m")) # Month as decimal number (01–12).
    auto[[iStat]]$day     = as.numeric(strftime(dateTime2, format="%e")) # Day of the month as decimal number (1–31), with a leading space for a single-digit number.
    auto[[iStat]]$hour    = as.numeric(strftime(dateTime2, format="%H")) # Hours as decimal number (00–23). As a special exception strings such as 24:00:00 are accepted for input, since ISO 8601 allows these.
    auto[[iStat]]$minutes = as.numeric(strftime(dateTime2, format="%M")) # Minute as decimal number (00–59).
#     Next comment lines are for reference.    
#     auto[[iStat]]$sec     = as.numeric(strftime(dateTime2, format="%S")) # Second as decimal number (00–61)
#     auto[[iStat]]$julian  = as.numeric(strftime(dateTime2, format="%j")) # Julian day of the year    
    
    # For the fixed hour measurements, the hour just shift one down. The day, month and year stay the same.
    auto3[[iStat]]$hour = auto3[[iStat]]$hour - 1
  }

  ## remove parts where there is only one measurement at a station
  for( iStat in 1:4 ) {
    # First remove the begin part and end part without temperature data
    # For the automatic probe
    indexBegin = which.max( !is.na(auto[[iStat]]$temp) )
    indexEnd   = which    ( !is.na(auto[[iStat]]$temp) )
    indexEnd   = indexEnd[length(indexEnd)]    
    noVarAuto = length(auto[[iStat]])
    for( i in 1:noVarAuto ) {
      auto[[iStat]][[i]] =  auto[[iStat]][[i]][indexBegin:indexEnd]
    }
    indexBegin = which.max( !is.na(auto3[[iStat]]$temp) )
    indexEnd   = which    ( !is.na(auto3[[iStat]]$temp) )
    indexEnd   = indexEnd[length(indexEnd)]    
    noVarAuto3 = length(auto3[[iStat]])
    for( i in 1:noVarAuto3 ) {
      auto3[[iStat]][[i]] =  auto3[[iStat]][[i]][indexBegin:indexEnd]
    }    
    # For the manual measurements
    indexBegin = which.max( !(is.na(manual[[iStat]]$T7) & is.na(manual[[iStat]]$T14) & is.na(manual[[iStat]]$T21)) )
    indexEnd   = which    ( !(is.na(manual[[iStat]]$T7) & is.na(manual[[iStat]]$T14) & is.na(manual[[iStat]]$T21)) )
    indexEnd   = indexEnd[length(indexEnd)]    
    noVarManual = length(manual[[iStat]])
    for( i in 1:noVarManual ) {
      manual[[iStat]][[i]] =  manual[[iStat]][[i]][indexBegin:indexEnd]
    }    
    # Only use full days, if not the case remove the partial days at the beginning and end
    # Force the automatic measurement to start at the beginning of the day (00:00) and end at the end of the day (23:45)    
    # For the manual data this is always the case
    indexBegin = which.max( auto[[iStat]]$hour == 0 & auto[[iStat]]$minutes == 0 )
    # Is there an equivalent to which.max for the last which that fits the criterion, rather than the first?
    indexEnd   = which( auto[[iStat]]$hour == 23 & auto[[iStat]]$minutes == 45 )
    indexEnd   = indexEnd[length(indexEnd)]
    for( i in 1:noVarAuto ) {
      auto[[iStat]][[i]] =  auto[[iStat]][[i]][indexBegin:indexEnd]
    }     
    indexBegin = which.max( auto3[[iStat]]$hour == 6  )
    indexEnd   = which(     auto3[[iStat]]$hour == 20 )
    indexEnd   = indexEnd[length(indexEnd)]
    for( i in 1:noVarAuto3 ) {
      auto3[[iStat]][[i]] =  auto3[[iStat]][[i]][indexBegin:indexEnd]
    }         
    # Now remove parts of the data where only one temperature measurement takes place
    beginDateAuto   = ISOdate( auto[[iStat]]$year[1],   auto[[iStat]]$month[1],   auto[[iStat]]$day[1],   tz="GMT" ) 
    beginDateAuto3  = ISOdate( auto3[[iStat]]$year[1],  auto3[[iStat]]$month[1],  auto3[[iStat]]$day[1],  tz="GMT" )     
    beginDateManual = ISOdate( manual[[iStat]]$year[1], manual[[iStat]]$month[1], manual[[iStat]]$day[1], tz="GMT" ) 
    beginDateParallel = max(c(beginDateAuto, beginDateAuto3, beginDateManual))

    noValAuto   = length(auto[[iStat]]$year)
    noValAuto3  = length(auto3[[iStat]]$year)
    noValManual = length(manual[[iStat]]$year)
    endDateAuto   = ISOdate( auto  [[iStat]]$year[noValAuto],   auto  [[iStat]]$month[noValAuto],   auto  [[iStat]]$day[noValAuto],   tz="GMT" )     
    endDateAuto3  = ISOdate( auto3 [[iStat]]$year[noValAuto3],  auto3 [[iStat]]$month[noValAuto3],  auto3 [[iStat]]$day[noValAuto3],  tz="GMT" )     
    endDateManual = ISOdate( manual[[iStat]]$year[noValManual], manual[[iStat]]$month[noValManual], manual[[iStat]]$day[noValManual], tz="GMT" ) 
    endDateParallel = min(c(endDateAuto, endDateAuto3, endDateManual)) # Returns time in local time, carefulll if you life close to date line.

    beginYearParallel  = as.numeric(strftime(beginDateParallel, format="%Y")) # Year with century.
    beginMonthParallel = as.numeric(strftime(beginDateParallel, format="%m")) # Month as decimal number (01–12).
    beginDayParallel   = as.numeric(strftime(beginDateParallel, format="%e")) # Day of the month as decimal number (1–31), with a leading space for a single-digit number.
    endYearParallel    = as.numeric(strftime(endDateParallel,   format="%Y"))
    endMonthParallel   = as.numeric(strftime(endDateParallel,   format="%m"))
    endDayParallel     = as.numeric(strftime(endDateParallel,   format="%e"))

    indexBegin = which.max( auto[[iStat]]$year == beginYearParallel & auto[[iStat]]$month == beginMonthParallel & auto[[iStat]]$day == beginDayParallel)
    indexEnd   = which    ( auto[[iStat]]$year == endYearParallel   & auto[[iStat]]$month == endMonthParallel   & auto[[iStat]]$day == endDayParallel)
    indexEnd   = indexEnd[length(indexEnd)]
    for( i in 1:noVarAuto ) {
      auto[[iStat]][[i]] =  auto[[iStat]][[i]][indexBegin:indexEnd]
    }  
    indexBegin = which.max( auto3[[iStat]]$year == beginYearParallel & auto3[[iStat]]$month == beginMonthParallel & auto3[[iStat]]$day == beginDayParallel)
    indexEnd   = which    ( auto3[[iStat]]$year == endYearParallel   & auto3[[iStat]]$month == endMonthParallel   & auto3[[iStat]]$day == endDayParallel)
    indexEnd   = indexEnd[length(indexEnd)]
    for( i in 1:noVarAuto3 ) {
      auto3[[iStat]][[i]] =  auto3[[iStat]][[i]][indexBegin:indexEnd]
    }      
    indexBegin = which.max( manual[[iStat]]$year == beginYearParallel & manual[[iStat]]$month == beginMonthParallel & manual[[iStat]]$day == beginDayParallel)
    indexEnd   = which    ( manual[[iStat]]$year == endYearParallel   & manual[[iStat]]$month == endMonthParallel   & manual[[iStat]]$day == endDayParallel)
    indexEnd   = indexEnd[length(indexEnd)]    
    for( i in 1:noVarManual ) {
      manual[[iStat]][[i]] =  manual[[iStat]][[i]][indexBegin:indexEnd]
    }    
    Q=9
  }

  # TODO
# Check for completness and make new time line if necessary 
#   difftime for time intervals.  
# difftime(dt1, dt2, units = "weeks")

  ## Save data
  # Local station number as inspiration for our longer station numbers: 
  # 38  Planina pod Golico, 76  Vojsko, 301  Slovenske Konjice, # 348  Jeruzalem; # 786  Ivanjkovci (same station from a paralel data view point (relocation))
  settings = read_software_settings() # Get main data directory
  stationNo = c("001400038", "00140076", "00140301", "00140348") 
  baseDirData = paste(settings$baseDir, "data", dataDirectory, "level2", sep=.Platform$file.sep)
  for( iStat in 1:4 ) {
    subDirData = sprintf('s%d', iStat) # Both directories baseDirData and subDirData have to be generated, the first time they are used. Thus two variables (prefer not to use recursive directory creation; dangerous)
    
    ## First save the fixed hour manual measurements, 3 per day, using the hourly file format
    # Combine 3 seperate fixed hour measurements to one vector    
    # First for date, time and flags used by all manual 3-daily observations
    year3      = combine_fixed_hour_measurements(list(manual[[iStat]]$year,  manual[[iStat]]$year,  manual[[iStat]]$year))
    month3     = combine_fixed_hour_measurements(list(manual[[iStat]]$month, manual[[iStat]]$month, manual[[iStat]]$month))
    dayMonth3  = combine_fixed_hour_measurements(list(manual[[iStat]]$day,   manual[[iStat]]$day,   manual[[iStat]]$day))    
    # Change to UTC later:
    zeros      = rep(0, length(manual[[iStat]]$T7))
    # The 7 hour measurement in CET, was at 6h UTC, etc.
    hour3      = combine_fixed_hour_measurements(list(zeros+6, zeros+13, zeros+20)) # zeros is a vector with zeros of the same lenght as the data
    flags3     = hour3 * 0
    
    # Combine measurements, make filename and save for all 3-daily obs
    temp       = combine_fixed_hour_measurements(list(manual[[iStat]]$T7,          manual[[iStat]]$T14,          manual[[iStat]]$T21))    
    fileNameList = list("p", "ra", "tr", "01", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, temp,          flags3, list(year3, month3, dayMonth3, hour3))    
    
    CC         = combine_fixed_hour_measurements(list(manual[[iStat]]$CC7,         manual[[iStat]]$CC14,         manual[[iStat]]$CC21))   
    fileNameList = list("p", "ra", "nn", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, CC,            flags3, list(year3, month3, dayMonth3, hour3))  
    
    RH         = combine_fixed_hour_measurements(list(manual[[iStat]]$RH7,         manual[[iStat]]$RH14,         manual[[iStat]]$RH21))       
    fileNameList = list("p", "ra", "rh", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, RH,            flags3, list(year3, month3, dayMonth3, hour3))      
    
    Twet       = combine_fixed_hour_measurements(list(manual[[iStat]]$Twet7,       manual[[iStat]]$Twet14,       manual[[iStat]]$Twet21))       
    fileNameList = list("p", "ra", "tw", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, Twet,          flags3, list(year3, month3, dayMonth3, hour3))  
    
    wind.dir   = combine_fixed_hour_measurements(list(manual[[iStat]]$wind.dir7,   manual[[iStat]]$wind.dir14,   manual[[iStat]]$wind.dir21))       
    fileNameList = list("p", "ra", "dd", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, wind.dir,      flags3, list(year3, month3, dayMonth3, hour3))  
    
    wind.speed = combine_fixed_hour_measurements(list(manual[[iStat]]$wind.speed7, manual[[iStat]]$wind.speed14, manual[[iStat]]$wind.speed21))
    fileNameList = list("p", "ra", "ff", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, wind.speed,    flags3, list(year3, month3, dayMonth3, hour3))  
    
    # Save the observations that are measured once a day
    flags = manual[[iStat]]$RR*0
    fileNameList = list("p", "ra", "rr", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, manual[[iStat]]$RR,         flags, list(manual[[iStat]]$year,  manual[[iStat]]$month,  manual[[iStat]]$day))  
    
    fileNameList = list("p", "ra", "tx", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, manual[[iStat]]$Tmax,       flags, list(manual[[iStat]]$year,  manual[[iStat]]$month,  manual[[iStat]]$day))  
    
    fileNameList = list("p", "ra", "tn", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, manual[[iStat]]$Tmin,       flags, list(manual[[iStat]]$year,  manual[[iStat]]$month,  manual[[iStat]]$day))  
    
    fileNameList = list("p", "ra", "ns", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, manual[[iStat]]$new.snow,   flags, list(manual[[iStat]]$year,  manual[[iStat]]$month,  manual[[iStat]]$day))  

    fileNameList = list("p", "ra", "ts", "00", "h", stationNo[iStat], ".read1")
    save_standard_data(baseDirData, subDirData, fileNameList, manual[[iStat]]$total.snow, flags, list(manual[[iStat]]$year,  manual[[iStat]]$month,  manual[[iStat]]$day))    

    # Save the automatic measurements
    # Full 15-minute resolution of original data
    fileNameList = list("p", "ra", "tr", "02", "h", stationNo[iStat], ".read1")
    flags = auto[[iStat]]$temp * 0
    save_standard_data(baseDirData, subDirData, fileNameList, auto[[iStat]]$temp,         flags, list(auto[[iStat]]$year,  auto[[iStat]]$month,  auto[[iStat]]$day,  auto[[iStat]]$hour,  auto[[iStat]]$minutes))  
    
    # Simulated 3 fixed hour measurements at 7, 14 and 21 h
    fileNameList = list("p", "ra", "tr", "02", "h", stationNo[iStat], ".read2")
    flags = auto3[[iStat]]$temp * 0
    save_standard_data(baseDirData, subDirData, fileNameList, auto3[[iStat]]$temp,        flags, list(auto3[[iStat]]$year, auto3[[iStat]]$month,auto3[[iStat]]$day,  auto3[[iStat]]$hour))        
}


## TODO: 
#   remove parts where there is only one station 
#       1. Or NOT? do this in conversion to daily data?
#       2. On the other hand, that would make the sub-daily data, different from the daily, monthly and annual data.
#   save the data
}



## Wastebin for old code


#     dateTime = ISOdatetime(auto[[iStat]]$year3, auto[[iStat]]$month3, auto[[iStat]]$day3, auto[[iStat]]$hour3, 0, 0, tz="CET")
#     dateTime2 = dateTime - 3600 # Subtract 1 hour (3600 s) to go from CET to UTC.
#     auto[[iStat]]$year3    = as.numeric(strftime(dateTime2, format="%Y")) # Year with century.
#     auto[[iStat]]$month3   = as.numeric(strftime(dateTime2, format="%m")) # Month as decimal number (01–12).
#     auto[[iStat]]$day3     = as.numeric(strftime(dateTime2, format="%e")) # Day of the month as decimal number (1–31), with a leading space for a single-digit number.
#     auto[[iStat]]$hour3    = as.numeric(strftime(dateTime2, format="%H")) # Hours as decimal number (00–23). As a special exception strings such as 24:00:00 are accepted for input, since ISO 8601 allows these.
#     
#     dateTime = ISOdatetime(auto[[iStat]]$year3, auto[[iStat]]$month3, auto[[iStat]]$day3, auto[[iStat]]$hour3, 0, 0, tz="CET")
#     dateTime2 = dateTime - 3600 # Subtract 1 hour (3600 s) to go from CET to UTC.
#     manual[[iStat]]$year    = as.numeric(strftime(dateTime2, format="%Y")) # Year with century.
#     manual[[iStat]]$month   = as.numeric(strftime(dateTime2, format="%m")) # Month as decimal number (01–12).
#     manual[[iStat]]$day     = as.numeric(strftime(dateTime2, format="%e")) # Day of the month as decimal number (1–31), with a leading space for a single-digit number.


#     hour3      = combine_fixed_hour_measurements(list(zeros+7, zeros+14, zeros+21)) # zeros is a vector with zeros of the same lenght as the data

#     noVar = length(manual[[iStat]])
#     indexBegin = which.max(manual[[iStat]]$hour == 0 & manual[[iStat]]$minutes == 0)
#     # Is there an equivalent to which.max for the last which that fits the criterion, rather than the first?
#     indexEnd   = which(auto[[iStat]]$hour == 23 & auto[[iStat]]$minutes == 45)
#     indexEnd   = indexEnd[length(indexEnd)]
#     for( i in 1:noVar ) {
#       auto[[iStat]][[i]] =  auto[[iStat]][[i]][indexBegin:indexEnd]
#     }
#     
#     indexBegin = which.max(is.na(manual[[iStat]]$temp)
#     indexEnd   = which    (is.na(manual[[iStat]]$temp)
#     indexEnd   = indexEnd[length(indexEnd)]    
#     noVar = length(manual[[iStat]])
#     for( i in 1:noVar ) {
#       manual[[iStat]][[i]] =  manual[[iStat]][[i]][indexBegin:indexEnd]
#     }  



