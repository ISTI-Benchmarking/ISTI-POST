combine_fixed_hour_measurements = function(dataList = "list") {
  
  noHours = length(dataList)  
  noVal = length(dataList[[1]])
  for( iHours in 2:noHours  ) {
    noVal2 = length(dataList[[iHours]])
    if( noVal2 != noVal ) {
      stop("Lenght of data series is not the same in function combine_fixed_hour_measurements().")
    }
  }
  
  dataHours = array(0, c(noHours,noVal))
  for( iHours in 1:noHours ) {
    dataHours[iHours,] = dataList[[iHours]]  
  }
  dim(dataHours) = noHours*noVal
  
  dataHours
}

#   noVal = length(manual[[iStat]]$T7)
#   # Add check on the length
#   data = array(0, c(3,noVal))
#   data[1,] = manual[[iStat]]$T7
#   data[2,] = manual[[iStat]]$T14
#   data[3,] = manual[[iStat]]$T21
#   data[1,] = 1
#   data[2,] = 2
#   data[3,] = 3
#   dim(data) = 3*noVal