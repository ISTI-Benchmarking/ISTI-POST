save_standard_data = function(baseDirData, subDirData, fileNameList, data, flags, dateList) {
# Saves one variable and its flags into a file in the standard format of the parallel database
# If the fileNameList is a list (components are clearer when writting name for first time), it will be collapsed into a filename.
# fileNameList does not have to be a list, but can also be normal string. (Can be handy in case of reading and writing data.)
# If the filename does not end in .txt, this will be added.
  
  # Check input (all input columns the same length) and compute noRows
  noVals = rep(NA, times=7)
  noVals[1] = length(dateList[[1]])  
  for( i in 2:5) {
    if( i <= length(dateList) ) {
        noVals[i] = length(dateList[[i]])    
      } else {
        noVals[i] = NA
      }
  }
  noVals[6] = length(data)
  noVals[7] = length(flags)
  if( sd(noVals, na.rm=TRUE) == 0 ) {
    noRows = noVals[1]
  } else {
    print(c(baseDirData, subDirData))
    print(fileNameList)
    print(noVals)
    stop("The lenght of all columns should be the same in function save_standard_data().")
  }

  # Generate directories and filename string
  if( !file.exists(baseDirData) ) {
    dir.create(baseDirData)
  }
  stationDirData = paste(baseDirData, subDirData, sep=.Platform$file.sep)
  if( !file.exists(stationDirData) ) {
    dir.create(stationDirData)
  }  
  if(is.list(fileNameList)) {
    fileName = paste(fileNameList, sep="", collapse="")  
  } else {
    fileName = fileNameList
  }
    
  if( substr(fileName, nchar(fileName)-3, nchar(fileName)) != ".txt") {
    fileName = paste(fileName, ".txt", sep="")
  }
  dirFileName = paste(stationDirData, fileName, sep=.Platform$file.sep)
  
  # Generate data to be saved
  if( sum(!is.na(data)) > 0 ) { # Only save data if some data is available
    data[is.na(data)] = -999.9
    
    noCols= sum(!is.na(noVals))
    x = array(0, c(noRows, noCols))  
    for( i in 1:(noCols-2) ) {
      x[,i] = dateList[[i]]
    }
    x[,noCols-1] = data
    x[,noCols]   = flags
      
    # Save data
    write.table(x, dirFileName, sep="\t", row.names=FALSE, col.names=FALSE)
  }
}