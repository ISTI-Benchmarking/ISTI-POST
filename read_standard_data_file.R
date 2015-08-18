read_standard_data_file <- 
  function(dirFileNames) {
    
  data = read.csv(dirFileNames, header=FALSE, sep = "\t",na.strings="-999.9", comment.char = "", colClasses="numeric")
  noCols = length(data)
  names(data)[noCols]   = "flags"
  names(data)[noCols-1] = "var"
  names(data)[1]        = "year"
  if(noCols >= 4) names(data)[2] = "month"
  if(noCols >= 5) names(data)[3] = "day"
  if(noCols >= 6) names(data)[4] = "hour"
  if(noCols >= 7) names(data)[5] = "min"
  if(noCols >= 8) names(data)[6] = "sec"
  if(noCols > 8) error(c("File contains too many columns: ", noCols))
  
  data
}