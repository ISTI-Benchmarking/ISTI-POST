generate_raw_data_dirs <-
function(newDirNo) {
  # This script generates a new data source directory with a level1 sub-directory for a new 
  # raw dataset for the parallel dataset.
  # The name of these directories is given by d#/level1, with # a number of arbitry length.
  # If also a level0 sub-directory is needed, this has to be added by hand (probably not often needed).
  # If the new directory number has more characters as the previous ones, 
  # zeros are added to the old directory names, to keep the length of all directories the same.
  # After adding the raw data to the directory, please continue with the script: generate_standard_data.R
  #
  # Written by Victor Venema
  # First version 31 August 2014
    
  # Settings
#   baseDir = "/home/victor/data/parallel_database/version0.1"
  settings = read_software_settings()
  baseDirData = paste(settings$baseDir, "data", sep=.Platform$file.sep)
  baseDirSoftware = paste(settings$baseDir, "software", sep=.Platform$file.sep)
    
  # Check input, directory number should be an integer and larger or equal to one.
  if( !missing(newDirNo) ) {
    if( newDirNo != round(newDirNo) ) {
      stop("Prescribed number for the new directory should be an integer.")
    }    
    if( newDirNo <= 0 ) {
      stop("Prescribed number for the new directory should be one or larger.")
    }
  }
  
  # If new number is not specified in call to function, 
  # read existing directory names to determine what number is next  
  dirs = dir(baseDirData, pattern ="^d[:digit:]*")  
  noDirs = length(dirs)
  if( missing(newDirNo) ) {
    number = rep(0, noDirs)
    for( i in 1:noDirs ) {
      number[i] = as.numeric(substr(dirs[i], start=2,  stop=nchar(dirs[i])))
    }
    if( is.na(number[1]) ) {
      newDirNo = 1
    } else {
      newDirNo = max(number)+1
    }
  }
  
  # Create new data source directory with name d#, with # the number newDirNo
  if( missing(newDirNo) )  {
    stop("Not able to generate new directory name. Possibly uncommon directory name in main data directory")
  } else {
    subDirName = paste("d", newDirNo, sep="")
    dirName = paste(baseDirData, subDirName, sep=.Platform$file.sep)
    dir.create(path = dirName) # R warns if directory already exists
  }
  # In this directory create a subdirectory called level1
  dirName = paste(dirName, "level1", sep=.Platform$file.sep)
  dir.create(path = dirName)
  
  # If the new data directory name is longer than the existing ones, add zeros in front of the number
  # of the existing directories
  lengthDiff = sum(nchar(subDirName) - nchar(dirs[]))
  if( sum(lengthDiff) > 0 ) { # If new dir name is longer than previous ones
    for( i in 1:noDirs ) { # Loop over all directories that existed before this function call that may need to be renamed
      lengthDiff = nchar(subDirName) - nchar(dirs[i])
      if( lengthDiff > 0 ) { # Additional check on length of directory name that should normally not be necessary        
        nullStr=""; for( l in 1:lengthDiff ) nullStr=paste0(nullStr, "0") # Generate string with the number of zeros missing
        newSubDirName = paste("d", nullStr, substr(dirs[i], start=2,  stop=nchar(dirs[i])), sep="")
        newDirName = paste(baseDirData, newSubDirName, sep=.Platform$file.sep)
        oldDirName = paste(baseDirData, dirs[i], sep=.Platform$file.sep)
        file.rename(from=oldDirName, to=newDirName)
      }
    }
  }
}
