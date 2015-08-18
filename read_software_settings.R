read_software_settings <- 
function() {
  # This function reads some settings for generating the database that are used by other functions. 
  # When transfering the database, only this settings file should be changed.
  
  baseDir = "/home/victor/data/parallel_database/version0.1"

  # Setting for the conversion from native data to standard format data
  #   conversion$dirNo[i]         # Number of directory of the level1 directory with the native data
  #   conversion$dirName[i]       # Directory name of the level1 directory with the native data, set by function
  #   conversion$dataFile[i]      # Filename of the datafiles, this may make it easier to reuse conversion functions, could also be a token name that just helps in selecting some settings
  #   conversion$conversionFunct  # Name of the conversion function
  #   conversion$status[i] =      # Status (not case sensitive)
  #                                        to_be_written,   # File need to be converted, but function not yet available
  #                                        to_be_converted, # File needs to be converted, function written
  #                                        ready            # File has already been converted, does not have be be done again
  
  # Maximum Number of source directories
  maxNoSourceDirs = 99
  dirNo    = rep(NA, maxNoSourceDirs)
  dirName  = rep("", maxNoSourceDirs)
  dataFile = rep("", maxNoSourceDirs)
  conversionFunct = rep("", maxNoSourceDirs)
  status   = rep("", maxNoSourceDirs)
  
  # Conversion information, the last item should have the largest number, best keep ranked.
  i=0
  i=i+1
  dirNo[i] = i
  dataFile[i] = ""
  conversionFunct[i] = "convert_d1_slovenian_nearby_pairs"
  status[i] = "to_be_written"

  i=i+1
  dirNo[i] = i
  dataFile[i] = ""
  conversionFunct[i] = "convert_d2_slovenian_automation_files"
  status[i] = "to_be_converted"
  
  i=i+1
  dirNo[i] = i
  dataFile[i] = ""
  conversionFunct[i] = "convert_d3_some_description_of_data"
  status[i] = "ready" # Just for testing and show all three options.
  
#   Template for new entries, please copy, do not modify
#   i=i+1
#   dirNo[i] = i
#   dataFile[i] = ""
#   conversionFunct[i] = "convert_d1_slovenian_nearby_pairs"
#   status[i] = "to_be_written"

  # Generate directory names, they should all have the same length, padded by zeros, lenght determined by largest number
  formatStr = sprintf('d%%0%dd', floor(log10(i))+1)
  
  for( ii in 1:maxNoSourceDirs  ) {
    if (!is.na(dirNo[ii]) ) {
      dirName[ii] = sprintf(formatStr, dirNo[ii]) # paste("d", ii, sep="")
    }
  }

  # Truncate conversion structure 
  dirNo    = dirNo[1:i]
  dirName  = dirName[1:i]
  dataFile = dataFile[1:i]
  conversionFunct = conversionFunct[1:i]
  status   = status[1:i]
  conversion = list(dirNo, dirName, dataFile, conversionFunct, status)
  names(conversion) = c("dirNo", "dirName", "dataFile", "conversionFunct", "status")

  settings = list(baseDir, maxNoSourceDirs, conversion)
  names(settings) = c("baseDir", "maxNoSourceDirs", "conversion")

  settings
}

