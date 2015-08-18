generate_standard_data <-
function() {
  # This script generates standard data files for the level 2 directories 
  # from the native format files in the level 1 directories.
  # 
  # Written by Victor Venema
  # First version 29 August 2014
  
  # Read the name of the conversion functions to be used from the settings 
  settings = read_software_settings()
  conversion = settings$conversion  

  # Loop over all data sources
  noSources = length(conversion$dirName)
  for( iSource in 1:noSources) {
    # Only the sources marked as "to_be_converted" in the settings will be converted
    if( tolower(conversion$status[iSource])=="to_be_converted" ) {
      # And the function should naturally exist (every new data source will in general need a new conversion function)
      if( exists(as.character(conversion$conversionFunct[iSource]), mode = "function") ) {
        func = as.character(conversion$conversionFunct[iSource]) # Name of function, convert from list to string
        arg1 = as.character(conversion$dirName[iSource])         # Name of directory with native data
        arg2 = as.character(conversion$dataFile[iSource])        # Name of (a) file, in case this is different in different sources (may also be "abused" as some sort of flag)
        args = list(arg1, arg2)
        do.call(func, args) # Execute conversion function
      }
    }    
  }
}
