write_conversion_file <-
function(x=NA) {
        
  settings = read_software_settings()
  conversionDirFileName = paste(settings$baseDir, "data", "level1.txt", sep=.Platform$file.sep)
  
  if( is.na(x) ) {
    dir = c("d1", "d2", "d3")
    dataFile = c("", "", "")
    filename_of_conversion_function = c("convert_d1_slovenian_nearby_pairs", "convert_d2_slovenian_automation_files", "convert_d3_some_description_of_data")
    status = c("to_be_written", "to_be_converted", "ready")
    x = list(dir, dataFile, filename_of_conversion_function, status)    
    names(x) = c("dir", "dataFile", "filename_of_conversion_function", "status")
  }

  write.table(x, file=conversionDirFileName, row.names = FALSE, quote= FALSE, sep = "\t")
}
