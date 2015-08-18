read_standard_data_directory <- 
function(dirName) {
  # This function read all the data files in the specified directory.
  # The file names are used to assing the name to the variables
  # If the variable is a covariate, it simply gets the abbreviation of the measured variable, e.g. tx, tn, pp
  # And the index is appended, e.g. tx_read, tn_mean, pp_TN10p
  # If the variable is a parallel measurement, a number is added at the end, e.g. tx1, tx2, pp1, pp2, pp3
  # With index: tx1_read, tx2_read, pp1_mean, pp2_mean, pp3_mean
  # If there are multiple version of the variable, e.g. because the mean daily temperature can be computed in multiple ways,
  # then the number from the filename is also added: tx1_read, tx2_read1, tx2_read2
  #
  # If all files are read or mean, we could also remove the index from the variable name. 
  # Would that be handy, because shorter, or not, because less consistent?
  
  # Determine which data file are in the directory
  fileNames = list.files(path=dirName,  pattern="[^p:print:*.txt$]", include.dirs=FALSE)
  dirFileNames = paste(dirName, fileNames, sep=.Platform$file.sep)
  noFiles = length(fileNames)
  
  # Derive the variable names from the file names
  # First determine whether there are any identical combinations of variable and index, that would necessitate an additional number behind the index
  variable   = vector("character", length=noFiles) # Abbreviation of variable from filename
  index      = vector("character", length=noFiles) # Index in file from filename
  indexNoNum = vector("character", length=noFiles) # The same index, but without the number at the end, that the filename always has
  double         = rep(FALSE,   length=noFiles)    # FALSE for unique variables-index files, TRUE for multiple files with same variable-index
  numberVariable = rep(NA,   length=noFiles)       # 00 for co-variate, 01 for first parallel measurement, 02 for second, etc.
  numberIndex    = rep(NA,   length=noFiles)       # The number of the index, typicaly 1, sometimes two if there are two ways to compute the same index, rarely even higher
  for( iFile in 1:noFiles ) {  
    nameSplit = unlist(strsplit(fileNames[iFile], ".", fixed=TRUE))     # Split into filename.index.extention
    variable[iFile]       = substr(nameSplit[1], 4, 5)                  # Extract abbreviation of variable
    numberVariable[iFile] = substr(nameSplit[1], 6, 7)                  # Extract number of variable
    index[iFile]      = nameSplit[2]                                    # Extract index and its running number
    indexNoNum[iFile] = substr(index[iFile], 1,  nchar(index[iFile])-1) # Extract index
    numberIndex            = substr(index[iFile], nchar(index[iFile]),    nchar(index[iFile]))  
    numberIndexPreviousPos = substr(index[iFile], nchar(index[iFile])-1,  nchar(index[iFile])-1)
    if( is.numeric(numberIndexPreviousPos) ) {
      stop("The code assumes that the number at the end of the index in the filename is single digit.")
    }
    if( as.numeric(numberIndex) > 1 ) {
      # If there is a variable with multiple indices, all these instances (also the index with the 1), 
      # need to be set to double=TRUE, to be later given a number in the variable name.
      double[which(variable==variable[iFile] & indexNoNum==indexNoNum[iFile])] = TRUE
    }
  }
  # Now generate the variable name to be given to the data
  variableName = vector("character", length=noFiles)
  for( iFile in 1:noFiles ) {
    # The co-variates (0) do not need a number after the variable abbreviation, the parallel data (=>1) keeps its running number
    if( as.numeric(numberVariable[iFile]) == 0 ) {
      variableName[iFile] = variable[iFile]
    } else {
      variableName[iFile] = paste(variable[iFile], numberVariable[iFile], sep="")
    }    
    indexStr = index[iFile]
    if( double[iFile] == 0 ) { # If there is only one variable-index pair, the number of the index from the filename can be removed for the variable name.
      indexStr = substr(indexStr, 1,  nchar(indexStr)-1)
    }
    variableName[iFile] = paste(variable[iFile], indexStr, sep="_")    # Combine variable and index name with an underscore
#     a=0
  }  
  
  # Now read data files and assign the variable names
  data = vector("list", length=noFiles)
  for( iFile in 1:noFiles ) {    
    data[[iFile]] = read_standard_data_file(dirFileNames[iFile])
    names(data)[iFile] = variableName[iFile]
#     data[[iFile]]$fileName[1] = dirFileNames[iFile]
  }
  list(data, dirFileNames)
}


## Wastebin

#     if sum( variable[iFile]==newVariable & index[iFile]==newIndex) {


#     variable[iFile] = newVariable
#     index[iFile]    = newIndex


# [1] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/pradd00h001400038.read1.txt"
# [2] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/praff00h001400038.read1.txt"
# [3] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/prann00h001400038.read1.txt"
# [4] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/prans00h001400038.read1.txt"
# [5] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/prarh00h001400038.read1.txt"
# [6] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/prarr00h001400038.read1.txt"
# [7] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/pratn00h001400038.read1.txt"
# [8] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/pratr01h001400038.read1.txt"
# [9] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/pratr02h001400038.read1.txt"
# [10] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/pratr02h001400038.read2.txt"
# [11] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/prats00h001400038.read1.txt"
# [12] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/pratw00h001400038.read1.txt"
# [13] "/home/victor/data/parallel_database/version0.1/data/d2/level2/s1/pratx00h001400038.read1.txt"

#     nameSplit = unlist(strsplit(fileNames[iFile], ".", fixed=TRUE))
#     variable[iFile] = substr(nameSplit[1], 4, 5)
#     data[[iFile]]$no

#     names(data)[iFile] = paste("temp", iFile, sep="")

