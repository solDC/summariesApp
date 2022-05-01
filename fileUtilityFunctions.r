# Utility function to load csv files from dropbox
loadCSV <- function(path,fileName){
  f <- tryCatch({
    filePath <- paste0(path,fileName,".csv")
    drop_read_csv(file=filePath,dest = tempdir(),dtoken=token)
  },
  error = function(e){
    msg <- paste0("Error. Problema con el fichero ",fileName,".")
    message(msg)
    return(NULL)
  },
  warning = function(w){
    msg <- paste0("Warning causado por el fichero ",fileName,".")
    message(msg)
  }  #, finally = {}
  )
}

# Utility function to save data into csv file in dropbox 
# --> NO LA ESTOY USANDO PERO DE USARLA TIENE QUE IR ACA (POR PERFORMANCE)
# Generates a unique-named file to save the validation 
saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}