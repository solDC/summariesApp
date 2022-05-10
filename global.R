library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr) 
library(tidyr) #spread
#library(readr) #read txt more efficient
library(rdrop2)
#library(httr)
library(shinyalert)

# Done once to create Dropbox authentification tokens
 token<-drop_auth()
 saveRDS(token, "droptoken.rds")

#Dropbox auth
token <- readRDS("droptoken.rds")

# Set Dropbox directories
outputDir <- "summariesApp/responses/"
inputDir <- "summariesApp/data/"

# Utility function to load csv files from dropbox
loadCSV <- function(path,fileName){
  f <- tryCatch({
    filePath <- paste0(path,fileName) #,".csv")
    print(filePath)
    # WARNING: next line needs to be the last one so the return value is the uploaded file
    drop_read_csv(file=filePath,dest = tempdir(),dtoken=token)
  },
  error = function(e){
    msg <- paste0("Error: no se ha podido leer el fichero ",filePath)
    message(msg)
    message(e$message)
    # shinyalert(title="Salga de la aplicación y hable con su administrador",
    #            text=paste0("No se ha cargado el fichero ",filePath," necesario el funcionamiento de la aplicación."),
    #            type="error")
    return(NULL)
  })
}

# Load files --> only one time when app loads
conf <- loadCSV(inputDir,"conf.csv")
credentials <- loadCSV(inputDir,"users.csv")

# Load Articles and Summaries
# Articles and Summaries are related by its position in the file
# I will only kept the ones the users need to validate (random sample depending on sample size defined by admin)
set.seed(123)
if(is.null(conf)){
  message("Error al leer el fichero configuración, sin contenido")
} else{
  articles <- loadCSV(inputDir,conf$fileArticles)
  numArticles <- nrow(articles)
  summaries <- loadCSV(inputDir,conf$fileSummaries)  
  sampleSize <- round(conf$sampleSize*numArticles/100,0)
  samplePositions <- sort(sample(1:numArticles,sampleSize,replace=F))
  articles <- articles[samplePositions,]
  articles$position <- samplePositions
  summaries <- as.data.frame(summaries[samplePositions,])
  summaries$position <- samplePositions
  colnames(summaries) <- c("summary","articlesPosition")
}

# Data structure of user validation responses
names <- c("usernameId","position","question1","question2","question3","timeStamp","summariesNameFile","articlesNameFile","articleTitle")
numColEV <- length(names)
expertsValidationsColNames <- vector("character",numColEV)
expertsValidationsColNames <- names

# Calculate individual level of agreement
# 1- Leer todas las respuestas de usuarios y las uno en un único dataset
# 2- Agrupar por position y calcular de agreement de cada resumen
# 3- agregarlo a summaries en una nueva columna
filesInfo <- drop_dir(outputDir)
agreem <- 0
if(dim(filesInfo)[1] > 1  && dim(filesInfo)[2] > 1  ){#(dim(filesInfo)[1] != 0 && dim(filesInfo)[2] != 0 )
  filePaths <- filesInfo$path_display
  expertsValidations <- lapply(filePaths, drop_read_csv, stringsAsFactors=FALSE)
  expertsValidations <- do.call(rbind,expertsValidations)
  validationsQ1 <- expertsValidations %>% select(position,question1,usernameId) %>%  
    distinct(position,usernameId, .keep_all = TRUE) %>% spread(usernameId,question1)
  numNAQ1 <- apply(X=is.na(validationsQ1), MARGIN=1,FUN=sum)
  # calcular el número de usuarios que han validado al menos un resumen
  numUsers <- ncol(validationsQ1)-1 #all columns but position
  validationsQ1$numResp <- numUsers-numNAQ1
  df <- validationsQ1[-c(1,length(validationsQ1)-1,length(validationsQ1))]
  comb <- combn(df,2,simplify = FALSE)
  if (length(numResp) > 0 ){  
    i <- 1
    for(val in validationsQ1$numResp){
      if(val >= conf$minNumValid){
        validationsQ1$possPairs[i] <- factorial(val) / (2 * factorial(val - 2)) 
      }
      else{
        validationsQ1$possPairs[i] <- NA
      }
      i <- i+1
    }#for
    n <- length(validationsQ1)+1
    for(val in comb){
      validationsQ1[n] <- (val[1]==val[2])
      n <- n+1
    }
    validationsQ1$agreemCount <- rowSums(validationsQ1[,(n-length(comb)):(n-1)],na.rm = TRUE)
    validationsQ1$agreemPerc <- validationsQ1$agreemCount / validationsQ1$possPairs * 100
    validationsQ1$agreedAnswer <- strtoi(apply(df,1,function(x) names(which.max(table(x)))))
    agreem <- 1
  }
} #outter if


    # if(XXXXXX == 1){
    #   validationsQ2 <- expertsValidations %>% select(position,question2,usernameId) %>%  
    #     distinct(position,usernameId, .keep_all = TRUE) %>% spread(usernameId,question2)
    #   validationsQ3 <- expertsValidations %>% select(position,question3,usernameId) %>%  
    #     distinct(position,usernameId, .keep_all = TRUE) %>% spread(usernameId,question3)    
    # }    


  



# Table used in the administrator dashboard --->   MOVER ESTO A SERVER EN LA PARTE DEL SI EL LOGADO ES EL ADMIN y REVISARLOOOOOOOO POSITIONS
if(is.null(articles)){
  message("El fichero con los artículos cuyos resúmenes hay que validar no se ha cargado.")
} else{
  adminArticles <- articles %>% select(title)
  adminArticles$row_num <- samplePositions
  adminArticles <- adminArticles[,c(ncol(adminArticles),1:(ncol(adminArticles)-1))] #dejar row_num como primera columna del dataset
  if(is.null(summaries)){
    message("El fichero con los resúmenes no se ha cargado.")
  }
  else{
    adminArticles <- cbind(adminArticles,summaries)
    names(adminArticles)[3] <- "generatedSummary" #Era el 4 campo pero me cargo objective summary
    #names(adminArticles)[3] <- "objectiveSummary"
    #print(adminArticles)
  }
}
