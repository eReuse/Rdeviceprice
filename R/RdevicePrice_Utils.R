######################### Libraries #########################
#### Development libraries ####
tryCatchError <- function(expr) {
  W <- NULL
  w.handler <- function(w){  
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}
# Rename columns of sourceData to fit with the program and ensure all required columns exists
adaptSchema <- function(schema, sourceData){
  columnNames <- schema # Load Config file
  isError  <- inherits(columnNames$value, "simpleError")
  if(!isError){
    oldColNames <- columnNames$sourceSchema # Column 1
    newColNames <- columnNames$targetSchema
    columnsMap <- data.frame(oldColNames,newColNames)
    adaptedSchema <- setSchema(sourceData,columnsMap)
    adaptedSchema <- adaptedSchema[,newColNames, with=FALSE] ## Filter so- 
  } else{
    adaptedSchema <- list(value = columnNames$value, warning = columnNames$warning) # colnames has the error
  }
return(adaptedSchema)
}

writeFilePrice <- function(fileOutput, dt){
  result <- tryCatchError(write.table(format(dt, digits=4, decimal.mark =","), file=fileOutput, col.names = TRUE,row.names = FALSE,append=FALSE, sep=";"))
}

setSchema <- function(sourceData, columnsMap){
  #' A schema mapping is a specification that describes how data structured under one schema (the source schema) 
  #' is to be transformed into data structured under a different schema (the target schema)
  #' column names are renamed to match the score algoritm, if the source schema do not has a match column in the target
  #' schema then a column is created.  
  
  #' Example:
  #' oldColNames <- c('Model','Manufacturer')
  #' newColNames <- c('model','manufacturer')
  #' columnsMap <- data.frame(oldColNames,newColNames)
  
  #' This function renames columns in the sourceData to match with the target schema (algorith variables)
  #' TODO launch a warning if the source schema do not has a match column in the target schema
  #i<-8
  for(i in 1:nrow(columnsMap)) {
    oldColumnName <- as.character(columnsMap$oldColNames[i])
    newColumnName <- as.character(columnsMap$newColNames[i])
    if(length(names(sourceData)[names(sourceData) == oldColumnName])!=0){ # column to rename exists
      names(sourceData)[names(sourceData) == oldColumnName] <- newColumnName # rename the column
    } else { # column to rename not exists, if already not created, then create it with NA value
      if(length(names(sourceData)[names(sourceData) == newColumnName]) == 0){ # new column no exist, create it
        sourceData$temp <- NA # temporal column
        names(sourceData)[names(sourceData) == 'temp'] <- newColumnName # rename the column
      }
    }
  }
  return(sourceData)
}


