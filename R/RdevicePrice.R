
#' Version: 0.1
#' - implemented price.2yearsGuarantee
#' Author: David Franquesa
#' Version: 1
#' Date: 27/10/2017
#' Description: Return Price
 
devicePriceMain <- function(input){
  sourceData    <- data.table(input$sourceData)
  schemaAux     <- data.table(input$schema)
  configData    <- data.table(input$config)
  versionSchema <- input$versionSchema
  versionPrice  <- input$versionPrice #' score v2-2017-09-20
  
  if(ncol(configData) == 1){ # if only 1 is because csv file is not structured 
    status <- "the character separator of the configuration file must be ; separator and decimals as coma separator"
    result <- list("price" = data.table(), "status" = -1, "status.description" = status)
  } 
  else {
    config   <- configData[configData$Version==versionPrice,] # load parameters afecting model and define key as param2
    DBschema <- data.frame(schemaAux[schemaAux$version == versionSchema]) # load p
    result   <- tryCatchError(adaptSchema(DBschema,sourceData)) # Etl Schema ensure required columns exists
    isErrorSchema <- inherits(result$value, "simpleError") 
    if(!isErrorSchema){
      result <- devicePrice(dt=result$value, config) # TODO: Control errors
    }
  }
  return(result)
}
devicePrice  <- function(dt, config){
  is.nan.data.frame <- function(x) {
    do.call(cbind, lapply(x, is.nan))
  }
  
  #' @description It creates a new column (or update if exists) and fill it with Price * Percentage of the variable (subtype, range, variant, actor)
  #' @param config It must have columns "Type", "Subtype", "Variant", "per.refurbisher", "per.platform" and "per.retaielr"
  #' @param data It is the data.table and it must have columns Subtype and Range
  #' @param subtypes It is a list of subtypes to set an amount
  #' @param ranges It is a list of ranges to set an amount
  #' @param actor It is a list of actors, the column created include it
  
  setAmountPerActorPerVariant <- function(config, dt1, subtypes,ranges,variant,actor){
    #i<-1
    #j<-1
    columnConfig = paste0("per.",actor)
    for(i in 1:length(subtypes)){
      for(j in 1:length(ranges)){
        aux <- as.double(config[Subtype == eval(subtypes[i]) & Range == eval(ranges[j]) & Variant == variant, eval(parse(text=columnConfig))]) # per refurbisher
        columnAmount = paste0("amount.",variant,".",actor)
        dt1[Subtype == eval(subtypes[i]) & Range == eval(ranges[j]), eval(columnAmount) := Price * aux]
        columnPer = paste0("per.",variant,".", actor)
        dt1[Subtype == eval(subtypes[i]) & Range == eval(ranges[j]), eval(columnPer) := aux]
      }
    }
  }
  
  ##### Config parameters ####
  #' Create variables based on config values
  # for(i in 1:nrow(config)){ # create required variables
  #   assign(rownames(config)[i], as.double(config[rownames(config)[i],"per.refurbisher"])) # create a variable 
  #   #cat(i,": ",as.double(config[rownames(config)[i],"value1"]),"\n")
  # }
 
  ##### Filter Computers ####
  dt1 <- dt[Type == "Computer"] ## Only devices created in 2017
  ##### Add id #####
  dt1$id <- seq(1, length.out=nrow(dt1), by=1) # add id
  ##### Fill NA with 0.0 #####
  dt1[is.na(dt1)] <- as.numeric(0.0)
  dt1[is.nan(dt1)] <- as.numeric(0.0)
  
  #### Convert to numerics ####
  dt1$Score <- as.numeric(dt1$Score)
  dt1$Appearance.Score <- as.numeric(dt1$Appearance.Score)
  dt1$Functionality.Score <- as.numeric(dt1$Functionality.Score)
  ##### Fill NA with 0.0 #####
  dt1[is.nan(dt1)] <- as.numeric(0.0)
  ######################### Step Pricing #########################
  #' 1 point 20 €
  
  eurosPerPointInDesktops <- 20
  eurosPerPointInLaptops  <- 30
  
  dt1$Price <- as.numeric(0.0)
  dt1[Subtype == "desktop",Price := Score* eurosPerPointInDesktops]
  dt1[Subtype == "laptop",Price := Score* eurosPerPointInLaptops]
  
  # config$per.refurbisher
  # 
  # subtypes = c("desktop","laptop")
  # ranges = c("High","Medium","Low")
  # variant = "standard"
  # actor = "refurbisher"
  # i<-1
  # j<-1
  # columnConfig = paste0("per.",actor)
  # aux <- config[Subtype == eval(subtypes[i]) & Range == eval(ranges[j]) & Variant == variant, eval(parse(text=columnConfig))] # per refurbisher
  # aux1 <- aux
  # aux1 + aux1
  # aux <- as.double(config[Subtype == eval(subtypes[i]) & Range == eval(ranges[j]) & Variant == variant, eval(parse(text=columnConfig))]) # per refurbisher
  # 
  #### Set Amounts
  # Set amount in column amount.standard.refurbisher
  setAmountPerActorPerVariant(config, dt1, subtypes = c("desktop","laptop"), ranges = c("High","Medium","Low"), variant = "standard", actor = "refurbisher")
  # Set amount in column amount.standard.platform
  setAmountPerActorPerVariant(config, dt1, subtypes = c("desktop","laptop"), ranges = c("High","Medium","Low"), variant = "standard", actor = "platform")
  # Set amount in column amount.standard.retailer
  setAmountPerActorPerVariant(config, dt1, subtypes = c("desktop","laptop"), ranges = c("High","Medium","Low"), variant = "standard", actor = "retailer")
  
  # Set amount in column amount.2yearsGuarantee.refurbisher (range low is not provided)
  setAmountPerActorPerVariant(config, dt1, subtypes = c("desktop","laptop"), ranges = c("High","Medium"), variant = "2yearsGuarantee", actor = "refurbisher")
  # Set amount in column amount.standard.platform
  setAmountPerActorPerVariant(config, dt1, subtypes = c("desktop","laptop"), ranges = c("High","Medium"), variant = "2yearsGuarantee", actor = "platform")
  # Set amount in column amount.standard.retailer
  setAmountPerActorPerVariant(config, dt1, subtypes = c("desktop","laptop"), ranges = c("High","Medium"), variant = "2yearsGuarantee", actor = "retailer")
  
  # Add Price.2yearsGuarantee
  dt1$Price.2yearsGuarantee <- as.numeric(0.0)
  dt1[,Price.2yearsGuarantee := amount.2yearsGuarantee.refurbisher + amount.2yearsGuarantee.platform + amount.2yearsGuarantee.retailer]

  
  #' Return values
  result <- dt1
  return(result) 
}
