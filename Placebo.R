library(Synth)
library(car)
library(zoo)
library(data.table)
library(ggplot2)



### Function returning gap in outcome variable for all available cities
placebo <- function(city, outcome, method, firstpreyear, lastpreyear, vars, predictorvars){
  ## Balance panel, remove NAs (if bothersome)
  # Select cities which are SEZs from firstpreyear (why?) onwards 
  citynames_firstpreyear <- SEZ_data_JK[SEZ_data_JK$year == firstpreyear, "cityname_e"]

  # Keep only citynames_firstpreyear and vars_all in time frame of interest
  synth_data <- SEZ_data_JK[SEZ_data_JK$cityname_e %in% citynames_firstpreyear & SEZ_data_JK$year >= firstpreyear & SEZ_data_JK$year <= 2010, vars]
 
  # Remove cities with NAs 1990-1997 (predictor years) (function from stack)
  na_cities <- unique(synth_data[unique(unlist(lapply(synth_data[synth_data$year <= lastpreyear, ], function (x) which (is.na (x))))), ]["cityname_e"])
  synth_data <- synth_data[!(synth_data$cityname_e %in% na_cities$cityname_e), ]
  na_cities_outcome <- unique(unlist(synth_data[which(is.na(synth_data[, outcome])), ]["cityname_e"]))
  
                                                      
  ## Function for synth
  #cityname <- "WENZHOU"
  synth.gaps <- function(cityname, outcome, method){
    # Prepare data 1 (remove cities with outcome NA)
    na_cities_outcome <- unique(unlist(synth_data[which(is.na(synth_data[, outcome])), ]["cityname_e"]))
    synth_data_outcome <- synth_data[!(synth_data$cityname_e %in% na_cities_outcome), ]
    
    # Prepare data 2
    foo = synth_data_outcome
    contr.citynames <- unique(foo[foo[, "cityname_e"] != cityname, "cityname_e"])
  
    # Prepare data 3 (dataprep function from Synth package)
    dataprep.out <- tryCatch(dataprep(
      foo = synth_data_outcome,
      predictors = predictorvars,
      predictors.op = method,
      time.predictors.prior = firstpreyear:lastpreyear,
      special.predictors = list(
        list("coast", firstpreyear, method),
        list("inland", firstpreyear, method)),
      dependent = outcome,
      unit.variable = "cityno",
      unit.names.variable = "cityname_e",
      time.variable = "year",
      treatment.identifier = cityname,
      controls.identifier = contr.citynames,
      time.optimize.ssr = firstpreyear:lastpreyear,
      time.plot = firstpreyear:2010), error = function(e) NULL)
   
    # Apply synth (from Synth package) #optimxmethod = "All"
    synth.out <-  tryCatch(synth(dataprep.out), error = function(e) NULL)
    gaps <- tryCatch(data.frame(dataprep.out$Y1plot - tryCatch(dataprep.out$Y0plot %*% synth.out$solution.w, error = function(e) NULL)), error = function(e) NULL)
    gaps
  }
  
                                                               
  ## Define cities synth.gaps is applied to 
  placebocities <- unique(synth_data[!(synth_data$cityname_e %in% na_cities_outcome), ]["cityname_e"])
  
                                                               
  ## Create placebo data set (Cities with errors are left out)
  # Apply placebo function to placebocities
  placebo_data <- sapply(placebocities$cityname_e, tryCatch(synth.gaps, error = function(e) NULL), outcome, method)
                                                            
  # Remove NULLs
  nulls1 <- placebo_data[sapply(placebo_data, is.null)] 
  list01 <- placebo_data[!placebo_data %in% nulls]
                                                            
  # Unlist
  list02 <- lapply(list01, "[[", 1)
  unlisted1 <- do.call(rbind.data.frame, list02)
                                                            
  # Rename and reorder
  placebo1 <- setDT(unlisted1, keep.rownames = TRUE)[] # Rownames as 1st column
  colnames(placebo1) <- c("cityname_e", paste("year", c(firstpreyear:2010), sep = "."))
  imp <- placebo1[order(placebo1$cityname_e),] # order alphabetically
                                                            
  # Convert to long format
  placebo.data <- as.data.frame(reshape(placebo1, 
                                        varying = c(list(2:length(colnames(placebo1)))),
                                        direction = "long", 
                                        idvar = "citynames_e", 
                                        sep = "."))[, 1:3]
                                                           
  # Adjust column names
  colnames(placebo.data) <- c("cityname_e", "year", paste(c("Gap_in", outcome), collapse = "_"))
                                                            
  # As reshape changed years to numbers, add firstpreyear-1 to each time value
  placebo.data[, "year"] <- placebo.data[, "year"] + (firstpreyear-1)
                                                            
  # Remove cityname suffixes that reshape added
  placebo.data[, "cityname_e"] <- gsub("\\..*","", placebo.data[, "cityname_e"])
                                                            
  # Print output
  placebo.data
}
#placebo.wz_ind <- placebo("WENZHOU", "indoutputa_pc", "mean", 1984, 1986, vars.wz, predictorvars.wz)  # Test



### Function determining cities which have no NA for a certain outcome variable
nooutcomena <- function(city, outcome, firstpreyear, lastpreyear, vars, predictorvars){
  # Select cities which are SEZs from firstpreyear (why?) onwards 
  citynames_firstpreyear <- SEZ_data_JK[SEZ_data_JK$year == firstpreyear, "cityname_e"]
  
  # Keep only citynames_firstpreyear and vars_all in time frame of interest
  synth_data <- SEZ_data_JK[SEZ_data_JK$cityname_e %in% citynames_firstpreyear & SEZ_data_JK$year >= firstpreyear & SEZ_data_JK$year <= 2010, vars]
  
  # Remove cities with NAs firstpreyear to lastpreyear (predictor years) (function from stack)
  na_cities <- unique(synth_data[unique(unlist(lapply(synth_data[synth_data$year <= lastpreyear, ], function (x) which (is.na (x))))), ]["cityname_e"])
  synth_data <- synth_data[!(synth_data$cityname_e %in% na_cities$cityname_e), ]
                                                      
  # Cities with outcome NA
  na_cities_outcome <- unique(unlist(synth_data[which(is.na(synth_data[, outcome])), ]["cityname_e"]))
                                                      
  # Cities without outcome NA
  placebocities <- unique(synth_data[!(synth_data$cityname_e %in% na_cities_outcome), ]["cityname_e"])
                                                      
  # Print                                                    
  placebocities
}
#nooutcomena("NANJING", "gdpcuracomb_pc", 1990, 1997, predictorvars.nj) # Test



### Mean squared predictive error (MSPE) function
mspe.auto <- function(placebodata, preperiod, nonacities){
  # Function determining MSPE for one city
  mspe <- function(cityname, placebodata, preperiod){
    gapvariablename <- colnames(placebodata)[3]
    mspe1 <- mean(placebodata[placebodata$cityname_e == cityname & placebodata$year %in% preperiod, gapvariablename]^2)
    result <- cbind(cityname, mspe1)
    result
  }
  
  # Function applying the above function to the entire placebodata data set
  MSPEpt_outcome <- as.data.frame(t(sapply(nonacities$cityname_e, mspe, placebodata, preperiod)), stringsAsFactors = F)
  colnames(MSPEpt_outcome) <- c("cityname_e", "MSPEpt")
  MSPEpt_outcome$MSPEpt <- as.numeric(as.character(MSPEpt_outcome$MSPEpt))
  MSPEpt_outcome <- MSPEpt_outcome[!is.na(MSPEpt_outcome$MSPEpt), ]
  MSPEpt_outcome
}
#mspe.auto(placebo.nj_gdp, 1990:1997, cities_nj_gdp) # Test



### Effect (gap between real and synthetic observation unit) function
effect.auto <- function(placebodata, trperiod, nonacities){
  # Function determining gap for one city
  effect <- function(cityname, placebodata, trperiod){
    gapvariablename <- colnames(placebodata)[3]
    eff <- mean(placebodata[placebodata$cityname_e == cityname & placebodata$year %in% trperiod, gapvariablename])
    result <- cbind(cityname, eff)
    result
  }
  
  # Function determining gap for entire placebodata data set
  effect_outcome <- as.data.frame(t(sapply(nonacities$cityname_e, effect, placebodata, trperiod)), stringsAsFactors = F)
  colnames(effect_outcome) <- c("cityname_e", "avg_effect")
  effect_outcome$avg_effect <- as.numeric(as.character(effect_outcome$avg_effect))
  effect_outcome <- effect_outcome[!is.na(effect_outcome$avg_effect), ]
  effect_outcome <- effect_outcome[order(-effect_outcome$avg_effect), ]
  effect_outcome
}
#effect.auto(placebo.nj_ele, 1998:2009, cities_nj_ele) # Test
                                                                                                         
                                                      
                                                      
### Post/pre-MSPE ratio (tryear: treatment year, nonacities: nooutcomena-function output 
f.postpre.mspe <- function(placebodata, tryear, nonacities){
  # for one city
  f.postpre <- function(cityname, placebodata, tryear){
    gapvariablename <- colnames(placebodata)[3]
    pre.mspe <- mean(placebodata[placebodata$cityname_e == cityname & placebodata$year < tryear, gapvariablename]^2)
    post.mspe <- mean(placebodata[placebodata$cityname_e == cityname & placebodata$year >= tryear, gapvariablename]^2)
    ratio <- post.mspe / pre.mspe
    result <- cbind(cityname, ratio)
    result
  }
  
  # for entire data set
  MSPE_ratio <- as.data.frame(t(sapply(nonacities$cityname_e, f.postpre, placebodata, preperiod)), stringsAsFactors = F)
  colnames(MSPE_ratio) <- c("cityname_e", "PostpreMSPEratio")
  MSPE_ratio$PostpreMSPEratio <- as.numeric(as.character(MSPE_ratio$PostpreMSPEratio))
  MSPE_ratio <- MSPE_ratio[!is.na(MSPE_ratio$PostpreMSPEratio), ]
  MSPE_ratio <- MSPE_ratio[order(-MSPE_ratio$PostpreMSPEratio), ]
  MSPE_ratio
}
#f.postpre.mspe(placebo.wz_ind, 1987, cities_wz_ind) # Test
