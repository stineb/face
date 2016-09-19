##------------------------------------------------------------
## This script unifies data from FACE and other global change 
## manipulation experiments into a nice consistent dataframe.
## One row for every data point (variable, year).
## ==> not multiple variable in separate columns and same row!
##------------------------------------------------------------
library(plyr)
library(dplyr)
source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/summarySE.R")

get_var <- function( var_long ){
  var <- var_long
  var <- ifelse( var=="aboveground biomass (corn)", "ab", var )
  var <- ifelse( var=="biomass (shoot + root)", "b", var )
  var <- ifelse( var=="cummulative root litter input", "blittp", var )
  var <- ifelse( var=="total soil C input", "soilp", var )
  var <- ifelse( var=="belowground NPP (soybean)", "bnpp", var )
  var <- ifelse( var=="aboveground NPP (soybean)", "anpp", var )
  var <- ifelse( var=="aboveground NPP", "anpp", var )
  var <- ifelse( var=="NPP", "npp", var )
  var <- ifelse( var=="aboveground yield", "yld", var )
  var <- ifelse( var=="yield", "yld", var )
  var <- ifelse( var=="annual yield", "yld", var )
  var <- ifelse( var=="peak biomass (shoot + root)", "b", var )
  var <- ifelse( var=="litter fall (average)", "littp", var )
  var <- ifelse( var=="total litterfall", "littp", var )
  var <- ifelse( var=="litterfall", "littp", var )
  var <- ifelse( var=="litter fall", "littp", var )
  var <- ifelse( var=="fine root biomass (average)", "frootm", var )
  var <- ifelse( var=="aboveground biomass", "ab", var )
  var <- ifelse( var=="fine root biomass", "frootm", var )
  var <- ifelse( var=="root biomass", "rootm", var )
  return(var)
}

get_exp_nam <- function( exp_nam ){
  ## Standardise experiment names
  exp_nam <- gsub( "ORNL FACE", "ORNL", exp_nam )
  exp_nam <- gsub( "FACTS II", "FACTSII", exp_nam )
  exp_nam <- gsub( "DUKE", "DukeFACE", exp_nam ) # MISSING IN GROENIGEN DATA
  exp_nam <- gsub( "ASPEN", "FACTSII", exp_nam ) # MISSING IN GROENIGEN DATA
  return( exp_nam )    
}

##------------------------------------------------------------
## Functions "ingests" original data ('data') and adds 
## additional row(s) to 'gcme' using standard formats (see 'init').
## Argument 'src' specifies what orinigal data is used. Add another
## specific one if additional data is to be ingested.
##------------------------------------------------------------
ingest_gcme <- function( data, gcme, init, src ){

  if (src=="vgr"){
    ##------------------------------------------------------------
    ## Get data Van Groenigen et al., 2014. Identifier "vgr"
    ##------------------------------------------------------------
  
    ## add data point identifiyer for associating the right ambient and elevated
    data$dataid <- paste( src, seq(1, dim(data)[1], 1), sep="" )

    ## loop through rows of original data frame
    for (idx in 1:dim(data)[1]){

      ## initialise new row
      newrow_amb <- init

      ## adopt some columns irrespective of manipulation
      newrow_amb$exp_nam    <- get_exp_nam( data$Experiment[idx] )
      newrow_amb$exp_type   <- ifelse( is.na(data$nfert[idx]), "c", "cf" )
      newrow_amb$dataid     <- data$dataid[idx]
      newrow_amb$date       <- data$Sample.date[idx]
      
      newrow_amb$nfert      <- ifelse( as.character(data$nfert[idx])  =="", NA, as.character(data$nfert[idx])     )
      newrow_amb$ozone      <- ifelse( as.character(data$ozone[idx])  =="", NA, as.character(data$ozone[idx])     )
      newrow_amb$burn       <- ifelse( as.character(data$burn[idx])   =="", NA, as.character(data$burn[idx])      )
      newrow_amb$water      <- ifelse( as.character(data$water[idx])  =="", NA, as.character(data$water[idx])     )
      newrow_amb$warm       <- ifelse( as.character(data$heat[idx])   =="", NA, as.character(data$heat[idx])      )
      newrow_amb$soil       <- ifelse( as.character(data$soil[idx])   =="", NA, as.character(data$soil[idx])      )
      newrow_amb$myc        <- ifelse( as.character(data$myc[idx])    =="", NA, as.character(data$myc[idx])       )
      newrow_amb$species    <- ifelse( as.character(data$species[idx])=="", NA, as.character(data$species[idx])   )

      newrow_amb$var        <- get_var( as.character(data$Proxy.type[idx]) )
      newrow_amb$var_long   <- as.character(data$Proxy.type[idx])
      newrow_amb$unit       <- as.character(data$Unit[idx])
      newrow_amb$fromdata   <- "data/face/data_raw/face_groenigen14_sheet2.csv"

      ## initialise additional rows
      newrow_ele  <- newrow_amb
        
      ## ambient row and add row to gcme dataframe
      newrow_amb$co2     <- "amb"
      newrow_amb$mean    <- as.numeric(as.character(data$aCO2[idx]))
      newrow_amb$sd      <- NA
      newrow_amb$N       <- NA
      newrow_amb$se      <- NA
      gcme <- rbind( gcme, newrow_amb )

      ## elevated row and add row to gcme dataframe
      newrow_ele$co2      <- "ele"
      newrow_ele$mean     <- as.numeric(as.character(data$eCO2[idx]))
      newrow_ele$sd       <- NA
      newrow_ele$se       <- NA
      newrow_ele$N        <- NA
      gcme <- rbind( gcme, newrow_ele )

    }

  } else if (src=="csr"){
    ##------------------------------------------------------------
    ## Get Nup data from file compiled and sent by Cesar. Identifyer "csr"
    ##------------------------------------------------------------

    ## loop through rows of original data frame
    for (idx in 1:dim(data)[1]){

      ## First standardise treatment codes
      data$CO2[idx] <- gsub( "elev", "ele", data$CO2[idx] )
      data$CO2[idx] <- gsub( "A", "amb", data$CO2[idx] )
      data$CO2[idx] <- gsub( "E", "ele", data$CO2[idx] )
      data$CO2[idx] <- gsub( "Camb", "amb", data$CO2[idx] )
      data$CO2[idx] <- gsub( "Cenrich", "ele", data$CO2[idx] )
      data$CO2[idx] <- gsub( "elev", "ele", data$CO2[idx] )

      ## initialise new row for N uptake
      newrow_nup  <- init

      ## adopt some columns irrespective of manipulation
      newrow_nup$exp_nam[]    <- get_exp_nam( data$SITE[idx] )
      newrow_nup$exp_type[]   <- ifelse( is.na(data$NFERT[idx]), "c", "cf" )
      newrow_nup$date[]       <- data$YEAR[idx]
      newrow_nup$co2[]        <- data$CO2[idx]
      newrow_nup$nfert[]      <- ifelse( as.character(data$NFERT[idx])  =="", NA, as.character(data$NFERT[idx])       )
      newrow_nup$myc[]        <- ifelse( as.character(data$myc[idx])    =="", NA, as.character(data$myc[idx])       )
      newrow_nup$species[]    <- ifelse( as.character(data$SPECIES[idx])=="", NA, as.character(data$SPECIES[idx])   )
      newrow_nup$var[]        <- "nup"
      newrow_nup$var_long[]   <- "N uptake"
      newrow_nup$fromdata[]   <- "data/face/data_raw/nup_new_cesar.csv"
      newrow_nup$N[]          <- 1
      newrow_nup$mean[]       <- data$Nup[idx]
      newrow_nup$unit[]       <- "missing"

      ## add row to gcme dataframe
      gcme <- rbind( gcme, newrow_nup )

      ## Initialise new row for NPP
      newrow_npp            <- newrow_nup
      newrow_npp$var        <- "npp"
      newrow_npp$var_long   <- "NPP"
      newrow_npp$mean       <- data$NPP[idx]

      ## add row to gcme dataframe
      gcme <- rbind( gcme, newrow_npp )

    }

  }
  return( gcme )
}

##------------------------------------------------------------
## Function returns dataframe holding data for CO2 response
## ratio, otherwise identical format as original dataframe.
## Length of first dimension of response ratio dataframe should
## be half of that of original data (for CO2 manipulation exp.)
##------------------------------------------------------------
get_gcme_dco2 <- function( gcme ){
  gcme_dco2 <- filter( gcme, co2=="ele" )
  for (idx in 1:dim(gcme_dco2)[1]){
    ## find corresponding ambient
    amb <- filter( gcme, co2=="amb", dataid==gcme_dco2$dataid[idx] )
    if (dim(amb)[1]==1) {
      gcme_dco2$mean[idx] <- ( gcme_dco2$mean[idx] / amb$mean )
    } else {
      print("PROBLEM: FOUND MORE THAN 1 CORRESPONDENCE")
    }
  }
  return( gcme_dco2 )
}



## Initialise standardised data frame (defines format for dataframes 'gcme*')
init <- data.frame( 
  exp_nam      = NA, # standardised experiment name
  exp_type     = NA, # experiment type (c, cf, ...)
  dataid       = NA, # data point identifier (two identical ones where low and ambient are given in same row in original data)
  year_start   = NA, # start year of period for which experiment is representative (if annual value, then year_start = year_end)
  co2          = NA, # CO2 level (amb, ele)
  nfert        = NA, # N fertilisation level (hi, lo). "ambient" is lo
  ozone        = NA, # ozone treatment (y, n). "ambient" is n
  burn         = NA, # burning treatment (y,n)
  water        = NA, # water addition treatment (y,n)
  warm         = NA, # warming (heat) treatment (y,n). "ambient" is n
  soil         = NA, # factor soil (category as given by specific experiment)
  myc          = NA, # mycorrhizal type
  species      = NA, # species used in experiment
  var          = NA, # variable name (ab, bb, tb, npp, ...)
  var_long     = NA, # long variable name as given in original file
  mean         = NA, # mean value of measurement(s)
  sd           = NA, # standard deviation of measurement(s)
  se           = NA, # standard error of measurement(s)
  N            = NA, # number of individual measurements if 'mean' is mean over multiple measurements, can be used as weight
  unit         = NA, # units in which mean, sd, and se are given
  fromdata     = NA  # data file from which data is read
  )

##------------------------------------------------------------
## DATA BY SARA VICCA ET AL. (GCME DATABASE)
## Separate columns are given for ambient and elevated (mean and SE/SD). 
## Experiment.Name provides information for what variable is manipulated (reflected by 'ambient' and 'elevated')
##------------------------------------------------------------
  ## Get measurement data
  data <- read.csv( "/alphadata01/bstocker/data/gcme/GCME_database_CSV/Data_tbl.csv")

  ## Get site information
  sites <- read.csv( "/alphadata01/bstocker/data/gcme/GCME_database_CSV/Sites_tbl.csv" )

  ## Get information about experiments available for each site
  experiments <- read.csv( "/alphadata01/bstocker/data/gcme/GCME_database_CSV/Experiments_tbl.csv" )

  ## Rename columns 
  data <- rename( data, 
    c(  
      "Experiment.Name"="exp_nam",
      "Data.type"="var_long"
      ) 
    )

  ## Replace blanks by underscores in Experiment.Name
  data$exp_nam <- gsub( " ", "_", data$exp_nam )

  ## Attach type of manipulation as separate column
  data$exp_type <-  sub( ".*_", "", data$exp_nam )

  ## Remove type of manipulation string suffix in experiment name ==> this is new standard name for all!!!
  data$exp_nam <- sapply( strsplit( data$exp_nam, split = "_"), function(v) {paste(rev(rev(v)[-1]), collapse ="_")} )

  ## year_start and year_end
  tmp <- as.character(data$Sampling.date)
  data$date <- tmp

  ## Standardise variable short-names
  data$var <- data$var_long
  data$var <- gsub( "soil C", "csoil", data$var )
  data$var <- gsub( "NPP", "npp", data$var )
  data$var <- gsub( "ANPP", "anpp", data$var )
  data$var <- gsub( "ANPP woody", "anppwood", data$var )

  data$var <- gsub( "total biomass", "totb", data$var )
  data$var <- gsub( "aboveground biomass", "ab", data$var )
  data$var <- gsub( "belowground biomass", "bb", data$var )
  data$var <- gsub( "leaf biomass", "leafb", data$var )
  data$var <- gsub( "leaf N", "leafn", data$var )

  data$var <- gsub( "coarse root biomass", "crootb", data$var )
  data$var <- gsub( "fine root biomass", "frootb", data$var )
  data$var <- gsub( "fine root production", "frootp", data$var )

  data$var <- gsub( "fine root respiration", "frootresp", data$var )
  data$var <- gsub( "fine root turnover", "frootto", data$var )

  data$var <- gsub( "microbial C", "micc", data$var )

  data$var <- gsub( "net N mineralization", "netnmin", data$var )


  for (idx in 1:dim(data)[1]){
  # for (idx in which( data$exp_nam=='BioCON' | data$exp_nam=='SwissFACE_lolium' )){
  # for (idx in which( data$exp_nam=='SwissFACE_lolium2' )){

    ## First conform original data array
    ## convert units
    if (as.character(data$Unit[idx])=="kg/m2"){
      data$Unit[idx]        <- "g/m2"
      data$ambient[idx]     <- data$ambient[idx] * 1e3
      data$ambient.Se[idx]  <- data$ambient.Se[idx] * 1e3
      data$ambient.Sd[idx]  <- data$ambient.Sd[idx] * 1e3
      data$elevated[idx]    <- data$elevated[idx] * 1e3
      data$elevated.Se[idx] <- data$elevated.Se[idx] * 1e3
      data$elevated.Sd[idx] <- data$elevated.Sd[idx] * 1e3
    }

    ## Sampling date
    if (grepl("-",as.character(data$Sampling.date[idx]))){
      ## For those data where sampling data is a range of years, split year_start and year_end
      data$date[idx] <- sub( "-.*", "", as.character(data$Sampling.date[idx]) )
    } else {
      ## For those data where sampling data is given as a date use only year of it
      data$date[idx] <- sub( ".*/", "", as.character(data$Sampling.date[idx]) )
    }  

    ## initialise new row
    newrow_amb <- init

    ## adopt some columns irrespective of manipulation
    newrow_amb$exp_nam    <- data$exp_nam[idx]
    newrow_amb$var        <- data$var[idx]
    newrow_amb$date <- data$date[idx]
    newrow_amb$unit       <- as.character( data$Unit[idx] )
    newrow_amb$var_long   <- data$Data.type[idx]
    newrow_amb$exp_type   <- data$exp_type[idx]
    newrow_amb$fromdata   <- "data/gcme/GCME_database_CSV/Data_tbl.csv"

    newrow_ele <- newrow_amb

    if (data$exp_type[idx]=="c"){
      
      ## ambient row
      newrow_amb$co2        <- "amb"
      newrow_amb$mean       <- data$ambient[idx]
      newrow_amb$sd         <- data$ambient.Sd[idx]
      newrow_amb$se         <- data$ambient.Se[idx]

      ## add row to gcme dataframe
      gcme <- rbind( gcme, newrow_amb )

      ## elevated row
      newrow_ele$co2        <- "ele"
      newrow_ele$mean       <- data$elevated[idx]
      newrow_ele$sd         <- data$elevated.Sd[idx]
      newrow_ele$se         <- data$elevated.Se[idx]

      ## add row to gcme dataframe
      gcme <- rbind( gcme, newrow_ele )

    } else if (data$exp_type[idx]=="f"){
      
      ## ambient row
      newrow_amb$nfert      <- "lo"
      newrow_amb$mean       <- data$ambient[idx]
      newrow_amb$sd         <- data$ambient.Sd[idx]
      newrow_amb$se         <- data$ambient.Se[idx]

      ## add row to gcme dataframe
      gcme <- rbind( gcme, newrow_amb )

      ## elevated row
      newrow_ele$nfert      <- "hi"
      newrow_ele$mean       <- data$elevated[idx]
      newrow_ele$sd         <- data$elevated.Sd[idx]
      newrow_ele$se         <- data$elevated.Se[idx]

      ## add row to gcme dataframe
      gcme <- rbind( gcme, newrow_ele )

    } else if (data$exp_type[idx]=="cf"){
      
      ## XXX ASSUMING THAT AMBIENT IS AMBIENT F (NFERT) BUT ELEVATED CO2 

      ## ambient row
      newrow_amb$nfert      <- "lo"
      newrow_amb$co2        <- "amb"
      newrow_amb$mean       <- data$ambient[idx]
      newrow_amb$sd         <- data$ambient.Sd[idx]
      newrow_amb$se         <- data$ambient.Se[idx]

      ## add row to gcme dataframe
      gcme <- rbind( gcme, newrow_amb )

      ## elevated row
      newrow_ele$nfert      <- "hi"
      newrow_ele$co2        <- "ele"
      newrow_ele$mean       <- data$elevated[idx]
      newrow_ele$sd         <- data$elevated.Sd[idx]
      newrow_ele$se         <- data$elevated.Se[idx]

      ## add row to gcme dataframe
      gcme <- rbind( gcme, newrow_ele )

      # ## apparently, there should be an experiment around for which manip is only "c" and one for which manip is only "f"
      # ## find them and fill complete their "co2" and "nfert" entries.
      # ## low N fert
      # idx_replace <- intersect( which(gcme$exp_nam==newrow_amb$exp_nam), which(gcme$exp_type=="c") ) # intersect( which(gcme$exp_nam==newrow$exp_nam), which(is.na(gcme$nfert)) )
      # # print(gcme[idx_replace,])
      # gcme$nfert[idx_replace]  <- "lo"
      # gcme$exp_type[idx_replace] <- "cf"

      # idx_replace <- intersect( which(gcme$exp_nam==newrow_amb$exp_nam), which(gcme$exp_type=="f") ) # intersect( which(gcme$exp_nam==newrow$exp_nam), which(is.na(gcme$co2)) )
      # # print(gcme[idx_replace,])
      # gcme$co2[idx_replace]  <- "amb"
      # gcme$exp_type[idx_replace] <- "cf"

    }

  }


##------------------------------------------------------------
## NUP / NPP DATA COLLECTED BY CESAR
## - statistics over replicates in each year and CO2 level (and N fert. level and species if available)
## - CO2 effect is based on means for each year, averaged over replicates (no statistics there) => one data point for each year
## - Final single value for CO2 effect is the mean over years (and standard deviation among years) 
##
## Get Nup data from file compiled and sent by Cesar. Identifyer "csr".
##------------------------------------------------------------
  gcme_csr_sum <- data.frame()
  gcme_csr <- data.frame()

  ## Get measurement data
  data <- read.csv( "/alphadata01/bstocker/data/face/data_raw/nup_new_cesar.csv", stringsAsFactors=FALSE )
  gcme_csr <- ingest_gcme( data, gcme_csr, init, "csr" )

  gcme_csr$exp_nam <- as.factor( gcme_csr$exp_nam )
  gcme_csr$date <- as.factor( gcme_csr$date )
  gcme_csr$co2 <- as.factor( gcme_csr$co2 )
  gcme_csr$nfert <- as.factor( gcme_csr$nfert )
  gcme_csr$species <- as.factor( gcme_csr$species )
  gcme_csr$var <- as.factor( gcme_csr$var )

  ##------------------------------------------------------------
  ## Statistics over replicates
  ## XXX USE OF SUMMARYSE() MAY BE AVOIDED. INSTEAD, DO AVERAGING
  ## BELOW ("GET PAIRS:...") WHEN MORE THAN 1 MEMBER IS FOUND. XXX
  ##------------------------------------------------------------  
  tmp <- summarySE( gcme_csr, measurevar="mean",  groupvars=c( "exp_nam", "year_start", "co2", "nfert", "species", "var", "myc" ), na.rm=TRUE )  ## sum over harvest for each treatment

  for (idx in 1:dim(tmp)[1]){

    ## initialise new row
    newrow <- init

    ## adopt some columns irrespective of manipulation
    newrow$exp_nam    <- as.character( tmp$exp_nam[idx] )
    newrow$date       <- as.numeric(as.character(tmp$date[idx]))
    newrow$co2        <- as.character(tmp$co2[idx])
    newrow$nfert      <- as.character(tmp$nfert[idx])
    newrow$species    <- as.character(tmp$species[idx])
    newrow$myc        <- as.character(tmp$myc[idx])
    newrow$var        <- as.character(tmp$var[idx])
    newrow$N          <- tmp$N[idx]
    newrow$mean       <- tmp$mean[idx]
    newrow$sd         <- tmp$sd[idx]
    newrow$se         <- tmp$se[idx]
    newrow$fromdata   <- "data/face/data_raw/nup_new_cesar.csv"

    gcme_csr_sum <- rbind( gcme_csr_sum, newrow )
  
  }

  ##------------------------------------------------------------
  ## Get pairs: amb and ele CO2 of corresponding experiment, year, variable (and species, nfert level if available)
  ##------------------------------------------------------------  
  gcme_csr_dco2 <- filter( gcme_csr_sum, co2=="amb" )
  for (idx in 1:dim(gcme_csr_dco2)[1]){
    ele <- filter( gcme_csr_sum, co2=="ele",
      exp_nam==gcme_csr_dco2$exp_nam[idx],
      year_start==gcme_csr_dco2$date[idx],
      var==gcme_csr_dco2$var[idx]
      )
    if (dim(ele)[1]>1){
      if (!is.na(ele$species[1])){
        ele <- filter( ele, species==gcme_csr_dco2$species[idx] )
      }
    }
    if (dim(ele)[1]>1){
      if (!is.na(ele$nfert[1])){
        ele <- filter( ele, nfert==gcme_csr_dco2$nfert[idx] )
      }
    }
    if (dim(ele)[1]==1){
      ## get response ratio
      gcme_csr_dco2$mean[idx] <- ele$mean / gcme_csr_dco2$mean[idx]
      gcme_csr_dco2$sd[idx]   <- NA
      gcme_csr_dco2$se[idx]   <- NA
    } else if (dim(ele)[1]==0) {
      print("PROBLEM: FOUND NO CORRESPONDENCE")
      print(paste("exp_nam:",gcme_csr_dco2$exp_nam[idx]))
      print(paste("year_start:",gcme_csr_dco2$date[idx]))
      print(paste("species:",gcme_csr_dco2$species[idx]))
      print(paste("var:",gcme_csr_dco2$var[idx]))
    } else {
      print("PROBLEM: FOUND MORE THAN ONE CORRESPONDENCE")
      print(paste("exp_nam:",gcme_csr_dco2$exp_nam[idx]))
      print(paste("year_start:",gcme_csr_dco2$date[idx]))
      print(paste("species:",gcme_csr_dco2$species[idx]))
      print(paste("var:",gcme_csr_dco2$var[idx]))
    }
  }


##------------------------------------------------------------
## VAN GROENIGEN 2014 DATA
## Data is usually given for each year but sometimes as average over multiple years. No uncertainty given.
## Each data point in the table has equal weight.
##
## Identifier "vgr".
##------------------------------------------------------------
  gcme_vgr <- data.frame()

  ## Get measurement data
  data <- read.csv( "/alphadata01/bstocker/data/face/soilc_groenigen14/face_groenigen14_sheet2.csv", stringsAsFactors=FALSE )
  gcme_vgr <- ingest_gcme( data, gcme_vgr, init, "vgr" )

  ##------------------------------------------------------------
  ## Create new data frame for CO2 response ratio
  ##------------------------------------------------------------
  gcme_vgr_dco2 <- get_gcme_dco2( gcme_vgr )


# XXX MAY COMBINE EQUALLY FORMATTED DATA.FRAMES (gcme_csr + gcme_vgr and gcme_csr_dco2 + gcme_vgr_dco2) HERE.


##------------------------------------------------------------
## ANALYSIS
##------------------------------------------------------------
  investment_dco2 <- filter( gcme_vgr_dco2, var=="frootm" | var=="rootm" | var=="blittp" | var=="soilp" | var=="bnpp"  )  # for 'var' short name, see function 'get_var()'
  investment_dco2_am <- filter( investment_dco2, myc=="am"  )
  investment_dco2_ecm <- filter( investment_dco2, myc=="ecm"  )

  return_dco2_am     <- filter( gcme_csr_dco2, var=="nup" & myc=="am" )
  return_dco2_ecm    <- filter( gcme_csr_dco2, var=="nup" & myc=="ecm" )

  print( paste( "mean investment effect, AM:     ",  mean( investment_dco2_am$mean, na.rm=TRUE ), "+/-", sd( investment_dco2_am$mean, na.rm=TRUE ) ) )
  print( paste( "mean investment effect, ECM:     ", mean( investment_dco2_ecm$mean, na.rm=TRUE ), "+/-", sd( investment_dco2_ecm$mean, na.rm=TRUE ) ) )

  print( paste( "mean return effect (N uptake), AM:     ",  mean( return_dco2_am$mean, na.rm=TRUE ), "+/-", sd( return_dco2_am$mean, na.rm=TRUE ) ) )
  print( paste( "mean return effect (N uptake), ECM:     ", mean( return_dco2_ecm$mean, na.rm=TRUE ), "+/-", sd( return_dco2_ecm$mean, na.rm=TRUE ) ) )


##------------------------------------------------------------
## write to file
##------------------------------------------------------------
write.csv( gcme, file="face_data_unified.csv", row.names=FALSE )


