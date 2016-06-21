##------------------------------------------------------------
## This script unifies data from FACE and other global change 
## manipulation experiments into a nice consistent dataframe.
## One row for every data point (variable, year).
## ==> not multiple variable in separate columns and same row!
## standard variable names are
## ab: aboveground biomass
## bb: belowground biomass
## pcfroot : fine root production
## csoil : soil C
## totb  : total biomass
##------------------------------------------------------------
library(plyr)
library(dplyr)
source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/summarySE.R")

##------------------------------------------------------------
## Function returns dataframe holding data for CO2 response
## ratio, otherwise identical format as original dataframe.
## Length of first dimension of response ratio dataframe should
## be half of that of original data (for CO2 manipulation exp.)
##------------------------------------------------------------
getdf_dco2 <- function( df ){
  df_dco2 <- filter( df, co2=="ele" )
  for (idx in 1:dim(df_dco2)[1]){
    ## find corresponding ambient
    amb <- filter( df, co2=="amb", dataid==df_dco2$dataid[idx] )
    if (dim(amb)[1]==1) {
      df_dco2$mean[idx] <- ( df_dco2$mean[idx] / amb$mean )
    } else {
      print("PROBLEM: FOUND MORE THAN 1 CORRESPONDENCE")
    }
  }
  return( df_dco2 )
}

## Initialise data frame
init <- data.frame( 
  exp_nam      = NA, # standardised experiment name
  exp_type     = NA, # experiment type (c, cf, ...)
  dataid       = NA, # data point identifier (two identical ones where low and ambient are given in same row in original data)
  year_start   = NA, # start year of period for which experiment is representative (if annual value, then year_start = year_end)
  year_end     = NA, # end year of period for which experiment is representative (if annual value, then year_start = year_end)
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
gcme <- data.frame()

gcme_vgr <- data.frame()  # xxx try: haven't been able to unify data yet

##------------------------------------------------------------
## Get data from Sara Vicca's database
## Separate columns are given for ambient and elevated (mean and SE/SD). 
## Experiment.Name provides information for what variable is manipulated (reflected by 'ambient' and 'elevated')
##------------------------------------------------------------
  # ## Get measurement data
  # data <- read.csv( "/alphadata01/bstocker/data/gcme/GCME_database_CSV/Data_tbl.csv")


  # ## Get site information
  # sites <- read.csv( "/alphadata01/bstocker/data/gcme/GCME_database_CSV/Sites_tbl.csv" )

  # ## Get information about experiments available for each site
  # experiments <- read.csv( "/alphadata01/bstocker/data/gcme/GCME_database_CSV/Experiments_tbl.csv" )

  # ## Rename columns 
  # data <- rename( data, 
  #   c(  
  #     "Experiment.Name"="exp_nam",
  #     "Data.type"="var_long"
  #     ) 
  #   )

  # ## Replace blanks by underscores in Experiment.Name
  # data$exp_nam <- gsub( " ", "_", data$exp_nam )

  # ## Attach type of manipulation as separate column
  # data$exp_type <-  sub( ".*_", "", data$exp_nam )

  # ## Remove type of manipulation string suffix in experiment name ==> this is new standard name for all!!!
  # data$exp_nam <- sapply( strsplit( data$exp_nam, split = "_"), function(v) {paste(rev(rev(v)[-1]), collapse ="_")} )

  # ## year_start and year_end
  # tmp <- as.character(data$Sampling.date)
  # data$year_end   <- tmp
  # data$year_start <- tmp

  # ## Standardise variable short-names
  # data$var <- data$var_long
  # data$var <- gsub( "soil C", "csoil", data$var )
  # data$var <- gsub( "NPP", "npp", data$var )
  # data$var <- gsub( "ANPP", "anpp", data$var )
  # data$var <- gsub( "ANPP woody", "anppwood", data$var )

  # data$var <- gsub( "total biomass", "totb", data$var )
  # data$var <- gsub( "aboveground biomass", "ab", data$var )
  # data$var <- gsub( "belowground biomass", "bb", data$var )
  # data$var <- gsub( "leaf biomass", "leafb", data$var )
  # data$var <- gsub( "leaf N", "leafn", data$var )

  # data$var <- gsub( "coarse root biomass", "crootb", data$var )
  # data$var <- gsub( "fine root biomass", "frootb", data$var )
  # data$var <- gsub( "fine root production", "frootp", data$var )

  # data$var <- gsub( "fine root respiration", "frootresp", data$var )
  # data$var <- gsub( "fine root turnover", "frootto", data$var )

  # data$var <- gsub( "microbial C", "micc", data$var )

  # data$var <- gsub( "net N mineralization", "netnmin", data$var )


  # for (idx in 1:dim(data)[1]){
  # # for (idx in which( data$exp_nam=='BioCON' | data$exp_nam=='SwissFACE_lolium' )){
  # # for (idx in which( data$exp_nam=='SwissFACE_lolium2' )){

  #   ## First conform original data array
  #   ## convert units
  #   if (as.character(data$Unit[idx])=="kg/m2"){
  #     data$Unit[idx]        <- "g/m2"
  #     data$ambient[idx]     <- data$ambient[idx] * 1e3
  #     data$ambient.Se[idx]  <- data$ambient.Se[idx] * 1e3
  #     data$ambient.Sd[idx]  <- data$ambient.Sd[idx] * 1e3
  #     data$elevated[idx]    <- data$elevated[idx] * 1e3
  #     data$elevated.Se[idx] <- data$elevated.Se[idx] * 1e3
  #     data$elevated.Sd[idx] <- data$elevated.Sd[idx] * 1e3
  #   }

  #   ## Sampling date
  #   if (grepl("-",as.character(data$Sampling.date[idx]))){
  #     ## For those data where sampling data is a range of years, split year_start and year_end
  #     data$year_end[idx]   <- sub( ".*-", "", as.character(data$Sampling.date[idx]))
  #     data$year_start[idx] <- sub( "-.*", "", as.character(data$Sampling.date[idx]) )
  #   } else {
  #     ## For those data where sampling data is given as a date use only year of it
  #     data$year_start[idx] <- sub( ".*/", "", as.character(data$Sampling.date[idx]) )
  #     data$year_end[idx]   <- sub( ".*/", "", as.character(data$Sampling.date[idx]) )
  #   }  

  #   ## initialise new row
  #   newrow_amb <- init

  #   ## adopt some columns irrespective of manipulation
  #   newrow_amb$exp_nam    <- data$exp_nam[idx]
  #   newrow_amb$var        <- data$var[idx]
  #   newrow_amb$year_start <- data$year_start[idx]
  #   newrow_amb$year_end   <- data$year_end[idx]
  #   newrow_amb$unit       <- as.character( data$Unit[idx] )
  #   newrow_amb$var_long   <- data$Data.type[idx]
  #   newrow_amb$exp_type   <- data$exp_type[idx]
  #   newrow_amb$fromdata   <- "data/gcme/GCME_database_CSV/Data_tbl.csv"

  #   newrow_ele <- newrow_amb

  #   if (data$exp_type[idx]=="c"){
      
  #     ## ambient row
  #     newrow_amb$co2        <- "amb"
  #     newrow_amb$mean       <- data$ambient[idx]
  #     newrow_amb$sd         <- data$ambient.Sd[idx]
  #     newrow_amb$se         <- data$ambient.Se[idx]

  #     ## add row to gcme dataframe
  #     gcme <- rbind( gcme, newrow_amb )

  #     ## elevated row
  #     newrow_ele$co2        <- "ele"
  #     newrow_ele$mean       <- data$elevated[idx]
  #     newrow_ele$sd         <- data$elevated.Sd[idx]
  #     newrow_ele$se         <- data$elevated.Se[idx]

  #     ## add row to gcme dataframe
  #     gcme <- rbind( gcme, newrow_ele )

  #   } else if (data$exp_type[idx]=="f"){
      
  #     ## ambient row
  #     newrow_amb$nfert      <- "lo"
  #     newrow_amb$mean       <- data$ambient[idx]
  #     newrow_amb$sd         <- data$ambient.Sd[idx]
  #     newrow_amb$se         <- data$ambient.Se[idx]

  #     ## add row to gcme dataframe
  #     gcme <- rbind( gcme, newrow_amb )

  #     ## elevated row
  #     newrow_ele$nfert      <- "hi"
  #     newrow_ele$mean       <- data$elevated[idx]
  #     newrow_ele$sd         <- data$elevated.Sd[idx]
  #     newrow_ele$se         <- data$elevated.Se[idx]

  #     ## add row to gcme dataframe
  #     gcme <- rbind( gcme, newrow_ele )

  #   } else if (data$exp_type[idx]=="cf"){
      
  #     ## XXX ASSUMING THAT AMBIENT IS AMBIENT F (NFERT) BUT ELEVATED CO2 

  #     ## ambient row
  #     newrow_amb$nfert      <- "lo"
  #     newrow_amb$co2        <- "amb"
  #     newrow_amb$mean       <- data$ambient[idx]
  #     newrow_amb$sd         <- data$ambient.Sd[idx]
  #     newrow_amb$se         <- data$ambient.Se[idx]

  #     ## add row to gcme dataframe
  #     gcme <- rbind( gcme, newrow_amb )

  #     ## elevated row
  #     newrow_ele$nfert      <- "hi"
  #     newrow_ele$co2        <- "ele"
  #     newrow_ele$mean       <- data$elevated[idx]
  #     newrow_ele$sd         <- data$elevated.Sd[idx]
  #     newrow_ele$se         <- data$elevated.Se[idx]

  #     ## add row to gcme dataframe
  #     gcme <- rbind( gcme, newrow_ele )

  #     # ## apparently, there should be an experiment around for which manip is only "c" and one for which manip is only "f"
  #     # ## find them and fill complete their "co2" and "nfert" entries.
  #     # ## low N fert
  #     # idx_replace <- intersect( which(gcme$exp_nam==newrow_amb$exp_nam), which(gcme$exp_type=="c") ) # intersect( which(gcme$exp_nam==newrow$exp_nam), which(is.na(gcme$nfert)) )
  #     # # print(gcme[idx_replace,])
  #     # gcme$nfert[idx_replace]  <- "lo"
  #     # gcme$exp_type[idx_replace] <- "cf"

  #     # idx_replace <- intersect( which(gcme$exp_nam==newrow_amb$exp_nam), which(gcme$exp_type=="f") ) # intersect( which(gcme$exp_nam==newrow$exp_nam), which(is.na(gcme$co2)) )
  #     # # print(gcme[idx_replace,])
  #     # gcme$co2[idx_replace]  <- "amb"
  #     # gcme$exp_type[idx_replace] <- "cf"

  #   }

  # }


##------------------------------------------------------------
## Get Nup data from file compiled and sent by Cesar. Identifyer "csr"
##------------------------------------------------------------
  # ## Get measurement data
  # data <- read.csv( 
  #   "/alphadata01/bstocker/data/face/unified/nup_new_cesar.csv",
  #   colClasses=c( "YEAR"="factor", "CO2"="factor", "NFERT"="factor", "SPECIES"="factor" )
  #   )

  # ## Rename columns 
  # data <- rename( data, 
  #   c(  
  #     "SITE"="exp_nam",
  #     "CO2"="co2",
  #     "Nup"="nup",
  #     "NPP"="npp",
  #     "NFERT"="nfert"
  #     ) 
  #   )

  # ## Standardise experiment names
  # data$exp_nam <- gsub( "ORNL", "ORNL_FACE", data$exp_nam )
  # data$exp_nam <- gsub( "DUKE", "DukeFACE", data$exp_nam ) # MISSING IN GROENIGEN DATA
  # data$exp_nam <- gsub( "ASPEN", "FACTSII", data$exp_nam ) # MISSING IN GROENIGEN DATA
  # data$exp_nam <- gsub( "BioCON", "BioCON", data$exp_nam ) # MISSING IN GROENIGEN DATA

  # ## Standardise treatment codes
  # data$co2 <- gsub( "elev", "ele", data$co2 )
  # data$co2 <- gsub( "A", "amb", data$co2 )
  # data$co2 <- gsub( "E", "ele", data$co2 )
  # data$co2 <- gsub( "Camb", "amb", data$co2 )
  # data$co2 <- gsub( "Cenrich", "ele", data$co2 )
  # data$co2 <- gsub( "elev", "ele", data$co2 )

  # data$SPECIES <- as.character(data$SPECIES)
  # data$SPECIES <- ifelse( data$SPECIES=="", NA, data$SPECIES )

  # # ## add data point identifiyer for associating the right ambient and elevated
  # # data$dataid <- paste( "csr", seq(1, dim(data)[1], 1), sep="" )

  # ## replace empty entries by NA
  # data$nfert[ which(data$nfert=="") ] <- NA

  # for (idx in 1:dim(data)[1]){

  #   ## XXX missing units in file

  #   ## N uptake 
  #   newrow_nup            <- init
  #   newrow_nup$var        <- "nup"
  #   newrow_nup$mean       <- data$nup[idx]
  #   newrow_nup$exp_nam    <- data$exp_nam[idx]
  #   newrow_nup$year_start <- data$YEAR[idx]
  #   newrow_nup$year_end   <- data$YEAR[idx]
  #   newrow_nup$unit       <- "missing"
  #   newrow_nup$var_long   <- "N uptake"
  #   newrow_nup$fromdata   <- "data/face/unified/nup_new_cesar.csv"
  #   newrow_nup$co2        <- data$co2[idx]
  #   newrow_nup$nfert      <- data$nfert[idx]
  #   newrow_nup$exp_type   <- ifelse( is.na(data$nfert[idx]), "c", "cf" )
  #   newrow_nup$species    <- as.character(data$SPECIES[idx])
  #   newrow_nup$ozone      <- data$ozone[idx]
  #   newrow_nup$burn       <- data$burn[idx]
  #   newrow_nup$soil       <- data$soil[idx]
  #   newrow_nup$warm       <- data$heat[idx]
  #   newrow_nup$N          <- 1

  #   ## add row to gcme dataframe
  #   gcme <- rbind( gcme, newrow_nup )

  #   ## NPP
  #   newrow_npp            <- newrow_nup
  #   newrow_npp$var        <- "npp"
  #   newrow_npp$mean       <- data$npp[idx]
  #   newrow_npp$unit       <- "missing"
  #   newrow_npp$var_long   <- "NPP"

  #   ## add row to gcme dataframe
  #   gcme <- rbind( gcme, newrow_npp )

  # }

  # ##------------------------------------------------------------
  # ## Statistics over replicates
  # ##------------------------------------------------------------  
  # gcme_sum <- summarySE( gcme, measurevar="mean",  groupvars=c( "exp_nam", "exp_type", "year_start", "co2", "nfert", "species", "var" ), na.rm=TRUE )  ## sum over harvest for each treatment

  # ##------------------------------------------------------------
  # ## Get pairs: amb and ele CO2 of corresponding experiment, year, variable (and species, nfert level if available)
  # ##------------------------------------------------------------  
  # gcme_dco2 <- filter( gcme_sum, co2=="amb" )
  # for (idx in 1:dim(gcme_dco2)[1]){
  #   ele <- filter( gcme_sum, co2=="ele",
  #     exp_nam==gcme_dco2$exp_nam[idx],
  #     year_start==gcme_dco2$year_start[idx],
  #     var==gcme_dco2$var[idx]
  #     )
  #   if (dim(ele)[1]>1){
  #     if (!is.na(ele$species[1])){
  #       ele <- filter( ele, species==gcme_dco2$species[idx] )
  #     }
  #   }
  #   if (dim(ele)[1]>1){
  #     if (!is.na(ele$nfert[1])){
  #       ele <- filter( ele, nfert==gcme_dco2$nfert[idx] )
  #     }
  #   }
  #   if (dim(ele)[1]==1){
  #     ## get response ratio
  #     gcme_dco2$mean[idx] <- ele$mean / gcme_dco2$mean[idx]
  #     gcme_dco2$sd[idx]   <- NA
  #     gcme_dco2$sum[idx]   <- NA
  #     gcme_dco2$min[idx]   <- NA
  #     gcme_dco2$max[idx]   <- NA
  #     gcme_dco2$se[idx]   <- NA
  #     gcme_dco2$ci[idx]   <- NA
  #   } else if (dim(ele)[1]==0) {
  #     print("PROBLEM: FOUND NO CORRESPONDENCE")
  #     print(paste("exp_nam:",gcme_dco2$exp_nam[idx]))
  #     print(paste("year_start:",gcme_dco2$year_start[idx]))
  #     print(paste("species:",gcme_dco2$species[idx]))
  #     print(paste("var:",gcme_dco2$var[idx]))
  #   } else {
  #     print("PROBLEM: FOUND MORE THAN ONE CORRESPONDENCE")
  #     print(paste("exp_nam:",gcme_dco2$exp_nam[idx]))
  #     print(paste("year_start:",gcme_dco2$year_start[idx]))
  #     print(paste("species:",gcme_dco2$species[idx]))
  #     print(paste("var:",gcme_dco2$var[idx]))
  #   }
  # }

  # ## add columns to conform
  # gcme_dco2$year_end <- gcme_dco2$year_start
  # gcme_dco2$ozone    <- rep( NA, dim(gcme_dco2)[1] )
  # gcme_dco2$burn    <- rep( NA, dim(gcme_dco2)[1] )
  # gcme_dco2$water   <- rep( NA, dim(gcme_dco2)[1] )
  # gcme_dco2$soil    <- rep( NA, dim(gcme_dco2)[1] )
  # gcme_dco2$warm    <- rep( NA, dim(gcme_dco2)[1] )
  # gcme_dco2$myc     <- rep( NA, dim(gcme_dco2)[1] )
  # gcme_dco2$var_long  <- rep( NA, dim(gcme_dco2)[1] )
  # gcme_dco2$sum     <- NULL
  # gcme_dco2$min     <- NULL
  # gcme_dco2$max     <- NULL
  # gcme_dco2$se      <- NULL
  # gcme_dco2$unit    <- rep( NA, dim(gcme_dco2)[1] )
  # gcme_dco2$fromdata <- rep( "data/face/unified/nup_new_cesar.csv", dim(gcme_dco2)[1] )


  # ##------------------------------------------------------------
  # ## Create new data frame for CO2 response ratio XXX something is not working here XXX
  # ##------------------------------------------------------------
  # gcme_dco2 <- getdf_dco2( gcme )


##------------------------------------------------------------
## Get data Van Groenigen et al., 2014. Identifier "vgr"
##------------------------------------------------------------
  ## Get measurement data
  data <- read.csv( "/alphadata01/bstocker/data/face/unified/face_groenigen14_sheet2.csv", stringsAsFactors=FALSE )

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
    exp_nam <- gsub( "ORNL FACE", "ORNL", exp_nam )
    exp_nam <- gsub( "FACTS II", "FACTSII", exp_nam )
    return( exp_nam )    
  }

  ingest_gcme <- function( data, gcme, init, src ){

    ## add data point identifiyer for associating the right ambient and elevated
    data$dataid <- paste( src, seq(1, dim(data)[1], 1), sep="" )

    ## loop through rows of original data frame
    for (idx in 1:dim(data)[1]){

      ## initialise new row
      newrow_amb <- init

      if (src=="vgr"){
        ## Van Groenigen dataset

        ## adopt some columns irrespective of manipulation
        newrow_amb$exp_nam    <- get_exp_nam( data$Experiment[idx] )
        newrow_amb$exp_type   <- ifelse( is.na(data$nfert[idx]), "c", "cf" )
        newrow_amb$dataid     <- data$dataid[idx]
        newrow_amb$year_start <- data$Sample.date[idx]
        newrow_amb$year_end   <- data$Sample.date[idx]
        
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
        newrow_amb$fromdata   <- "data/face/unified/face_groenigen14_sheet2.csv"

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
    }
    return( gcme )
  }

  gcme <- ingest_gcme( data, gcme, init, "vgr" )

  # ## Standardise experiment names
  # ## XXX Duke missing in Groenigen data
  # data$exp_nam <- gsub( "ORNL FACE", "ORNL", data$exp_nam )
  # data$exp_nam <- gsub( "FACTS II", "FACTSII", data$exp_nam )

  # ## Standardise variable short-names
  # data$var <- data$var_long
  # data$var <- ifelse( data$var=="aboveground biomass (corn)", "ab", data$var )
  # data$var <- ifelse( data$var=="biomass (shoot + root)", "b", data$var )
  # data$var <- ifelse( data$var=="cummulative root litter input", "blittp", data$var )
  # data$var <- ifelse( data$var=="total soil C input", "soilp", data$var )
  # data$var <- ifelse( data$var=="belowground NPP (soybean)", "bnpp", data$var )
  # data$var <- ifelse( data$var=="aboveground NPP (soybean)", "anpp", data$var )
  # data$var <- ifelse( data$var=="aboveground NPP", "anpp", data$var )
  # data$var <- ifelse( data$var=="NPP", "npp", data$var )
  # data$var <- ifelse( data$var=="aboveground yield", "yld", data$var )
  # data$var <- ifelse( data$var=="yield", "yld", data$var )
  # data$var <- ifelse( data$var=="annual yield", "yld", data$var )
  # data$var <- ifelse( data$var=="peak biomass (shoot + root)", "b", data$var )
  # data$var <- ifelse( data$var=="litter fall (average)", "littp", data$var )
  # data$var <- ifelse( data$var=="total litterfall", "littp", data$var )
  # data$var <- ifelse( data$var=="litterfall", "littp", data$var )
  # data$var <- ifelse( data$var=="litter fall", "littp", data$var )
  # data$var <- ifelse( data$var=="fine root biomass (average)", "frootm", data$var )
  # data$var <- ifelse( data$var=="aboveground biomass", "ab", data$var )
  # data$var <- ifelse( data$var=="fine root biomass", "frootm", data$var )
  # data$var <- ifelse( data$var=="root biomass", "rootm", data$var )

  # ## units as characters
  # data$Unit <- as.character(data$Unit)

  # ## convert factors to numerics
  # data$aCO2 <- as.numeric(as.character(data$aCO2))
  # data$eCO2 <- as.numeric(as.character(data$eCO2))

  # data$nfert <- as.character(data$nfert)
  # data$species <- as.character(data$species)
  # data$ozone <- as.character(data$ozone)
  # data$burn <- as.character(data$burn)
  # data$soil <- as.character(data$soil)
  # data$heat <- as.character(data$heat)

  # ## replace empty entries by NA
  # data$nfert[which(data$nfert=="")] <- NA
  # data$species[which(data$species=="")] <- NA
  # data$ozone[which(data$ozone=="")] <- NA
  # data$burn[which(data$burn=="")] <- NA
  # data$soil[which(data$soil=="")] <- NA
  # data$heat[which(data$heat=="")] <- NA

  # ## add data point identifiyer for associating the right ambient and elevated
  # data$dataid <- paste( "vgr", seq(1, dim(data)[1], 1), sep="" )

  # for (idx in 1:dim(data)[1]){

  #   ## First conform original data array
  #   ## convert units
  #   if (data$Unit[idx]=="kg/m2"){
  #     data$Unit[idx] <- "g/m2"
  #     data$aCO2[idx] <- data$aCO2[idx] * 1e3
  #     data$eCO2[idx] <- data$eCO2[idx] * 1e3
  #   }

  #   ## initialise new row
  #   newrow_amb <- init

  #   ## adopt some columns irrespective of manipulation
  #   newrow_amb$exp_nam    <- data$exp_nam[idx]
  #   newrow_amb$dataid     <- data$dataid[idx]
  #   newrow_amb$var        <- data$var[idx]
  #   newrow_amb$var_long   <- data$var_long[idx]
  #   newrow_amb$year_start <- data$sample_date[idx]
  #   newrow_amb$year_end   <- data$sample_date[idx]
  #   newrow_amb$nfert      <- data$nfert[idx]
  #   newrow_amb$species    <- data$species[idx]
  #   newrow_amb$ozone      <- data$ozone[idx]
  #   newrow_amb$burn       <- data$burn[idx]
  #   newrow_amb$soil       <- data$soil[idx]
  #   newrow_amb$warm       <- data$heat[idx]
  #   newrow_amb$unit       <- data$Unit[idx]
  #   newrow_amb$exp_type   <- ifelse( is.na(data$nfert[idx]), "c", "cf" )
  #   newrow_amb$fromdata   <- "data/face/unified/face_groenigen14_sheet2.csv"

  #   ## initialise additional rows
  #   newrow_ele  <- newrow_amb
  #   newrow_dco2 <- newrow_amb
      
  #   ## ambient row and add row to gcme dataframe
  #   newrow_amb$co2        <- "amb"
  #   newrow_amb$mean       <- data$aCO2[idx]
  #   gcme_vgr <- rbind( gcme_vgr, newrow_amb )

  #   ## elevated row and add row to gcme_vgr dataframe
  #   newrow_ele$co2        <- "ele"
  #   newrow_ele$mean       <- data$eCO2[idx]
  #   gcme_vgr <- rbind( gcme_vgr, newrow_ele )

  # }

  ##------------------------------------------------------------
  ## Create new data frame for CO2 response ratio XXX something is not working here XXX
  ##------------------------------------------------------------
  gcme_vgr_dco2 <- getdf_dco2( gcme )


##------------------------------------------------------------
## ANALYSIS
##------------------------------------------------------------
  # investment_dco2 <- filter( gcme_vgr_dco2, var=="frootm" | var=="rootm" | var=="blittp" | var=="soilp" | var=="bnpp" )
  # return_dco2     <- filter( gcme_dco2, var=="nup" )

  # print( paste( "mean investment effect: ", mean( investment_dco2$mean, na.rm=TRUE ) ) )
  # print( paste( "mean return effect:     ", mean( return_dco2$mean, na.rm=TRUE ) ) )

# ##------------------------------------------------------------
# ## write to file
# ##------------------------------------------------------------
# write.csv( gcme, file="face_data_unified.csv", row.names=FALSE )


