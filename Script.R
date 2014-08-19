rm(list=ls())

# Suggested in: 
#https://github.com/skardhamar/rga/issues/6
#options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

# TO DO ;
# residual is expected - actual not the other way around. [DONE]
# Finally correct this in the exp_14,, exp_14_all, and exp_eu and exp_eu_all counts
# Once I've done this make sure any little 'tricks' applied later are removed to avoid unfixing!

# Change for

###########################################################################################################
###########################################################################################################
# Run some of the existing scripts and analyses using the newer data : use this to produce the 
# derived data 

source("Scripts/LoadPackages.R")

RequiredPackages(
  c(
    "xlsx",
    "plyr",
    "reshape2",
    "lattice",
    "repmis",
    "RCurl",
    "devtools",
    "httr",
    "digest",
    "ggplot2",
    "stringr",
    "car",
    "RColorBrewer"
    )
  )





############################################################################################################
############################################################################################################
source("Scripts/load_tidy_data.r")

#source("scripts/analyses_with_tidy_data.r")
