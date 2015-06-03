rm(list=ls())

# Suggested in: 
#https://github.com/skardhamar/rga/issues/6
#options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

# TO DO ;
# 1) Re-do analyses with Switzerland as part of Europe. 
# 2) Incorporate composite graph



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
    "RColorBrewer",
    "latticeExtra",
    "dplyr"
    )
  )





############################################################################################################
############################################################################################################
source("Scripts/load_tidy_data.r")

#source("scripts/analyses_with_tidy_data.r")
