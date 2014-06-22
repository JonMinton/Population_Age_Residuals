# Run some of the existing scripts and analyses using the newer data : use this to produce the 
# derived data 

require(xlsx)
require (plyr)
require(reshape2)
require(ggplot2)
require(lattice)
require(repmis)



# require("xlsx")
# Country_Codes <- read.xlsx(
#     file="Data/CountryCodes.xlsx",
#     sheetName="Country_Codes"
#     )
# 


source("Scripts/Functions.r")
source("Scripts/Make_Derived_Data_Function.R")
source("Scripts/Script_for_Producing_Raw_Analyses.R")


load("Data/RObj/Expectations_and_Residuals.RData")

names(Outfile$expectations$male) <- Country.Codes[,1]
names(Outfile$expectations$female) <- Country.Codes[,1]
names(Outfile$expectations$total) <- Country.Codes[,1]
names(Outfile$residuals$male) <- Country.Codes[,1]
names(Outfile$residuals$female) <- Country.Codes[,1]
names(Outfile$residuals$total) <- Country.Codes[,1]


Europe_Indicators <- c(
    2,  3,  4,  7,  9,  11, 13, 14, 15, 16,
    18, 19, 21, 23, 24, 25, 28, 30, 31, 32,
    33, 34, 38, 39, 41, 42, 43
)

Outlist_subset <- list(
    expectations=list(
        male=Outlist$expectations$male[Europe_Indicators],
        female=Outlist$expectations$female[Europe_Indicators],
        total=Outlist$expectations$total[Europe_Indicators]
    ),
    residuals=list(
        male=Outlist$residuals$male[Europe_Indicators],
        female=Outlist$residuals$female[Europe_Indicators],
        total=Outlist$residuals$total[Europe_Indicators]
    )
)

