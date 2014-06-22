rm(list=ls())


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
    "ggplot2"
    )
  )



############################################################################################################
############################################################################################################


source("Scripts/Functions.r")
source("Scripts/Script_for_Producing_Raw_Analyses.R")

############################################################################################################
############################################################################################################


load("Data/RObj/Expectations_and_Residuals.RData")

country_codes <- read.csv("Data/HMD/country_codes__new.csv", stringsAsFactors=FALSE)

europe_indicators <- country_codes$short[country_codes$europe==T]  

names(Outlist$expectations$male) <- country_codes$short
names(Outlist$expectations$female) <- country_codes$short
names(Outlist$expectations$total) <- country_codes$short
names(Outlist$residuals$male) <- country_codes$short
names(Outlist$residuals$female) <- country_codes$short
names(Outlist$residuals$total) <- country_codes$short


eu_res_male <- Outlist$residuals$male[europe_indicators]
eu_res_female <- Outlist$residuals$female[europe_indicators]
eu_res_total <- Outlist$residuals$total[europe_indicators]

# I want the following long format dataframe

# identifiers
#  country, gender, age, year
# value: quantity

fm <- function(x){
  out <- melt(x, varnames=c("age", "year"), value.name="residual")
  out <- data.frame(out, sex="male")
  return(out)            
}

ff <- function(x){
  out <- melt(x, varnames=c("age", "year"), value.name="residual")
  out <- data.frame(out, sex="female")
  return(out)            
}

ft <- function(x){
  out <- melt(x, varnames=c("age", "year"), value.name="residual")
  out <- data.frame(out, sex="total")
  return(out)            
}

dta_long_male <- ldply(
  eu_res_male,
  fm
  )

names(dta_long_male)[1] <- "country"

dta_long_female <- ldply(
  eu_res_female,
  ff
  )

names(dta_long_female)[1] <- "country"

dta_long_total <- ldply(
  eu_res_total,
  ft
  )

names(dta_long_total)[1] <- "country"

dta_long <- rbind(dta_long_male, dta_long_female, dta_long_total)

rm(dta_long_male, dta_long_female, dta_long_total)

dta_long2 <- melt(dta_long, id.var=c(1,2,3,5))
dta_wide <- dcast(dta_long2, country + age + year ~ sex)

# g <- ggplot(subset(dta_wide, year > 1995)) + aes(x=year, y=male, group=age)

# dta_wide2 <- data.frame(dta_long2, age_group=cut_interval(dta_wide$age, 10))


g <- ggplot(subset(dta_long2, year > 1995)) + aes(x=year, y=value, group=age, colour=age)
g + geom_line() + facet_grid(country ~ sex) + coord_flip()

ggsave("Figures/All_Lattice.png")


### What I want: 
# for each country,
 # for years from 1990 onwards

# relationship between residuals and time

