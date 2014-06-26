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

fn <- function(x, label=NA, sex_=NA){
  out <- melt(x, varnames=c("age", "year"), value.name=label)
  out <- data.frame(out, sex=sex_)
  return(out)            
}


dta_long_male <- ldply(
  eu_res_male,
  fn,
  label="residual",
  sex_="male"
  )

dta_long_female <- ldply(
  eu_res_female,
  fn, 
  label="residual",
  sex_="female"
  )

dta_long_total <- ldply(
  eu_res_total,
  fn,
  label="residual",
  sex_="total"
  )

names(dta_long_male)[1] <- "country"
names(dta_long_female)[1] <- "country"
names(dta_long_total)[1] <- "country"


dta_long <- rbind(dta_long_male, dta_long_female, dta_long_total)

rm(dta_long_male, dta_long_female, dta_long_total)

dta_long2 <- melt(dta_long, id.var=c("country", "age", "year", "sex"))
dta_wide <- dcast(dta_long2, country + age + year ~ sex)


dta_res_tidy <- dta_long2


####################################################################################

eu_exp_male <- Outlist$expectations$male[europe_indicators]
eu_exp_female <- Outlist$expectations$female[europe_indicators]
eu_exp_total <- Outlist$expectations$total[europe_indicators]

dta_long_male <- ldply(
  eu_exp_male,
  fn,
  label="expectation",
  sex_="male"
)

dta_long_female <- ldply(
  eu_exp_female,
  fn, 
  label="expectation",
  sex_="female"
)

dta_long_total <- ldply(
  eu_exp_total,
  fn,
  label="expectation",
  sex_="total"
)


names(dta_long_male)[1] <- "country"
names(dta_long_female)[1] <- "country"
names(dta_long_total)[1] <- "country"

dta_long <- rbind(dta_long_male, dta_long_female, dta_long_total)

rm(dta_long_male, dta_long_female, dta_long_total)

dta_long2 <- melt(dta_long, id.var=c("country", "age", "year", "sex"))

dta_tidy_long <- rbind(dta_res_tidy, dta_long2)

dta_tidy <- dcast(dta_tidy_long, country + age + year + sex ~ variable)
# g <- ggplot(subset(dta_wide, year > 1995)) + aes(x=year, y=male, group=age)
dta_tidy <- mutate(dta_tidy, residual_proportion=residual/expectation)

save(dta_tidy, file="Data/RObj/Tidy_Data.RData")


####################
## contour plot
load("Data/RObj/Tidy_Data.RData")

g <- ggplot(subset(dta_tidy, year > 2000 & sex=="male")) + aes(x=year, y=age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap ( ~ country, nrow=4)
g3 <- g2 + scale_fill_gradientn(
  colours=c("blue", "white", "red"), limits=c(-0.15, 0.15)
  )
print (g3)

ggsave("Figures/Tile_Male.png")

#################
g <- ggplot(subset(dta_tidy, year > 2000 & sex=="female")) + aes(x=year, y=age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap ( ~ country, nrow=4)
g3 <- g2 + scale_fill_gradientn(
  colours=c("blue", "white", "red"), limits=c(-0.15, 0.15)
)
print (g3)

ggsave("Figures/Tile_Female.png")

########
g <- ggplot(subset(dta_tidy, year > 2000 & sex=="total")) + aes(x=year, y=age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap ( ~ country, nrow=4)
g3 <- g2 + scale_fill_gradientn(
  colours=c("blue", "white", "red"), limits=c(-0.15, 0.15)
)
print (g3)

ggsave("Figures/Tile_Total.png")


g <- ggplot(subset(dta_tidy, year > 2000)) + aes(x=year, y=residual_proportion, group=age, colour=age)
g2 <- g + geom_line() + facet_grid(country ~ sex)
print(g2)

ggsave("Figures/All_Lattice.png")




# # log on y axis
# 
# ?coord_cartesian
# 
# g3 <- g2 + scale_y_log10()

# Separate by gender?
g <- ggplot(subset(dta_tidy, year > 2000 & sex=="male")) + aes(x=year, y= residual_proportion, group=age, colour=age)
g2 <- g + geom_line() + facet_wrap( ~ country, nrow=4)
print(g2)
ggsave("Figures/Lattice_Male.png")


g <- ggplot(subset(dta_tidy, year > 2000 & sex=="female")) + aes(x=year, y= residual_proportion, group=age, colour=age)
g2 <- g + geom_line() + facet_wrap( ~ country, nrow=4)
print(g2)

ggsave("Figures/Lattice_Female.png")

# Log scale on y axis

#### What I now want are the residuals as a percentage of the expectation




### What I want: 
# for each country,
 # for years from 1990 onwards

# relationship between residuals and time

