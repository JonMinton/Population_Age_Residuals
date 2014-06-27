rm(list=ls())

##########################################################################################################
# Tasks

# 1) Script for checking and loading tidy data
# 2) script for creating tidy data if it cannot be loaded



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
    "stringr"
    )
  )





############################################################################################################
############################################################################################################


source("Scripts/Functions.r")
source("Scripts/Script_for_Producing_Raw_Analyses.R")

############################################################################################################
############################################################################################################
load("Data/RObj/Tidy_Data.RData")
load("Data/RObj/counts_and_rates.RData")

# I want to create a subset of deathrates_tidy and counts_tidy for European countries only

# 

country_codes <- read.csv("Data/HMD/country_codes__new.csv", stringsAsFactors=FALSE)

europe_indicators <- country_codes$short[country_codes$europe==T] 

counts_europe <- subset(counts_tidy, subset=country %in% europe_indicators)

counts_europe_aggregated <- ddply(counts_europe, .(year, age, sex), 
                                  summarise, 
                                  death_count=sum(death_count),
                                  population_count=sum(population_count),
                                  death_rate =death_count / population_count
                                  )



########################################################################


europe_residuals_aggregated <- ddply(dta_tidy, .(age, year, sex), summarise,
                 countries_observed=length(residual),
                 residual=sum(residual), 
                 expectation=sum(expectation),
                 residual_proportion = residual/expectation
                                  )

joined_data_aggregated <- join(
  counts_europe_aggregated, europe_residuals_aggregated, 
                    by=c("year", "age", "sex"),
  type="inner"
  )

joined_data_aggregated <- arrange(joined_data_aggregated, 
                                  year, sex, age
                                  )

g <- ggplot(subset(joined_data_aggregated, subset=year >=1990 & age >20 & age < 45 & sex !="total")) 
g2 <- g + aes(x=residual_proportion, y=log(death_rate), colour=sex)
g3 <- g2 + geom_line() + facet_wrap( ~ year, nrow=3)
print(g3)

ggsave("Figures/lines_residual_deathrates.png")


# now I want something that adds points for each country to 
# g3 above


####################################################################################



g <- ggplot(subset(dta_agg, year >= 1990)) + aes(x=year, y= age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap( ~ sex)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue")
  )
print(g3)

ggsave("Figures/Tile_Countries_Combined.png")


g <- ggplot(subset(dta_agg, year >= 1970)) + aes(x=year, y= age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap( ~ sex)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue"),
  limits=c(-0.025, 0.025)
)
print(g3)

ggsave("Figures/Tile_Countries_Combined_from1970.png")


g <- ggplot(subset(dta_agg, year >= 1950)) + aes(x=year, y= age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap( ~ sex)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue"),
  limits=c(-0.025, 0.025)
)
print(g3)

ggsave("Figures/Tile_Countries_Combined_from1950.png")
####################
## contour plot
load("Data/RObj/Tidy_Data.RData")

g <- ggplot(subset(dta_tidy, year > 2000 & sex=="male")) + aes(x=year, y=age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap ( ~ country, nrow=4)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue"), limits=c(-0.15, 0.15)
)
print (g3)

ggsave("Figures/Tile_Male.png")

#################
g <- ggplot(subset(dta_tidy, year > 2000 & sex=="female")) + aes(x=year, y=age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap ( ~ country, nrow=4)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue"), limits=c(-0.15, 0.15)
)
print (g3)

ggsave("Figures/Tile_Female.png")

########
g <- ggplot(subset(dta_tidy, year > 2000 & sex=="total")) + aes(x=year, y=age, z=residual_proportion)
g2 <- g + geom_tile(aes(fill=residual_proportion)) + facet_wrap ( ~ country, nrow=4)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue"), limits=c(-0.15, 0.15)
)
print (g3)

ggsave("Figures/Tile_Total.png")




g <- ggplot(subset(dta_tidy, year > 2000)) + aes(x=year, y=residual_proportion, group=age, colour=age)
g2 <- g + geom_line() + facet_grid(country ~ sex)
print(g2)

ggsave("Figures/All_Lattice.png")


#####################################################################################################
#####################################################################################################



#####################################################################################################
#####################################################################################################

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





########################################################################################################
#########################################################################################
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


