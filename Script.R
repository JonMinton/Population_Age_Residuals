rm(list=ls())

# Suggested in: 
#https://github.com/skardhamar/rga/issues/6
#options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))


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
    "car"
    )
  )





############################################################################################################
############################################################################################################


#source("Scripts/functions.r")

# Have 'tidy' files been found?
#source("Scripts/manage_data.r")

# Tidy files to load:

if(!exists("counts")) {counts <- read.csv("Data/Tidy/counts.csv")}
if(!exists("rates")) {rates <- read.csv("Data/Tidy/rates.csv")}
if(!exists("expectations")){expectations <- read.csv("Data/Tidy/expectations.csv")}
if(!exists("country_codes")) {country_codes <- read.csv("Data/Raw/country_codes__new.csv", stringsAsFactors=F)}
if(!exists("counts_eu")){counts_eu <- read.csv("Data/Tidy/counts_eu.csv")}
if(!exists("rates_eu")){rates_eu <- read.csv("Data/Tidy/rates_eu.csv")}
if(!exists("exp_eu")){exp_eu <- read.csv("Data/Tidy/exp_eu.csv")}
if(!exists("counts_eu_all")){counts_eu_all <- read.csv("Data/Tidy/counts_eu_all.csv")}
if(!exists("rates_eu_all")){rates_eu_all <- read.csv("Data/Tidy/rates_eu_all.csv")}
if(!exists("exp_eu_all")){exp_eu_all <- read.csv("Data/Tidy/exp_eu_all.csv")}


#   Maybe draw a single overview graph, with lines again, and with the Y axis 
# still being death rates - but the X axis  being year 1990-2008. An a separate 
# line for each single year of age and sex.
# 
# In other words just one line for women aged 25 on the graph with the 
# height of points being death rates and the X position be year. It should 
# work with so many lines as they tend to be parallel. lines will bend quickly 
# down towards 2008 as mortality rates of the youngest age groups drop quickly 
# in recent years - quicker than makes much sense unless there is a problem 
# with the data.

# join exp_eu_all and rates_eu_all

mrate_resp <- join(
  rates_eu_all,
  exp_eu_all, 
  by=c("year", "age", "sex"),
  type="inner"
  )


mrate_resp <- arrange(mrate_resp, age, year, sex)


mrate_resp$age_group <- recode(
  mrate_resp$age,
  recodes="
    1:5='5 or under';
    6:10 = '6-10';
    11:15 = '11-15';
    16:20 = '16-20';
    21:25 = '21-25';
    26:30 = '26-30';
    31:35 = '31-35';
    36:40 = '36-40';
    41:45 = '41-45';
    else = 'over 45'
  ",
  as.factor.result=T
  )
# cut ages into groups 

mrate_resp_agegroup$age_group <- ordered(
  mrate_resp_agegroup$age_group, 
  levels=c(
    "5 or under", 
    "6-10", 
    "11-15", 
    "16-20", 
    "21-25", 
    "26-30", 
    "31-35", 
    "36-40", 
    "41-45", 
    "over 45"
    )
  )


g <- ggplot(subset(
  mrate_resp,
  subset= sex!="total" & age <=50 & age > 20 & year > 1990
  ))

g2 <- g + aes(x=residual_prop, y=log(death_rate), group=sex)
g3 <- g2 + geom_line(aes(colour=year)) + facet_wrap( ~ age) + geom_point(aes(pch=sex), alpha=0.2)
g4 <- g3 + scale_colour_gradient(low="blue", high="red")
g4

ggsave("Figures/deathrate_vs_res_facet_age.png")


####
g <- ggplot(
  subset(
  mrate_resp,
  subset= sex!="total" & age <= 50 & age >=20 & year >=1990
  )
)

g2 <- g + aes(x=year, y=log(death_rate), group=age, colour=age)
g3 <- g2 + geom_line() + facet_wrap(~ sex) 
print(g3)

ggsave("Figures/deathrates_age.png")


mrate_resp_agegroup <- ddply(
  mrate_resp, 
  .(sex, year, age_group), 
  summarise,
  population_count=sum(population_count),
  death_count=sum(death_count),
  population_actual=sum(population_actual),
  population_expected=sum(population_expected),
  death_rate=death_count/population_count,
  residual_count=population_actual - population_expected,
  residual_prop=residual_count/population_actual
                             )

mrate_resp_agegroup <- arrange(mrate_resp_agegroup, age_group, sex, year)
####
agegroups_of_interest <- c("11-15", "16-20", "21-25", "26-30", "31035", "36-40")

g <- ggplot(
  subset(
    mrate_resp_agegroup,
    subset= sex!="total" & year >=1990 & age_group %in% agegroups_of_interest
  )
)

g2 <- g + aes(x=year, y=log(death_rate), group=age_group, colour=age_group, lty=age_group)
g3 <- g2 + geom_line(size=1.1) + facet_wrap(~ sex) 
print(g3)

ggsave("Figures/deathrates_agegroup.png")


########################################################################


g <- ggplot(subset(mrate_resp, subset=year >=1990 & age >20 & age < 45 & sex !="total")) 
g2 <- g + aes(x=residual_prop, y=log(death_rate), colour=sex)
g3 <- g2 + geom_line() + facet_wrap( ~ year, nrow=3)
g4 <- g3 + geom_vline(v=0, lty="dashed")

print(g4)

ggsave("Figures/lines_residual_deathrates.png")


# now I want something that adds points for each country to 
# g3 above


####################################################################################



g <- ggplot(subset(mrate_resp, year >= 1990 & age < 50 & sex!="total")) + aes(x=year, y= age, z=residual_prop)
g2 <- g + geom_tile(aes(fill=residual_prop)) + facet_wrap( ~ sex)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue"), 
  limits=c(-0.015, 0.015)
  )
print(g3)

ggsave("Figures/Tile_Countries_Combined.png")


g <- ggplot(subset(mrate_resp, year >= 1970 & age < 50 & sex!="total")) + aes(x=year, y= age, z=residual_prop)
g2 <- g + geom_tile(aes(fill=residual_prop)) + facet_wrap( ~ sex)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue"),
  limits=c(-0.015, 0.015)
)
print(g3)

ggsave("Figures/Tile_Countries_Combined_from1970.png")


g <- ggplot(subset(mrate_resp, year >= 1950 & age < 50 & sex!="total")) + aes(x=year, y= age, z=residual_prop)
g2 <- g + geom_tile(aes(fill=residual_prop)) + facet_wrap( ~ sex)
g3 <- g2 + scale_fill_gradientn(
  colours=c("red", "white", "blue"),
  limits=c(-0.015, 0.015)
)
print(g3)

ggsave("Figures/Tile_Countries_Combined_from1950.png")
####################

