
# Tidy files to load:

if(!exists("counts")) {counts <- read.csv("Data/Tidy/counts.csv")}
if(!exists("rates")) {rates <- read.csv("Data/Tidy/rates.csv")}
if(!exists("expectations")){expectations <- read.csv("Data/Tidy/expectations.csv")}
if(!exists("country_codes")) {country_codes <- read.csv("Data/Tidy/country_codes__new.csv", stringsAsFactors=F)}
if(!exists("counts_eu")){counts_eu <- read.csv("Data/Tidy/counts_eu.csv")}

if(!exists("rates_eu")){rates_eu <- read.csv("Data/Tidy/rates_eu.csv")}
if(!exists("exp_eu")){exp_eu <- read.csv("Data/Tidy/exp_eu.csv")}
if(!exists("counts_eu_all")){counts_eu_all <- read.csv("Data/Tidy/counts_eu_all.csv")}
if(!exists("rates_eu_all")){rates_eu_all <- read.csv("Data/Tidy/rates_eu_all.csv")}
if(!exists("exp_eu_all")){exp_eu_all <- read.csv("Data/Tidy/exp_eu_all.csv")}

# 15 countries reporting in 2011 only
if(!exists("counts_15")){counts_15 <- read.csv("Data/Tidy/counts_15.csv")}
if(!exists("rates_15")){rates_15 <- read.csv("Data/Tidy/rates_15.csv")}
if(!exists("exp_15")){exp_15 <- read.csv("Data/Tidy/exp_15.csv")}
if(!exists("counts_15_all")){counts_15_all <- read.csv("Data/Tidy/counts_15_all.csv")}
if(!exists("rates_15_all")){rates_15_all <- read.csv("Data/Tidy/rates_15_all.csv")}
if(!exists("exp_15_all")){exp_15_all <- read.csv("Data/Tidy/exp_15_all.csv")}
