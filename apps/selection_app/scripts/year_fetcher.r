# script to extract first and last year for each dataset in the hmd

require(dplyr)
setwd("e:/repos/Population_Age_Residuals/apps/selection_app/")
counts <- read.csv("data/counts.csv")
codes <- read.csv("data/country_codes__new.csv")

counts <- tbl_df(counts)

counts %>% group_by(country) %>% summarise(min_year=min(year), max_year=max(year)) %>% arrange(min_year) -> hmd_years

write.csv(hmd_years, file="data/hmd_years.csv", row.names=F)
