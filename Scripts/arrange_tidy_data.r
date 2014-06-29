
europe_codes <- country_codes$short[which(country_codes$europe==1)]

counts_eu <- subset(
  counts,
  subset=country %in% europe_codes                  
)
write.csv(counts_eu, file="Data/Tidy/counts_eu.csv", row.names=F)

rates_eu <- subset(
  rates,
  subset=country %in% europe_codes
)
write.csv(rates_eu, file="Data/Tidy/rates_eu.csv", row.names=F)

exp_eu <- subset(
  expectations,
  subset=country %in% europe_codes
)
write.csv(exp_eu, file="Data/Tidy/exp_eu.csv", row.names=F)


counts_eu_all <- ddply(
  counts_eu,
  .(sex, year, age),
  summarise,
  n_countries=length(death_count),
  death_count=sum(death_count),
  population_count=sum(population_count)
)

write.csv(counts_eu_all, file="Data/Tidy/counts_eu_all.csv", row.names=F)

rates_eu_all <- mutate(counts_eu_all, death_rate=death_count/population_count)

write.csv(rates_eu_all, file="Data/Tidy/rates_eu_all.csv", row.names=F)

exp_eu_all <- ddply(
  exp_eu,
  .(sex, year, age),
  summarise,
  population_actual=sum(population_actual),
  population_expected=sum(population_expected),
  residual_count=population_expected - population_actual,
  residual_prop=residual_count/population_actual
)

write.csv(exp_eu_all, file="Data/Tidy/exp_eu_all.csv", row.names=F)




residuals <- mutate(
  expectations, 
  population_residual =population_expected - population_actual, 
  residual_prop=population_residual/population_actual
)

residuals$population_actual <- NULL
residuals$population_expected <- NULL
residuals$population_residual <- NULL

dta_joined <- join(residuals, rates, by=c("country", "year", "age", "sex"), type="inner")
dta_joined <- merge(dta_joined, country_codes, by.x="country", by.y="short")
dta_joined <- subset(dta_joined, subset=europe==1)
dta_joined$europe <- NULL


# 
# 
# 
# country_codes <- read.csv("Data/HMD/country_codes__new.csv", stringsAsFactors=FALSE)
# 
# europe_indicators <- country_codes$short[country_codes$europe==T] 
# 
# counts_europe <- subset(counts_tidy, subset=country %in% europe_indicators)
# 
# counts_europe_aggregated <- ddply(counts_europe, .(year, age, sex), 
#                                   summarise, 
#                                   death_count=sum(death_count),
#                                   population_count=sum(population_count),
#                                   death_rate =death_count / population_count
# )
# 
