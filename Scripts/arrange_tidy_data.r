
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
  residual_count=population_actual - population_expected,
  residual_prop=residual_count/population_actual
)

write.csv(exp_eu_all, file="Data/Tidy/exp_eu_all.csv", row.names=F)




residuals <- mutate(
  expectations, 
  population_residual =population_actual - population_expected, 
  residual_prop=population_residual/population_actual
)

residuals$population_actual <- NULL
residuals$population_expected <- NULL
residuals$population_residual <- NULL

dta_joined <- join(residuals, rates, by=c("country", "year", "age", "sex"), type="inner")
dta_joined <- merge(dta_joined, country_codes, by.x="country", by.y="short")
dta_joined <- subset(dta_joined, subset=europe==1)
dta_joined$europe <- NULL



##### Want to do the above but only for the 14 countries observed in 2011
countries_2011 <- c(
  "BEL", "CZE", "DEUTNP", "DNK", "ESP", "EST",
  "FRATNP", "GBR_NIR", "GBR_SCO", "GBRTENW",
  "LTU",     "LVA",     "PRT",     "SWE"
)


counts_14 <- subset(
  counts,
  subset=country %in% countries_2011                  
)

write.csv(counts_14, file="Data/Tidy/counts_14.csv", row.names=F)

rates_14 <- subset(
  rates,
  subset=country %in% countries_2011
)
write.csv(rates_14, file="Data/Tidy/rates_14.csv", row.names=F)

exp_14 <- subset(
  expectations,
  subset=country %in% countries_2011
)
write.csv(exp_14, file="Data/Tidy/exp_14.csv", row.names=F)


counts_14_all <- ddply(
  counts_14,
  .(sex, year, age),
  summarise,
  n_countries=length(death_count),
  death_count=sum(death_count),
  population_count=sum(population_count)
)

write.csv(counts_14_all, file="Data/Tidy/counts_14_all.csv", row.names=F)

rates_14_all <- mutate(counts_14_all, death_rate=death_count/population_count)

write.csv(rates_14_all, file="Data/Tidy/rates_14_all.csv", row.names=F)

exp_14_all <- ddply(
  exp_14,
  .(sex, year, age),
  summarise,
  population_actual=sum(population_actual),
  population_expected=sum(population_expected),
  residual_count=population_actual - population_expected,
  residual_prop=residual_count/population_actual
)

write.csv(exp_eu_all, file="Data/Tidy/exp_14_all.csv", row.names=F)




residuals <- mutate(
  expectations, 
  population_residual =population_actual - population_expected, 
  residual_prop=population_residual/population_actual
)

residuals$population_actual <- NULL
residuals$population_expected <- NULL
residuals$population_residual <- NULL

dta_joined <- join(residuals, rates, by=c("country", "year", "age", "sex"), type="inner")
dta_joined <- merge(dta_joined, country_codes, by.x="country", by.y="short")
dta_joined <- subset(dta_joined, subset=europe==1)
dta_joined$europe <- NULL


