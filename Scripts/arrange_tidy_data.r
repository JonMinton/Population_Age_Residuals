## OK, so the problem seems to be, that including germany means the data is only from 
## 1990 onwards. But East and West germany have data for the whole period.
## SO I'll first check if after 1990 they simply sum up to DEUTNP
germanies <- subset(counts,
                    subset = country %in% c("DEUTE", "DEUTNP", "DEUTW"))
library(reshape2)
germanies <- melt(germanies, id.vars = c("country", "year", "age", "sex"))
germanies <- dcast(germanies, year+ age+ sex ~ variable + country, value.var = c("value") )
germanies <- germanies[germanies$year >= 1990,]
all.equal(germanies$death_count_DEUTE + germanies$death_count_DEUTW ,germanies$death_count_DEUTNP)
# is TRUE!
# OK, this means basically we can just use DEUTE + DEUTW for the whole period! and not DEUTNP
rm(germanies)

country_codes[country_codes$short == "DEUTNP", "europe"] <- 0
country_codes[country_codes$short == "DEUTE", "europe"] <- 1
country_codes[country_codes$short == "DEUTW", "europe"] <- 1

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

### This has been changed so that residual prop is now (actual - expected)/expected
# not (actual - expected)/actual

exp_eu_all <- ddply(
  exp_eu,
  .(sex, year, age),
  summarise,
  population_actual=sum(population_actual),
  population_expected=sum(population_expected),
  residual_count=population_actual - population_expected,
  residual_prop=residual_count/population_expected
)

write.csv(exp_eu_all, file="Data/Tidy/exp_eu_all.csv", row.names=F)

residuals <- mutate(
  expectations, 
  residual_count =population_actual - population_expected, 
  residual_prop=residual_count/population_expected
)

residuals$population_actual <- NULL
residuals$population_expected <- NULL
residuals$population_residual <- NULL

dta_joined <- join(residuals, rates, by=c("country", "year", "age", "sex"), type="inner")
dta_joined <- merge(dta_joined, country_codes, by.x="country", by.y="short")
dta_joined <- subset(dta_joined, subset=europe==1)
dta_joined$europe <- NULL




##### Want to do the above but only for the 15 countries observed in 2011
countries_2011 <- c(
  "BEL", "CHE", "CZE", "DEUTE", "DEUTW" ,"DNK", "ESP", "EST",
  "FRATNP", "GBR_NIR", "GBR_SCO", "GBRTENW",
  "LTU",     "LVA",     "PRT",     "SWE"
)


counts_15 <- subset(
  counts,
  subset=country %in% countries_2011                  
)

write.csv(counts_15, file="Data/Tidy/counts_15.csv", row.names=F)

rates_15 <- subset(
  rates,
  subset=country %in% countries_2011
)
write.csv(rates_15, file="Data/Tidy/rates_15.csv", row.names=F)

exp_15 <- subset(
  expectations,
  subset=country %in% countries_2011
)
write.csv(exp_15, file="Data/Tidy/exp_15.csv", row.names=F)


counts_15_all <- ddply(
  counts_15,
  .(sex, year, age),
  summarise,
  n_countries=length(death_count),
  death_count=sum(death_count),
  population_count=sum(population_count)
)

write.csv(counts_15_all, file="Data/Tidy/counts_15_all.csv", row.names=F)

rates_15_all <- mutate(counts_15_all, death_rate=death_count/population_count)

write.csv(rates_15_all, file="Data/Tidy/rates_15_all.csv", row.names=F)

# This was changed to (actual-expected)/expected not (actual-expected)/actual

exp_15_all <- ddply(
  exp_15,
  .(sex, year, age),
  summarise,
  population_actual=sum(population_actual),
  population_expected=sum(population_expected),
  residual_count=population_actual - population_expected,
  residual_prop=residual_count/population_expected
)

write.csv(exp_eu_all, file="Data/Tidy/exp_15_all.csv", row.names=F)