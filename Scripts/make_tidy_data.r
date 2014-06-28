
# Make tidy data

#rm(list=ls())
load("Data/RObj/Derived_Data.RData")

deaths_long <- ldply(Deaths.numeric)
deaths_long <- rename(deaths_long, c(".id"= "country"))
names(deaths_long) <- tolower(names(deaths_long))
deaths_long <- melt(deaths_long, id.var=c("country", "year", "age"))
deaths_long <- rename(deaths_long, c("variable"="sex", "value"= "death_count"))


pops_long <- ldply(Populations.numeric)
pops_long <- rename(pops_long, c(".id"="country"))
names(pops_long) <- tolower(names(pops_long))
pops_long <- melt(pops_long, id.var=c("country", "year", "age"))
pops_long <- rename(pops_long, c("variable"="sex", "value"= "population_count"))


counts_tidy <- join(x=deaths_long, y=pops_long, by=c("country", "year", "age", "sex"), type="inner")

rates_tidy <- counts_tidy 
rates_tidy <- mutate(rates_tidy, death_rate = death_count / population_count)
rates_tidy$death_count <- NULL
rates_tidy$population_count <- NULL

write.csv(counts_tidy, file="Data/Tidy/Counts.csv")


# Ideally should be able to produce the expectations from the count file directly

fn <- function(x){
  
  years <- sort(unique(x$year))
  ages <- sort(unique(x$age))

  years_viable <- years[-1]
  ages_viable <- ages[ages!=0]
  
  out <- x
  out$population_expected <- NA
  
  for (i in years_viable){
    for (j in ages_viable){
      population.lastyear <- subset(x, subset =year==i-1 & age==j-1)$population_count
      deaths.lastyear <- subset(x, subset=year==i-1 & age ==j - 1)$death_count
      
      if ((length(population.lastyear)==1) & (length(deaths.lastyear)==1)){
        population.expected <- population.lastyear - deaths.lastyear
        out$population_expected[out$year==i & out$age==j] <- population.expected
      } else {
        cat("year: ", i, "\tage: ", j, "\n")
      }
    }
  }
  return(out)
}

expectations_tidy <- ddply(counts_tidy, .(country, sex), fn, .progress="text")
