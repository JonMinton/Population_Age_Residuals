print("Fetching tidy data")

##############
# The preparatory stage should produce the following dataframes

# 1) dta_long 
#    key:       country, year, age, sex
#    variable:  population, deaths (main)
#    derived:   expected, actual, residual, residual_proportion, 

# Check for data long

# Tidy data are:

# residuals and expectations
# counts
# rates

if (file.exists("Data/RObj/counts_tidy.rdata")){
  print("Found counts_tidy. Loading")
  load("Data/RObj/counts_tidy.rdata")
  
  if (!file.exists("Data/Raw/counts_tidy.csv")){
    save(counts_tidy, file="Data/Raw/counts_tidy.csv")
  }
  
} else {
  print("Could not find counts_tidy as rdata. Searching for it as csv")
  if (file.exists("Data/Raw/counts_tidy.csv")){
    print("Found counts_tidy as csv. Loading")
    counts_tidy <- read.csv("Data/Raw/counts_tidy.csv")
    
  } else {
    print("Could not find counts_tidy as rdata or csv. Creating afresh")
    #####
  }
}

if (file.exists("Data/RObj/rates_tidy.rdata")){
  print("Found rates_tidy. Loading")
  load("Data/RObj/rates_tidy.rdata")
  
  if (!file.exists("Data/Raw/rates_tidy.csv")){
    save(counts_tidy, file="Data/Raw/rates_tidy.csv")
  }
  
} else {
  print("Could not find counts_tidy as rdata. Searching for it as csv")
  if (file.exists("Data/Raw/counts_tidy.csv")){
    print("Found counts_tidy as csv. Loading")
    counts_tidy <- read.csv("Data/Raw/counts_tidy.csv")
    
  } else {
    print("Could not find counts_tidy as rdata or csv. Creating afresh")
    #####
  }
}


if (file.exists("Data/RObj/residuals_tidy.rdata")){
  print("Found residuals_tidy as R object. Loading")
  load("Data/RObj/residuals_tidy.rdata")
  
} else {
  print("Found residuals_tidy as R object. Searching for it as a csv file")
  if (file.exists("Data/Raw/residuals_tidy.csv")){
    print("Found residuals_tidy as csv. Loading")
    residuals_tidy <- read.csv("Data/Raw/residuals_tidy.csv")
    
    
  } else {
    print("Did not find residuals_tidy as csv. Creating afresh")
    print("First searching for derived data")
    
    if (file.exists("Data/RObj/Derived_Data.RData")){
      print("Found Derived_Data.RData. creating tidy data using it")
      load("Data/RObj/Derived_Data.RData")
      out <- ldply(Populations.numeric)
      out <- rename(out, c(".id"="country"))
      names(out) <- tolower(names(out))
      
      out <- melt(out, id.var=c("country", "year", "age"))
      out$vartype <- "population_count"
      out <- rename(out, c("value"="count", "variable"="sex"))
      
      population_long <- out
      rm(out)
      
      out <- ldply(Deaths.numeric)
      out <- rename(out, c(".id"="country"))
      names(out) <- tolower(names(out))
      
      out <- melt(out, id.var=c("country", "year", "age"))
      out$vartype <- "death_count"
      out <- rename(out, c("value"="count", "variable"="sex"))
      death_long <- out
      rm(out)
      
      counts_long <- rbind(population_long, death_long)
      
      
      counts_tidy <- ddply(counts_long, .(country), dcast, 
                           formula=country + year + age + sex ~ vartype, 
                           value.var="count")
      
      deathrates_tidy <- mutate(counts_tidy, rate=death_count / population_count)
      deathrates_tidy$death_count <- NULL
      deathrates_tidy$population_count <- NULL
      
      save(counts_tidy, deathrates_tidy, file="Data/RObj/counts_and_rates.RData")
      
      
    }
    
  }
 }
  print("Have not found population and death file in long format. Looking for it in list format")
  # try to make this using Derived_Data_Older.RData
  if (file.exists("Data/RObj/Derived_Data.RData")){
    load("Data/RObj/Derived_Data.RData")
    print ("Have found the data in list format. Turning to long format")
    
    
  } else {
    print("Have not found the data in list format. Need to create from text files")
    
  }
  
}




