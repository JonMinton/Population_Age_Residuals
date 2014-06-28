## This script should manage all the prerequisites required to analyse the 
# data using ggplot2 at a later stage



datasets_needed <- c(
  "counts_tdy"= "Data/Raw/counts_tdy.csv",
  "rates_tdy" = "Data/Raw/rates_tdy.csv",
  "expectations_tdy" = "Data/Raw/expectations_tdy.csv",
  "residuals_tdy" = "Data/Raw/residuals_tdy.csv",
  "country_codes" = "Data/Raw/country_codes__new.csv"
)


# firstly: are any of the above files missing?

datasets_found <- file.exists(datasets_needed)

if (all(datasets_found)==T){
  print("all datasets found. Loading them")  
  
  for (i in 1:length(datasets_needed)){
    assign(
      names(datasets_needed)[i], 
      read.csv(
        datasets_needed[i],
        header=T
        ),
      envir = .GlobalEnv
      )
  }  
  
} else {
  print("Not all files have been found. Loading those that exist and fetching/deriving those that don't")
  datasets_needed_and_found <- datasets_needed[datasets_found]
  n <- length(datasets_needed_and_found)
  
  if (n > 0){
    cat ("Found ", n, " files. Loading now.\n")
    
    for (i in 1:n){
      assign(
        names(datasets_needed_and_found)[i],
        read.csv(
          datasets_needed_and_found[i],
          header=T
          ),
        envir = .GlobalEnv
        )
    }
  }
  
  print("Doing what's needed to create the rest of the tidy datasets.")
  
  source("Scripts/manage_derived_data.r")
  
  
}
############################################################################################################
############################################################################################################
load("Data/RObj/Tidy_Data.RData")
load("Data/RObj/counts_and_rates.RData")