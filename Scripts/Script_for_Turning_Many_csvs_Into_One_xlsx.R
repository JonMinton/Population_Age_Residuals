# Script to take csv files and put them in a single .xlsx file

rm(list=ls())

require("xlsx")
#help("xlsx")


files.em <- list.files("Outputs/Raw/Expectations/Male", pattern="*.csv")
files.ef <- list.files("Outputs/Raw/Expectations/Female", pattern="*.csv")
files.et <- list.files("Outputs/Raw/Expectations/Total", pattern="*.csv")

files.rm <- list.files("Outputs/Raw/Residuals/Male", pattern="*.csv")
files.rf <- list.files("Outputs/Raw/Residuals/Female", pattern="*.csv")
files.rt <- list.files("Outputs/Raw/Residuals/Total", pattern="*.csv")

Make_Excel_Workbook <- function(dir_location, files_list, wb_name){
    n.files <- length(files_list)
    wb <- createWorkbook()
    
    for (i in 1:n.files){
        this_csv <- read.csv(paste0(dir_location, files_list[i]))
        
        this_sheetname <- strsplit(files_list[i], "[.]")[[1]][1]
        
        sheet <- createSheet(wb, sheetName=this_sheetname)
        
        addDataFrame(this_csv, sheet)
    }
    saveWorkbook(wb, file=paste0(wb_name, ".xlsx"))
    print("Done")
}

Make_Excel_Workbook("Figures/Expectations/Male/", files.em, "Expectations_Males")
Make_Excel_Workbook("Figures/Expectations/Female/", files.ef, "Expectations_Females")
Make_Excel_Workbook("Figures/Expectations/Total/", files.et, "Expectations_Total")

Make_Excel_Workbook("Figures/Residuals/Male/", files.rm, "Residuals_Males")
Make_Excel_Workbook("Figures/Residuals/Female/", files.rf, "Residuals_Females")
Make_Excel_Workbook("Figures/Residuals/Total/", files.rt, "Residuals_Total")

#################
#################

# Including only those data relating to European nations in a single excel workbook

Europe_country_ids <- read.csv("Outputs/Country_Codes/Europe_Only.csv")
Europe_country_ids <- as.vector(Europe_country_ids[,1])

files.em <- list.files("Outputs/Raw/Expectations/Male", pattern="*.csv")
files.ef <- list.files("Outputs/Raw/Expectations/Female", pattern="*.csv")
files.et <- list.files("Outputs/Raw/Expectations/Total", pattern="*.csv")

files.rm <- list.files("Outputs/Raw/Residuals/Male", pattern="*.csv")
files.rf <- list.files("Outputs/Raw/Residuals/Female", pattern="*.csv")
files.rt <- list.files("Outputs/Raw/Residuals/Total", pattern="*.csv")

files.em.e <- files.em[Europe_country_ids]
files.ef.e <- files.ef[Europe_country_ids]
files.et.e <- files.et[Europe_country_ids]


files.rm.e <- files.rm[Europe_country_ids]
files.rf.e <- files.rf[Europe_country_ids]
files.rt.e <- files.rt[Europe_country_ids]

# Residuals only?

wb <- createWorkbook()
saveWorkbook(wb, file="Europe_Residuals.xlsx")
for (gender in c("Male", "Female", "Total")){
    gender <- "Female"
    dir_location <- paste0("Outputs/Raw/Residuals/", gender, "/")
    files_list <- switch(gender,
                         Male = files.rm.e,
                         Female = files.rf.e,
                         Total = files.rt.e
                         )
    
    n.files <- length(files_list)
    
    for (i in 1:n.files){
        this_csv <- read.csv(paste0(dir_location, files_list[i]))
        
        this_sheetname <- strsplit(files_list[i], "[.]")[[1]][1]
        
        write.xlsx(this_csv, file="Europe_residuals.xlsx", 
                   sheetName=this_sheetname,
                   append=TRUE)
    }    
}



# Work out the last available year for each dataset

files <- dir("Outputs/Raw/Residuals/Male", pattern="*.csv")

countries <- sapply(files, function (x) strsplit(x, "[_]")[[1]][1])
N.files <- length(files)
df <- data.frame(country=countries, last_year=NA)
for (i in 1:N.files){
    try (tmp <- read.csv(paste("Outputs/Raw/Residuals/Male", files[i], sep="/")))
    nm <- tail(names(tmp),1)
    nm <- gsub("X", "", nm)
    nm <- as.numeric(nm)
    df[i,"last_year"] <- nm 
}







