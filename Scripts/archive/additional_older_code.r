
New_Country_Codes <- Make_Country_DF(
    directory="Data/HMD/population/Population/"    
)

Make_Derived_Data(
    HMD_Location="Data/HMD",
    Country.Codes=New_Country_Codes,
    Outfile_Location="Data/Derived",
    Outfile_Name="Derived_Data.RData",
    old_HMD=FALSE
)




# Want to also know for which countries the data go to either 2010 or 2011

subset_residuals_male <- Outlist$residuals$male[Europe_Indicators]
subset_residuals_female <- Outlist$residuals$female[Europe_Indicators]
subset_residuals_total <- Outlist$residuals$total[Europe_Indicators]
subset_expectations_male <- Outlist$expectations$male[Europe_Indicators]
subset_expectations_female <- Outlist$expectations$female[Europe_Indicators]
subset_expectations_total <- Outlist$expectations$total[Europe_Indicators]

Europe_labels <- as.character(New_Country_Codes[Europe_Indicators,1])

names(subset_residuals_male) <- Europe_labels
names(subset_residuals_female) <- Europe_labels
names(subset_residuals_total) <- Europe_labels
names(subset_expectations_male) <- Europe_labels
names(subset_expectations_female) <- Europe_labels
names(subset_expectations_total) <- Europe_labels


# For each of the years 2008 to 2011 
# If the year exists, add it to the matrix

mat <- matrix(NA, nrow=80, ncol=length(Europe_labels))
rownames(mat) <- as.character(1:80)
colnames(mat) <- Europe_labels

Rearranged <- list(
    residuals=list(
        male=list(
            y2008=mat,
            y2009=mat,
            y2010=mat,
            y2011=mat
        ),
        female=list(
            y2008=mat,
            y2009=mat,
            y2010=mat,
            y2011=mat
        ),
        total=list(
            y2008=mat,
            y2009=mat,
            y2010=mat,
            y2011=mat
        )
    ),
    expectations=list(
        male=list(
            y2008=mat,
            y2009=mat,
            y2010=mat,
            y2011=mat
        ),
        female=list(
            y2008=mat,
            y2009=mat,
            y2010=mat,
            y2011=mat
        ),
        total=list(
            y2008=mat,
            y2009=mat,
            y2010=mat,
            y2011=mat
        )
    )
)

fcn <- function(x){
    test <- try(x, silent=T)
    out <- if (class(test)=="try-error"){
        NA
    } else {
        test
    }
    return(out)
}
years <- as.character(2008:2010)
N.years <- length(years)
for (i in 1:length(Europe_labels)){
    for (y in 1:N.years){
        
        Rearranged$residuals$male[[y]][,i] <- fcn(subset_residuals_male[[i]][,years[y]])
        Rearranged$residuals$female[[y]][,i] <- fcn(subset_residuals_female[[i]][,years[y]])
        Rearranged$residuals$total[[y]][,i] <- fcn(subset_residuals_total[[i]][,years[y]])
        
        Rearranged$expectations$male[[y]][,i] <- fcn(subset_expectations_male[[i]][,years[y]])
        Rearranged$expectations$female[[y]][,i] <- fcn(subset_expectations_female[[i]][,years[y]])
        Rearranged$expectations$total[[y]][,i] <- fcn(subset_expectations_total[[i]][,years[y]])        
    }
    
}


# Add it all to a single excel workbook
require("xlsx")

wb <- createWorkbook()

sh_r_m_2008 <- createSheet(wb, sheetName="r_m_2008")
sh_r_m_2009 <- createSheet(wb, sheetName="r_m_2009")
sh_r_m_2010 <- createSheet(wb, sheetName="r_m_2010")

sh_r_f_2008 <- createSheet(wb, sheetName="r_f_2008")
sh_r_f_2009 <- createSheet(wb, sheetName="r_f_2009")
sh_r_f_2010 <- createSheet(wb, sheetName="r_f_2010")

sh_r_t_2008 <- createSheet(wb, sheetName="r_t_2008" )
sh_r_t_2009 <- createSheet(wb, sheetName="r_t_2009")
sh_r_t_2010 <- createSheet(wb, sheetName="r_t_2010" )


sh_e_m_2008 <- createSheet(wb, sheetName="e_m_2008" )
sh_e_m_2009 <- createSheet(wb, sheetName="e_m_2009" )
sh_e_m_2010 <- createSheet(wb, sheetName="e_m_2010" )

sh_e_f_2008 <- createSheet(wb, sheetName="e_f_2008")
sh_e_f_2009 <- createSheet(wb, sheetName="e_f_2009" )
sh_e_f_2010 <- createSheet(wb, sheetName="e_f_2010")

sh_e_t_2008 <- createSheet(wb, sheetName="e_t_2008")
sh_e_t_2009 <- createSheet(wb, sheetName="e_t_2009")
sh_e_t_2010 <- createSheet(wb, sheetName="e_t_2010")



addDataFrame(   
    Rearranged$residuals$male[[1]],           
    sh_r_m_2008            
)
addDataFrame(   
    Rearranged$residuals$male[[2]],           
    sh_r_m_2009            
)
addDataFrame(   
    Rearranged$residuals$male[[3]],           
    sh_r_m_2010            
)

addDataFrame(   
    Rearranged$residuals$female[[1]],           
    sh_r_f_2008            
)
addDataFrame(   
    Rearranged$residuals$female[[2]],           
    sh_r_f_2009            
)
addDataFrame(   
    Rearranged$residuals$female[[3]],           
    sh_r_f_2010            
)

addDataFrame(   
    Rearranged$residuals$total[[1]],           
    sh_r_t_2008            
)
addDataFrame(   
    Rearranged$residuals$total[[2]],           
    sh_r_t_2009            
)
addDataFrame(   
    Rearranged$residuals$total[[3]],           
    sh_r_t_2010            
)



addDataFrame(   
    Rearranged$expectations$male[[1]],           
    sh_e_m_2008            
)
addDataFrame(   
    Rearranged$expectations$male[[2]],           
    sh_e_m_2009            
)
addDataFrame(   
    Rearranged$expectations$male[[3]],           
    sh_e_m_2010            
)

addDataFrame(   
    Rearranged$expectations$female[[1]],           
    sh_e_f_2008            
)
addDataFrame(   
    Rearranged$expectations$female[[2]],           
    sh_e_f_2009            
)
addDataFrame(   
    Rearranged$expectations$female[[3]],           
    sh_e_f_2010            
)

addDataFrame(   
    Rearranged$expectations$total[[1]],           
    sh_e_t_2008            
)
addDataFrame(   
    Rearranged$expectations$total[[2]],           
    sh_e_t_2009            
)
addDataFrame(   
    Rearranged$expectations$total[[3]],           
    sh_e_t_2010            
)


saveWorkbook(wb, file="Rearranged_Data.xlsx")




# require("xlsx")
# Country_Codes <- read.xlsx(
#     file="Data/CountryCodes.xlsx",
#     sheetName="Country_Codes"
#     )
# 

###############################################################################################################
# 
# 
# # Now to save this all to .csv files
# 
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/")
# 
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/")
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/")
# 
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Male/")
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Female/")
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Total/")
# 
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Male/")
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Female/")
# dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Total/")
# 
# 
# 
# 
# 
# for (i in 1:N.countries){
#     
#     write.csv(
#         residuals.male.list[[i]],
#         file=paste0(
#             "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Male/",
#             Country.Codes[i,1], "_Residuals_Males.csv"
#         )
#     )
#     
#     write.csv(
#         residuals.female.list[[i]],
#         file=paste0(
#             "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Female/",
#             Country.Codes[i,1], "_Residuals_Females.csv"
#         )
#     )
#     
#     write.csv(
#         residuals.total.list[[i]],
#         file=paste0(
#             "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Total/",
#             Country.Codes[i,1], "_Residuals_Total.csv"
#         )
#     )
#     
#     
#     write.csv(
#         expected.male.list[[i]],
#         file=paste0(
#             "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Male/",
#             Country.Codes[i,1], "_Expectations_Males.csv"
#         )
#     )
#     
#     write.csv(
#         expected.female.list[[i]],
#         file=paste0(
#             "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Female/",
#             Country.Codes[i,1], "_Expectations_Females.csv"
#         )
#     )
#     
#     write.csv(
#         expected.total.list[[i]],
#         file=paste0(
#             "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Total/",
#             Country.Codes[i,1], "_Expectations_Total.csv"
#         )
#     )
#     
# }
# 
# 
# write.csv(Country.Codes,
#           file="/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Country Codes.csv"
# )

