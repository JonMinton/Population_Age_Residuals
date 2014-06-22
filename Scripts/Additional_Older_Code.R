
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


# short                         long
# 1   AUS                    Australia
# 2   AUT                      Austria **
# 3      BEL                      Belgium **
# 4      BGR                     Bulgaria **
# 5      BLR                      Belarus 
# 6      CAN                       Canada
# 7      CHE                  Switzerland **
# 8      CHL                        Chile
# 9      CZE               Czech Republic **
# 10   DEUTE                 East Germany
# 11  DEUTNP                      Germany **
# 12   DEUTW                 West Germany
# 13     DNK                      Denmark **
# 14     ESP                        Spain **
# 15     EST                      Estonia **
# 16     FIN                      Finland **
# 17  FRACNP                       France (Civilian)
# 18  FRATNP                       France (Total) **
# 19 GBR_NIR             Northern Ireland **
# 20  GBR_NP               United Kingdom
# 21 GBR_SCO                     Scotland **
# 22 GBRCENW            England and Wales (Civilian) 
# 23 GBRTENW            England and Wales (Total) **
# 24     HUN                      Hungary **
# 25     IRL                      Ireland **
# 26     ISL                      Iceland
# 27     ISR                       Israel
# 28     ITA                        Italy **
# 29     JPN                        Japan
# 30     LTU                    Lithuania **
# 31     LUX                   Luxembourg **
# 32     LVA                       Latvia **
# 33     NLD                  Netherlands **
# 34     NOR                       Norway **
# 35  NZL_MA         New Zealand -- Maori
# 36  NZL_NM     New Zealand -- Non-Maori
# 37  NZL_NP                  New Zealand
# 38     POL                       Poland **
# 39     PRT                     Portugal **
# 40     RUS                       Russia 
# 41     SVK                     Slovakia **
# 42     SVN                     Slovenia **
# 43     SWE                       Sweden **
# 44     TWN                       Taiwan
# 45     UKR                      Ukraine 
# 46     USA The United States of America    

Europe_Indicators <- c(
    2,  3,  4,  7,  9,  11, 13, 14, 15, 16,
    18, 19, 21, 23, 24, 25, 28, 30, 31, 32,
    33, 34, 38, 39, 41, 42, 43
)

Outlist_subset <- Outlist[Europe_Indicators]

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

