
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


#####################################################################################################
#####################################################################################################

# # log on y axis
# 
# ?coord_cartesian
# 
# g3 <- g2 + scale_y_log10()

# Separate by gender?
g <- ggplot(subset(residuals, year > 2000 & sex=="male" & age >=20 & age < 50) ) + aes(x=year, y= residual_prop, group=age, colour=age)
g2 <- g + geom_line() + facet_wrap( ~ country, nrow=4)
print(g2)
ggsave("Figures/Lattice_Male.png")


g <- ggplot(subset(dta_tidy, year > 2000 & sex=="female")) + aes(x=year, y= residual_proportion, group=age, colour=age)
g2 <- g + geom_line() + facet_wrap( ~ country, nrow=4)
print(g2)

ggsave("Figures/Lattice_Female.png")

# Log scale on y axis

#### What I now want are the residuals as a percentage of the expectation





########################################################################################################
#########################################################################################
load("Data/RObj/Expectations_and_Residuals.RData")

country_codes <- read.csv("Data/HMD/country_codes__new.csv", stringsAsFactors=FALSE)

europe_indicators <- country_codes$short[country_codes$europe==T]  

names(Outlist$expectations$male) <- country_codes$short
names(Outlist$expectations$female) <- country_codes$short
names(Outlist$expectations$total) <- country_codes$short
names(Outlist$residuals$male) <- country_codes$short
names(Outlist$residuals$female) <- country_codes$short
names(Outlist$residuals$total) <- country_codes$short


eu_res_male <- Outlist$residuals$male[europe_indicators]
eu_res_female <- Outlist$residuals$female[europe_indicators]
eu_res_total <- Outlist$residuals$total[europe_indicators]

# I want the following long format dataframe

# identifiers
#  country, gender, age, year
# value: quantity

fn <- function(x, label=NA, sex_=NA){
  out <- melt(x, varnames=c("age", "year"), value.name=label)
  out <- data.frame(out, sex=sex_)
  return(out)            
}


dta_long_male <- ldply(
  eu_res_male,
  fn,
  label="residual",
  sex_="male"
)

dta_long_female <- ldply(
  eu_res_female,
  fn, 
  label="residual",
  sex_="female"
)

dta_long_total <- ldply(
  eu_res_total,
  fn,
  label="residual",
  sex_="total"
)

names(dta_long_male)[1] <- "country"
names(dta_long_female)[1] <- "country"
names(dta_long_total)[1] <- "country"


dta_long <- rbind(dta_long_male, dta_long_female, dta_long_total)

rm(dta_long_male, dta_long_female, dta_long_total)

dta_long2 <- melt(dta_long, id.var=c("country", "age", "year", "sex"))
dta_wide <- dcast(dta_long2, country + age + year ~ sex)



dta_res_tidy <- dta_long2


####################################################################################

eu_exp_male <- Outlist$expectations$male[europe_indicators]
eu_exp_female <- Outlist$expectations$female[europe_indicators]
eu_exp_total <- Outlist$expectations$total[europe_indicators]

dta_long_male <- ldply(
  eu_exp_male,
  fn,
  label="expectation",
  sex_="male"
)

dta_long_female <- ldply(
  eu_exp_female,
  fn, 
  label="expectation",
  sex_="female"
)

dta_long_total <- ldply(
  eu_exp_total,
  fn,
  label="expectation",
  sex_="total"
)


names(dta_long_male)[1] <- "country"
names(dta_long_female)[1] <- "country"
names(dta_long_total)[1] <- "country"

dta_long <- rbind(dta_long_male, dta_long_female, dta_long_total)

rm(dta_long_male, dta_long_female, dta_long_total)

dta_long2 <- melt(dta_long, id.var=c("country", "age", "year", "sex"))

dta_tidy_long <- rbind(dta_res_tidy, dta_long2)

dta_tidy <- dcast(dta_tidy_long, country + age + year + sex ~ variable)
# g <- ggplot(subset(dta_wide, year > 1995)) + aes(x=year, y=male, group=age)
dta_tidy <- mutate(dta_tidy, residual_proportion=residual/expectation)

save(dta_tidy, file="Data/RObj/Tidy_Data.RData")


dta_res_tidy <- dta_long2


####################################################################################

eu_exp_male <- Outlist$expectations$male[europe_indicators]
eu_exp_female <- Outlist$expectations$female[europe_indicators]
eu_exp_total <- Outlist$expectations$total[europe_indicators]

dta_long_male <- ldply(
  eu_exp_male,
  fn,
  label="expectation",
  sex_="male"
)

dta_long_female <- ldply(
  eu_exp_female,
  fn, 
  label="expectation",
  sex_="female"
)

dta_long_total <- ldply(
  eu_exp_total,
  fn,
  label="expectation",
  sex_="total"
)


names(dta_long_male)[1] <- "country"
names(dta_long_female)[1] <- "country"
names(dta_long_total)[1] <- "country"

dta_long <- rbind(dta_long_male, dta_long_female, dta_long_total)

rm(dta_long_male, dta_long_female, dta_long_total)

dta_long2 <- melt(dta_long, id.var=c("country", "age", "year", "sex"))

dta_tidy_long <- rbind(dta_res_tidy, dta_long2)

dta_tidy <- dcast(dta_tidy_long, country + age + year + sex ~ variable)
# g <- ggplot(subset(dta_wide, year > 1995)) + aes(x=year, y=male, group=age)
dta_tidy <- mutate(dta_tidy, residual_proportion=residual/expectation)

save(dta_tidy, file="Data/RObj/Tidy_Data.RData")

###############################################################################
# thought I was being stupid.
# 
# Lines are so much better. Things start to look odd now after 2001 - 
# especially for young women - first rapid move to the left and mortality 
# rates for eth youngest appear to drop below log -8.
# 
# Men in 2002 do what women did in 2001.
# 
# Note how little change there is for 40 year olds. 
# Stuck in the country they were in for the last ten years - 
#   so apparent rapid improvement in mortality and no sudden apparent migration…
# 
# Is the data not complete for 2007, 2009 and 2010?
# 
# 1990 is a good year to start - even though the data look erratic until 1993. 
# After the wall fell in 1989 there was quite rapid movement - 
#   especially of young women from East Germany to the West, and probably from 
# the other countries East of the iron curtain.
# 
# A final suggestion from me before I call it a weekend!
#   
#   Maybe draw a single overview graph, with lines again, and with the Y axis 
# still being death rates - but the X axis  being year 1990-2008. An a separate 
# line for each single year of age and sex.
# 
# In other words just one line for women aged 25 on the graph with the 
# height of points being death rates and the X position be year. It should 
# work with so many lines as they tend to be parallel. lines will bend quickly 
# down towards 2008 as mortality rates of the youngest age groups drop quickly 
# in recent years - quicker than makes much sense unless there is a problem 
# with the data.
# 
# However I’ll just find you an email from BRAKE I got in the last two days 
#which gives another possibility….
# 
# Danny

