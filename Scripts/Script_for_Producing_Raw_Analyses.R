

# Let's look at people aged under 80 years

#  Start with one country and then generalise

Make_Residuals_and_Expectations <- function(
    Country.Codes,
    Populations.numeric,
    Deaths.numeric,
    max_age=80
    ){
    N.countries <- dim(Country.Codes)[1]
    
    
    residuals.male.list <- vector("list", length=dim(Country.Codes)[1])
    names(residuals.male.list) <- as.character(Country.Codes[,1])

    residuals.total.list <- residuals.female.list <- residuals.male.list    
    expected.male.list <- residuals.total.list
    expected.total.list <- expected.female.list <- expected.male.list
    
    for (cntry in 1:N.countries){
        pops.ds <- Populations.numeric[[Country.Codes[cntry,1]]]
        pops.ds <- subset(pops.ds, Age <= max_age) 
        pops.ds$Year <- as.numeric(pops.ds$Year)
        
        deaths.ds <- Deaths.numeric[[Country.Codes[cntry,1]]]
        deaths.ds <- subset(deaths.ds, Age <= max_age)
        deaths.ds$Year <- as.numeric(deaths.ds$Year)
        
        years <- intersect(unique(pops.ds$Year), unique(deaths.ds$Year))
        ages <- intersect(unique(pops.ds$Age), unique(deaths.ds$Age))
        
        expected.ds.male <- matrix(NA,
                                   nrow=length(ages) - 1,
                                   ncol=length(years) - 1
        )
        
        dimnames(expected.ds.male) <- list(
            ages[-1],
            years[-1]
        )
        
        expected.ds.total <- expected.ds.female <- expected.ds.male
        residual.ds.total <- residual.ds.female <- residual.ds.male <- expected.ds.total
        
        for (i in 2:length(ages)){
            for (j in 2:length(years)){
                last.age <- ages[i-1]
                last.year <- years[j-1]
                this.age <- ages[i]
                this.year <- years[j]
                
                tmp1 <- subset(pops.ds, Age==last.age & Year == last.year)
                if (dim(tmp1)[1]!=1) break
                
                lives.expected.male  <- tmp1$Male
                lives.expected.female <- tmp1$Female
                lives.expected.total <- tmp1$Total
                
                tmp2 <- subset(deaths.ds, Age==last.age & Year==last.year)
                if (dim(tmp2)[1]!=1) break
                
                deaths.reported.male <- tmp2$Male
                deaths.reported.female <- tmp2$Female
                deaths.reported.total <- tmp2$Total
                
                lives.expected.male <- lives.expected.male - deaths.reported.male
                lives.expected.female <- lives.expected.female - deaths.reported.female
                lives.expected.total <- lives.expected.total - deaths.reported.total
                
                if (length(lives.expected.male)==1){   expected.ds.male[ i - 1, j - 1] <- lives.expected.male }
                if (length(lives.expected.female)==1) { expected.ds.female[i - 1, j - 1] <- lives.expected.female}
                if (length(lives.expected.total)==1) { expected.ds.total[i - 1, j -1 ] <- lives.expected.total}            
                
                tmp3 <- subset(pops.ds, Age==this.age & Year==this.year)
                if (dim(tmp3)[1]!=1) break
                
                lives.actual.male <- tmp3$Male
                lives.actual.female <- tmp3$Female
                lives.actual.total <- tmp3$Total
                
                lives.residual.male <- lives.expected.male - lives.actual.male
                lives.residual.female <- lives.expected.female - lives.actual.female
                lives.residual.total <- lives.expected.total - lives.actual.total
                
                
                
                if (length(lives.residual.male)==1) { residual.ds.male[   i - 1, j - 1] <- lives.residual.male }
                if (length(lives.residual.female)==1) { residual.ds.female[i - 1, j - 1] <- lives.residual.female}
                if (length(lives.residual.total)==1) {residual.ds.total[i - 1, j - 1] <- lives.residual.total}
            }
        }
        
    
        residuals.male.list[[cntry]] <- residual.ds.male
        residuals.female.list[[cntry]] <- residual.ds.female
        residuals.total.list[[cntry]] <- residual.ds.total
        
        expected.male.list[[cntry]] <- expected.ds.male
        expected.female.list[[cntry]] <- expected.ds.female
        expected.total.list[[cntry]] <- expected.ds.total
        
    }
    outlist <- list(
        residuals=list(
            male=residuals.male.list,
            female=residuals.female.list,
            total=residuals.total.list
            ),
        expectations=list(
            male=expected.male.list,
            female=expected.female.list,
            total=expected.total.list
            )
        )
    return(outlist)
}

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


