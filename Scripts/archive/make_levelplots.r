
dir.create("Figures/Expectations/Male", recursive=T)
dir.create("Figures/Expectations/Female", recursive=T)
dir.create("Figures/Expectations/Total", recursive=T)
dir.create("Figures/Residuals/Male", recursive=T)
dir.create("Figures/Residuals/Female", recursive=T)
dir.create("Figures/Residuals/Total", recursive=T)


cols_to_use <- c(
    "#67001f",
    "#b2182b",
    "#d6604d",
    "#f4a582",
    "#fddbc7",
    "#f7f7f7",
    "#d1e5f0",
    "#92c5de",
    "#4393c3",
    "#2166ac",
    "#053061"
) # selected using colorbrewer website



for (i in c("Residuals", "Expectations")){
    for (j in c("Male", "Female", "Total")){
     input_dir <- paste0(
         "Outputs/Raw/",
         i, "/",
         j, "/"
     )
     files_to_access <- list.files(input_dir, pattern="*.csv")
     N.files <- length(files_to_access)
     
     for (k in 1:N.files){
         data_input <- read.csv(
             paste0(
                 input_dir,
                 files_to_access[k]
                 )
             )
         mtrx <- as.matrix(data_input[,-1])
         rownames(mtrx) <- data_input$X
         colnames(mtrx) <- substring(names(data_input)[-1], 2)
         
         lim <- max(abs(data_input))
         cuts <- c(-(1/5)*lim * 5:0,
                   (1/5) * lim * 1:5)
            
         tiff(
             paste0(
                 "Figures/", i, "/",
                 j, "/", 
                 strsplit(files_to_access[k], "[.]")[[1]][1],
                 ".tiff"
                 ),
             height=1000, width=1000
             )
         
         print(
             levelplot(
             mtrx, 
             region=T,  
             pretty=T, 
             col.regions=rev(cols_to_use), 
             at=cuts, 
             ylab="year", 
             xlab="age", 
             scales=list(x=list(at=seq(from=5, to=80, by=5)), 
                         y=list(at=seq(from=5, to= dim(mtrx)[2], by=5)
                                )
                         )
             )
         )
         dev.off()
         
        }
    }
}

