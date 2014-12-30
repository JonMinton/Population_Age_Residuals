
rm(list=ls())

require(plyr)
require(lattice)
require(latticeExtra)
require(RColorBrewer)



setwd("E:/repos/Population_Age_Residuals/apps/selection_app")
counts <- read.csv("data/counts.csv")
counts$X <- NULL
counts <- subset(counts, subset=sex!="total")
print("loaded count data")

expectations <- read.csv("data/expectations.csv")
expectations <- subset(expectations, subset=sex!="total")
print("loaded expectations data")

country_codes <- read.csv("data/country_codes__new.csv", stringsAsFactors=F)
hmd_years <- read.csv("data/hmd_years.csv")
country_info <- country_codes %>% left_join(hmd_years, by=c("short"="country"))
country_info$labels <- with(country_info,
                            paste0(
                              long, " (", min_year, "-", max_year, ")"
                            )
)
print("loaded and composed country info and labels")

shinyServer(function(input, output){
  print("entered main shiny server")
  #select specific country
  redo_everything <- reactive({
    go <- input$country_group_selection_compile
    
    if (go){
      out <- get_country_selection()
    } else {out <- NULL}
    return(out)
  })
  
  get_country_selection <- reactive({
    tmp <- input$country_group_selection
    
    age_min <- input$age_range[1]
    age_max <- input$age_range[2]
    year_min <- input$year_range[1]
    year_max <- input$year_range[2]
    
    countries_selected <- subset(
        country_info,
        subset=labels %in% tmp
        )$short
      
    counts_ss <- subset(
      counts,
      subset = country %in% countries_selected & 
        age >= age_min & age <=age_max & 
        year >= year_min & year <= year_max
      )
  
    expectations_ss <- subset(
      expectations,
      subset= country %in% countries_selected & 
        age >= age_min & age <=age_max & 
        year >= year_min & year <= year_max
      )
    
    out <- join(counts_ss, expectations_ss)
    out <- ddply(
      out, .(sex, year, age),
      summarise,
      death_count=sum(death_count),
      population_count=sum(population_count),
      population_expected= sum(population_expected)
      )
    out <- mutate(out,
                  death_rate = death_count/population_count,
                  ppr = 1000 * (population_count - population_expected) / population_expected
                  )
    
    return(out)
  })
  
  output$table01 <- renderTable({
    tmp <- redo_everything()
    
    if (!is.null(tmp)){
      out <- head(tmp)
    } else {out <- NULL}
    return(out)
  })
  
  output$plot_scp_mort <- renderPlot({
    dta <- redo_everything()
    
    if(!is.null(dta)){
       out <- contourplot(
      death_rate ~ year * age | sex, 
      data=dta, 
      region=T, 
      col.regions=rev(heat.colors(200)), 
      cuts=50, 
      main=NULL
      )      
    } else {out <- NULL}
    return(out)
  }, height=800, width=1600)
  
  output$plot_scp_ppr <- renderPlot({
    dta <- redo_everything()
    
    if(!is.null(dta)){
 
         mx <- max(abs(dta$ppr))
         
         lims <- seq(from= -18, to = 18, by=2)
         lims <- lims[c(-1, -length(lims))]
         cols_to_use <- brewer.pal(5, "RdBu") # red-blue diverging scale
      #   # interpolate to more colours
         cols_to_use.fn <- colorRampPalette(cols_to_use)
       out <- contourplot(
        ppr ~ year * age | sex, 
        data=dta, 
        region=T, 
        at=lims,
        col.regions=rev(cols_to_use.fn(200)), 
        main=NULL
        )
      
    } else {out <- NULL}
    return(out)
  }, height=800, width=1600)
  
  
})
#   
#   output$text_clp_title <- renderText({
#   })
#   

  
#   
#   output$plot_overall <- renderPlot({
#   }, )
#   
#   output$plot_clp <- renderPlot({
#   }, height=800, width=1600)
#   
#   output$plot_composite <- renderPlot({
#   }, height=800, width=1600)
  



#   #   Maybe draw a single overview graph, with lines again, and with the Y axis 
#   # still being death rates - but the X axis  being year 1990-2008. An a separate 
#   # line for each single year of age and sex.
#   # 
#   # In other words just one line for women aged 25 on the graph with the 
#   # height of points being death rates and the X position be year. It should 
#   # work with so many lines as they tend to be parallel. lines will bend quickly 
#   # down towards 2008 as mortality rates of the youngest age groups drop quickly 
#   # in recent years - quicker than makes much sense unless there is a problem 
#   # with the data.
#   
#   
#   
#   
#   ###################################################################################################
#   
#   ## as above but only the 15 countries in 2011
#   europe_codes <- country_codes$short[which(country_codes$europe==1)]
#   
#   tmp <- dcast(counts, year + country ~ . , length) 
#   tmp <- subset(tmp, subset=year==2011)
#   tmp[,3] <- NULL
#   eu_2011_countries <- as.character(tmp$country[which(tmp$country %in%  europe_codes)])
#   
#   
#   
#   ## Now looking at the ages 20 to 50, from 1970 onwards
#   # ##################
#   # png(
#   #   "figures/contour_later15europe_identity.png",  
#   #   height=1000, width=2000
#   # )

#   # dev.off()
#   
#   #####################
#   png(
#     "figures/figure_a_contour_later15europe_log.png",  
#     height=1000, width=2000
#   )
#   print(
#     contourplot(
#       log(death_rate) ~ year * age | sex, 
#       data=subset(rates_15_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970 & year <=2011), 
#       region=T, 
#       col.regions=rev(heat.colors(200)), 
#       cuts=50, 
#       main=NULL
#     )
#   )
#   dev.off()
#   
#   
#   
#   
#   ###########################################################################
#   # RESIDUALS ONLY 
#   ## Now looking at the ages 20 to 50, from 1970 onwards
#   ##################
#   png(
#     "figures/figure_b__residuals_later15europe.png",  
#     height=1000, width=2000
#   )
#   dta_ss <- subset(exp_15_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970 & year <=2011)
#   
#   
#   
#   # mortality (log) and residuals on same plot, 20-50
#   png("Figures/figure_c_combined_later15europe.png",  
#       height=1000, width=2000)
#   p1 <- levelplot(
#     residual_prop * 1000 ~ year * age | sex, 
#     data = subset(exp_15_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970 & year <= 2011),
#     cuts=50,
#     at = lims,
#     col.regions = rev(cols_to_use.fn(200)),
#     main = NULL
#   )
#   
#   p2 <- contourplot(
#     log(death_rate) ~ year * age | sex, 
#     data=subset(rates_15_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970 & year <= 2011), 
#     cuts=50)
#   
#   print(p1 + p2)
#   dev.off()
#   
#   
#   # EVERYTHING BELOW THIS LINE SHOULD BE CONSIDERED SECONDARY 
#   ##########################################################################################
#   ##########################################################################################
#   
#   # mortality regular and residuals on same plot, 20-50
#   png("Figures/mortality_and_residuals_later15europe_identity.png",  
#       height=1000, width=2000)
#   p1 <- levelplot(
#     residual_prop * 1000 ~ year * age | sex, 
#     data = subset(exp_15_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970),
#     cuts=50,
#     at = seq(from= -20, to = 20, by=2),
#     col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(32),
#     main = "mortality rates (contour) and population errors (shaded), identity scale, European subset")
#   
#   p2 <- contourplot(
#     death_rate ~ year * age | sex, 
#     data=subset(rates_15_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970), 
#     cuts=50)
#   
#   print(p1 + p2)
#   dev.off()
#   
#   # mortality (log) and residuals on same plot, 20-50
#   png("Figures/mortality_and_residuals_later_older14europe_log01.png",  
#       height=1000, width=2000)
#   
#   p1 <- levelplot(
#     residual_prop * 1000 ~ year * age | sex, 
#     data = subset(exp_15_all, subset=sex!="total" & age >= 50 &age <= 80 & year >= 1970),
#     cuts=50,
#     at = seq(from= -16, to = 16, by=2),
#     col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(32),
#     main = "mortality rates (contour) and population errors (shaded), log scale, 50 to 80 years European subset")
#   
#   p2 <- contourplot(
#     log(death_rate) ~ year * age | sex, 
#     data=subset(rates_15_all, subset=sex!="total" & age >= 50 & age <=80 & year >=1970), 
#     cuts=50)
#   
#   print(p1 + p2)
#   dev.off()