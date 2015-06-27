rm(list=ls())

# Suggested in: 
#https://github.com/skardhamar/rga/issues/6
#options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

# TO DO ;
# 1) Re-do analyses with Switzerland as part of Europe. 
# 2) Incorporate composite graph

###########################################################################################################
###########################################################################################################
# Run some of the existing scripts and analyses using the newer data : use this to produce the 
# derived data 


require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(RColorBrewer)
require(lattice)
require(latticeExtra)
require(ggplot2)

require(fields) 
require(spatstat)

############################################################################################################
############################################################################################################
source("Scripts/make_tidy_data.r")
source("Scripts/smoother_function.R")
source("Scripts/lexis_surface_helper_functions.R")

# SCPs --------------------------------------------------------------------

png(
  "figures/ppr_all_europe2011_age20_50_year1970_2010.png",  
  height=20, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >=20 & age <=50 & 
           year >=1970 & year <=2011 & 
           region =="All") %>% 
  
  plot_ppr(., COL="darkgrey") %>% print

dev.off()

###############################################################################

png(
  "figures/ppr_all_europe2011_age0_90_year1950_2010.png",  
  height=20, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >=0 & age <=80 & 
           year >=1950 & year <=2011 & 
           region =="All") %>% 
  mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)  %>% 
  plot_smoothed_ppr(., COL="darkgrey", SMOOTH_PAR=1.0) %>% print


dev.off()


# for each region



png(
  "figures/residuals_lattice_2011.png",  
  height=25, width=30,
  res=300, units="cm"
)
expected_europe_2011 %>% 
  filter(!is.na(expected_count)) %>% 
  mutate(residual_prop = 1000 * (population_count - expected_count) / expected_count) %>% 
  filter(sex !="total" & 
           age <= 80 & 
           year >= 1960 & year <= 2010
  ) %>% 
  plot_smoothed_region_ppr(. , 
                           LIMS = seq(from = -40, to = 40, by = 5),
                           SMOOTH_PAR = 1.0,
                           COL = "darkgrey"
  ) 

dev.off()

# Mortality rates: all 


png(
  "figures/lgcmr_all_europe2011_age20_50_year1970_2010.png",  
  height=20, width=30,
  res=300, units="cm"
)
expected_europe_2011 %>% 
  filter(
    sex!="total" & age >= 20 & age <=50 & 
      year >=1970 & year <=2011 & 
      region == "All"
  ) %>% plot_lgcmr() %>% print
dev.off()

# for each region
png(
  "figures/lg_cmr_lattice_age_20_50_year_1970_2011.png",  
  height=25, width=40,
  res=300, units="cm"
)

expected_europe_2011 %>% 
  filter(
    sex!="total" & age >= 20 & age <=50 & 
      year >=1970 & year <=2011 
  ) %>% plot_smoothed_region_lgcmr(. , SMOOTH_PAR=0.8) %>% print

dev.off()


png(
  "figures/lg_cmr_lattice_age_0_80_year_1960_2010.png",  
  height=25, width=30,
  res=300, units="cm"
)

expected_europe_2011 %>% 
  filter(
    sex!="total" & age >= 0 & age <=90 & 
      year >=1960 & year <= 2010 
  ) %>% plot_smoothed_region_lgcmr(. , SMOOTH_PAR=0.8) %>% print

dev.off()




png(
  "figures/figure_c_combined.png",  
  height=20, width=40,
  res=300, units="cm"
)

# mortality (log) and residuals on same plot, 20-50
png("figures/composite_years_1970_2011_ages_20_50.png",
    height = 30, width = 30,
    res=300, units = "cm"
    )
expected_europe_2011 %>% 
  filter( age >= 20 & age <= 50 & 
            year >= 1970 & year <=2011 & 
  sex != "total" & region == "All" ) %>% 
  plot_composite(.)  %>% print
dev.off()

png("figures/composite_years_1960_2011_ages_20_800.png",
    height = 30, width = 40,
    res=300, units = "cm"
)

expected_europe_2011 %>% 
  filter( age >= 0 & age <= 80 & 
            year >= 1960 & year <=2011 & 
            sex != "total" & region == "All" ) %>% 
  plot_composite(., LIMS = seq(from= -20, to = 20, by=2), CUTS = 25)  %>% print
dev.off()



# Map of local correlations -----------------------------------------------

local_corrs <- expected_europe_2011 %>% 
  filter( age >= 0 & age <= 80 & 
            year >= 1960 & year <=2011 & 
            sex != "total" & region == "All" )  %>% 
  calc_windowed_correlations(WINDOW = 6)


png("figures/local_correlations_map.png",
    height = 30, width = 40,
    res=300, units = "cm"
)
  cols_to_use.fn <- colorRampPalette(brewer.pal(5, "RdBu"))
  contourplot(
    local_cor ~ year * age | sex, 
    data=local_corrs, 
    region=T, 
    at=  seq(from= -1, to = 1, by=0.2),                 
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL,
    col="black",
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    labels=list(cex=1.2),
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  ) %>% print
  
  dev.off()
  
