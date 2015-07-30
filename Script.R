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


#Outputs of interest

outputs <- expand.grid(
  dta = c("all", "2011"),
  range = c("short", "long"),
  plt = c("overall", "lattice"),
  out = c("cmr", "ppr", "composite", "cors")
  )

# > outputs
# dta range     plt       out
# 1   all short overall       cmr
# 2  2011 short overall       cmr
# 3   all  long overall       cmr
# 4  2011  long overall       cmr
# 5   all short lattice       cmr
# 6  2011 short lattice       cmr
# 7   all  long lattice       cmr
# 8  2011  long lattice       cmr
# 9   all short overall       ppr
# 10 2011 short overall       ppr
# 11  all  long overall       ppr
# 12 2011  long overall       ppr
# 13  all short lattice       ppr
# 14 2011 short lattice       ppr
# 15  all  long lattice       ppr
# 16 2011  long lattice       ppr
# 17  all short overall composite
# 18 2011 short overall composite
# 19  all  long overall composite
# 20 2011  long overall composite
# 21  all short lattice composite
# 22 2011 short lattice composite
# 23  all  long lattice composite
# 24 2011  long lattice composite
# 25  all short overall      cors XXX
# 26 2011 short overall      cors XXX
# 27  all  long overall      cors
# 28 2011  long overall      cors
# 29  all short lattice      cors XXX
# 30 2011 short lattice      cors XXX
# 31  all  long lattice      cors
# 32 2011  long lattice      cors

# > outputs
# dta range     plt       out
# 1   all short overall       cmr
png(
  "figures/all_short_overall_cmr.png",  
  height=20, width=40,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >=20 & age <=50 & 
           year >=1970 & year <=2009 & 
           region =="All") %>%   
  plot_lgcmr(.) %>% print

dev.off()


# 2  2011 short overall       cmr

png(
  "figures/2011_short_overall_cmr.png",  
  height=20, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >=20 & age <=50 & 
           year >=1970 & year <=2011 & 
           region =="All") %>%   
  plot_lgcmr(.) %>% print

dev.off()

# 3   all  long overall       cmr
png(
  "figures/all_long_overall_cmr.png",  
  height=20, width=40,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <=90 & 
           year >=1950 & year <=2011 & 
           region =="All") %>%   
  plot_lgcmr(., CUTS = 20) %>% print

dev.off()


# 4  2011  long overall       cmr
png(
  "figures/2011_long_overall_cmr.png",  
  height=20, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <=90 & 
           year >=1950 & year <=2011 & 
           region =="All") %>%   
  plot_lgcmr(., CUTS = 20) %>% print

dev.off()


# 5   all short lattice       cmr

png(
  "figures/all_short_lattice_cmr.png",  
  height=20, width=40,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20  & age <=40 & 
           year >=1970 & year <=2011  
           ) %>%   
  plot_region_lgcmr(.) %>% print

dev.off()

# 6  2011 short lattice       cmr

png(
  "figures/2011_short_lattice_cmr.png",  
  height=20, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20  & age <=50 & 
           year >=1970 & year <=2010  
  ) %>%   
  plot_smoothed_region_lgcmr(., EDGE = 3) %>% 
  print

dev.off()

# 7   all  long lattice       cmr

png(
  "figures/all_long_lattice_cmr.png",  
  height=40, width=40,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 90 & 
           year >=1950 & year <=2009  
  ) %>%   
  plot_smoothed_region_lgcmr(., EDGE = 3) %>% 
  print

dev.off()


# 8  2011  long lattice       cmr

png(
  "figures/all_long_lattice_cmr.png",  
  height=40, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 90 & 
           year >=1950 & year <=2011  
  ) %>%   
  plot_smoothed_region_lgcmr(., EDGE = 3) %>% 
  print

dev.off()

# 9   all short overall       ppr

png(
  "figures/all_short_overall_ppr.png",  
  height=40, width=40,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20 & age <= 50 & 
           year >=1970 & year <=2011  
  ) %>%   
  plot_ppr(., COL = "grey") %>% 
  print

dev.off()

# 10 2011 short overall       ppr

png(
  "figures/2011_short_overall_ppr.png",  
  height=40, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20 & age <= 50 & 
           year >=1970 & year <=2011  
  ) %>%   
  plot_ppr(., COL = "grey") %>% 
  print

dev.off()

# 11  all  long overall       ppr

png(
  "figures/all_long_overall_ppr.png",  
  height=40, width=40,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 90 & 
           year >=1950 & year <=2011  
  ) %>%   
  plot_ppr_level(.) %>% 
  print

dev.off()


# 12 2011  long overall       ppr

png(
  "figures/2011_long_overall_ppr.png",  
  height=40, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 90 & 
           year >=1950 & year <=2011  
  ) %>%   
  plot_ppr_level(.) %>% 
  print

dev.off()

# 13  all short lattice       ppr
png(
  "figures/all_short_lattice_ppr.png",  
  height=25, width=50,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20 & age <= 50 & 
           year >=1970 & year <=2011  
  ) %>%   
  plot_level_region_ppr(. , 
                        LIMS = seq(from= -34, to = 34, by=2)                 
                        
                        ) %>% 
  print

dev.off()

# 14 2011 short lattice       ppr

png(
  "figures/2011_short_lattice_ppr.png",  
  height=25, width=50,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20 & age <= 50 & 
           year >=1970 & year <=2011  
  ) %>%   
  plot_level_region_ppr(. , 
                        LIMS = seq(from= -40, to = 40, by=2)                 
                        
  ) %>% 
  print

dev.off()

# 15  all  long lattice       ppr
png(
  "figures/all_long_lattice_ppr.png",  
  height=40, width=50,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 90 & 
           year >= 1950 & year <=2009  
  ) %>%   
  plot_smoothed_region_ppr(. , COL = "grey",
                        LIMS = seq(from= -35, to = 35, by=5)                 
                        
  ) %>% 
  print

dev.off()


# 16 2011  long lattice       ppr

png(
  "figures/2011_long_lattice_ppr.png",  
  height=40, width=50,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 80 & 
           year >= 1950 & year <=2011  
  ) %>%   
  plot_level_region_ppr(. , 
                           LIMS = seq(from= -40, to = 40, by=5)                 
                           
  ) %>% 
  print

dev.off()

# 17  all short overall composite

png(
  "figures/all_short_overall_composite.png",  
  height=25, width=40,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20 & age <= 50 & 
           year >= 1970 & year <=2009  &
           region =="All"
  ) %>%   
  plot_composite(.       
  ) %>% 
  print

dev.off()

# 18 2011 short overall composite
png(
  "figures/2011_short_overall_composite.png",  
  height=25, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20 & age <= 50 & 
           year >= 1970 & year <=2009  &
           region =="All"
  ) %>%   
  plot_composite(.       
  ) %>% 
  print

dev.off()

# 19  all  long overall composite
png(
  "figures/all_long_overall_composite.png",  
  height=40, width=40,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 80 & 
           year >= 1950 & year <=2009  &
           region =="All"
  ) %>%   
  plot_composite(.       
  ) %>% 
  print

dev.off()


# 20 2011  long overall composite
png(
  "figures/2011_long_overall_composite.png",  
  height=30, width=40,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 80 & 
           year >= 1950 & year <=2011  &
           region =="All"
  ) %>%   
  plot_composite(.       
  ) %>% 
  print

dev.off()

# 21  all short lattice composite

png(
  "figures/all_short_lattice_composite.png",  
  height=25, width=50,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20 & age <= 50 & 
           year >= 1970 & year <=2009 
  ) %>%   
  plot_region_composite(.       
  ) %>% 
  print

dev.off()


# 22 2011 short lattice composite

png(
  "figures/2011_short_lattice_composite.png",  
  height=25, width=50,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age >= 20 & age <= 50 & 
           year >= 1970 & year <=2011 
  ) %>%   
  plot_region_composite(. ,
  LIMS = seq(from= -50, to = 50, by=2)
  ) %>% 
  print

dev.off()


# 23  all  long lattice composite

png(
  "figures/all_long_lattice_composite.png",  
  height=25, width=50,
  res=300, units="cm"
)

expected_europe  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 80 & 
           year >= 1950 & year <=2009 
  ) %>%   
  plot_region_composite(.       
  ) %>% 
  print

dev.off()

# 24 2011  long lattice composite

png(
  "figures/2011_long_lattice_composite.png",  
  height=25, width=50,
  res=300, units="cm"
)

expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  filter(sex!="total" & 
           age <= 80 & 
           year >= 1950 & year <=2011 
  ) %>%   
  plot_region_composite(. , LIMS = seq(-40, 40, by=4)
                        
  ) %>% 
  print

dev.off()

# 27  all  long overall      cors

local_corrs_all <-   expected_europe %>% 
  filter( age >= 0 & age <= 80 & 
            year >= 1960 & year <=2009 & 
            sex != "total" & region == "All")  %>% 
  do(calc_windowed_correlations(. , WINDOW = 6))
png(
  "figures/all_long_overall_corrs.png",  
  height=40, width=60,
  res=300, units="cm"
)
local_corrs_all %>% plot_local_cor(.) %>% print
dev.off()

# 28 2011  long overall      cors
local_corrs_2011 <-   expected_europe_2011 %>% 
  filter( age >= 0 & age <= 80 & 
            year >= 1960 & year <=2011 & 
            sex != "total" & region =="All")  %>% 
  do(calc_windowed_correlations(. , WINDOW = 6))
png(
  "figures/2011_long_overall_corrs.png",  
  height=40, width=60,
  res=300, units="cm"
)

local_corrs_2011 %>% plot_local_cor(.) %>% print
dev.off()
# 31 all  long lattice      cors
local_corrs_region_all <-   expected_europe %>% 
  filter( age >= 0 & age <= 80 & 
            year >= 1960 & year <=2009 & 
            sex != "total" )  %>% 
  group_by(region) %>% do(calc_windowed_correlations(. , WINDOW = 6))

png(
  "figures/all_long_lattice_corrs.png",  
  height=40, width=60,
  res=300, units="cm"
)

local_corrs_region_all %>% plot_local_cor_region(.) %>% print
dev.off()

# 32 2011  long lattice      cors
local_corrs_region_2011 <-   expected_europe_2011 %>% 
  filter( age >= 0 & age <= 80 & 
            year >= 1960 & year <=2011 & 
            sex != "total" )  %>% 
  group_by(region) %>% do(calc_windowed_correlations(. , WINDOW = 6))

png(
  "figures/2011_long_lattice_corrs.png",  
  height=40, width=60,
  res=300, units="cm"
)
local_corrs_region_2011 %>% plot_local_cor_region(.) %>% print
dev.off()

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
  
  
local_corrs_region <-   expected_europe_2011 %>% 
    filter( age >= 0 & age <= 80 & 
              year >= 1960 & year <=2011 & 
              sex != "total" )  %>% 
    group_by(region) %>% do(calc_windowed_correlations(. , WINDOW = 6))

png("figures/local_correlations_by_region_map.png",
    height = 40, width = 80,
    res=300, units = "cm"
)
cols_to_use.fn <- colorRampPalette(brewer.pal(5, "RdBu"))
contourplot(
  local_cor ~ year * age | region + sex, 
  data=local_corrs_region, 
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
