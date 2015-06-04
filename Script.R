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


# SCPs --------------------------------------------------------------------



png(
  "figures/residuals_all_2011.png",  
  height=20, width=40,
  res=300, units="cm"
)

dta_ss <- expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)  %>% 
  filter(sex!="total" & age >=20 & age <=50 & 
           year >=1970 & year <=2011 & region =="All")

mx <- max(abs(dta_ss$residual_prop))

lims <- seq(from= -20, to = 20, by=2)

cols_to_use <- brewer.pal(5, "RdBu") # red-blue diverging scale
# interpolate to more colours
cols_to_use.fn <- colorRampPalette(cols_to_use)
#greyscale strip

print(
  contourplot(
    residual_prop ~ year * age | sex, 
    data=dta_ss, 
    region=T, 
    at=lims,
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL,
    col="black",
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
  )
)
dev.off()

# for each region

dta_ss <- expected_europe_2011  %>% 
  filter(!is.na(expected_count))  %>% 
  mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)  %>% 
  filter(sex!="total" & age >=20 & age <=50 & year >=1970 & year <=2011)

mx <- max(abs(dta_ss$residual_prop))

lims <- seq(from= -40, to = 40, by=5)

cols_to_use <- brewer.pal(5, "RdBu") # red-blue diverging scale
# interpolate to more colours
cols_to_use.fn <- colorRampPalette(cols_to_use)

png(
  "figures/residuals_lattice_2011.png",  
  height=20, width=30,
  res=300, units="cm"
)

print(
  contourplot(
    residual_prop ~ year * age | region * sex, 
    data=dta_ss, 
    region=T, 
    at=lims,
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL,
    col="grey",
    strip=strip.custom(par.strip.text=list(cex=1), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    labels=list(cex=0.9),
    scales=list(
      x=list(cex=1.0), 
      y=list(cex=1.0),
      alternating=3
    )
  )
)
dev.off()

# Mortality rates: all 


png(
  "figures/log_mort_all_2011.png",  
  height=20, width=40,
  res=300, units="cm"
)
dta_ss <- expected_europe_2011 %>% 
  filter(
    sex!="total" & age >= 20 & age <=50 & 
      year >=1970 & year <=2011 & 
      region=="All"
  ) %>% 
  mutate(
    death_rate = death_count/population_count,
    lg_death_rate = log(death_rate, base=10)
    ) 

print(
  contourplot(
    lg_death_rate ~ year * age | sex, 
    data=dta_ss, 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=15,
    col.regions=colorRampPalette(brewer.pal(9, "Reds"))(100),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
    
  )
)
dev.off()

# for each region

png(
  "figures/log_mort_lattice_2011.png",  
  height=20, width=30,
  res=300, units="cm"
)

dta_ss <- expected_europe_2011 %>% 
  filter(
    sex!="total" & age >= 20 & age <=50 & 
      year >=1970 & year <=2011  ) %>% 
  mutate(
    death_rate = death_count/population_count,
    lg_death_rate = log(death_rate, base=10)
  ) 

# should be smoothed for Eastern and Northern Europe...


print(
  contourplot(
    lg_death_rate ~ year * age | region * sex, 
    data=dta_ss, 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=15,
    col.regions=colorRampPalette(brewer.pal(9, "Reds"))(100),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
    
  )
)
dev.off()


png(
  "figures/figure_c_combined.png",  
  height=20, width=40,
  res=300, units="cm"
)

# mortality (log) and residuals on same plot, 20-50
p1 <- expected_europe_2011 %>% 
  filter(!is.na(expected_count))  %>% 
  mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)  %>% 
  filter(sex!="total" & age >=20 & age <=50 & year >=1970 & year <=2011) %>% 
  levelplot(
  residual_prop  ~ year * age | sex, 
  data = . ,
  cuts=30,
  at = lims,
  col.regions = rev(cols_to_use.fn(200)),
  main = NULL,
  strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  cex=1.4,
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
)

p2 <- contourplot(
  log(death_rate) ~ year * age | sex, 
  data=subset(rates_15_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970 & year <= 2011), 
  cuts=30,
  labels=list(cex=1.2),
  cex=1.4,
  region=F,
  main=NULL,
  col="black",
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
)


print(p1 + p2)
dev.off()

