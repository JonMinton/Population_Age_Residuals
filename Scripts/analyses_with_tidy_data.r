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




###################################################################################################

## as above but only the 15 countries in 2011
europe_codes <- country_codes$short[which(country_codes$europe==1)]

tmp <- dcast(counts, year + country ~ . , length) 
tmp <- subset(tmp, subset=year==2011)
tmp[,3] <- NULL
eu_2011_countries <- as.character(tmp$country[which(tmp$country %in%  europe_codes)])



## Now looking at the ages 20 to 50, from 1970 onwards
# ##################
# png(
#   "figures/contour_later15europe_identity.png",  
#   height=1000, width=2000
# )
# contourplot(
#   death_rate ~ year * age | sex, 
#   data=subset(rates_15_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970), 
#   region=T, 
#   col.regions=rev(heat.colors(200)), 
#   cuts=50, 
#   main=NULL
# )
# dev.off()

# need to do so that res is 300dpi 
# 10 by 20 cm
#cm to inch
#0.394
#####################


cm_to_in <- 0.394
ppi <- 300
png(
  "figures/figure_b_lmort.png",  
  height=ppi * 10 * cm_to_in, width=ppi * 20 * cm_to_in
)
print(
  contourplot(
    log(death_rate) ~ year * age | sex, 
    data=subset(rates_15_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970 & year <=2011), 
    region=T, 
    col.regions=rev(heat.colors(200)), 
    cuts=50, 
    main=NULL,
    par.strip.text=list(cex=1.2, fontface="bold"),
    ylab=list(cex=1.3, label="Age in years"),
    xlab=list(cex=1.3, label="Year"),
    scales=list(cex=1.1)
    
    )
)
dev.off()

# Example of same image using small multiples

# sectioning by year

require(dplyr)

rates_15_all %>%
  filter(age >=20 & age <=50 & year >=1970 & year <=2011 & sex !="total") %>%
  ggplot(data=.) +
  geom_line(aes(x=age, y=log(death_rate), group=sex, colour=sex)) +
  facet_wrap(~year)
  


###########################################################################
# RESIDUALS ONLY 
## Now looking at the ages 20 to 50, from 1970 onwards
##################

cm_to_in <- 0.394
ppi <- 300
png(
  "figures/figure_a_residuals.png",  
  height=ppi * 10 * cm_to_in, width=ppi * 20 * cm_to_in
)
dta_ss <- subset(exp_15_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970 & year <=2011)
# want to know the maximum deviation from 0
dta_ss$residual_prop <- dta_ss$residual_prop * 1000


mx <- max(abs(dta_ss$residual_prop))

lims <- seq(from= -18, to = 18, by=2)
lims <- lims[c(-1, -length(lims))]
cols_to_use <- brewer.pal(5, "RdBu") # red-blue diverging scale
# interpolate to more colours
cols_to_use.fn <- colorRampPalette(cols_to_use)
print(
  contourplot(
  residual_prop ~ year * age | sex, 
  data=dta_ss, 
  region=T, 
  at=lims,
  col.regions=rev(cols_to_use.fn(200)), 
  main=NULL,
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab=list(cex=1.3, label="Age in years"),
  xlab=list(cex=1.3, label="Year"),
  scales=list(cex=1.1)
  )
)
dev.off()

# ggplot example

require(dplyr)

exp_15_all %>%
  filter(age >=20 & age <=50 & year >=1970 & year <=2011 & sex !="total") %>%
  ggplot(data=.) +
  geom_line(aes(x=age, y=residual_prop, group=sex, colour=sex)) +
  facet_wrap(~year)


cm_to_in <- 0.394
ppi <- 300
png(
  "figures/figure_c_combined.png",  
  height=ppi * 10 * cm_to_in, width=ppi * 20 * cm_to_in
)

# mortality (log) and residuals on same plot, 20-50
p1 <- levelplot(
  residual_prop * 1000 ~ year * age | sex, 
  data = subset(exp_15_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970 & year <= 2011),
  cuts=50,
  at = lims,
  col.regions = rev(cols_to_use.fn(200)),
  main = NULL,
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab=list(cex=1.3, label="Age in years"),
  xlab=list(cex=1.3, label="Year"),
  scales=list(cex=1.1)
)

p2 <- contourplot(
  log(death_rate) ~ year * age | sex, 
  data=subset(rates_15_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970 & year <= 2011), 
  cuts=50)

print(p1 + p2)
dev.off()


# EVERYTHING BELOW THIS LINE SHOULD BE CONSIDERED SECONDARY 
##########################################################################################
##########################################################################################

# Illustration for AQMEN briefing paper

###########################################################################
# RESIDUALS ONLY 
## Now looking at the ages 20 to 50, from 1970 onwards
##################
png(
  "figures/briefing_paper/residuals_later15europe_male_only.png",  
  height=800, width=800
)
dta_ss <- subset(exp_15_all, subset=sex=="total" & age >= 15 &age <= 60 & year >= 1970 & year <=2011)
# want to know the maximum deviation from 0
dta_ss$residual_prop <- dta_ss$residual_prop * 1000


mx <- max(abs(dta_ss$residual_prop))

lims <- seq(from= -18, to = 18, by=2)
lims <- lims[c(-1, -length(lims))]
cols_to_use <- brewer.pal(5, "RdBu") # red-blue diverging scale
# interpolate to more colours
cols_to_use.fn <- colorRampPalette(cols_to_use)
print(
  contourplot(
    residual_prop ~ year * age, 
    data=dta_ss, 
    region=T, 
    at=lims,
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL
  )
)
dev.off()


#########################################################################################
#########################################################################################



# mortality regular and residuals on same plot, 20-50
png("Figures/mortality_and_residuals_later15europe_identity.png",  
    height=1000, width=2000)
p1 <- levelplot(
  residual_prop * 1000 ~ year * age | sex, 
  data = subset(exp_15_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970),
  cuts=50,
  at = seq(from= -20, to = 20, by=2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(32),
  main = "mortality rates (contour) and population errors (shaded), identity scale, European subset")

p2 <- contourplot(
  death_rate ~ year * age | sex, 
  data=subset(rates_15_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970), 
  cuts=50)

print(p1 + p2)
dev.off()

# mortality (log) and residuals on same plot, 20-50
png("Figures/mortality_and_residuals_later_older14europe_log01.png",  
    height=1000, width=2000)

p1 <- levelplot(
  residual_prop * 1000 ~ year * age | sex, 
  data = subset(exp_15_all, subset=sex!="total" & age >= 50 &age <= 80 & year >= 1970),
  cuts=50,
  at = seq(from= -16, to = 16, by=2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(32),
  main = "mortality rates (contour) and population errors (shaded), log scale, 50 to 80 years European subset")

p2 <- contourplot(
  log(death_rate) ~ year * age | sex, 
  data=subset(rates_15_all, subset=sex!="total" & age >= 50 & age <=80 & year >=1970), 
  cuts=50)

print(p1 + p2)
dev.off()



##########################################################################################
#### Longer period of time 

#############################################################################################
#############################################################################################

png(
  "figures/contour_alleurope_identity.png",  
  height=1000, width=2000
)
contourplot(
  death_rate ~ year * age | sex, 
  data=subset(rates_eu_all, subset=sex!="total" & age <=80), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  main="")
dev.off()

#####################
png(
  "figures/contour_alleurope_log.png",  
  height=1000, width=2000
)
contourplot(
  log(death_rate) ~ year * age | sex, 
  data=subset(rates_eu_all, subset=sex!="total" & age <=80), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  main="")
dev.off()

#######################################################################

##################
png(
  "figures/contour_15europe_identity.png",  
  height=1000, width=2000
)
contourplot(
  death_rate ~ year * age | sex, 
  data=subset(rates_15_all, subset=sex!="total" & age <=80), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  main="")
dev.off()

#####################
png(
  "figures/contour_15europe_log.png",  
  height=1000, width=2000
)
contourplot(
  log(death_rate) ~ year * age | sex, 
  data=subset(rates_15_all, subset=sex!="total" & age <=80), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  main=NULL
)
dev.off()

##################################



########################################################################################
##### Analyses with ggplot (probably won't use)

####################################################################################################
####################################################################################################

# DERIVED DATA MANAGEMENT

# The code below will create the following datasets

# mrate_resp : mortality rate and residual proportion for the eu as a whole

# join exp_eu_all and rates_eu_all

mrate_resp <- join(
  rates_eu_all,
  exp_eu_all, 
  by=c("year", "age", "sex"),
  type="inner"
)

mrate_resp <- arrange(mrate_resp, age, year, sex)


mrate_resp$age_group <- recode(
  mrate_resp$age,
  recodes="
  1:4='4 or under';
  5:9 = '5-9';
  10:14 = '10-14';
  15:19 = '15-19';
  20:24 = '20-24';
  25:29 = '25-29';
  30:34 = '30-34';
  35:39 = '35-39';
  40:44 = '40-44';
  45:49 = '45-49';
  else = '50 or older'
  ",
  as.factor.result=T,
  levels=c(
    '4 or under',
    '5-9',
    '10-14',
    '15-19',
    '20-24',
    '25-29',
    '30-34',
    '35-39',
    '40-44',
    '45-49',
    '50 or older'
  )  
)

# cut ages into groups 

mrate_resp$age_group <- ordered(
  mrate_resp$age_group, 
  levels=c(
    '4 or under',
    '5-9',
    '10-14',
    '15-19',
    '20-24',
    '25-29',
    '30-34',
    '35-39',
    '40-44',
    '45-49',
    '50 or older'
  )
)


#############################################################################################
mrate_resp_agegroup <- ddply(
  mrate_resp, 
  .(sex, year, age_group), 
  summarise,
  population_count=sum(population_count),
  death_count=sum(death_count),
  population_actual=sum(population_actual),
  population_expected=sum(population_expected),
  death_rate=death_count/population_count,
  residual_count=population_actual - population_expected,
  residual_prop=residual_count/population_actual
)

mrate_resp_agegroup <- arrange(mrate_resp_agegroup, age_group, sex, year)
####

agegroups_of_interest <- c(
  '20-24',
  '25-29',
  '30-34',
  '35-39',
  '40-44',
  '45-49'
)



###############################################################################
###############################################################################
#####################
g <- ggplot(subset(
  mrate_resp,
  subset= sex!="total" & age <=50 & age > 20 & year > 1990
))

g2 <- g + aes(x=residual_prop, y=log(death_rate), group=sex)
g3 <- g2 + geom_line(aes(colour=year)) + facet_wrap( ~ age) + geom_point(aes(pch=sex), alpha=0.2)
g4 <- g3 + scale_colour_gradient(low="blue", high="red")
g4

ggsave("Figures/deathrate_vs_res_facet_age.png")


############################################################################

g <- ggplot(
  subset(
    mrate_resp,
    subset= sex!="total" & age <= 50 & age >=20 & year >=1990
  )
)

g2 <- g + aes(x=year, y=log(death_rate), group=age, colour=age)
g3 <- g2 + geom_line() + facet_wrap(~ sex) 
print(g3)

ggsave("Figures/deathrates_age.png")



###########################################################################

g <- ggplot(
  subset(
    mrate_resp_agegroup,
    subset= sex!="total" & year >=1990 & age_group %in% agegroups_of_interest
  )
)

g2 <- g + aes(x=year, death_rate, group=age_group, colour=age_group, lty=age_group)
g3 <- g2 + geom_line(size=1.1) + facet_wrap(~ sex) 
g4 <- g3 + scale_y_log10("death rate", limits=c(0.0001, 0.01), breaks=c(0.0001, 0.001, 0.01), labels=c("0.0001", "0.001", "0.01"))  
g5 <- g4

g5$labels$group <- "age group"
g5$labels$colour <- "age group"
g5$labels$linetype <- "age group"
print(g5)
# YAY! Manual adjustment of elements is possible!

ggsave("Figures/deathrates_agegroup.png")



###################################################################################

g <- ggplot(
  subset(mrate_resp_agegroup,
         subset= sex!="total" & year >=1990 &  year <= 2011 & age_group %in% agegroups_of_interest))

g2 <- g + aes(y=residual_prop, x=death_rate, group = sex, colour=year)
g3 <- g2 + geom_path(size=1.1) + facet_grid(age_group~.) + geom_hline(y=0, lty="dashed")
g4 <- g3 + geom_point(colour = "black", shape= "|", size = 3) + scale_x_log10(
  limits=c(0.0002, 0.010), 
  breaks=c(0.0002, 0.0005, 0.001, 0.002, 0.004, 0.01), 
  labels=c("0.0002", "0.0005", "0.001", "0.002", "0.004", "0.01"))
g5 <- g4 + labs(x="death rate", y="residual proportion")
print(g5)

ggsave("Figures/deathrates_residual_majaplot.png")
########################################################################


g <- ggplot(subset(mrate_resp, subset=year >=1990 & age >20 & age < 45 & sex !="total")) 
g2 <- g + aes(x=residual_prop, y=log(death_rate), colour=sex)
g3 <- g2 + geom_line() + facet_wrap( ~ year, nrow=3)
g4 <- g3 + geom_vline(v=0, lty="dashed")

print(g4)

ggsave("Figures/lines_residual_deathrates.png")


###############################################################################################
###############################################################################################


#############################################################################

g <- ggplot(
  subset(
    mrate_resp,
    subset= sex!="total" & age <= 50 & age >=20 & year >=1990
  )
)

g2 <- g + aes(x=year, y=death_rate, group=age, colour=age)
g3 <- g2 + geom_line() + facet_wrap(~ sex) 
g4 <- g3 + scale_y_log10(
  limits=c(0.0002, 0.010), 
  breaks=c(0.0002, 0.0005, 0.001, 0.002, 0.004, 0.01), 
  labels=c("0.0002", "0.0005", "0.001", "0.002", "0.004", "0.01"))
g5 <- g4 + labs(y="death rate", x="residual proportion")
print(g5)

ggsave("Figures/deathrates_age.png")

#############################################################################



g <- ggplot(
  subset(
    mrate_resp_agegroup,
    subset= sex!="total" & year >=1990 & age_group %in% agegroups_of_interest
  )
)

g2 <- g + aes(x=year, y=death_rate, group=age_group, colour=age_group, lty=age_group)
g3 <- g2 + geom_line(size=1.1) + facet_wrap(~ sex) 
g4 <- g3 + scale_y_log10(
  limits=c(0.0002, 0.01), 
  breaks=c(0.0002, 0.0005, 0.001, 0.002, 0.004, 0.01), 
  labels=c("0.0002", "0.0005", "0.001", "0.002", "0.004", "0.01"))
g5 <- g4 + labs(y="death rate", x="residual proportion")
print(g5)



ggsave("Figures/deathrates_agegroup_2011countriesonly.png")

############################################################################
# As above, but with a longer time series

g <- ggplot(
  subset(
    mrate_resp_agegroup,
    subset= sex!="total" & year >=1970 & age_group %in% agegroups_of_interest
  )
)

g2 <- g + aes(x=year, y=log(death_rate), group=age_group, colour=age_group, lty=age_group)
g3 <- g2 + geom_line(size=1.1) + facet_wrap(~ sex) 
print(g3)

ggsave("Figures/deathrates_agegroup_2011countriesonly_from1970.png")


#############################################################################

g <- ggplot(
  subset(
    mrate_resp_agegroup,
    subset= sex!="total" & year >=1990 & age_group %in% agegroups_of_interest
  )
)

g2 <- g + aes(x=residual_prop, y=log(death_rate), group=age_group, colour=year)
g3 <- g2 + geom_line(size=1.1) + facet_grid(age_group~ sex) + geom_vline(x=0, lty="dashed") 
print(g3)

ggsave("Figures/deathrates_resprop_agegroup.png")

g3 <- g2 + geom_point() + facet_grid(age_group~ sex) + geom_vline(x=0, lty="dashed") 
print(g3)
ggsave("Figures/deathrates_resprop_agegroup_scatter.png")

##########################################################################

g <- ggplot(
  subset(mrate_resp_agegroup,
         subset= sex!="total" & year >=1990 &  year <= 2011 & age_group %in% agegroups_of_interest))

g2 <- g + aes(y=residual_prop, x=log(death_rate), group = sex, colour=year)
g3 <- g2 + geom_path(size=1.1) + facet_grid(age_group~.) + geom_hline(y=0, lty="dashed")
g4 <- g3 + geom_point(colour = "black", shape= "|", size = 3)

print(g4)

ggsave("Figures/deathrates_residual_majaplot_2011countriesonly.png")
########################################################################


# As above, but with a longer time series

g <- ggplot(
  subset(mrate_resp_agegroup,
         subset= sex!="total" & year >=1970 &  year <= 2011 & age_group %in% agegroups_of_interest))

g2 <- g + aes(y=residual_prop, x=log(death_rate), group = sex, colour=year)
g3 <- g2 + geom_path(size=1.1) + facet_grid(age_group~.) + geom_hline(y=0, lty="dashed")
g4 <- g3 + geom_point(colour = "black", shape= "|", size = 3)

print(g4)

ggsave("Figures/deathrates_residual_majaplot_2011countriesonly_from1970.png")


########################################################################

# 
# g <- ggplot(subset(
#   mrate_resp,
#   subset= sex!="total" & age <=50 & age > 20 & year > 1990
# ))
# 
# g2 <- g + aes(x=residual_prop, y=log(death_rate), group=sex)
# g3 <- g2 + geom_line(aes(colour=year)) + facet_wrap( ~ age) + geom_point(aes(pch=sex), alpha=0.2)
# g4 <- g3 + scale_colour_gradient(low="blue", high="red")
# g4
# 
# ggsave("Figures/deathrate_vs_res_facet_age.png")
# 





####################################################################################
# 
# 
# 
# g <- ggplot(subset(mrate_resp, year >= 1990 & age < 50 & sex!="total")) + aes(x=year, y= age, z=residual_prop)
# g2 <- g + geom_tile(aes(fill=residual_prop)) + facet_wrap( ~ sex)
# g3 <- g2 + scale_fill_gradientn(
#   colours=c("red", "white", "blue"), 
#   limits=c(-0.015, 0.015)
# )
# print(g3)
# 
# ggsave("Figures/Tile_Countries_Combined.png")


# g <- ggplot(subset(mrate_resp, year >= 1970 & age < 50 & sex!="total")) + aes(x=year, y= age, z=residual_prop)
# g2 <- g + geom_tile(aes(fill=residual_prop)) + facet_wrap( ~ sex)
# g3 <- g2 + scale_fill_gradientn(
#   colours=c("red", "white", "blue"),
#   limits=c(-0.015, 0.015)
# )
# print(g3)
# 
# ggsave("Figures/Tile_Countries_Combined_from1970.png")


# g <- ggplot(subset(mrate_resp, year >= 1950 & age < 50 & sex!="total")) + aes(x=year, y= age, z=residual_prop)
# g2 <- g + geom_tile(aes(fill=residual_prop)) + facet_wrap( ~ sex)
# g3 <- g2 + scale_fill_gradientn(
#   colours=c("red", "white", "blue"),
#   limits=c(-0.015, 0.015)
# )
# print(g3)
# 
# ggsave("Figures/Tile_Countries_Combined_from1950.png")
####################



# As above, but only the 14 countries included in 2011
# join exp_eu_all and rates_eu_all




# #############################################################################
# 
# g <- ggplot(
#   subset(
#     mrate_resp_agegroup,
#     subset= sex!="total" & year >=1990 & age_group %in% agegroups_of_interest
#   )
# )
# 
# g2 <- g + aes(x=residual_prop, y=log(death_rate), group=age_group, colour=year)
# g3 <- g2 + geom_line(size=1.1) + facet_grid(age_group~ sex) + geom_vline(x=0, lty="dashed") 
# print(g3)
# 
# ggsave("Figures/deathrates_resprop_agegroup.png")
# 
# g3 <- g2 + geom_point() + facet_grid(age_group~ sex) + geom_vline(x=0, lty="dashed") 
# print(g3)
# ggsave("Figures/deathrates_resprop_agegroup_scatter.png")
# 