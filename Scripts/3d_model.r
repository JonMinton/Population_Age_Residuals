rm(list=ls())


# Packages ----------------------------------------------------------------


# 3d Vis scripts

require(plyr)
require(tidyr)
require(dplyr)

require(ggplot2)
require(lattice)
require(rgl)
require(r2stl)




# Data --------------------------------------------------------------------



# 15 countries reporting in 2011 only
counts_15 <- read.csv("Data/Tidy/counts_15.csv") %>% tbl_df
rates_15 <- read.csv("Data/Tidy/rates_15.csv") %>% tbl_df
exp_15 <- read.csv("Data/Tidy/exp_15.csv") %>% tbl_df
counts_15_all <- read.csv("Data/Tidy/counts_15_all.csv") %>% tbl_df
rates_15_all <- read.csv("Data/Tidy/rates_15_all.csv") %>% tbl_df
exp_15_all <- read.csv("Data/Tidy/exp_15_all.csv") %>% tbl_df
country_codes <- read.csv("Data/Tidy/country_codes__new.csv", stringsAsFactors=F) %>% tbl_df


# l_mort ------------------------------------------------------------------


tmp <-rates_15_all %>%
  filter(year >=1970 & year <= 2011 & age >=20 & age <=50 & sex !="total") 

tmp_m <-tmp %>%
  mutate(ldeath_rate=log(death_rate)) %>%
  mutate(ldeath_rate=ldeath_rate-min(ldeath_rate)) %>%
  filter(sex=="male") %>%
  select(year, age, ldeath_rate) %>%
  spread(age, ldeath_rate)

yrs <- tmp_m$year %>%
  as.numeric %>%
  as.character

tmp_m <- as.matrix(tmp_m)
rownames(tmp_m) <- yrs
tmp_m <- tmp_m[,-1]

tmp_f <- tmp %>%
  mutate(ldeath_rate=log(death_rate)) %>%
  mutate(ldeath_rate=ldeath_rate-min(ldeath_rate)) %>%
  filter(sex=="female") %>%
  select(year, age, ldeath_rate) %>%
  spread(age, ldeath_rate)

yrs <- tmp_f$year %>%
  as.numeric %>%
  as.character

tmp_f <- as.matrix(tmp_f)
rownames(tmp_f) <- yrs 
tmp_f <- tmp_f[,-1]

tmp_f <- t(apply(tmp_f, 1, rev))


spacer <- matrix(0, ncol=2, nrow=dim(tmp_f)[1])

tmp_joined <- cbind(tmp_m, spacer, tmp_f)

persp3d(x=tmp_joined, col="white")

new_mat <- matrix(
  max(tmp_joined) * 0.02, 
  ncol=ncol(tmp_joined) + 6,
  nrow=nrow(tmp_joined) + 6
)

new_mat[
  4:(nrow(new_mat) - 3),
  4:(ncol(new_mat) - 3)
  ] <- tmp_joined

persp3d(x=new_mat, col="white")

r2stl(x=1:nrow(new_mat), y=1:ncol(new_mat), z=new_mat, file="stl/lmort_both.stl", z.expand=TRUE)




# residuals ---------------------------------------------------------------

tmp_m <- exp_15_all %>%
  filter(sex=="male" & age >=20 & age <=50 & year >=1970 & year <= 2011) %>%
  mutate(residual_prop = residual_prop * 1000) 

tmp_m <- tmp_m  %>% 
  select(year, age, residual_prop) %>%
  spread(age, residual_prop)

yrs <- tmp_m$year %>%
  as.numeric %>%
  as.character

tmp_m <- as.matrix(tmp_m)
rownames(tmp_m) <- yrs
tmp_m <- tmp_m[,-1]

persp3d(x=tmp_m, col="white")


tmp_f <- exp_15_all %>%
  filter(sex=="female" & age >=20 & age <=50 & year >=1970 & year <= 2011) %>%
  mutate(residual_prop = residual_prop * 1000) 

tmp_f <- tmp_f  %>% 
  select(year, age, residual_prop) %>%
  spread(age, residual_prop)

yrs <- tmp_f$year %>%
  as.numeric %>%
  as.character

tmp_f <- as.matrix(tmp_f)
rownames(tmp_f) <- yrs
tmp_f <- tmp_f[,-1]

tmp_f <- t(apply(tmp_f, 1, rev))

spacer <- matrix(0, ncol=2, nrow=dim(tmp_f)[1])

tmp_joined <- cbind(tmp_m, spacer, tmp_f)

persp3d(x=tmp_joined, col="white")

new_mat <- matrix(
  0, 
  ncol=ncol(tmp_joined) + 6,
  nrow=nrow(tmp_joined) + 6
                  )

new_mat[
  4:(nrow(new_mat) - 3),
  4:(ncol(new_mat) - 3)
  ] <- tmp_joined

persp3d(x=new_mat, col="white")

r2stl(x=1:nrow(new_mat), y=1:ncol(new_mat), z=new_mat, file="stl/ppr.stl", z.expand=TRUE)
# 


###########################################################################
# RESIDUALS ONLY 
## Now looking at the ages 20 to 50, from 1970 onwards
##################
png(
  "figures/figure_b__residuals_later15europe.png",  
  height=1000, width=2000
)
dta_ss <- subset(exp_15_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970 & year <=2011)
# want to know the maximum deviation from 0


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
    main=NULL
  )
)
dev.off()



# mortality (log) and residuals on same plot, 20-50
png("Figures/figure_c_combined_later15europe.png",  
    height=1000, width=2000)
p1 <- levelplot(
  residual_prop * 1000 ~ year * age | sex, 
  data = subset(exp_15_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970 & year <= 2011),
  cuts=50,
  at = lims,
  col.regions = rev(cols_to_use.fn(200)),
  main = NULL
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

