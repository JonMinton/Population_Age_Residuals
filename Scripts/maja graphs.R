library(latticeExtra)

# mortality (log) and residuals on same plot, 20-50
png("Figures/mortality_and_residuals_later14europe_log01.png",  
  height=1000, width=2000)
p1 <- levelplot(
  residual_prop * 1000 ~ year * age | sex, 
  data = subset(exp_14_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970),
  cuts=50,
  at = seq(from= -16, to = 16, by=2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(32),
  main = "mortality rates (contour) and population errors (shaded), log scale, European subset")

p2 <- contourplot(
  log(death_rate) ~ year * age | sex, 
  data=subset(rates_14_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970), 
  cuts=50)

print(p1 + p2)
dev.off()

# mortality regular and residuals on same plot, 20-50
png("Figures/mortality_and_residuals_later14europe_identity.png",  
    height=1000, width=2000)
p1 <- levelplot(
  residual_prop * 1000 ~ year * age | sex, 
  data = subset(exp_14_all, subset=sex!="total" & age >= 20 &age <= 50 & year >= 1970),
  cuts=50,
  at = seq(from= -16, to = 16, by=2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(32),
  main = "mortality rates (contour) and population errors (shaded), identity scale, European subset")

p2 <- contourplot(
  death_rate ~ year * age | sex, 
  data=subset(rates_14_all, subset=sex!="total" & age >= 20 & age <=50 & year >=1970), 
  cuts=50)

print(p1 + p2)
dev.off()

# mortality (log) and residuals on same plot, 20-50
png("Figures/mortality_and_residuals_later_older14europe_log01.png",  
    height=1000, width=2000)

p1 <- levelplot(
  residual_prop * 1000 ~ year * age | sex, 
  data = subset(exp_14_all, subset=sex!="total" & age >= 50 &age <= 80 & year >= 1970),
  cuts=50,
  at = seq(from= -16, to = 16, by=2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(32),
  main = "mortality rates (contour) and population errors (shaded), log scale, 50 to 80 years European subset")

p2 <- contourplot(
  log(death_rate) ~ year * age | sex, 
  data=subset(rates_14_all, subset=sex!="total" & age >= 50 & age <=80 & year >=1970), 
  cuts=50)

print(p1 + p2)
dev.off()