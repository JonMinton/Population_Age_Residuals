# Lexis surface helper functions

plot_ppr <- function(
  DTA,
  LIMS = seq(from= -20, to = 20, by=2),                 
  COL = "black",
  COLS_TO_USE = brewer.pal(5, "RdBu"),
  ASPECT = "iso",
  FLATTEN = T
){
  DTA <- DTA %>% 
    mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)
  if (FLATTEN){
    DTA <- DTA %>% 
      mutate(
        residual_prop = ifelse(residual_prop < min(LIMS), min(LIMS), residual_prop),
        residual_prop = ifelse(residual_prop > max(LIMS), max(LIMS), residual_prop)
      )
  }  
  cols_to_use.fn <- colorRampPalette(COLS_TO_USE)
  
  output <- contourplot(
    residual_prop ~ year * age | sex, 
    data=DTA, 
    region=T, 
    at=LIMS,
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL,
    col=COL,
    aspect = ASPECT,
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
  
return(output)  
}

plot_ppr_level <- function(
  DTA,
  LIMS = seq(from= -20, to = 20, by=2),                 
  COLS_TO_USE = brewer.pal(5, "RdBu"),
  ASPECT = "iso",
  FLATTEN = T
){
  DTA <- DTA %>% 
    mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)   
  if (FLATTEN){
    DTA <- DTA %>% 
      mutate(
        residual_prop = ifelse(residual_prop < min(LIMS), min(LIMS), residual_prop),
        residual_prop = ifelse(residual_prop > max(LIMS), max(LIMS), residual_prop)
      )
  }  
  
  cols_to_use.fn <- colorRampPalette(COLS_TO_USE)
  
  output <- levelplot(
    residual_prop ~ year * age | sex, 
    data=DTA, 
    at = LIMS,
    
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL,
    
    aspect = ASPECT,
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
  
  return(output)  
}


plot_smoothed_ppr <- function(
  DTA,
  LIMS = seq(from= -20, to = 20, by=2),                 
  COL = "black",
  COLS_TO_USE = brewer.pal(5, "RdBu"),
  SMOOTH_PAR = 1.3,
  ASPECT = "iso",
  FLATTEN = T
){
  DTA <- DTA %>% 
    mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)   
  
  if (FLATTEN){
    DTA <- DTA %>% 
      mutate(
        residual_prop = ifelse(residual_prop < min(LIMS), min(LIMS), residual_prop),
        residual_prop = ifelse(residual_prop > max(LIMS), max(LIMS), residual_prop)
      )
  }  
  
  cols_to_use.fn <- colorRampPalette(COLS_TO_USE)
  
  dta_smoothed <- DTA %>% 
    smooth_var(. , 
      smooth_var = "residual_prop",
      smooth_par = SMOOTH_PAR,
      group_vars = "sex"
                )
  
  level_part <- levelplot(
    residual_prop ~ year * age | sex, 
    data=DTA, 
    region=T, 
    at=LIMS,
    aspect = ASPECT,
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL,
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

  contour_part <- contourplot(
    residual_prop ~ year * age | sex, 
    data=dta_smoothed, 
    region=F, 
    at=LIMS,
    main=NULL,
    col=COL,
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab="",
    xlab="",
    cex=1.4,
    aspect = ASPECT,
    labels=list(cex=1.2),
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
  
  output <- level_part + contour_part
  
  return(output)  
}


plot_smoothed_region_ppr <- function(
  DTA,
  LIMS = seq(from= -20, to = 20, by=2),                 
  COL = "black",
  COLS_TO_USE = brewer.pal(5, "RdBu"),
  SMOOTH_PAR = 1.3,
  ASPECT = "iso",
  FLATTEN = T
){
  DTA <- DTA %>% 
    mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)   
  
  if (FLATTEN){
    DTA <- DTA %>% 
      mutate(
        residual_prop = ifelse(residual_prop < min(LIMS), min(LIMS), residual_prop),
        residual_prop = ifelse(residual_prop > max(LIMS), max(LIMS), residual_prop)
      )
  }  
  
  cols_to_use.fn <- colorRampPalette(COLS_TO_USE)
  
  dta_smoothed <- DTA %>% 
    smooth_var(. , 
               smooth_var = "residual_prop",
               smooth_par = SMOOTH_PAR,
               group_vars = c("sex", "region")
    )
  
  level_part <- levelplot(
    residual_prop ~ year * age | region + sex, 
    data=DTA, 
    region=T, 
    at=LIMS,
    aspect = ASPECT,
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL,
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
  
  contour_part <- contourplot(
    residual_prop ~ year * age | region + sex, 
    data=dta_smoothed, 
    region=F, 
    at=LIMS,
    main=NULL,
    col=COL,
    aspect = ASPECT,
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab="",
    xlab="",
    cex=1.4,
    labels=list(cex=1.2),
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
  
  output <- level_part + contour_part
  
  return(output)  
}

plot_level_region_ppr <- function(
  DTA,
  LIMS = seq(from= -20, to = 20, by=2),                 
  COLS_TO_USE = brewer.pal(5, "RdBu"),
  ASPECT = "iso",
  FLATTEN = T
){
  DTA <- DTA %>% 
    mutate(residual_prop = 1000 *(population_count - expected_count)/ expected_count)   
  
  if (FLATTEN){
    DTA <- DTA %>% 
      mutate(
        residual_prop = ifelse(residual_prop < min(LIMS), min(LIMS), residual_prop),
        residual_prop = ifelse(residual_prop > max(LIMS), max(LIMS), residual_prop)
      )
  }  
  
  cols_to_use.fn <- colorRampPalette(COLS_TO_USE)
  
  
  output <- levelplot(
    residual_prop ~ year * age | region + sex, 
    data=DTA, 
    region=T, 
    at=LIMS,
    aspect = ASPECT,
    col.regions=rev(cols_to_use.fn(200)), 
    main=NULL,
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
  
  return(output)  
}

  
# Lexis surface helper functions

plot_lgcmr <- function(
  DTA,
  COL = "black",
  COLS_TO_USE =colorRampPalette(brewer.pal(9, "Greens"))(100),
  ASPECT = "iso",
  CUTS = 15
){
  DTA <- DTA %>% 
    mutate(cmr = death_count / population_count,
           lg_cmr = log(cmr, base = 10)
    )
  
  output <- DTA %>% contourplot(
      lg_cmr ~ year * age | sex, 
      data=., 
      region=T, 
      strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      cuts=CUTS,
      col.regions=COLS_TO_USE,
      main=NULL,
      labels=list(cex=1.2),
      col=COL,
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
      
    )
  
  return(output)  
}



plot_region_lgcmr <- function(
  DTA,
  COL = "black",
  COLS_TO_USE =colorRampPalette(brewer.pal(9, "Greens"))(100),
  ASPECT = "iso",
  SMOOTH_PAR = 1.3,
  CUTS = 15
){
  DTA <- DTA %>% 
    mutate(cmr = death_count / population_count,
           lg_cmr = log(cmr, base = 10)
    )
  
  output <- DTA %>% contourplot(
    lg_cmr ~ year * age | region + sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=CUTS,
    col.regions=COLS_TO_USE,
    main=NULL,
    labels=list(cex=1.2),
    col=COL,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
    
  )
  
  return(output)  
}




plot_smoothed_region_lgcmr <- function(
  DTA,
  COL = "black",
  COLS_TO_USE =colorRampPalette(brewer.pal(9, "Greens"))(100),
  ASPECT = "iso",
  SMOOTH_PAR = 1.3, 
  EDGE = 2,
  CUTS = 15
){
  DTA <- DTA %>% 
    mutate(cmr = death_count / population_count,
           lg_cmr = log(cmr, base = 10)
    )
  
  dta_smoothed <- DTA %>% 
    smooth_var(. , 
    group_vars = c("region", "sex"), smooth_var = "lg_cmr", 
    smooth_par= SMOOTH_PAR)
  
  level_part <- DTA %>% levelplot(
    lg_cmr ~ year * age | region + sex, 
    data=., 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=CUTS,
    col.regions=COLS_TO_USE,
    main=NULL,
    labels=list(cex=1.2),
    aspect = ASPECT,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
  
  contour_part <- dta_smoothed %>% 
    filter(year >= min(.$year) + EDGE &
             year <= max(.$year) - EDGE &
             age >= min(.$age) + EDGE & 
             age <= max(.$age) - EDGE
             ) %>% 
    contourplot(
    lg_cmr ~ year * age | region + sex, 
    data=., 
    region=F,
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab="", 
    xlab="",
    cex=1.4,
    cuts=CUTS,
    main=NULL,
    aspect = ASPECT,
    labels=list(cex=1.2),
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
  
  output <- level_part + contour_part
  
  return(output)  
}


plot_composite <- function(
  DTA,
  COLS_TO_USE = brewer.pal(5, "RdBu"),
  ASPECT = "iso",
  LIMS = seq(from= -20, to = 20, by=2),
  CUTS = 15,
  FLATTEN = T
                           
){
  
  DTA <- DTA %>% 
    filter(!is.na(expected_count) & sex !="total") %>% 
    mutate(
      residual_prop = 1000 *(population_count - expected_count)/ expected_count,
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base = 10)
  )
  
  if (FLATTEN){
    DTA <- DTA %>% 
      mutate(
        residual_prop = ifelse(residual_prop < min(LIMS), min(LIMS), residual_prop),
        residual_prop = ifelse(residual_prop > max(LIMS), max(LIMS), residual_prop)
      )
  }  
  
  
  mx <- max(abs(DTA$residual_prop))
  cols_to_use.fn <- colorRampPalette(COLS_TO_USE)
  
  
  shade_residual <- DTA %>% 
    levelplot(
      residual_prop  ~ year * age | sex, 
      data = . ,
      at = LIMS,
      col.regions = cols_to_use.fn,
      main = NULL,
      strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      aspect = ASPECT,
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  contour_lgcmr <- DTA %>% 
    contourplot(
    lg_cmr ~ year * age | sex, 
    data=. ,  
    cuts=CUTS,
    labels=list(cex=1.2),
    cex=1.4,
    region=F,
    main=NULL,
    col="black",
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab="",
    xlab="", 
    aspect = ASPECT,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
  
  output <- shade_residual + contour_lgcmr
  return(output)
}


plot_region_composite <- function(
  DTA,
  COLS_TO_USE = brewer.pal(5, "RdBu"),
  ASPECT = "iso",
  LIMS = seq(from= -20, to = 20, by=2),
  CUTS = 15,
  FLATTEN = T
  
){
  
  DTA <- DTA %>% 
    filter(!is.na(expected_count) & sex !="total") %>% 
    mutate(
      residual_prop = 1000 *(population_count - expected_count)/ expected_count,
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base = 10)
    )
  if (FLATTEN){
    DTA <- DTA %>% 
      mutate(
        residual_prop = ifelse(residual_prop < min(LIMS), min(LIMS), residual_prop),
        residual_prop = ifelse(residual_prop > max(LIMS), max(LIMS), residual_prop)
      )
  }  
  
  
  mx <- max(abs(DTA$residual_prop))
  cols_to_use.fn <- colorRampPalette(COLS_TO_USE)
  
  
  shade_residual <- DTA %>% 
    levelplot(
      residual_prop  ~ year * age | region + sex, 
      data = . ,
      at = LIMS,
      col.regions = cols_to_use.fn,
      main = NULL,
      strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      aspect = ASPECT,
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  contour_lgcmr <- DTA %>% 
    contourplot(
      lg_cmr ~ year * age | region + sex, 
      data=. ,  
      cuts=CUTS,
      labels=list(cex=1.2),
      cex=1.4,
      region=F,
      main=NULL,
      col="black",
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab="",
      xlab="", 
      aspect = ASPECT,
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  output <- shade_residual + contour_lgcmr
  return(output)
}

calc_windowed_correlations <- function(DTA, WINDOW=4){
  DTA <- DTA %>% 
    filter(!is.na(expected_count) & sex !="total") %>% 
    mutate(
      residual_prop = 1000 *(population_count - expected_count)/ expected_count,
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base = 10)
    )
  age_range <- c(min(DTA$age) + WINDOW, max(DTA$age) - WINDOW) 
  year_range <- c(min(DTA$year) + WINDOW, max(DTA$year) - WINDOW)
  
  out <- DTA %>% mutate(local_cor = NA) %>% 
    select(sex, age, year, local_cor) %>% 
    filter(age > age_range[1] & age < age_range[2] &
             year > year_range[1] & year < year_range[2])
  
  fn <- function(x){
    local_dta <- DTA %>% 
      filter(
        sex == x$sex,
        year >= x$year  - WINDOW & 
          year <= x$year  + WINDOW &
          age >= x$age  - WINDOW &
          age <= x$age  + WINDOW
          ) %>% select(residual_prop, lg_cmr)
    local_cor <- cor(x=local_dta$residual_prop, y = local_dta$lg_cmr, method = "spearman")
    out <- data.frame(year = x$year, age = x$age, sex = x$sex, local_cor=local_cor)
    
    return(out)
  }
  
  out <- out  %>% 
    rowwise() %>% 
    do(fn(.))

    return(out)
}

plot_local_cor_region <- function(DTA){
  
  cols_to_use.fn <- colorRampPalette(brewer.pal(5, "RdBu"))
  output <- contourplot(
    local_cor ~ year * age | region + sex, 
    data=DTA, 
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
  )
  
  
  return(output)
}


plot_local_cor <- function(DTA){
  
  cols_to_use.fn <- colorRampPalette(brewer.pal(5, "RdBu"))
  output <- contourplot(
    local_cor ~ year * age | sex, 
    data=DTA, 
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
  )
  
  
  return(output)
}


