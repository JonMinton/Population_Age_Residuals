
# Make tidy data

counts <- read.csv(file="data/Tidy/counts.csv") %>% tbl_df

# group germany together

tmp <- counts %>% 
  filter(country %in% c("DEUTE", "DEUTW")) %>% 
  group_by(year, age, sex) %>% 
  summarise(
    death_count=sum(death_count), 
    population_count=sum(population_count)
            ) %>% 
  mutate(country="DEUT") %>% 
  select(country, year, age, sex, death_count, population_count)

counts <- counts %>% bind_rows(tmp)


# Define groups of countries ----------------------------------------------


europe_2011_subset <- c(
    Belgium="BEL", 
    Switzerland="CHE", 
    `Czech Republic`="CZE", 
    Germany="DEUT",
    Denmark="DNK", 
    Spain="ESP", 
    Estonia="EST",
    France="FRATNP", 
    `Northern Ireland`="GBR_NIR", 
    Scotland="GBR_SCO", 
    `England and Wales`="GBRTENW",
    Lithuania="LTU",     
    Latvia="LVA",     
    Portugal="PRT",     
    Sweden="SWE"
  )

europe_all <- c(
  Austria="AUT",
  Belgium="BEL", 
  Switzerland="CHE", 
  `Czech Republic`="CZE", 
  Germany="DEUT",
  Denmark="DNK", 
  Spain="ESP", 
  Estonia="EST",
  France="FRATNP", 
  `Northern Ireland`="GBR_NIR", 
  Scotland="GBR_SCO", 
  `England and Wales`="GBRTENW",
  Lithuania="LTU",     
  Latvia="LVA",     
  Portugal="PRT",     
  Sweden="SWE",
  Slovenia = "SVN",
  Slovakia = "SVK",
  Poland="POL",
  Norway="NOR",
  Ireland="IRL",
  Poland="POL",
  Luxembourg="LUX",
  Italy="ITA",
  Hungary="HUN",
  Ukraine="UKR",
  Belarus="BLR", 
  Finland="FIN"
)

europe_western <- c(
  Austria="AUT",
  Belgium="BEL", 
  Switzerland="CHE", 
  Germany="DEUT",
  France="FRATNP", 
  `Northern Ireland`="GBR_NIR", 
  Scotland="GBR_SCO", 
  `England and Wales`="GBRTENW",
  Ireland="IRL",
  Luxembourg="LUX"
)

europe_northern <- c(
  Denmark="DNK", 
  Sweden="SWE",
  Finland="FIN",
  Norway="NOR"
)

europe_southern <- c(
  Spain="ESP", 
  Portugal="PRT",     
  Luxembourg="LUX",
  Italy="ITA",
  Hungary="HUN",
  Ukraine="UKR",
  Belarus="BLR", 
  Finland="FIN"
)

europe_eastern <- c(
  Estonia="EST",
  Lithuania="LTU",     
  Latvia="LVA",     
  Slovenia = "SVN",
  Slovakia = "SVK",
  Poland="POL",
  Ukraine="UKR",
  Belarus="BLR"
)

aggregate_counts <- function(x, selection){
  out <- x %>% filter(country %in% selection) %>% 
    group_by(year, age, sex) %>% 
    summarise(
      death_count=sum(death_count), 
      population_count=sum(population_count)
      ) %>% ungroup
  
  return(out)
}

calculate_expected_counts <- function(x, selection){
  y <- aggregate_counts(x, selection)
  
  expd_mtrx_list <- list()
  ages <- sort(unique(y$age))
  years <- sort(unique(y$year))
  for (sx in unique(y$sex)){
    dth <- y %>% 
      filter(sex==sx) %>% 
      select(age, year, death_count) %>% 
      spread(key=year, value=death_count) 
    ages <- dth$age
    dth <- dth[,-1]
    dth <- as.matrix(dth)
    rownames(dth) <- ages
    dms <- dim(dth)
    
    pop <- y %>% 
      filter(sex==sx) %>% 
      select(age, year, population_count) %>% 
      spread(key=year, value=population_count) 
    pop <- pop[,-1]
    pop <- as.matrix(pop)
    rownames(pop) <- ages
    
    expd <- pop[-dms[1],-dms[2]] - dth[-dms[1], -dms[2]]
    
    dimnames(expd) <- dimnames(pop[-1,-1])
    
    expd_mtrx_list[[sx]]<- expd    
  }
  
  subfn <- function(xx){
    ages <- rownames(xx)
    years <- colnames(xx)
    xx <- as.data.frame(xx)
    xx$age <- ages 
    xx <- xx %>% 
      gather(key=year, value=expected_count, -age) %>% 
      mutate(year=as.integer(as.character(year)), 
             age=as.integer(as.character(age))
             )
    xx <- xx %>% select(year, age, expected_count)
    return(xx)
  }
  
  expd_long <- ldply(expd_mtrx_list, subfn, .id="sex")
  out <- y %>% left_join(expd_long) %>% tbl_df
  return(out)
}

expected_europe_all <- counts %>% calculate_expected_counts(selection=europe_all)
expected_europe_northern <- counts %>% calculate_expected_counts(selection=europe_northern)
expected_europe_southern <- counts %>% calculate_expected_counts(selection=europe_southern)
expected_europe_western <- counts %>% calculate_expected_counts(selection=europe_western)
expected_europe_eastern <- counts %>% calculate_expected_counts(selection=europe_eastern)

expected_europe_all_2011 <- counts %>% calculate_expected_counts(selection=intersect(europe_all, europe_2011_subset))
expected_europe_northern_2011 <- counts %>% calculate_expected_counts(selection=intersect(europe_northern, europe_2011_subset))
expected_europe_southern_2011 <- counts %>% calculate_expected_counts(selection=intersect(europe_southern, europe_2011_subset))
expected_europe_western_2011 <- counts %>% calculate_expected_counts(selection=intersect(europe_western, europe_2011_subset))
expected_europe_eastern_2011 <- counts %>% calculate_expected_counts(selection=intersect(europe_eastern, europe_2011_subset))

expected_europe <- bind_rows(
  expected_europe_all %>% mutate(region="All"),
  expected_europe_northern %>% mutate(region="Northern"),
  expected_europe_southern %>% mutate(region="Southern"),
  expected_europe_western %>% mutate(region="Western"),
  expected_europe_eastern %>% mutate(region="Eastern")
)
  
expected_europe_2011 <- bind_rows(
  expected_europe_all_2011 %>% mutate(region="All"),
  expected_europe_northern_2011 %>% mutate(region="Northern"),
  expected_europe_southern_2011 %>% mutate(region="Southern"),
  expected_europe_western_2011 %>% mutate(region="Western"),
  expected_europe_eastern_2011 %>% mutate(region="Eastern")
)



rm(
  expected_europe_all, 
  expected_europe_northern,
  expected_europe_southern, 
  expected_europe_western, 
  expected_europe_eastern, 
  
  expected_europe_all_2011, 
  expected_europe_northern_2011, 
  expected_europe_southern_2011, 
  expected_europe_western_2011, 
  expected_europe_eastern_2011 
)  

