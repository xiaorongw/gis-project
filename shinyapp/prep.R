packages = c('tmap', 'SpatialAcc', 'sf', 'ggstatsplot', 'tidyverse')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

subzone = st_read(dsn = "data/geospatial/subzone.kml")

subzone <- st_set_crs(subzone, 3414)
subzone<- st_make_valid(subzone)

getName <- function(input){
  noHtml <- str_replace_all(input,"<.*?>","")
  matched_str <- str_match(noHtml, "\\sSUBZONE_N (.*?) SUBZONE_C")[,2]
  return(str_trim(matched_str))
}

subzone <- subzone %>%
  mutate(`SUBZONE_N` = getName(subzone$Description)) 
head(subzone, 1)

hdb <- read_csv('data/aspatial/hdb_osm.csv')

hdb <- st_as_sf(hdb, 
                coords = c('X', 'Y'),
                crs = 4326) %>%
  st_transform(3414)

population <- read_csv('data/aspatial/population.csv')

children_population <- population %>%
  filter(Time == 2020) %>%
  group_by(PA, SZ, AG, TOD) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup() %>%
  pivot_wider(names_from = AG, values_from = POP) %>%
  mutate(children_population = `0_to_4` + (3/5 * `5_to_9`)) %>%
  select(PA, SZ, TOD, children_population) %>%
  pivot_wider(names_from = TOD, values_from = children_population) %>%
  mutate_at(.vars = vars(PA, SZ), .funs = ~ toupper(.)) %>%
  mutate(children_hdb = rowSums(.[4:7])) %>%
  select(SZ, children_hdb)


subzone_children <- subzone %>%
  left_join(children_population, by = c('SUBZONE_N' = 'SZ')) %>%
  select(-c(Description, Name))

subzone_children$num_hdb <- lengths(st_intersects(subzone_children, hdb))

subzone_children <- subzone_children %>%
  # Calculate demand. If there are no HDB
  mutate(children_each_hdb = ifelse(num_hdb != 0, children_hdb / num_hdb, 0))

hdb <- hdb %>%
  select(`ID`, `addr_house`, `addr_postc`, `addr_stree`, `building_l`) %>%
  rename(house_number = addr_house,
         postal_code = addr_postc,
         street = addr_stree,
         num_levels = building_l) %>%
  st_join(subzone_children, join = st_intersects)

student_care <- st_read(dsn = 'data/geospatial/distance_matrix_', layer = 'student_care') %>%
  select(ID, Name, ADDRESSSTR, ADDRESSPOS) %>%
  rename(name = Name,
         address = ADDRESSSTR,
         postal_code = ADDRESSPOS)

dm_student_care <- read_csv('data/aspatial/distance matrix/hdb_studentcare.csv')

dm_student_care <- dm_student_care %>%
  # Obtain final matrix
  select(origin_id, destination_id, total_cost) %>%
  pivot_wider(names_from = destination_id, values_from = total_cost) %>%
  select(-origin_id)

# Convert to km
dm_student_care <- as.matrix(dm_student_care/1000)

student_care_capacity <- round(42907 / nrow(student_care), 0)

student_care_capacity_list <- rep(student_care_capacity, nrow(student_care))

acc_Hansen <- data.frame(ac(hdb$children_each_hdb,
                            student_care_capacity_list,
                            dm_student_care, 
                            d0 = 50,
                            power = 1.5, 
                            family = "Hansen"))

colnames(acc_Hansen) <- "accHansen"
acc_Hansen <- as_tibble(acc_Hansen)
hansen_student_care <- bind_cols(hdb, acc_Hansen)

pri_schools <- st_read(dsn = 'data/geospatial/distance_matrix_', layer = 'schools_primary') %>%
  select(ID, school_nam, address, postal_cod) %>%
  rename(name = school_nam,
         postal_code = postal_cod)

dm_pri_schools <- read_csv('data/aspatial/distance matrix/hdb_school.csv')


dm_pri_schools <- dm_pri_schools %>%
  # Obtain final matrix
  select(origin_id, destination_id, total_cost) %>%
  pivot_wider(names_from = destination_id, values_from = total_cost) %>%
  select(-origin_id)

# Convert to km
dm_pri_schools <- as.matrix(dm_pri_schools/1000)

school_capacity <- round(37671 / nrow(pri_schools))

school_capacity_list <- rep(school_capacity, nrow(pri_schools))

acc_Hansen <- data.frame(ac(hdb$children_each_hdb,
                            school_capacity_list,
                            dm_pri_schools, 
                            d0 = 50,
                            power = 2, 
                            family = "Hansen"))

colnames(acc_Hansen) <- "accHansen"
acc_Hansen <- as_tibble(acc_Hansen)
hansen_pri_schools <- bind_cols(hdb, acc_Hansen)


watersports_facilities <- st_read(dsn = 'data/geospatial/distance_matrix_', layer = 'water_sport_facilities') %>%
  select(ID, NAME, ADDRESSSTR, ADDRESSPOS) %>%
  rename(name = NAME,
         address = ADDRESSSTR,
         postal_code = ADDRESSPOS)

dm_watersports_facilities <- read_csv('data/aspatial/distance matrix/hdb_watersports.csv')

dm_watersports_facilities <- dm_watersports_facilities %>%
  # Obtain final matrix
  select(origin_id, destination_id, total_cost) %>%
  pivot_wider(names_from = destination_id, values_from = total_cost) %>%
  select(-origin_id)

# Convert to km
dm_watersports_facilities <- as.matrix(dm_watersports_facilities/1000)

watersports_capacity_list <- rep(1, nrow(watersports_facilities))

acc_Hansen <- data.frame(ac(hdb$children_each_hdb,
                            watersports_capacity_list,
                            dm_watersports_facilities, 
                            d0 = 50,
                            power = 1.5, 
                            family = "Hansen"))

colnames(acc_Hansen) <- "accHansen"
acc_Hansen <- as_tibble(acc_Hansen)
hansen_watersports_facilities <- bind_cols(hdb, acc_Hansen)

dus_sports_facilities <- st_read(dsn = 'data/geospatial/distance_matrix_', layer = 'dus_school_sports_facilities') %>%
  select(ID, SCHOOL_NAM, ADDRESS, POSTAL_COD, FACILITIES) %>%
  rename(name = SCHOOL_NAM,
         address = ADDRESS,
         postal_code = POSTAL_COD,
         facilities = FACILITIES)

dm_dus_school_sports_facilities <- read_csv('data/aspatial/distance matrix/hdb_dus.csv')

dm_dus_school_sports_facilities <- dm_dus_school_sports_facilities %>%
  # Obtain final matrix
  select(origin_id, destination_id, total_cost) %>%
  pivot_wider(names_from = destination_id, values_from = total_cost) %>%
  select(-origin_id)

# Convert to km
dm_dus_school_sports_facilities <- as.matrix(dm_dus_school_sports_facilities/1000)

dus_capacity_list <- rep(1, nrow(dus_sports_facilities))

acc_Hansen <- data.frame(ac(hdb$children_each_hdb,
                            dus_capacity_list,
                            dm_dus_school_sports_facilities, 
                            d0 = 50,
                            power = 2, 
                            family = "Hansen"))

colnames(acc_Hansen) <- "accHansen"
acc_Hansen <- as_tibble(acc_Hansen)
hansen_dus_school_sports_facilities <- bind_cols(hdb, acc_Hansen)

cib_gardens <- st_read(dsn = 'data/geospatial/distance_matrix_', layer = 'cib_gardens') %>%
  select(ID, ADDRESS, DIVISION, CONSTITUEN, CATEGORY) %>%
  rename(address = ADDRESS,
         division = DIVISION, 
         constituency = CONSTITUEN,
         category = CATEGORY)


dm_cib_gardens <- read_csv('data/aspatial/distance matrix/hdb_cib.csv')

dm_cib_gardens <- dm_cib_gardens %>%
  # Obtain final matrix
  select(origin_id, destination_id, total_cost) %>%
  pivot_wider(names_from = destination_id, values_from = total_cost) %>%
  select(-origin_id)

# Convert to km
dm_cib_gardens <- as.matrix(dm_cib_gardens/1000)

cib_capacity_list <- rep(1, nrow(cib_gardens))

acc_Hansen <- data.frame(ac(hdb$children_each_hdb,
                            cib_capacity_list,
                            dm_cib_gardens, 
                            d0 = 50,
                            power = 2.5, 
                            family = "Hansen"))

colnames(acc_Hansen) <- "accHansen"
acc_Hansen <- as_tibble(acc_Hansen)
hansen_cib_gardens <- bind_cols(hdb, acc_Hansen)

preschools <- st_read(dsn = 'data/geospatial/distance_matrix_', layer = 'preschools') %>%
  select(ID, CENTRE_NAM, ADDRESS, POSTAL_COD) %>%
  rename(name = CENTRE_NAM,
         address = ADDRESS,
         postal_code = POSTAL_COD)

dm_preschools <- read_csv('data/aspatial/distance matrix/hdb_preschools.csv')

dm_preschools <- dm_preschools %>%
  # Obtain final matrix
  select(origin_id, destination_id, total_cost) %>%
  pivot_wider(names_from = destination_id, values_from = total_cost) %>%
  select(-origin_id)

# Convert to km
dm_preschools <- as.matrix(dm_preschools/1000)

preschool_capacity <- round(215579 / nrow(preschools))

preschool_capacity_list <- rep(preschool_capacity, nrow(preschools))

acc_Hansen <- data.frame(ac(hdb$children_each_hdb,
                            preschool_capacity_list,
                            dm_preschools, 
                            d0 = 50,
                            power = 2, 
                            family = "Hansen"))

colnames(acc_Hansen) <- "accHansen"
acc_Hansen <- as_tibble(acc_Hansen)
hansen_preschools <- bind_cols(hdb, acc_Hansen)

hansen_preschools[is.na(hansen_preschools)] <- 0

compute_risk_weights <- function(df, name) {
  first_quantile_hansen <- quantile(df$accHansen, 0.25)[[1]]
  median_hansen <- median(df$accHansen)
  third_quantile_hansen <- quantile(df$accHansen, 0.75)[[1]]
  max_hansen <- max(df$accHansen)
  
  df1 <- df %>%
    mutate('risk_{name}' := case_when(
      accHansen < first_quantile_hansen ~ 4,
      (accHansen >= first_quantile_hansen) & (accHansen < median_hansen) ~ 3,
      (accHansen >= median_hansen) & (accHansen < third_quantile_hansen) ~ 2,
      (accHansen >= third_quantile_hansen) ~ 1))
  
  return(df1)
}

hansen_cib_gardens <- compute_risk_weights(hansen_cib_gardens, 'cib_gardens')
hansen_dus_school_sports_facilities <- compute_risk_weights(hansen_dus_school_sports_facilities, 'dus_sports')
hansen_preschools <- compute_risk_weights(hansen_preschools, 'preschools')
hansen_pri_schools <- compute_risk_weights(hansen_pri_schools, 'pri_schools')
hansen_student_care <- compute_risk_weights(hansen_student_care, 'student_care')
hansen_watersports_facilities <- compute_risk_weights(hansen_watersports_facilities, 'watersports_facilities')

hdb_risk <- hdb %>%
  cbind(hansen_cib_gardens$risk_cib_gardens,
        hansen_dus_school_sports_facilities$risk_dus_sports,
        hansen_preschools$risk_preschools,
        hansen_pri_schools$risk_pri_schools,
        hansen_student_care$risk_student_care,
        hansen_watersports_facilities$risk_watersports_facilities) %>%
  rename(risk_cib_gardens = hansen_cib_gardens.risk_cib_gardens,
         risk_dus_sports = hansen_dus_school_sports_facilities.risk_dus_sports,
         risk_preschools = hansen_preschools.risk_preschools,
         risk_pri_schools = hansen_pri_schools.risk_pri_schools,
         risk_student_care = hansen_student_care.risk_student_care,
         risk_watersports_facilities = hansen_watersports_facilities.risk_watersports_facilities) %>%
  mutate(physical_health_risk = (risk_dus_sports + risk_watersports_facilities) / 2) %>%
  mutate(social_competence_risk = (risk_dus_sports + risk_preschools + risk_pri_schools + risk_student_care + risk_watersports_facilities) / 5) %>%
  mutate(emotional_maturity_risk = risk_cib_gardens) %>%
  mutate(composite_risk = (physical_health_risk + social_competence_risk + emotional_maturity_risk) / 3)
