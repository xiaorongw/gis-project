library('shiny')
library('shinydashboard')
library('shinydashboardPlus')
library('tidyverse')
library('sf')
library('sp')
library('tmap')
library('SpatialAcc')
library("shinyLP")
library('shinyWidgets')
library('scales')
library('shinycustomloader')
library('DT')

###################################################################################################################

# Data preparation

# icons for dropdown menu
images <- c("<div class='amenity-dropdown'><img src='footprint.png' width=15px>Activity Areas</div>",
            "<div class='amenity-dropdown'><img src='comm_club.png' width=15px>Community Clubs</div>",
            "<div class='amenity-dropdown'><img src='community.png' width=15px>Community Use Sites</div>",
            "<div class='amenity-dropdown'><img src='flower.png' width=15px>Community in Bloom Gardens</div>",
            "<div class='amenity-dropdown'><img src='football.png' width=15px>Dual Use Scheme School Sports Facility</div>",
            "<div class='amenity-dropdown'><img src='pine.png' width=15px>Nature Areas</div>",
            "<div class='amenity-dropdown'><img src='tree.png' width=15px>Parks</div>",
            "<div class='amenity-dropdown'><img src='triangle.png' width=13px>Play and Fitness Equipment</div>",
            "<div class='amenity-dropdown'><img src='kid.png' width=13px>Pre-schools</div>",
            "<div class='amenity-dropdown'><img src='grad_cap.png' width=15px>Primary Schools</div>",
            "<div class='amenity-dropdown'><img src='badminton.png' width=15px>SportSG Facilities</div>",
            "<div class='amenity-dropdown'><img src='school-book-bag.png' width=15px>Student Care</div>",
            "<div class='amenity-dropdown'><img src='swimming.png' height=15px>Water Sports Facilities</div>")

# save(activity_area, cib_gardens, community_clubs,community_use_sites,dus_sports_facilities,hdb,nature_area,parks,play_fitness,preschools,pri_schools,sportsg_facilities,student_care,towns,watersports_facilities, file = "spatial_data.rda")
load(file = "spatial_data.rda")
# towns <- st_read(dsn = 'data/geospatial', layer = 'plan_area')
sf::st_crs(towns) <- 3414

# hdb <- st_read(dsn = 'data/geospatial', layer = 'hdb_processed')
sf::st_crs(hdb) <- 3414

### data from distance_matrix_ ###

load(file = 'distance_matrices.rda')

## Student care
# student_care <- st_read(dsn = 'data/geospatial', layer = 'student_care') %>%
#     select(ID, Name, ADDRESSSTR, ADDRESSPOS) %>%
#     rename(name = Name,
#            address = ADDRESSSTR,
#            postal_code = ADDRESSPOS)
sf::st_crs(student_care) <- 3414
# dm_student_care <- read_csv('data/aspatial/distance matrix/hdb_studentcare.csv')
# dm_student_care <- dm_student_care %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
student_care_capacity <- round(42907 / nrow(student_care), 0)
student_care_capacity_list <- rep(student_care_capacity, nrow(student_care))

## Primary schools
# pri_schools <- st_read(dsn = 'data/geospatial', layer = 'schools_primary') %>%
#     select(ID, school_nam, address, postal_cod) %>%
#     rename(name = school_nam,
#            postal_code = postal_cod)
sf::st_crs(pri_schools) <- 3414
# dm_pri_schools <- read_csv('data/aspatial/distance matrix/hdb_school.csv')
# dm_pri_schools <- dm_pri_schools %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
school_capacity <- round(37671 / nrow(pri_schools))
school_capacity_list <- rep(school_capacity, nrow(pri_schools))

## Water sports facilities
# watersports_facilities <- st_read(dsn = 'data/geospatial', layer = 'water_sport_facilities') %>%
#     select(ID, NAME, ADDRESSSTR, ADDRESSPOS) %>%
#     rename(name = NAME,
#            address = ADDRESSSTR,
#            postal_code = ADDRESSPOS)
sf::st_crs(watersports_facilities) <- 3414
# dm_watersports_facilities <- read_csv('data/aspatial/distance matrix/hdb_watersports.csv')
# dm_watersports_facilities <- dm_watersports_facilities %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost) 
watersports_capacity_list <- rep(1, nrow(watersports_facilities))

## DUS sports facilities
# dus_sports_facilities <- st_read(dsn = 'data/geospatial', layer = 'dus_school_sports_facilities') %>%
#     select(ID, SCHOOL_NAM, ADDRESS, POSTAL_COD, FACILITIES) %>%
#     rename(name = SCHOOL_NAM,
#            address = ADDRESS,
#            postal_code = POSTAL_COD,
#            facilities = FACILITIES)
sf::st_crs(dus_sports_facilities) <- 3414
# dm_dus_school_sports_facilities <- read_csv('data/aspatial/distance matrix/hdb_dus.csv')
# dm_dus_school_sports_facilities <- dm_dus_school_sports_facilities %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost) 
dus_capacity_list <- rep(1, nrow(dus_sports_facilities))

## Community in bloom garden
# cib_gardens <- st_read(dsn = 'data/geospatial', layer = 'cib_gardens') %>%
#     select(ID, ADDRESS, DIVISION, CONSTITUEN, CATEGORY) %>%
#     rename(address = ADDRESS,
#            division = DIVISION,
#            constituency = CONSTITUEN,
#            category = CATEGORY)
sf::st_crs(cib_gardens) <- 3414
# dm_cib_gardens <- read_csv('data/aspatial/distance matrix/hdb_cib.csv')
# dm_cib_gardens <- dm_cib_gardens %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
cib_capacity_list <- rep(1, nrow(cib_gardens))

## Preschools
# preschools <- st_read(dsn = 'data/geospatial', layer = 'preschools') %>%
#     select(ID, CENTRE_NAM, ADDRESS, POSTAL_COD) %>%
#     rename(name = CENTRE_NAM,
#            address = ADDRESS,
#            postal_code = POSTAL_COD)
sf::st_crs(preschools) <- 3414
# dm_preschools <- read_csv('data/aspatial/distance matrix/hdb_preschools.csv')
# dm_preschools <- dm_preschools %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
preschool_capacity <- round(215579 / nrow(preschools))
preschool_capacity_list <- rep(preschool_capacity, nrow(preschools))

## Sportsg sports facilities
# sportsg_facilities <- st_read(dsn = 'data/geospatial', layer = 'sportsg_sports_facilities') %>%
#     select(ROAD_NAME, FACILITIES) %>%
#     rename(address = ROAD_NAME,
#            facilities = FACILITIES)
sf::st_crs(sportsg_facilities) <- 3414
# dm_sportsg_facilities <- read_csv('data/aspatial/distance matrix/hdb_sportsg.csv')
# dm_sportsg_facilities <- dm_sportsg_facilities %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
sportsg_capacity_list <- rep(1, nrow(sportsg_facilities))

## Play and fitness equipment
# play_fitness <- st_read(dsn = 'data/geospatial', layer = 'nparks_play_fitness_equipment') %>%
#     select(geometry)
sf::st_crs(play_fitness) <- 3414
# dm_play_fitness <- read_csv('data/aspatial/distance matrix/hdb_play_fitness.csv')
# dm_play_fitness <- dm_play_fitness %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
play_fitness_capacity_list <- rep(1, nrow(play_fitness))

## Parks
# parks <- st_read(dsn = 'data/geospatial', layer = 'nparks_parks')  %>%
#     select(Name) %>%
#     rename(name = Name)
sf::st_crs(parks) <- 3414
# dm_parks <- read_csv('data/aspatial/distance matrix/hdb_nparks.csv')
# dm_parks <- dm_parks %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
parks_capacity_list <- rep(1, nrow(parks))

## Community use sites
# community_use_sites <- st_read(dsn = 'data/geospatial', layer = 'community_use_sites') %>%
#     select(Name) %>%
#     rename(name = Name)
sf::st_crs(community_use_sites) <- 3414
# dm_community_use_sites <- read_csv('data/aspatial/distance matrix/hdb_community_use_sites.csv')
# dm_community_use_sites <- dm_community_use_sites %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
community_use_sites_capacity_list <- rep(1, nrow(community_use_sites))

## Activity areas
# activity_area <- st_read(dsn = 'data/geospatial', layer = 'nparks_activity_area') %>%
#     select(geometry)
sf::st_crs(activity_area) <- 3414
# dm_activity_area <- read_csv('data/aspatial/distance matrix/hdb_activity_areas.csv')
# dm_activity_area <- dm_activity_area %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
activity_area_capacity_list <- rep(1, nrow(activity_area))

## Nature areas
# nature_area <- st_read(dsn = 'data/geospatial', layer = 'nature_areas') %>%
#     select(geometry)
sf::st_crs(nature_area) <- 3414
# dm_nature_area <- read_csv('data/aspatial/distance matrix/hdb_nature_areas.csv')
# dm_nature_area <- dm_nature_area %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
nature_area_capacity_list <- rep(1, nrow(nature_area))

## Community clubs
# community_clubs <- st_read(dsn = 'data/geospatial', layer = 'community_clubs') %>%
#     select(Name, descriptio, ADDRESSSTR, ADDRESSPOS) %>%
#     rename(name = Name,
#            type = descriptio,
#            address = ADDRESSSTR,
#            postal_code = ADDRESSPOS)
sf::st_crs(community_clubs) <- 3414
# dm_community_clubs <- read_csv('data/aspatial/distance matrix/hdb_community_clubs.csv')
# dm_community_clubs <- dm_community_clubs %>%
#     select(origin_id, destination_id, total_cost) %>%
#     pivot_wider(names_from = destination_id, values_from = total_cost)
community_clubs_capacity_list <- rep(1, nrow(community_clubs))

# save(dm_activity_area, dm_cib_gardens, dm_community_clubs, dm_community_use_sites,
#      dm_dus_school_sports_facilities, dm_nature_area, dm_parks, dm_play_fitness,
#      dm_preschools, dm_pri_schools, dm_sportsg_facilities, dm_student_care, dm_watersports_facilities,
#      file = 'distance_matrices.rda')

####################################################################################################

# Enabling index map computation

# Function to calculate hansen accessibility
calculate_acc <- function(power, dist_mat, capacity_list, hdb_points) {
    dm <- subset(dist_mat, origin_id %in% hdb_points$ID) %>%
        select(-origin_id)
    dm <- as.matrix(dm/1000)
    acc <- data.frame(ac(hdb_points$children_each_hdb,
                         capacity_list,
                         dm,
                         d0 = 50,
                         power = power,
                         family = 'Hansen'))
    
    colnames(acc) <- "acc"
    acc <- as_tibble(acc)
    return(bind_cols(hdb_points, acc)) 
}

# Function to compute enabling scores
compute_enabling_scores <- function(df, name) {
    decile <- quantile(df$acc, prob = seq(0, 1, length = 11), type=5)
    
    decile10 <- decile[[2]]
    decile20 <- decile[[3]]
    decile30 <- decile[[4]]
    decile40 <- decile[[5]]
    decile50 <- decile[[6]]
    decile60 <- decile[[7]]
    decile70 <- decile[[8]]
    decile80 <- decile[[9]]
    decile90 <- decile[[10]]
    
    df1 <- df %>%
        mutate('score' = case_when(
            acc < decile10 ~ 1,
            (acc >= decile10) & (acc < decile20) ~ 2,
            (acc >= decile20) & (acc < decile30) ~ 3,
            (acc >= decile30) & (acc < decile40) ~ 4,
            (acc >= decile40) & (acc < decile50) ~ 5,
            (acc >= decile50) & (acc < decile60) ~ 6,
            (acc >= decile60) & (acc < decile70) ~ 7,
            (acc >= decile70) & (acc < decile80) ~ 8,
            (acc >= decile80) & (acc < decile90) ~ 9,
            (acc >= decile90) ~ 10))
    
    return(df1)
}

####################################################################################################

# UI
ui <- dashboardPagePlus(
    skin = "yellow",
    dashboardHeaderPlus(
        title = tagList(
            span(class = "logo-lg", "Tiny Blocks"), 
            #need to change to our icon
            img(src = "logo2.png", height = '35px'))
    ),
    
    title = "Tiny Blocks",
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "Home", icon = icon("home")),
            menuItem("Enabling Index Map", tabName = "Enabling", icon = icon("child")),
            menuItem("Acessibility Map", tabName = "Acessibility", icon = icon("directions")),
            menuItem("Data Explorer", tabName = "Data", icon = icon("table"))
        )
    ),
    ## Body content
    dashboardBody(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tags$script(HTML("$('body').addClass('fixed');")),
        
        tabItems(
        
            tabItem(tabName = "Home",
                    h2('Are our built spaces enabling healthy child development?'),
                    h5('The physical environment around our homes have an impact in the healthy development of children. 
                              Research has shown that the built spaces that children (aged 0 to 7) access and interact with daily, can enable their social, emotional and physical competence.
                              As such, an understanding of how well our HDBs enable development will assist policy planners to improve amenity provision and continue enabling the healthy development of children.
                              This project aims to model the geographic accessibility of built factors, and map the developmental enabling index of HDBs in each planning area.'),
                    br(),
                    h5('The project evaluates the child development based on the following three developmental domains. The accessibility of various built factors is used to measure these domains.'),
                    br(),
                    img(src = 'domain2.png', width = '100%')
            ),
            
            tabItem(tabName = "Enabling",
                    boxPlus(
                        width = 12,
                        title = "ENABLING INDEX MAP", 
                        closable = FALSE, 
                        # status = "warning", 
                        solidHeader = FALSE, 
                        collapsible = TRUE,
                        enable_dropdown = TRUE,
                        dropdown_icon = 'info',
                        dropdown_menu = dropdownItemList(
                            dropdownItem(name = 'Enabling index (1 to 10) is a measure of how well children in a specific HDB are developmentally enabled by the built environment surrounding them.'),
                            dropdownItem(name = 'A higher enabling index indicates a better environment for child development.')
                        ),
                        enable_sidebar = TRUE,
                        sidebar_width = 25,
                        sidebar_start_open = TRUE,
                        sidebar_content = tagList(
                            radioGroupButtons(
                                inputId = "select_zoom",
                                label = "Zoom Level", 
                                choices = c('Singapore' = 'sg',
                                            'Planning Area' = 'town'),
                                selected = 'town',
                                status = "warning"),
                            conditionalPanel(
                                condition = "input.select_zoom == 'town'",
                                pickerInput(
                                    inputId = 'select_zoom_town',
                                    label = 'Planning Area',
                                    choices = sort(towns$Town),
                                    options = list(`live-search` = TRUE,
                                                   size = 6)
                                )),
                            radioButtons(
                                inputId = 'select_class_index',
                                label = 'Classification Method',
                                choices = sort(c('Quantile' = 'quantile',
                                                 'Pretty' = 'pretty',
                                                 'Equal Interval' = 'equal',
                                                 'Standard Deviation' = 'sd',
                                                 'Jenks' = 'jenks')),
                                selected = 'quantile'
                            ),
                            sliderInput("select_numclass_index",
                                        "Number of Classes",
                                        min = 4,
                                        max = 12, 
                                        value = 5,
                                        step = 1)
                        ),
                        withLoader(tmapOutput("enabling_index_map", height = '70vh'), type = "html", loader="loader1")
                    ),
                    boxPlus(
                        width = 12,
                        title = 'ADJUST PARAMETERS',
                        closable = FALSE,
                        solidHeader = FALSE, 
                        collapsible = TRUE,
                        enable_sidebar = FALSE,
                        fluidRow(
                            column(width = 12,
                                   h4('Domain Weights'),
                                   p('A larger percentage places a greater importance on the developmental domain.', class='param-desc'))
                        ),
                        fluidRow(
                            column(width = 4,
                                   numericInputIcon('physical_perc',
                                                    label = "Physical Health & Wellbeing Domain",
                                                    value = 40,
                                                    min = 0,
                                                    max = 100,
                                                    step = 5,
                                                    icon = list(icon('shoe-prints'), icon('percent')))),
                            column(width = 4,
                                   numericInputIcon('social_perc',
                                                    label = "Social Competence Domain",
                                                    value = 30,
                                                    min = 0,
                                                    max = 100,
                                                    step = 5,
                                                    icon = list(icon('comments'), icon('percent')))),
                            column(width = 4, 
                                   numericInputIcon('emotional_perc',
                                                    label = "Emotional Maturity Domain",
                                                    value = 30,
                                                    min = 0,
                                                    max = 100,
                                                    step = 5,
                                                    icon = list(icon('brain'), icon('percent')))),
                        ),
                        fluidRow(
                            column(width = 12,
                                   conditionalPanel(
                                       condition = "(input.physical_perc + input.social_perc + input.emotional_perc) != 100",
                                       p('Domain weights must sum up to 100!',
                                         style='background-color:#c4603f; padding:10px; color:white; border-radius:10px')
                                   ))
                        ),
                        fluidRow(
                            column(width = 12,
                                   h4('Distance Decay Parameter'),
                                   p('A larger value indicates a greater sensitivity of the willingness to travel (from source to destination) to distance.', class='param-desc'))
                        ),
                        fluidRow(
                            column(width = 3,
                                   sliderInput("power_activityarea",
                                               "Activity Areas",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_commclub",
                                               "Community Clubs",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_cib",
                                               "Community in Bloom Gardens",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_commuse",
                                               "Community Use Sites",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1))
                            ),
                        fluidRow(
                            column(width = 3,
                                   sliderInput("power_dus",
                                               "DUS School Sports Facility",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_naturearea",
                                               "Nature Areas",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_park",
                                               "Parks",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_playfitness",
                                               "Play and Fitness Equipment",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1))
                            ),
                        fluidRow(
                            column(width = 3,
                                   sliderInput("power_preschool",
                                               "Pre-schools",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_prischool",
                                               "Primary Schools",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_sportsg",
                                               "SportSG Facilities",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1)),
                            column(width = 3,
                                   sliderInput("power_studentcare",
                                               "Student Care",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1))
                            ),
                        fluidRow(
                            column(width = 3,
                                   sliderInput("power_watersports",
                                               "Water Sports Facilities",
                                               min = 0.5,
                                               max = 2.5, 
                                               value = 2,
                                               step = 0.1))
                            )
                        
                    ),
                    boxPlus(
                        width = 12,
                        title = "ENABLING INDEX TABLE", 
                        closable = FALSE, 
                        # status = "warning", 
                        solidHeader = FALSE, 
                        collapsible = TRUE,
                        enable_sidebar = FALSE,
                        fluidRow(
                            column(width = 12,
                                   withLoader(DT::dataTableOutput('table'), type = "html", loader="loader1")
                                   )
                            
                        )
                    )
            ),
            
            tabItem(tabName = "Acessibility",
                    boxPlus(
                        width = 12,
                        title = "ACCESSIBILITY MAP", 
                        closable = FALSE, 
                        # status = "warning", 
                        solidHeader = FALSE, 
                        collapsible = TRUE,
                        enable_dropdown = TRUE,
                        dropdown_icon = 'info',
                        dropdown_menu = dropdownItemList(
                            dropdownItem(name = 'Hansen accessibility index is a measure of geographical accessibility between a source and destination, taking into account the capacity of the destination. It includes a distance decay parameter that is calibrated to reflect the willingness to travel from source to destination in the real world.'),
                            dropdownItem(name = 'A higher hansen accessibility index indicates greater accessibility. This index has been standardised (min-max standardisation) for consistency purposes.')
                        ),
                        enable_sidebar = TRUE,
                        sidebar_width = 25,
                        sidebar_start_open = TRUE,
                        sidebar_content = tagList(
                            pickerInput(
                                inputId = 'select_town',
                                label = 'Planning Area',
                                choices = sort(towns$Town),
                                options = list(`live-search` = TRUE,
                                               size = 6)
                            ),
                            pickerInput(
                                inputId = 'select_amenity',
                                label = 'Amenity',
                                choices = c('Activity Areas' = 'activity_area',
                                            'Community Clubs' = 'community_clubs',
                                            'Community Use Sites' = 'community_use_sites',
                                            'Community in Bloom Gardens' = 'cib_gardens',
                                            'Dual Use Scheme School Sports Facility' = 'dus_sports',
                                            'Nature Areas' = 'nature_areas',
                                            'Parks' = 'parks',
                                            'Play and Fitness Equipment' = 'play_fitness',
                                            'Pre-schools' = 'preschools',
                                            'Primary Schools' = 'pri_school',
                                            'SportSG Facilities' = 'sportsg',
                                            'Student Care' = 'student_care',
                                            'Water Sports Facilities' = 'water_sports'),
                                selected = 'preschools',
                                choicesOpt = list(content = images),
                                options = list(size = 6)
                            ),
                            sliderInput("select_power",
                                        "Distance Decay Parameter",
                                        min = 0.5,
                                        max = 2.5, 
                                        value = 2,
                                        step = 0.1),
                            radioButtons(
                                inputId = 'select_class',
                                label = 'Classification Method',
                                choices = sort(c('Quantile' = 'quantile',
                                                 'Pretty' = 'pretty',
                                                 'Equal Interval' = 'equal',
                                                 'Standard Deviation' = 'sd',
                                                 'Jenks' = 'jenks')),
                                selected = 'quantile'
                            ),
                            sliderInput("select_numclass",
                                        "Number of Classes",
                                        min = 4,
                                        max = 12, 
                                        value = 5,
                                        step = 1),
                        ),
                        conditionalPanel('input.select_amenity == "student_care"', withLoader(tmapOutput("accmap_student_care", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "pri_school"', withLoader(tmapOutput("accmap_pri_school", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "water_sports"', withLoader(tmapOutput("accmap_water_sports", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "dus_sports"', withLoader(tmapOutput("accmap_dus_sports", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "cib_gardens"', withLoader(tmapOutput("accmap_cib_gardens", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "preschools"', withLoader(tmapOutput("accmap_preschools", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "sportsg"', withLoader(tmapOutput("accmap_sportsg", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "play_fitness"', withLoader(tmapOutput("accmap_play_fitness", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "parks"', withLoader(tmapOutput("accmap_parks", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "community_use_sites"', withLoader(tmapOutput("accmap_community_use_sites", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "activity_area"', withLoader(tmapOutput("accmap_activity_area", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "nature_areas"', withLoader(tmapOutput("accmap_nature_areas", height='80vh'),type = "html", loader="loader1")),
                        conditionalPanel('input.select_amenity == "community_clubs"', withLoader(tmapOutput("accmap_community_clubs", height='80vh'),type = "html", loader="loader1"))
                    )
            ),
            tabItem(tabName = "Data",
                    DT::dataTableOutput('raw_table'),
            )
        )
    ))


# Server

server <- function(input, output, session) {
    
    ######################################## ENABLING INDEX MAP ######################################## 
    
    hdb_points <- reactive({
        if (input$select_zoom != 'sg') {
            st_intersection(hdb, subset(towns, Town == input$select_zoom_town))
        }
        else {
            hdb
        }
        
    })
    
    sg_polygon <- reactive({
        if (input$select_zoom != 'sg') {
            subset(towns, Town == input$select_zoom_town)
        }
        else {
            towns
        }
        
    })
    
    
    ## Compute hansen accessibility and enabling scores
    hansen_activityarea <- reactive({
        df <- calculate_acc(power = input$power_activityarea,
                            dist_mat = dm_activity_area,
                            capacity_list = activity_area_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_commclub <- reactive({
        df <- calculate_acc(power = input$power_commclub,
                            dist_mat = dm_community_clubs,
                            capacity_list = community_clubs_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_cib <- reactive({
        df <- calculate_acc(power = input$power_cib,
                            dist_mat = dm_cib_gardens,
                            capacity_list = cib_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_commuse <- reactive({
        df <- calculate_acc(power = input$power_commuse,
                            dist_mat = dm_community_use_sites,
                            capacity_list = community_use_sites_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_dus <- reactive({
        df <- calculate_acc(power = input$power_dus,
                            dist_mat = dm_dus_school_sports_facilities,
                            capacity_list = dus_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_naturearea <- reactive({
        df <- calculate_acc(power = input$power_naturearea,
                            dist_mat = dm_nature_area,
                            capacity_list = nature_area_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_park <- reactive({
        df <- calculate_acc(power = input$power_park,
                            dist_mat = dm_parks,
                            capacity_list = parks_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_playfitness <- reactive({
        df <- calculate_acc(power = input$power_playfitness,
                            dist_mat = dm_play_fitness,
                            capacity_list = play_fitness_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_preschool <- reactive({
        df <- calculate_acc(power = input$power_preschool,
                            dist_mat = dm_preschools,
                            capacity_list = preschool_capacity_list,
                            hdb_points = hdb_points())
        df$acc <- rescale(df$acc)
        df[is.na(df)] <- 0
        compute_enabling_scores(df)
    })
    
    hansen_prischool <- reactive({
        df <- calculate_acc(power = input$power_prischool,
                            dist_mat = dm_pri_schools,
                            capacity_list = school_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_sportsg <- reactive({
        df <- calculate_acc(power = input$power_sportsg,
                            dist_mat = dm_sportsg_facilities,
                            capacity_list = sportsg_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_studentcare <- reactive({
        df <- calculate_acc(power = input$power_studentcare,
                            dist_mat = dm_student_care,
                            capacity_list = student_care_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    hansen_watersports <- reactive({
        df <- calculate_acc(power = input$power_watersports,
                            dist_mat = dm_watersports_facilities,
                            capacity_list = watersports_capacity_list,
                            hdb_points = hdb_points())
        compute_enabling_scores(df)
    })
    
    
    hdb_enabling_index <- reactive({
        hdb_points() %>%
            as.data.frame() %>%
            cbind(hansen_activityarea()$score,
                  hansen_cib()$score,
                  hansen_commuse()$score,
                  hansen_commclub()$score,
                  hansen_dus()$score,
                  hansen_naturearea()$score,
                  hansen_park()$score,
                  hansen_playfitness()$score,
                  hansen_preschool()$score,
                  hansen_prischool()$score,
                  hansen_sportsg()$score,
                  hansen_studentcare()$score,
                  hansen_watersports()$score) %>%
            mutate('physical' = ((hansen_prischool()$score + hansen_preschool()$score 
                                + hansen_studentcare()$score + hansen_sportsg()$score
                                + hansen_dus()$score + hansen_watersports()$score
                                + hansen_playfitness()$score + hansen_activityarea()$score) / 8) * (input$physical_perc/100)) %>%
            mutate('social' = ((hansen_prischool()$score + hansen_preschool()$score 
                                + hansen_studentcare()$score + hansen_commclub()$score
                                + hansen_playfitness()$score + hansen_park()$score 
                                + hansen_activityarea()$score + hansen_sportsg()$score
                                + hansen_dus()$score + hansen_watersports()$score
                                + hansen_commuse()$score) / 11) * (input$social_perc/100)) %>%
            mutate('emotional' = ((hansen_naturearea()$score + hansen_park()$score
                                   + hansen_cib()$score) / 3) * (input$emotional_perc/100)) %>%
            mutate('enabling_index' = physical + social + emotional) %>%
            st_as_sf(sf_column_name = 'geometry')
    })
        

    output$enabling_index_map <- renderTmap({
        validate(need((input$physical_perc + input$social_perc + input$emotional_perc) == 100, message=FALSE))
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(sg_polygon()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(hdb_enabling_index()) +
            tm_dots(col = 'enabling_index',
                    palette = 'Greens',
                    style = input$select_class_index,
                    n = input$select_numclass_index,
                    id = 'street',
                    title = 'Enabling Index',
                    popup.vars = c('Enabling Index' = 'enabling_index',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Planning Area' = 'Town',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(hdb_enabling_index()))
    })
    
    ######################################## ACCESSIBILITY MAP ######################################## 
    
    hdb_clipped <- reactive({
        st_intersection(hdb, subset(towns, Town == input$select_town))
    })
    
    town_clipped <- reactive({
        subset(towns, Town == input$select_town)
    })
    
    output$accmap_student_care <- renderTmap({
        validate(need(input$select_amenity=="student_care", message=FALSE))
        
        dm <- subset(dm_student_care, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             student_care_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(student_care) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/school-book-bag.png', width = 20),
                       id = 'name',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    output$accmap_pri_school <- renderTmap({
        
        validate(need(input$select_amenity=="pri_school", message=FALSE))
        
        dm <- subset(dm_pri_schools, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             school_capacity_list,
                             dm, 
                             d0 = 50,
                             power = input$select_power, 
                             family = "Hansen"))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(pri_schools) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/grad_cap.png', width = 20),
                       id = 'name',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    output$accmap_water_sports <- renderTmap({
        validate(need(input$select_amenity=="water_sports", message=FALSE))
        
        dm <- subset(dm_watersports_facilities, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             watersports_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(watersports_facilities) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/swimming.png', width = 25),
                       id = 'name',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    output$accmap_dus_sports <- renderTmap({
        validate(need(input$select_amenity=="dus_sports", message=FALSE))
        
        dm <- subset(dm_dus_school_sports_facilities, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             dus_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(dus_sports_facilities) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/football.png', width = 20),
                       id = 'name',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    
    output$accmap_cib_gardens <- renderTmap({
        validate(need(input$select_amenity=="cib_gardens", message=FALSE))
        
        dm <- subset(dm_cib_gardens, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             cib_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(cib_gardens) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/flower.png', width = 20),
                       id = 'address',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    
    output$accmap_preschools <- renderTmap({
        validate(need(input$select_amenity=="preschools", message=FALSE))
        
        dm <- subset(dm_preschools, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             preschool_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        accessibility[is.na(accessibility)] <- 0
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(preschools) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/kid.png', height = 20),
                       id = 'name',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    
    output$accmap_sportsg <- renderTmap({
        validate(need(input$select_amenity=="sportsg", message=FALSE))
        
        dm <- subset(dm_sportsg_facilities, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             sportsg_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(sportsg_facilities) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/badminton.png', width = 22),
                       id = 'address',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    
    output$accmap_play_fitness <- renderTmap({
        validate(need(input$select_amenity=="play_fitness", message=FALSE))
        
        dm <- subset(dm_play_fitness, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             play_fitness_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(play_fitness) +
            tm_symbols(size = 0.5,
                       shape = tmap_icons('icons/triangle.png', width = 12),
                       popup.vars = FALSE) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    
    
    output$accmap_parks <- renderTmap({
        validate(need(input$select_amenity=="parks", message=FALSE))
        
        dm <- subset(dm_parks, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             parks_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(parks) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/tree.png', width = 20),
                       id = 'name',
                       popup.vars = c(),
                       alpha = 0.5) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    
    output$accmap_community_use_sites <- renderTmap({
        validate(need(input$select_amenity=="community_use_sites", message=FALSE))
        
        dm <- subset(dm_community_use_sites, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             community_use_sites_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(community_use_sites) +
            tm_symbols(size = 0.5,
                       shape = tmap_icons('icons/community.png', width = 20),
                       id = 'name',
                       popup.vars = c(),
                       alpha = 0.5) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    
    output$accmap_activity_area <- renderTmap({
        validate(need(input$select_amenity=="activity_area", message=FALSE))
        
        dm <- subset(dm_activity_area, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             activity_area_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(activity_area) +
            tm_symbols(size = 0.5,
                       shape = tmap_icons('icons/footprint.png', height = 20),
                       popup.vars = FALSE) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })
    
    
    output$accmap_nature_areas <- renderTmap({
        validate(need(input$select_amenity=="nature_areas", message=FALSE))
        
        dm <- subset(dm_nature_area, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             nature_area_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc) 
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(nature_area) +
            tm_symbols(size = 0.5,
                       shape = tmap_icons('icons/pine.png', width = 20),
                       popup.vars = FALSE) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })   
    
    
    output$accmap_community_clubs <- renderTmap({
        validate(need(input$select_amenity=="community_clubs", message=FALSE))
        
        dm <- subset(dm_community_clubs, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        dm <- as.matrix(dm/1000)
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             community_clubs_capacity_list,
                             dm,
                             d0 = 50,
                             power = input$select_power,
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        accessibility <- bind_cols(hdb_clipped(), acc)
        accessibility$acc <- rescale(accessibility$acc)
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'white',
                    alpha = 0.5,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.9, lwd = 1.8, col = 'black') +
            # tm_text('Town') +
            tm_shape(community_clubs) +
            tm_symbols(size = 0.5,
                       shape = tmap_icons('icons/comm_club.png', width = 20),
                       id = 'name',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    n = input$select_numclass,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children (estimated)' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE),
                    bbox = st_bbox(accessibility))
    })   

    
    ##########################################################################################################
    
    output$table <- DT::renderDataTable({
        data_table <- hdb_enabling_index() %>%
            st_set_geometry(NULL) %>%
            select("Town", "hs_nmbr","street", "pstl_cd", "chldr__", "physical","social",
                   "emotional", "enabling_index") %>%
            mutate(`enabling_index` = round(enabling_index, 1),
                   physical = round(physical, 1),
                   social = round(social, 1), 
                   emotional = round(emotional, 1)) %>%
            rename(`Physical Domain` = physical,
                   `Social Domain` = social,
                   `Emotional Domain` = emotional,
                   `Enabling Index` = enabling_index,
                   `House Number` = hs_nmbr,
                   Street = street,
                   `Postal Code` = pstl_cd,
                   `Number of Children` = chldr__)
        data_table <- data_table[order(data_table$`Enabling Index`, decreasing=TRUE),]
        DT::datatable(data = data_table,
                      options = list(pageLength = 5,
                                     scrollX = TRUE
                                     # initComplete = JS(
                                     #     "function(settings, json) {",
                                     #     "$(this.api().table().header()).css({'background-color': '#53a7ad', 'color': '#fff'});",
                                     #     "}")
                                     ),
                      rownames = FALSE)
    })
    
    output$raw_table <- DT::renderDataTable({
        DT::datatable(data = hdb %>%
                          st_set_geometry(NULL) %>%
                          select(-c(chldrn_, num_hdb)),
                      options = list(pageLength = 10,
                                     scrollX = TRUE
                      ),
                      rownames = FALSE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
