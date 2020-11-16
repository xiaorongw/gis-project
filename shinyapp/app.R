packages = c('shiny','shinydashboard','shinydashboardPlus', 'tidyverse', 'sf', 'sp', 'tmap', 'SpatialAcc')

for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

# Import maps
# source("prep.R")

###################################################################################################################

# Data preparation

towns <- st_read(dsn = 'data/geospatial', layer = 'hdb_towns')
towns <- st_transform(towns, 3414)

hdb <- st_read(dsn = 'data/geospatial', layer = 'hdb_processed')

### data from distance_matrix_ ###
## Student care
student_care <- st_read(dsn = 'data/geospatial', layer = 'student_care') %>%
    select(ID, Name, ADDRESSSTR, ADDRESSPOS) %>%
    rename(name = Name,
           address = ADDRESSSTR,
           postal_code = ADDRESSPOS)
dm_student_care <- read_csv('data/aspatial/distance matrix/hdb_studentcare.csv')
dm_student_care <- dm_student_care %>%
    select(origin_id, destination_id, total_cost) %>%
    pivot_wider(names_from = destination_id, values_from = total_cost)
student_care_capacity <- round(42907 / nrow(student_care), 0)
student_care_capacity_list <- rep(student_care_capacity, nrow(student_care))

## Primary schools
pri_schools <- st_read(dsn = 'data/geospatial', layer = 'schools_primary') %>%
    select(ID, school_nam, address, postal_cod) %>%
    rename(name = school_nam,
           postal_code = postal_cod)
dm_pri_schools <- read_csv('data/aspatial/distance matrix/hdb_school.csv')
dm_pri_schools <- dm_pri_schools %>%
    select(origin_id, destination_id, total_cost) %>%
    pivot_wider(names_from = destination_id, values_from = total_cost)
school_capacity <- round(37671 / nrow(pri_schools))
school_capacity_list <- rep(school_capacity, nrow(pri_schools))

## Water sports facilities
watersports_facilities <- st_read(dsn = 'data/geospatial', layer = 'water_sport_facilities') %>%
    select(ID, NAME, ADDRESSSTR, ADDRESSPOS) %>%
    rename(name = NAME,
           address = ADDRESSSTR,
           postal_code = ADDRESSPOS)
dm_watersports_facilities <- read_csv('data/aspatial/distance matrix/hdb_watersports.csv')
dm_watersports_facilities <- dm_watersports_facilities %>%
    select(origin_id, destination_id, total_cost) %>%
    pivot_wider(names_from = destination_id, values_from = total_cost) 
watersports_capacity_list <- rep(1, nrow(watersports_facilities))

## DUS sports facilities
dus_sports_facilities <- st_read(dsn = 'data/geospatial', layer = 'dus_school_sports_facilities') %>%
    select(ID, SCHOOL_NAM, ADDRESS, POSTAL_COD, FACILITIES) %>%
    rename(name = SCHOOL_NAM,
           address = ADDRESS,
           postal_code = POSTAL_COD,
           facilities = FACILITIES)
dm_dus_school_sports_facilities <- read_csv('data/aspatial/distance matrix/hdb_dus.csv')
dm_dus_school_sports_facilities <- dm_dus_school_sports_facilities %>%
    select(origin_id, destination_id, total_cost) %>%
    pivot_wider(names_from = destination_id, values_from = total_cost) 
dus_capacity_list <- rep(1, nrow(dus_sports_facilities))

## Community in bloom garden
cib_gardens <- st_read(dsn = 'data/geospatial', layer = 'cib_gardens') %>%
    select(ID, ADDRESS, DIVISION, CONSTITUEN, CATEGORY) %>%
    rename(address = ADDRESS,
           division = DIVISION, 
           constituency = CONSTITUEN,
           category = CATEGORY)
dm_cib_gardens <- read_csv('data/aspatial/distance matrix/hdb_cib.csv')
dm_cib_gardens <- dm_cib_gardens %>%
    select(origin_id, destination_id, total_cost) %>%
    pivot_wider(names_from = destination_id, values_from = total_cost)
cib_capacity_list <- rep(1, nrow(cib_gardens))

## Preschools
preschools <- st_read(dsn = 'data/geospatial', layer = 'preschools') %>%
    select(ID, CENTRE_NAM, ADDRESS, POSTAL_COD) %>%
    rename(name = CENTRE_NAM,
           address = ADDRESS,
           postal_code = POSTAL_COD)
dm_preschools <- read_csv('data/aspatial/distance matrix/hdb_preschools.csv')
dm_preschools <- dm_preschools %>%
    select(origin_id, destination_id, total_cost) %>%
    pivot_wider(names_from = destination_id, values_from = total_cost)
preschool_capacity <- round(215579 / nrow(preschools))
preschool_capacity_list <- rep(preschool_capacity, nrow(preschools))

## Sportsg sports facilities
sportsg_facilities <- st_read(dsn = 'data/geospatial', layer = 'sportsg_sports_facilities') %>%
    select(ROAD_NAME, FACILITIES) %>%
    rename(address = ROAD_NAME,
           facilities = FACILITIES)
dm_sportsg_facilities <- read_csv('data/aspatial/distance matrix/hdb_sportsg.csv')
dm_sportsg_facilities <- dm_sportsg_facilities %>%
    select(origin_id, destination_id, total_cost) %>%
    pivot_wider(names_from = destination_id, values_from = total_cost)
sportsg_capacity_list <- rep(1, nrow(sportsg_facilities))

####################################################################################################

hdb_risk_sf <- st_read(dsn = 'data/geospatial', layer = 'hdb_risk') 
hdb_risk_sf <- hdb_risk_sf%>%
    rename(risk_cib_gardens = rsk_cb_,
           risk_dus_sports = "rsk_ds_",
           risk_preschools = "rsk_prs",
           risk_pri_schools = "rsk_pr_",
           risk_student_care = "rsk_st_",
           risk_watersports_facilities = "rsk_wt_")
hdb_risk <-as(hdb_risk_sf, "Spatial")


hdb_accessibility <- st_read(dsn = 'data/geospatial', layer = 'hdb_accessibility') 
hdb_accessibility <-as(hdb_accessibility, "Spatial")

###################################################################################################################


# UI
ui <- dashboardPagePlus(
    skin = "purple",
    dashboardHeaderPlus(
        title = tagList(
            span(class = "logo-lg", "Tiny Blocks"), 
            #need to change to our icon
            img(src = "https://image.flaticon.com/icons/svg/204/204074.svg"))
        #img(src="logo.png"))
    ),
    
    title = "Tiny Blocks",
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Risk Map", tabName = "Risk", icon = icon("exclamation-triangle")),
            menuItem("Acessibility Map", tabName = "Acessibility", icon = icon("directions")),
            menuItem("Data Explorer", tabName = "Data", icon = icon("table"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "Risk",
                    # Can include our map in this box
                    boxPlus(
                        width = 12,
                        title = "Risk map", 
                        closable = FALSE, 
                        status = "warning", 
                        solidHeader = FALSE, 
                        collapsible = TRUE,
                        enable_sidebar = FALSE,
                        sidebar_width = 25,
                        sidebar_start_open = FALSE,
                        sidebar_content = tagList(
                            checkboxInput("somevalue", "Some value", FALSE),
                            verbatimTextOutput("value"),
                            sliderInput(
                                "slider_boxsidebar", 
                                "Number of observations:",
                                min = 0, 
                                max = 1000, 
                                value = 500
                            )
                        ),
                        tmapOutput("risk_map")
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "Acessibility",
                    boxPlus(
                        width = 12,
                        title = "Accessibility Map", 
                        closable = FALSE, 
                        status = "warning", 
                        solidHeader = FALSE, 
                        collapsible = TRUE,
                        enable_sidebar = TRUE,
                        sidebar_width = 25,
                        sidebar_start_open = TRUE,
                        sidebar_content = tagList(
                            selectInput(
                                inputId = 'select_town',
                                label = 'HDB Town',
                                choices = sort(towns$Town)
                            ),
                            selectInput(
                                inputId = 'select_amenity',
                                label = 'Amenity',
                                choices = c('Community in Bloom Gardens' = 'cib_gardens',
                                            'Dual Use Scheme School Sports Facility' = 'dus_sports',
                                            'Pre-schools' = 'preschools',
                                            'Primary Schools' = 'pri_school',
                                            'SportSG Facilities' = 'sportsg',
                                            'Student Care' = 'student_care',
                                            'Water Sports Facilities' = 'water_sports'),
                                selected = 'student_care'
                            ),
                            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
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
                            )
                        ),
                        conditionalPanel('input.select_amenity == "student_care"', tmapOutput("accmap_student_care", height='80vh')),
                        conditionalPanel('input.select_amenity == "pri_school"', tmapOutput("accmap_pri_school", height='80vh')),
                        conditionalPanel('input.select_amenity == "water_sports"', tmapOutput("accmap_water_sports", height='80vh')),
                        conditionalPanel('input.select_amenity == "dus_sports"', tmapOutput("accmap_dus_sports", height='80vh')),
                        conditionalPanel('input.select_amenity == "cib_gardens"', tmapOutput("accmap_cib_gardens", height='80vh')),
                        conditionalPanel('input.select_amenity == "preschools"', tmapOutput("accmap_preschools", height='80vh')),
                        conditionalPanel('input.select_amenity == "sportsg"', tmapOutput("accmap_sportsg", height='80vh'))
                    )
            ),
            # Third tab content
            tabItem(tabName = "Data",
                    DT::dataTableOutput('Table'),
            )
        )
    ))


# Server

server <- function(input, output, session) {
    
    # Interactive tmap output 
    output$risk_map <- renderTmap({
        tm_shape(hdb_risk) +
            tm_bubbles(col = 'cmpst_r',
                       size = 0.1,
                       alpha = 0.5,
                       border.lwd = 0.01)
    })
    
    ######################################## ACCESSIBILITY MAP ######################################## 
    
    hdb_clipped <- reactive({
        st_intersection(hdb, subset(towns, Town == input$select_town))
    })

    town_clipped <- reactive({
        subset(towns, Town == input$select_town)
    })

    # dm_clipped <- reactive({
    #     dm <- subset(dm_student_care, origin_id %in% hdb_clipped()$ID) %>%
    #         select(-origin_id)
    #     as.matrix(dm/1000)
    # })
    # 
    # accessibility <- reactive({
    #     acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
    #                          student_care_capacity_list,
    #                          dm_clipped(),
    #                          d0 = 50,
    #                          power = input$select_power,
    #                          family = 'Hansen'))
    # 
    #     colnames(acc) <- "acc"
    #     acc <- as_tibble(acc)
    #     bind_cols(hdb_clipped(), acc)
    # })

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
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
        tm_shape(town_clipped()) +
            tm_fill(col = 'lightgray',
                    alpha = 0.85,
                    id = 'Town',
                    popup.vars = c()
                    ) +
            tm_borders(alpha = 0.6, lwd = 0.5) +
            # tm_text('Town') +
        tm_shape(student_care) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/teddy-bear.png', width = 20),
                       id = 'name',
                       popup.vars = c()) +
        tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('Town' = 'Town',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children' = 'chldr__')) +
        tm_view(view.legend.position = c('left', 'bottom'),
                leaflet.options = c(attributionControl = FALSE))
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
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'lightgray',
                    alpha = 0.85,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.6, lwd = 0.5) +
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
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('Town' = 'Town',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE))
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
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'lightgray',
                    alpha = 0.85,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.6, lwd = 0.5) +
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
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('Town' = 'Town',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE))
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
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'lightgray',
                    alpha = 0.85,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.6, lwd = 0.5) +
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
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('Town' = 'Town',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE))
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
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'lightgray',
                    alpha = 0.85,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.6, lwd = 0.5) +
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
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('Town' = 'Town',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE))
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
        accessibility[is.na(accessibility)] <- 0
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'lightgray',
                    alpha = 0.85,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.6, lwd = 0.5) +
            # tm_text('Town') +
            tm_shape(preschools) +
            tm_symbols(col = 'black',
                       size = 0.5,
                       shape = tmap_icons('icons/school.png', width = 20),
                       id = 'name',
                       popup.vars = c()) +
            tm_shape(accessibility) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('Town' = 'Town',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE))
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
        accessibility[is.na(accessibility)] <- 0
        
        tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
            tm_shape(town_clipped()) +
            tm_fill(col = 'lightgray',
                    alpha = 0.85,
                    id = 'Town',
                    popup.vars = c()
            ) +
            tm_borders(alpha = 0.6, lwd = 0.5) +
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
                    id = 'street',
                    title = 'Hansen Accessibility',
                    popup.vars = c('Town' = 'Town',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children' = 'chldr__')) +
            tm_view(view.legend.position = c('left', 'bottom'),
                    leaflet.options = c(attributionControl = FALSE))
    })
    
    
    ##########################################################################################################
    
    output$Table <- DT::renderDataTable({
        DT::datatable(data = hdb_risk_sf %>%
                          select(1:15),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
