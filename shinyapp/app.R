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

## 


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
                                choices = c('Community in Bloom Gardens' = 'hnsn_c_',
                                            'Dual Use Scheme School Sports Facility' = 'hnsn_d_',
                                            'Pre-schools' = 'hnsn_pr',
                                            'Primary Schools' = 'hnsn_p_',
                                            'Student Care' = 'hnsn_s_',
                                            'Water Sports Facilities' = 'hnsn_w_'),
                                selected = 'hnsn_pr'
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
                        tmapOutput("accessibility_map", height='80vh')
                    )
            ),
            # Third tab content
            tabItem(tabName = "Data",
                    DT::dataTableOutput('Table'),
            )
        )
    ))


# Server

server <- function(input, output) {
    
    # Interactive tmap output 
    output$risk_map <- renderTmap({
        tm_shape(hdb_risk) +
            tm_bubbles(col = 'cmpst_r',
                       size = 0.1,
                       alpha = 0.5,
                       border.lwd = 0.01)
    })
    
    hdb_clipped <- reactive({
        st_intersection(hdb, subset(towns, Town == input$select_town))
    })
    
    town_clipped <- reactive({
        subset(towns, Town == input$select_town)
    })
    
    dm_clipped <- reactive({
        dm <- subset(dm_student_care, origin_id %in% hdb_clipped()$ID) %>%
            select(-origin_id)
        as.matrix(dm/1000)
    })
    
    accessibility <- reactive({
        acc <- data.frame(ac(hdb_clipped()$children_each_hdb,
                             student_care_capacity_list,
                             dm_clipped(), 
                             d0 = 50,
                             power = input$select_power, 
                             family = 'Hansen'))
        
        colnames(acc) <- "acc"
        acc <- as_tibble(acc)
        bind_cols(hdb_clipped(), acc)
    })
    
    output$accessibility_map <- renderTmap({
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
                       shape = tmap_icons('icons/book.png', width = 20),
                       id = 'name',
                       popup.vars = c()) +
        tm_shape(accessibility()) +
            tm_dots(col = 'acc',
                    palette = '-Oranges',
                    style = input$select_class,
                    id = 'street',
                    popup.vars = c('Town' = 'Town',
                                   'House Number' = 'hs_nmbr',
                                   'Street' = 'street',
                                   'Postal Code' = 'pstl_cd',
                                   'Number of Levels' = 'nm_lvls',
                                   'Number of Children' = 'chldr__')) 
    })
    
    output$Table <- DT::renderDataTable({
        DT::datatable(data = hdb_risk_sf %>%
                          select(1:15),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
