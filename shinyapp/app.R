packages = c('shiny','shinydashboard','shinydashboardPlus', 'tidyverse', 'sf', 'sp', 'tmap')

for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

# Import maps
# source("prep.R")

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
print(hdb_accessibility)


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
                        title = "Accessibility Map: Community in Bloom Gardens", 
                        closable = FALSE, 
                        status = "warning", 
                        solidHeader = FALSE, 
                        collapsible = TRUE,
                        enable_sidebar = FALSE,
                        sidebar_width = 25,
                        sidebar_start_open = FALSE,
                        sidebar_content = tagList(
                            selectInput(
                                inputId = 'acc_select',
                                label = 'Accessbility to Amenity',
                                choices = c('Community in Bloom Gardens' = 'hnsn_c_',
                                            'Dual Use Scheme School Sports Facility' = 'hnsn_d_',
                                            'Pre-schools' = 'hnsn_pr',
                                            'Primary Schools' = 'hnsn_p_',
                                            'Student Care' = 'hnsn_s_',
                                            'Water Sports Facilities' = 'hnsn_w_'),
                                selected = 'hnsn_pr'
                            )
                        ),
                        tmapOutput("accessibility_map1")
                    ),
                    boxPlus(
                        width = 12,
                        title = "Accessibility Map: Preschools", 
                        closable = FALSE, 
                        status = "warning", 
                        solidHeader = FALSE, 
                        collapsible = TRUE,
                        enable_sidebar = FALSE,
                        sidebar_width = 25,
                        sidebar_start_open = FALSE,
                        sidebar_content = tagList(
                            selectInput(
                                inputId = 'acc_select',
                                label = 'Accessbility to Amenity',
                                choices = c('Community in Bloom Gardens' = 'hnsn_c_',
                                            'Dual Use Scheme School Sports Facility' = 'hnsn_d_',
                                            'Pre-schools' = 'hnsn_pr',
                                            'Primary Schools' = 'hnsn_p_',
                                            'Student Care' = 'hnsn_s_',
                                            'Water Sports Facilities' = 'hnsn_w_'),
                                selected = 'hnsn_pr'
                            )
                        ),
                        tmapOutput("accessibility_map2")
                    ),
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
    
    output$accessibility_map1 <- renderTmap({
        tm_shape(hdb_accessibility) +
            tm_bubbles(col = 'hnsn_c_',
                       size = 0.1,
                       alpha = 0.5,
                       border.lwd = 0.01)

    })
    
    output$accessibility_map2 <- renderTmap({
        tm_shape(hdb_accessibility) +
            tm_bubbles(col = 'hnsn_pr',
                       size = 0.1,
                       alpha = 0.5,
                       border.lwd = 0.01)
        
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
