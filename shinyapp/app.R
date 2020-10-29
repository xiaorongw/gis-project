

packages = c('shiny','shinydashboard','shinydashboardPlus', 'tidyverse', 'sf','sp','tmap','tmaptools','leaflet')

for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

# Import maps
source("prep.R")
#hdb_risk <- st_read(dsn = 'data/geospatial', layer = 'hdb_risk') 
hdb_risk <-as(hdb_risk,"Spatial")
#subzone_children <- st_read(dsn = 'data/geospatial', layer = 'subzone_children')
subzone_children <-as(subzone_children,"Spatial")
# UI



ui <- dashboardPagePlus(
    skin = "purple",
    dashboardHeaderPlus(
        title = tagList(
        span(class = "logo-lg", "Tiny Block"), 
        #need to change to our icon
        img(src = "https://image.flaticon.com/icons/svg/204/204074.svg"))
        #img(src="logo.png"))
    ),
    
    title = "Tiny Block",
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
                #tmapOutput('risk_map')
                tmapOutput("risk_map")
            )
            ),
            
            # Second tab content
            tabItem(tabName = "Acessibility",
                    h2("Widgets tab content")
            ),
            # Third tab content
            tabItem(tabName = "Data")
        )
    ))


# Server

server <- function(input, output) {
    
    # Interactive tmap output 
    output$risk_map <- renderTmap({
        tm_shape(hdb_risk) +
            tm_bubbles(col = 'composite_risk',
                       size = 0.1,
                       alpha = 0.5,
                       border.lwd = NA)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
