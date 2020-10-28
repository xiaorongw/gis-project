

packages = c('shiny','shinydashboard','shinydashboardPlus')

for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

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
                title = "boxPlus with sidebar", 
                closable = FALSE, 
                status = "warning", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 25,
                sidebar_start_open = TRUE,
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
                #sample does not work
                plotOutput("boxSidebarPlot")
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

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
