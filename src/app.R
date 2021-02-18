#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinyFiles)

source(file.path("src","leaflet_nrcan.r"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Match Line list data to Geograpnical System"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "shp", label = "Choose shp File", accept = ".shp"),

            shinyFilesButton(id = 'files', label='File select', title='Please select a file', multiple=FALSE)

        #     sliderInput("bins",
        #                 "Number of bins:",
        #                 min = 1,
        #                 max = 50,
        #                 value = 30)
        #
        )
        ,

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("mymap") #%>%


                #addPolygons(data=leaflet_nrcan_shp_transform(read_sf(file.path("data", "HR_000a18a_e", "HR_000a18a_e.shp"))), weight = 3, color = "black")
                # addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                #             opacity = 1.0, fillOpacity = 0.5,
                #             fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                #             highlightOptions = highlightOptions(color = "white", weight = 2,
                #                                                 bringToFront = TRUE),
                #             data = getMapData(
                #                 read_sf(file.path("data", "HR_000a18a_e", "HR_000a18a_e.shp"))
                #
                #             ))


            , p()
           # plotOutput("distPlot")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {





    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)




    output$mymap <- renderLeaflet({
        m <- leaflet_nrcan()


        asdf <- shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('json', 'shp', 'GeoJSON'))
        print(asdf)


        # shp <- input$shp
        # if(is.null(shp_fn)){
            m
        # }else{
        #
        #     req(shp)
        #     validate(need(ext == "csv", "Please upload a csv file"))
        #     #read_sf(file.path("data", "HR_000a18a_e", "HR_000a18a_e.shp"))
        #     shp_sf <- read_sf(shp_fn)
        #     shp_sf_t <- leaflet_nrcan_shp_transform(shp_sf)
        #     m %>%
        #         addPolygons(data = shp_sf_t,
        #                     weight = 3, color = "black")
        # }


        # leaflet() %>%
        #     addProviderTiles(providers$Stamen.TonerLite,
        #                      options = providerTileOptions(noWrap = TRUE)
        #     ) #%>%          addMarkers(data = points())
    })


    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application
shinyApp(ui = ui, server = server)
