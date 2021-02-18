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
library(data.table)
library(DT)

source(file.path("src","leaflet_nrcan.r"))

source(file.path("src","functions.r"))

###################################################
# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Match Line list data to Geograpnical System"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            shinyFilesButton(id = 'files', label='polygon file', title='Please select a shape file', multiple=FALSE),
            verbatimTextOutput('filepaths'),
            shinyFilesButton(id = 'files_patient', label='patien files', title='Please select a file', multiple=FALSE),
            verbatimTextOutput('filepaths_patient'),
            selectInput(inputId = "postal_nm", label = "postal code", choices = NULL),
            selectInput(inputId = "time_nm", label = "time", choices = NULL),
            selectInput(inputId = "time_type_nm", label = "type Time", choices = c("episode " = "episode",
                                                                                   "onset" = "onset",
                                                                                   "report" = "report",
                                                                                   "test" = "test"
                                                                                   )),
            actionButton("Summarize_by_shp", "Summarize"),
            dataTableOutput('patient_table')
        ),


        # Show a plot of the map
        mainPanel(
            leafletOutput("mymap", height = "95vh")



        )
    )
)


summary_df = NULL

###################################################
# Define server logic required
server <- function(input, output, session) {


    roots = c(wd='.')
    shinyFileChoose(input = input, id = 'files', root=roots, filetypes=c('',' ','json', 'shp', 'GeoJSON'))

    # output$filepaths <- renderText({inFile})
    output$filepaths <- renderPrint({parseFilePaths(roots, input$files)})



    shinyFileChoose(input = input, id = 'files_patient', root=roots, filetypes=c('',' ','txt', 'csv', 'tab'))
    output$filepaths_patient <- renderPrint({parseFilePaths(roots, input$files_patient)})


    Summarize_by_shp <- eventReactive(input$Summarize_by_shp, {
        get_patient_dat_summary_map()
    })



    get_patient_dat <- reactive({
        inFile_p <- parseFilePaths(roots, input$files_patient)
        dat <- NULL
        if( NROW(inFile_p)) {
            dat <- read_csv(as.character(inFile_p$datapath))
            # dat <-
            #     dat %>%
            #     sample_n(n)%>% t() %>% as.tibble() %>%
            #     setnames(paste("record", 1:n)) %>%
            #     mutate(columns = colnames(dat)) %>%
            #     relocate(columns)
            #print(head(dat))
        }
        return(dat)
    })

    get_patient_dat_summary_map <- reactive({
        data <- get_patient_dat()
        shp <- get_shp_dat()

        summary_df <-
            wbe_gis_summarize_postal_in_shp(data = data,
                                            data_shp = shp,
                                            postal_col = input$postal_nm,
                                            date_col = input$time_nm,
                                            input$time_type_nm
            )
    })




    get_shp_dat <- reactive({
        inFile <- parseFilePaths(roots, input$files)
        dat <- NULL
        if( NROW(inFile)) {
            dat <- read_sf(as.character(inFile$datapath))
        }
        return(dat)
    })


    choices_cols <- reactive({
        choices_cities <- get_patient_dat() %>% colnames() %>% sort()
    })

    observe({
        updateSelectInput(session = session, inputId = "postal_nm", choices = choices_cols())
    })
    observe({
        updateSelectInput(session = session, inputId = "time_nm", choices = choices_cols())
    })


    #dat <- wbe_gis_gererate_fake_covid_data()
    # dat <- iris
    # dat <-
    #     dat %>% sample_n(n)%>% t() %>% as.tibble() %>%
    #     setnames(paste("record", 1:n)) %>%
    #     mutate(columns = colnames(dat)) %>%
    #     relocate(columns)

    #output$table <- renderDataTable(dat)
    output$patient_table <- DT::renderDT({
        Summarize_by_shp()
        #
        #
        # dat <- get_patient_dat()
        # if( ! is.null(dat)) {
        #     dat <-
        #         dat %>%
        #          sample_n(n)%>% t() %>% as.tibble() %>%
        #          setnames(paste("record", 1:n)) %>%
        #          mutate(columns = colnames(dat)) %>%
        #          relocate(columns)
        #
        # }
        # return(dat)
    },
    server=FALSE,
    extensions = c('Buttons', 'Scroller'),
    options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
    )

    )





    output$mymap <- renderLeaflet({
        m <- leaflet_nrcan()

        data <- get_patient_dat()
        shp <- get_shp_dat()
        # inFile<- parseFilePaths(roots, input$files)
        # shp <- NULL
        # if( NROW(inFile)) {
        #     shp <- read_sf(as.character(inFile$datapath))
        #     print(head(shp))
        # }
        if(!is.null(shp)){
            shp_sf_t <- leaflet_nrcan_shp_transform(shp)
            m<-
                m %>%
                addPolygons(data = shp_sf_t,
                            weight = 3, color = "black")
        }
        if(!is.null(data) & input$postal_nm %in% colnames(data)
           ){

            p_shp <- wbe_gis_match_postal_df(data = data, postal_col = input$postal_nm)
            p_shp_t <- leaflet_nrcan_point_extract(p_shp)
            m<-
                m %>% addMarkers(lng = p_shp_t$lng, lat = p_shp_t$lat)

        }

        return(m)
    })


}





# Run the application
shinyApp(ui = ui, server = server, options = list(height = 9999))
