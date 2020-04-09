# CENSUS # USE THIS ONE
#################
# LOAD PACKAGES #
#################
library(shiny)
library(leaflet)
library(tigris)
library(leaflet.extras)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(DT)

####################
# GET SPATIAL DATA #
####################
lnd <- tracts(state="IN", county=141, cb=TRUE)
def <- tracts(state="IN", county=141, cb=TRUE)

############################
# READ EXCEL TABULAR DATA #
###########################
data <- read.csv("finaltable.csv", header=TRUE)
data$NAME <- gsub(", St. Joseph County, Indiana", "\\1", data$NAME) #Remove the extra bit at the end
data$GEOID <- as.character(data$GEOID) #Convert GEOID in data to Character so it may merge
lnd@data <- left_join(lnd@data, data, by = 'GEOID') #Merge data

###########################
# CREATE CLEAN DATA TABLE #
###########################
datadata <- data
datadata$X.1 <- NULL
datadata$X.2 <- NULL
datadata$GEOID <- NULL

datadata$X1950 = paste0(datadata$X1950,"%")
datadata$X1960 = paste0(datadata$X1960,"%")
datadata$black = paste0(datadata$black,"%")
datadata$hispanic = paste0(datadata$hispanic,"%")
datadata$hs = paste0(datadata$hs,"%")
datadata$poverty = paste0(datadata$poverty,"%")
datadata$renter = paste0(datadata$renter,"%")


datadata$income <- formatC(datadata$income, big.mark=",")
datadata$income = paste0("$",datadata$income)

datadata <- rename(datadata, 'Pre-1950 Housing'=X1950)
datadata <- rename(datadata, 'Pre-1960 Housing'=X1960)
datadata <- rename(datadata, 'Black Population'=black)
datadata <- rename(datadata, 'Hispanic Population'=hispanic)
datadata <- rename(datadata, 'Attained HS Diploma'=hs)
datadata <- rename(datadata, 'Below Poverty Line'=poverty)
datadata <- rename(datadata, 'Housing Renter Occupied Units'=renter)
datadata <- rename(datadata, 'Median Year Housing Built'=year)
datadata <- rename(datadata, 'Median Household Income'=income)
datadata <- rename(datadata, 'Census Tract'=NAME)
datadata <- rename(datadata, 'Actual BLL (μg/dL)'=pb_act)
datadata <- rename(datadata, 'Model Predicted BLL (μg/dL)'=pb_pred)
datadata <- rename(datadata, 'Schultz Predicted BLL (μg/dL)'=pb_schu)
datadata <- rename(datadata, 'Census Order'=X)



#########################
# CREATE LEAFLET POPUPS #
#########################
popup <- paste0("<center>","<strong>", toupper(lnd$NAME.y), "</strong>","</center>",
                "<center>","<strong>","<code>","Predicted BLL: ",round(lnd$pb_pred,3)," μg/dL","</code>","</strong>","</center>",
                "<center>","<strong>","<code>","Actual BLL: ",round(lnd$pb_act,3)," μg/dL","</code>","</strong>","</center>",
                "<br>","Black Population: ","<strong>",round(lnd$black,2),"%","</strong>",
                "<br>","Population Below Poverty Line: ","<strong>", round(lnd$poverty,2),"%","</strong>",
                "<br>","Pre-1960 Housing: ","<strong>",round(lnd$X1960,3),"%","</strong>",
                "<br>","Pre-1950 Housing: ","<strong>", round(lnd$X1950,3),"%","</strong>",
                "<br>","Attained HS Diploma: ", "<strong>",round(lnd$hs,2),"%","</strong>",
                "<br>",
                "<br>","Hispanic Population: ","<strong>", round(lnd$hispanic,2),"%","</strong>",
                "<br>","Housing Renter Occupied Units: ","<strong>", round(lnd$renter,2),"%","</strong>",
                "<br>","Median Household Income: ","<strong>","$", format(round(lnd$income,2),
                                                                          big.mark=",", scientific=FALSE),"</strong>",
                "<br>","Median Year Housing Buit: ","<strong>", round(lnd$year,2),"</strong>")

########################
# CREATE LEAFLET COLOR #
########################
palBLLa <- colorNumeric(palette = "Blues",  domain = lnd$pb_act)
palBLLp <- colorNumeric(palette = "Greens", domain = lnd$pb_pred)
palB <- colorNumeric(palette = "Oranges", domain = lnd$black)
palH <- colorNumeric(palette = "Blues", domain = lnd$hispanic)
palHS <- colorNumeric(palette = "Greys", domain = lnd$hs)
palR <- colorNumeric(palette = "Blues", domain = lnd$renter)
pal5 <- colorNumeric(palette = "Blues", domain = lnd$X1950)
pal6 <- colorNumeric(palette = "Purples", domain = lnd$X1960)
palI <- colorNumeric(palette = "Blues", domain = lnd$income)
palY <- colorNumeric(palette = "Blues", domain = lnd$year, reverse = TRUE)
palP <- colorNumeric(palette = "BuGn", domain = lnd$poverty)

###############
# WRITE SHINY #
###############
ui <- dashboardPage(skin="blue",
                    dashboardHeader(
                        title = "PREDICTING BLOOD-LEAD LEVELS",
                        titleWidth = 350
                    ),
                    dashboardSidebar(
                        width = 350,
                        sidebarMenu(
                            id = "tabs",
                            menuItem(
                                "Lead Prediction Map",
                                tabName = "lead",
                                icon = icon("map"),
                                badgeLabel = "new",
                                badgeColor = "green"),
                            conditionalPanel(
                                "input.tabs == 'lead'",
                                radioButtons("variable", "Choose a dataset: ",c("Actual Recorded BLL" = "pb_act",
                                                                                "Model Predicted BLL" = "pb_pred"))
                            ),
                            menuItem(
                                "Socioeconomic Factors",
                                tabName = "maps",
                                icon = icon("map")),
                            conditionalPanel(
                                "input.tabs == 'maps'",
                                radioButtons("variable2", "Choose a variable: ",c("Black Population" = "black",
                                                                                  "Hispanic Population" = "hispanic",
                                                                                  "High School Diploma Attainment" = "hs",
                                                                                  "Housing Renter Occupied Units" = "renter",
                                                                                  "Pre-1950 Housing" = "1950",
                                                                                  "Pre-1960 Housing" = "1960",
                                                                                  "Median Household Income" = "income",
                                                                                  "Median Year Housing Buit" = "year",
                                                                                  "Population Below Poverty Line" = "poverty",
                                                                                  "Census Tract Map"="none"),
                                             selected = "black")
                            ),
                            menuItem("Dataset", tabName="datatable", icon=icon("database")),
                            menuItem("Methodology", tabName="method", icon=icon("pencil")),
                            menuItem("Research Abstract", tabName = "abstract", icon=icon("inbox")),
                            menuItem("About", tabName="about", icon=icon("info")),
                            conditionalPanel(
                                "input.tabs == 'results'",
                                radioButtons("variable3", "Choose a variable: ",c("Year of Sample"="yearSample",
                                                                                  "Month of Sample"="monthSample",
                                                                                  "Age of Child"="ageSample"))
                            )
                        )
                    ),
                    body <- dashboardBody(
                        tabItems(
                            tabItem(tabName="maps",
                                    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                    leafletOutput("map")
                            ),
                            tabItem(tabName="lead",
                                    tags$style(type = "text/css", "#map1 {height: calc(100vh - 80px) !important;}"),
                                    leafletOutput("map1")
                            ),
                            tabItem(tabName="datatable",
                                    h1("Census Tract Data (provided by the ACS)",align = "center"),
                                    DT::dataTableOutput("data1")
                            ),
                            tabItem(tabName="about",
                                    h3("What is this application?", align="center"),
                                    p("This application is designed to predict the blood-lead levels among children residing in all 75 census tracts in St. Joseph County, Indiana.",
                                      align = "center"),
                                    hr(),
                                    h4("Author Notes", align= 'center'),
                                    tags$p("Created by Elijah Silva",align = "center"),
                                    tags$p("University of Notre Dame",align = "center"),
                                    tags$p("Master of Science in Global Health",align = "center"),
                                    tags$p(a(href="https://www.linkedin.com/in/elijahsilva/","LinkedIn"),align = "center")
                            ),
                            tabItem(tabName="method",
                                    tags$img(src='flowchart.png')),
                            tabItem(tabName="abstract",
                                    h3("Why was this model created?", align="center"),
                                    tags$p("This regression model was developed to explore the relationship between childhood blood-lead levels and socioeconomic factors at the Saint Joseph County, Indiana, census tract level.  The childhood BLL records were obtained through the Saint Joseph County Health Department. The dataset included 35,936 screenings, collected between January 2005 and December 2015. Following data preparation, the final sample size included 12,364 children, between the ages of 6 and 36 months. Childhood BLL were regressed against nine socioeconomic status factors, in addition to the covariates: sampling test type, year of sample, month of sampling, and age of child in months. This model was developed because there is a lack of childhood blood-lead level data across many states; thus, public health officials cannot identify the areas at risk for lead exposure. As a result, children remain exposed to the harmful effects associated with lead. To help fill that gap, a regression model was developed to predict childhood blood-lead levels at the census tract level. The model identified percentage of population that is non-Hispanic black, percentage of population with a high school diploma, percentage of pre-1950 housing, percentage of pre-1960 housing and percentage below poverty line as strong predictors, in addition to the significant covariates of sampling test type, year of sample, month of sampling and age of child in months. The model based on the Saint Joseph County dataset predicted childhood blood-lead levels with an R2 of 0.12."),
                                    br(),
                                    tags$p("Abbreviations: BLL = blood lead level; SES = socioeconomic status"))
                        )
                    )
)


server <- function(input, output) {
    output$data1 <- DT::renderDataTable(datadata,
                                        options = list(scrollX = TRUE))
    observe({
        if (input$variable3 == "yearSample"){
            value <- tags$img(src='flowchart.png')
        }
        if (input$variable3 == "monthSample"){
            tags$img(src='flowchart.png')
        }
    })
    output$map1 <- renderLeaflet({
        leaflet() %>%
            addPolygons(data=def) %>% 
            addSearchOSM(options = searchOptions(zoom=15, position = 'topleft')) %>%
            addFullscreenControl() %>%
            addTiles() %>%
            clearShapes()
    })
    observe({
        if (input$tabs == 'lead'){
            leafletProxy("map1", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palBLLa(pb_act)) %>%
                addLegend("bottomright", title = "Actual BLL", pal = palBLLa, values = ~lnd$pb_act, opacity = 1,
                          labFormat = labelFormat(suffix = "μg/dL"))
        }
        if (input$variable == "none"){
            leafletProxy("map", data=def) %>%
                clearShapes() %>%
                addPolygons(data=def, stroke=TRUE, weight=1, fillOpacity = 0.001,smoothFactor = 0, popup=popup) %>%
                clearControls()
        }
        if (input$variable == "pb_pred"){
            leafletProxy("map1", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palBLLp(pb_pred)) %>%
                addLegend("bottomright", title = "Predicted BLL", pal = palBLLp, values = ~lnd$pb_pred, opacity = 1,
                          labFormat = labelFormat(suffix = "μg/dL"))
        }
    })
    output$map <- renderLeaflet({
        leaflet() %>%
            addPolygons(data=def) %>% 
            addSearchOSM(options = searchOptions(zoom=15, position = 'topleft')) %>%
            addFullscreenControl() %>%
            addTiles() %>%
            clearShapes()
    })
    observe({
        if (input$tabs == 'maps'){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palB(black)) %>%
                addLegend("bottomright", title = "Black Population", pal = palB, values = ~lnd$black, opacity = 1,
                          labFormat = labelFormat(suffix = "%"))
        }
        if (input$variable2 == "none"){
            leafletProxy("map", data=def) %>%
                clearShapes() %>%
                addPolygons(data=def, stroke=TRUE, weight=1, fillOpacity = 0.001,smoothFactor = 0, popup=popup) %>%
                clearControls()
        }
        if (input$variable2 == "black"){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palB(black)) %>%
                addLegend("bottomright", title = "Black Population", pal = palB, values = ~lnd$black, opacity = 1,
                          labFormat = labelFormat(suffix = "%"))
        }
        if (input$variable2 == "hispanic"){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palH(hispanic)) %>%
                addLegend("bottomright", title = "Hispanic Population", pal = palH, values = ~lnd$hispanic, opacity = 1,
                          labFormat = labelFormat(suffix = "%"))
        }
        if (input$variable2 == "hs"){ 
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palHS(hs)) %>%
                addLegend("bottomright", title = "Attained High School Diploma", pal = palHS, values = ~lnd$hs, opacity = 1,
                          labFormat = labelFormat(suffix = "%"))
        }
        if (input$variable2 == "renter"){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palR(renter)) %>%
                addLegend("bottomright", title = "Housing Renter Occupied Units", pal = palR, values = ~lnd$renter, opacity = 1,
                          labFormat = labelFormat(suffix = "%"))
        }
        if (input$variable2 == "1950"){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~pal5(X1950)) %>%
                addLegend("bottomright", title = "Pre-1950 Housing", pal = pal5, values = ~lnd$X1950, opacity = 1,
                          labFormat = labelFormat(suffix = "%"))
        }
        if (input$variable2 == "1960"){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~pal6(X1960)) %>%
                addLegend("bottomright", title = "Pre-1960 Housing", pal = pal6, values = ~lnd$X1960, opacity = 1,
                          labFormat = labelFormat(suffix = "%"))
        }
        if (input$variable2 == "income"){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palI(income)) %>%
                addLegend("bottomright", title = "Median Household Income", pal = palI, values = ~lnd$income, opacity = 1,
                          labFormat = labelFormat(prefix = "$"))
        }
        if (input$variable2 == "year"){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palY(year)) %>%
                addLegend("bottomright", title = "Median Year Housing Built", pal = palY, values = lnd$year, opacity = 1, 
                          labFormat = labelFormat(big.mark = ""))
        }
        if (input$variable2 == "poverty"){
            leafletProxy("map", data=lnd) %>%
                clearControls() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, popup = popup,
                            color = ~palP(poverty)) %>%
                addLegend("bottomright", title = "Population Below Poverty Line", pal = palP, values = ~lnd$poverty, opacity = 1,
                          labFormat = labelFormat(suffix = "%"))
        }
    })
}

shinyApp(ui, server)