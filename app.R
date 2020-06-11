library(shiny)
library(tidyverse)
library(leaflet)
library(jsonlite)
library(httr)
library(maps)
library(mapdata)
library(stringr)
library(rvest)
library(ggplot2)

####################### data/var #############################
### page 1 ###
#background list
background <- list("default"= "OpenStreetMap.Mapnik", "vivid" = "Esri.NatGeoWorldMap", "black/white"= "Stamen.Toner")

#full university dataset
allu.ep <- GET(
    "http://universities.hipolabs.com/search?name=NULL&country=NULL", 
    query=list(name = NULL, country = NULL)
)
allu <- fromJSON(content(allu.ep, as = "text", encoding="UTF-8"), flatten = TRUE)


### page2 ###
#US NEWS top 100 school rank table
namelist <- c()
ranklist <- c()
countrylist <- c()
citylist <- c()
scorelist <- c()
enrolllist <- c()
urllist <- c()

for (i in 1:10){
    
    usnewsurl <- str_glue("https://www.usnews.com/education/best-global-universities/rankings?page={pnum}",
                          pnum = as.character(i)) 
    
    readurl <- read_html(usnewsurl)
    
    uniname <- readurl %>% 
        html_nodes("div.mb2.md-mb3.lg-mb4") %>%
        html_nodes("a") %>%
        html_text()
    
    unirank <- readurl %>%
        html_nodes("div.my3.lg-my4") %>%
        html_nodes("strong") %>%
        html_text() %>%
        str_subset("#\\d+") 
    
    unilocation <- readurl %>%
        html_nodes("div.mb2.md-mb3.lg-mb4") %>%
        html_nodes("span") %>%
        html_text() %>%
        str_subset("\\w")
    
    unicountry <- unilocation[c(TRUE, FALSE)]
    unicity <- unilocation[c(FALSE, TRUE)]
    
    uniscore_enroll <- readurl %>%
        html_nodes("dd.QuickStatHug__Description-hb1bl8-1.kLMBJU") %>%
        html_text()
    
    uniscore <- uniscore_enroll[c(TRUE, FALSE)]
    unienroll <- uniscore_enroll[c(FALSE, TRUE)]
    
    uniurl <- readurl %>%
        html_nodes("div.mb2.md-mb3.lg-mb4") %>%
        html_nodes("a") %>%
        html_attr("href")
    
    namelist <- c(namelist, uniname)
    ranklist <- c(ranklist, unirank)
    countrylist <- c(countrylist, unicountry)
    citylist <- c(citylist, unicity)
    scorelist <- c(scorelist, uniscore)
    enrolllist <- c(enrolllist, unienroll)
    urllist <- c(urllist, uniurl)
}

usnews <- tibble(Rank = ranklist, University = namelist, Country = countrylist, 
                 City = citylist, Enrollment = enrolllist, Score = scorelist, URL = urllist)

### page 3 ###

#currency abb list by country
currencylist <- list("United States"="USD", "United Kingdom" = "GBP", "Canada"= "CAD", "Switzerland" = "CHF", "Australia"= "AUD", 
                     "Denmark" = "DKK", "Singapore" = "SGD", "China" = "CNY", "France" = "EUR","Netherlands" ="NZD",
                     "Germany" = "EUR", "Sweden" = "SEK", "Saudi Arabia" = "SAR", "Belgium" ="EUR", "Japan" ="JPY",
                     "Finland" =  "EUR", "Norway" = "NOK", "Spain"= "ESP", "Israel"= "ILS", "Hong Kong" = "HKD")


####################### ui #############################

ui <- navbarPage("WorldTopU", id="worldU",
                 
    ### page 1 ### 
    tabPanel("Interactive map",
                 
                 leafletOutput("map", width=1000, height=500),
                 
                 # Shiny versions prior to 0.11 should use class = "modal" instead.
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                               width = "auto", height = "auto",
                               
                               h4("University Locator"),
                               
                               selectInput("ucountry", "Country", choices = c("---", unique(allu$country)), selected = "---"),
                               selectInput("uname", "University Name", choices = NULL, selected = "---" ),
                               radioButtons("background", "Visual Background",
                                            choices = names(background), 
                                            selected = "default", inline = TRUE)
                 ),                
                 tags$div(id="cite",'Project by Annie Tang (2020).')
       
             
    ),
    
    ### page 2 ###
    tabPanel("Rank Table",

             fluidRow(
                 column(4,
                        selectInput("country",
                                    "Country:",
                                    c("All",
                                      unique(as.character(usnews$Country))))
                 ),
                 column(4,
                        selectInput("city",
                                    "City:",
                                    c("All",
                                      unique(as.character(usnews$City))))
                 ),
                 column(4,
                        selectInput("range",
                                    "Rank Range:",
                                    c("All", "Top10", "11 to 30", "31 to 50", "51 to 100"))
                 )
             ),
             # Create a new row for the table.
             DT::dataTableOutput("table")
    ),
    
    ### page 3 ###
    tabPanel("Compare Details",
             
             fluidRow(
                 column(12, wellPanel(
                     checkboxGroupInput("criteria", "Criteria", choices = c("Date"="date","Time"="time",
                                                                            "Weather"="weather", "CurrencyExRate_USD"="currency"),
                                        inline=TRUE)
                 )
                 ),
                 
                 fluidRow(
                     column(6, wellPanel(
                         tags$h3("University A"),
                         selectInput("namesearchA", label="Select School Name:", choices = namelist, selected = "Harvard University"),
                         h6("Criteria at this Moment:"),
                         tableOutput("detailA")
                     )
                     ),
                     column(6, wellPanel(
                         tags$h3("University B"),
                         selectInput("namesearchB", label="Select School Name:", choices = namelist, selected = namelist[2]),
                         h6("Criteria at this Moment:"),
                         tableOutput("detailB")
                     )
                     )
                 ),
                 fluidRow(
                     column(12, h6("Bar Graph for Univeristy Details"),
                            plotOutput("bargraph"))
                 )
                 
             )
             
             
    )
    )


####################### server #############################

server <- function(input, output, session) {
    
    ### page1 interactive map ###
    
    #update name search input depend on country 
    observe({
        namematch <- allu %>% filter(country == input$ucountry) %>% pull(name)
        updateSelectInput(session, "uname", choices = c("---", namematch), selected = "---")
    })   
    
    #get location by university name
    ulocdata <- reactive({
        if (input$uname != "---") {
            uloc.ep <- str_glue("https://geocode.xyz/{placename}?json=1",
                                placename = URLencode(input$uname))
            ulocation <- fromJSON(
                content(GET(uloc.ep), as = "text", encoding="UTF-8"), flatten = TRUE
            )
            tibble(place = input$uname, lat = as.numeric(ulocation$latt), long = as.numeric(ulocation$longt))
        }
        
    })    
    
    #create the map
    output$map <- renderLeaflet({
        if (input$ucountry != "---" & input$uname == "---") {
            cbond <- iso3166 %>% filter(ISOname == input$ucountry) %>% pull(mapname)
            bounds <- map("world", cbond, fill = TRUE, plot = FALSE)
            
            leaflet() %>%
                addProviderTiles(background[[input$background]]) %>%
                addPolygons(data = bounds, group = "Countries", 
                            color = "red", 
                            weight = 2,
                            popup = ~names,
                            label = ~names,
                            fillOpacity = 0.1,
                            highlightOptions = highlightOptions(color = "black", 
                                                                weight = 2,
                                                                bringToFront = TRUE))
        } else if (input$uname != "---") {
            ulocdata() %>% 
                leaflet() %>%
                addProviderTiles(background[[input$background]]) %>%
                addMarkers(~long, ~lat, popup= ~place, label= ~place)
            
        } else {
            leaflet() %>%
                addTiles() %>%
                setView(lng = -93.85, lat = 37.45, zoom = 2) %>%
                addProviderTiles(background[[input$background]])
        }
        
    })
    
    
    
    ### page2 Rank Table ###
    
    #filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- usnews
        if (input$country != "All") {
            data <- data[data$Country == input$country,]
        }
        if (input$city != "All") {
            data <- data[data$City == input$city,]
        }
        if (input$range != "All") {
            rangelist <- list("Top10" = as.character(1:10), 
                              "11 to 30" = as.character(11:30), 
                              "31 to 50" = as.character(31:50), 
                              "51 to 100" = as.character(51:100))
            data <- data %>% filter(str_extract(Rank,"\\d+") %in% rangelist[[input$range]])
        }
        data
    }))
    
    
    ### page3 Compare in Detail ###   
    
    #get latest currency rate
    exrate <- fromJSON(
        content(
            GET("https://api.exchangeratesapi.io/latest?base=USD"),as = "text", encoding="UTF-8"),
        flatten = TRUE
    )
    
    #detail table for school A
    output$detailA <- renderTable({
        
        if (!(is.null(input$criteria))){
            
            #get A's currency exchange rate
            countryA <- usnews %>% filter(University == input$namesearchA) %>% pull(Country)
            exratenowA <- exrate$rates[[currencylist[[countryA]]]]
            
            #get A's location latt&long
            getcityA <- usnews %>% filter(University == input$namesearchA) %>% pull(City)
            
            locationA.ep <- str_glue("https://geocode.xyz/{placename}?json=1",
                                     placename = URLencode(getcityA))
            
            locationA <- fromJSON(
                content(GET(locationA.ep), as = "text", encoding="UTF-8"), flatten = TRUE
            )
            
            #get A's woeid
            woeidA.ep <- GET(
                str_glue("https://www.metaweather.com/api/location/search/?lattlong={lattlong}",
                         lattlong = paste(locationA$latt, locationA$longt, sep=","))
                
            )
            woeidataA <- fromJSON(content(woeidA.ep , as = "text", encoding="UTF-8"), flatten = TRUE)        
            woeidA <- woeidataA$woeid[1]
            
            #get A's local infomation
            metaA.ep <- str_glue("https://www.metaweather.com/api/location/{woeid}/",
                                 woeid = as.character(woeidA))
            
            metadataA <- fromJSON(
                content(GET(metaA.ep), as = "text", encoding="UTF-8"), flatten = TRUE
            )
            
            datenowA <- metadataA$consolidated_weather$applicable_date[[1]]
            timenowA <- metadataA$time
            weathernowA <- metadataA$consolidated_weather$weather_state_name[[1]]
            
            #output table A
            detailA <- tibble(date = datenowA, time = timenowA, weather = weathernowA, currency = exratenowA)
            detailA[, c(input$criteria)]
        }
    })
    
    #similar detail table for school B
    output$detailB <- renderTable({
        if (!(is.null(input$criteria))){
            countryB <- usnews %>% filter(University == input$namesearchB) %>% pull(Country)
            exratenowB <- exrate$rates[[currencylist[[countryB]]]]
            
            ##get extra info
            getcityB <- usnews %>% filter(University == input$namesearchB) %>% pull(City)
            
            locationB.ep <- str_glue("https://geocode.xyz/{placename}?json=1",
                                     placename = URLencode(getcityB))
            
            locationB <- fromJSON(
                content(GET(locationB.ep), as = "text", encoding="UTF-8"), flatten = TRUE
            )
            
            woeidB.ep <- GET(
                str_glue("https://www.metaweather.com/api/location/search/?lattlong={lattlong}",
                         lattlong = paste(locationB$latt, locationB$longt, sep=","))
                
            )
            woeidataB <- fromJSON(content(woeidB.ep , as = "text", encoding="UTF-8"), flatten = TRUE)        
            woeidB <- woeidataB$woeid[1]
            
            metaB.ep <- str_glue("https://www.metaweather.com/api/location/{woeid}/",
                                 woeid = as.character(woeidB))
            
            metadataB <- fromJSON(
                content(GET(metaB.ep), as = "text", encoding="UTF-8"), flatten = TRUE
            )
            
            datenowB <- metadataB$consolidated_weather$applicable_date[[1]]
            timenowB <- metadataB$time
            weathernowB <- metadataB$consolidated_weather$weather_state_name[[1]]
            
            detailB <- tibble(date = datenowB, time = timenowB, weather = weathernowB, currency = exratenowB)
            detailB[, c(input$criteria)]
        }
    })
    
    #page3 bargraph 
    
    output$bargraph <- renderPlot({
        
        #get A's info
        urlreadA <- usnews %>% filter(University==input$namesearchA) %>% pull(URL) %>% read_html()
        tagA <- urlreadA %>%
            html_node("div#uniData") %>%
            html_nodes("p.Paragraph-sc-1iyax29-0.cbeUjd") %>%
            html_text() %>%
            str_replace("^(Total\\s)?(N|n)umber\\sof\\s", "") %>%
            str_to_title()
        dataA <- urlreadA %>%
            html_node("div#uniData") %>%
            html_nodes("p.Paragraph-sc-1iyax29-0.gynMKH") %>%
            html_text()
        dfA <- tibble(tagA, dataA) %>% transmute( "School A" = dataA, Category = tagA)
        
        #get B's info
        urlreadB <- usnews %>% filter(University==input$namesearchB) %>% pull(URL) %>% read_html()
        tagB <- urlreadB %>%
            html_node("div#uniData") %>%
            html_nodes("p.Paragraph-sc-1iyax29-0.cbeUjd") %>%
            html_text()%>%
            str_replace("^(Total\\s)?(N|n)umber\\sof\\s", "") %>%
            str_to_title()
        dataB <- urlreadB %>%
            html_node("div#uniData") %>%
            html_nodes("p.Paragraph-sc-1iyax29-0.gynMKH") %>%
            html_text()
        dfB <- tibble(tagB, dataB) %>% transmute("School B" = dataB, Category = tagB)
        
        #get df for A and B
        dfAB <- dfA %>% inner_join(dfB, by="Category") %>%
            pivot_longer(-Category, names_to = "University", values_to = "Number")
        
        #output bargraph
        ggplot(data=dfAB, aes(x=Category, y=Number, fill=University)) +
            geom_bar(stat="identity", position=position_dodge())
    })
    
}

shinyApp(ui = ui, server = server)
