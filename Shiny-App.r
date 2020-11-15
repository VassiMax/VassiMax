#SHINY PROJECT : Maxime VASSILIEV & Benjamin NIEL

###########
#LIBRARIES
###########

library(shiny)
library(rsconnect)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(maps)
library(DT)
library(babynames)
library(viridis)
library(hrbrthemes)
library(plotly)
library(readxl)
library(fmsb)



#############
#SOURCE FILES
#############

#1. TAB 2 : "WHERE TO INVEST ?"

olympics<-read.csv("C:/Users/benie/OneDrive/Documents/EDHEC/M2/S1/R Programming/Shiny/medals.csv", header = TRUE, sep=";")
olympics$Latitude <- as.numeric(gsub(",", ".", olympics$Latitude, fixed = TRUE))
olympics$Longitude <- as.numeric(gsub(",", ".", olympics$Longitude, fixed = TRUE))
olympics<-olympics %>% group_by(Year, Country_name) %>% summarize(Latitude = mean(Latitude), Longitude = mean(Longitude), Total = sum(Total))

development1 <- read.csv("C:/Users/benie/OneDrive/Documents/EDHEC/M2/S1/R Programming/Shiny/dev_countries1.csv", header = TRUE, sep =';')
development <- development1 %>%
  select(Country, Latitude, Longitude, Population,GDP....per.capita.)
colnames(development)=c("Country","Latitude","Longitude","Population","GDP")

#2. TAB 3 : "WICH SPORT ?"

spider_chart <- read_excel("C:/Users/benie/OneDrive/Documents/EDHEC/M2/S1/R Programming/Shiny/DATA_GraphSpider.xlsx")

#3. TAB 4 : "WHICH MARKETING STRATEGY ?"

olympic <- read_excel("C:/Users/benie/OneDrive/Documents/EDHEC/M2/S1/R Programming/Shiny/olympic.xlsx")

#4. TAB 5 : "PARIS 2024"

tourism<-read.csv("C:/Users/benie/OneDrive/Documents/EDHEC/M2/S1/R Programming/Shiny/Tourism.csv", header = TRUE, sep=";")





##########
#SHINY APP
##########

#1. DEFINING UI

ui <- fluidPage(
  
tags$head(tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700'); body {background-color: #C0D6DF;}"))),
  
navbarPage("Decathlon Strategy",
                navbarMenu("WHERE TO INVEST ?",
                      tabPanel("Richest countries", 
                          sidebarPanel(id="sidebar",
                            sliderInput("inhabitants",
                                             h5("Minimum number of inhabitants :"),
                                             min = 7026,
                                             max = 1313973713,
                                             value = 60000000, animate=T)),
                          mainPanel(
                            leafletOutput("pib"))),

                      tabPanel("More sporty countries",
                          sidebarPanel(id="sidebar",
                            sliderInput("date",
                                             h5("Select a Year :"),
                                             min = 1980,
                                             max = 2012,
                                             step = 4, #De 4 ans en 4 ans
                                             sep='', #Pas de séparateur pour les milliers
                                             value = 2000, animate=T)),
                          mainPanel(
                            leafletOutput("medals")))),
           
                tabPanel("ON WHICH SPORT ?",
                      sidebarPanel(id="sidebar",
                         selectInput("year",
                                     label = h5("Select Year of Olympics"), 
                                     choices = unique(spider_chart$Year),
                                     selected='1980')),
                        
                      mainPanel(
                         uiOutput("countryOutput"),
                         plotOutput("spiderchart"))),
           
                tabPanel("WHICH MARKETING STRATEGY ?",
                      sidebarPanel(id="sidebar",
                         
                         selectInput("country", 
                                     label = h5("Select a Country"), 
                                     choices = unique(olympic$Country),
                                     selected='USA')),
                      mainPanel(
                         plotOutput("Gendergraph"))),
           
                tabPanel("FOCUS ON PARIS 2024 ?",
                      sidebarPanel(id="sidebar",
                         checkboxGroupInput(
                                     inputId="box",
                                     label=h5("Choose countries"),
                                     choices=c('Australia', 'China', 'United Kingdom', 'United States', 'Greece'), 
                                     selected='China')),
                      mainPanel(
                         plotOutput("olympic_games")))
    ),
)

#2. DEFINING SERVER

server <- function(input, output) {
  
  #2.1 TAB 2 : "WHERE TO INVEST ?"
  
  #2.1.1 TAB "Richest countries"
  
  output$pib <- renderLeaflet({
    pib <- development %>%
      filter(Population >= input$inhabitants)
    
    couleurs <- colorNumeric("YlOrRd", pib$GDP, n=22)
    
    map <- leaflet (pib) %>%
      addTiles() %>%
      #Format des cercles et fenêtres pop-up lorsqu'on clique dessus
      addCircleMarkers(lng = pib$Longitude, 
                       lat = pib$Latitude, 
                       fillOpacity = 10,
                       radius = ~ sqrt(Population * 0.000001), #Taille des cercles
                       popup = ~paste(pib$Country, #Affichage fenêtre pop-up au clic
                                      "in 2018 :",
                                      br(),
                                      "GDP/capita :",
                                      prettyNum(pib$GDP, big.mark = ","), #Affichage du PIB avec une ',' en séparateur des milliers
                                      "$",
                                      br(),
                                      "Population :",
                                      prettyNum(pib$Population, big.mark = ",")), #Affichage de la population avec une ',' en séparateur des milliers
                       color= ~couleurs(GDP))
  })
  
  #2.1.2 TAB "More sporty countries"
  
  output$medals <- renderLeaflet({
    medals<- olympics %>%
      filter(Year >= input$date) %>%
      group_by(Country_name) %>% summarize(Latitude = mean(Latitude), Longitude = mean(Longitude), Total = sum(Total))
    
    couleurs <- colorNumeric("YlOrRd", medals$Total, n=22)
    
    map <- leaflet (medals) %>%
      addTiles() %>%
      #Format des cercles et fenêtres pop-up lorsqu'on clique dessus
      addCircleMarkers(lng = medals$Longitude, 
                       lat = medals$Latitude, 
                       fillOpacity = 10,
                       radius = medals$Total/(2050-input$date), #Comme c'est une somme cumulée, on adapte l'échelle en fonction de la date sélectionnée pour plus de lisibilité
                       popup = ~paste("Number of medals for ",
                                      medals$Country_name, #Affichage fenêtre pop-up au clic
                                      "since",
                                      input$date,
                                      ":",
                                      br(),
                                      prettyNum(medals$Total, big.mark = ",")), #Affichage du PIB avec une ',' en séparateur des milliers
                       color= ~couleurs(medals$Total))
    
  })
  
  
  
  #2.2 TAB 3 : "WICH SPORT ?"
  
  filteredForCountry <- reactive({
    spider_chart %>%
      filter(Year == input$year) %>%
      select(Country)
  })
  #filter data to avoid country which didn't attend the OG
  
  output$countryOutput <- renderUI({
    df <- filteredForCountry()
    
    if (!is.null(df)) {
      selectInput("countryInput",
                  label = h5("Choose the country to represent"),
                  choices = unique(df$Country))
    }
  })
  
  output$spiderchart <- renderPlot({
    
    t=c()
    for(i in 1:2753)
      
      if(spider_chart$Country[i] == input$countryInput & spider_chart$Year[i]== input$year)
        t[i]=spider_chart$Total[i]
      t <- t[!is.na(t)]
      
      n=c()
      for(i in 1:2753)
        
        if(spider_chart$Country[i]== input$countryInput & spider_chart$Year[i]== input$year)
          n[i]=spider_chart$Sport[i]
      n <- n[!is.na(n)]
      
      # Create data
      
      data <- as.data.frame(matrix(t , ncol=length(n)))
      colnames(data) <- n
      
      # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
      data <- rbind(rep(100,length(n)) , rep(0,length(n)) , data)
      if(ncol(data) >= 3) {
        fig <- radarchart(data,axistype=1,
                          pcol=rgb(0.2,0.5,0.5,0.9) , 
                          pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
                          cglcol="grey", cglty=1, axislabcol="grey", 
                          caxislabels=seq(0,100,10), cglwd=0.8,vlcex=0.8 )
      }
      else{
        fig <- plot(x=0,y=0 ,xaxt="n", yaxt="n", type="n", 
                    main='The Country selected is not relevant for our study', 
                    sub='Because the medals are won in one or two different disciplines maximum')}
      
  })
  
  
  
  #2.3 TAB 4 : "WHICH MARKETING STRATEGY ?"
  
  FilteredData <- reactive({
    olympic %>%
      filter(Country == input$country) })
  
  # Plot
  
  output$Gendergraph <- renderPlot({
    p <- ggplot(FilteredData(), aes(x=Year, y=Medals, fill=Gender)) + 
      geom_area()+
      ggtitle("Evolution of the distribution of medals per gender in Olympic Games")+
      theme_dark()+
      theme(plot.background = element_rect(fill = "#C0D6DF"))
    p
  })
  
  
  
  #2.4 TAB 5 : "PARIS 2024 ?"
  
  output$olympic_games<-renderPlot({
    tourism_new <- tourism %>%
      filter(Country %in% input$box)
    tourism_new$Year<-factor(tourism_new$Year, levels = c("Y-2", "Y-1", "Y", "Y+1", "Y+2"))
    validate(need(input$box != "", "Please select a country"))
    ggplot(tourism_new, 
           aes(x=Year, 
               y=Value,
               colour=Country,
               group=Country,
               plot_bgcolor='rgb(192, 214, 223)'))+
      ggtitle("Evolution of tourism for countries hosting the Olympic Games")+
      theme_dark()+
      theme(plot.background = element_rect(fill = "#C0D6DF"))+
      geom_line()
  })
}

#3. RUN THE APPLICATION

shinyApp(server = server, ui = ui)
