#SHINY PROJECT : Maxime VASSILIEV 54885 & Benjamin NIEL 56570

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
library(viridis)
library(hrbrthemes)
library(plotly)
library(readxl)
library(fmsb)
library(shinythemes)



#############
#SOURCE FILES
#############

#1. TAB 2 : "WHERE TO INVEST ?"

olympics<-read.csv("medals.csv", header = TRUE, sep=";")
olympics$Latitude <- as.numeric(gsub(",", ".", olympics$Latitude, fixed = TRUE))
olympics$Longitude <- as.numeric(gsub(",", ".", olympics$Longitude, fixed = TRUE))
olympics<-olympics %>% group_by(Year, Country_name) %>% summarize(Latitude = mean(Latitude), Longitude = mean(Longitude), Total = sum(Total))

development1 <- read.csv("dev_countries1.csv", header = TRUE, sep =';')
development <- development1 %>%
    select(Country, Latitude, Longitude, Population,GDP....per.capita.)
colnames(development)=c("Country","Latitude","Longitude","Population","GDP")

#2. TAB 3 : "WICH SPORT ?"

spider_chart <- read_excel("DATA_GraphSpider.xlsx")

#3. TAB 4 : "WHICH MARKETING STRATEGY ?"

olympic <- read_excel("olympic.xlsx")

#4. TAB 5 : "PARIS 2024"

tourism<-read.csv("Tourism.csv", header = TRUE, sep=";")





##########
#SHINY APP
##########

#1. DEFINING UI

ui <- fluidPage(
    
    theme = shinytheme("sandstone"),
    navbarPage( theme = shinytheme("sandstone"),
                "Decathlon Strategy",
                
                tabPanel("General Informations",
                         h5("Through this dashboard we are trying to answer the question : Which international marketing strategy Decathlon should design ?"),
                         br(),
                         "- First, which countries should the company establish in ?
                         Through 2 maps, we highlight countries
                         with a rich population, more likely to consume, and with a sporty population, more likely to be interested in sport equipements.",
                         br(),
                         br(),
                         "- Second, in these countries, which sport shoult Decathlon focus on ?
                         Each country in the world have prevailing sports and the spider chart permit to understand it",
                         br(),
                         br(),
                         "- Third, which image should Decathlon deploy to attract consumers ?
                         This strategy will differ from a country to another. We choose to focus on gender image to predict which people will be more interested, comparing the number of medals of men and women in each country.",
                         br(),
                         br(),
                         "- Finally, Decathlon as a French company may wonder : are the 2024 Paris Olympic Games a good way to promote the brand ?
                         We study impact of the Olympic Games on tourism for each hosting countries since 1996 to understand which significance have the Olympic Games in the world.",
                         br(),
                         br(),
                         a("OLYMPIC GAMES STATS", href="https://www.olympic.org/fr/jeux-olympiques"),
                         br(),
                         br(),
                         "NIEL Benjamin (56570) & VASSILIEV Maxime (54885)"),
                
                
                navbarMenu("WHERE TO INVEST ?",
                           tabPanel("Richest countries", 
                                    sidebarPanel(id="sidebar",
                                                 sliderInput("inhabitants",
                                                             h5("Minimum number of inhabitants :"),
                                                             min = 7026,
                                                             max = 1313973713,
                                                             value = 60000000, animate=T)),
                                    mainPanel(
                                        leafletOutput("pib"),
                                        br(),
                                        "You can click on the country circles to have more details.")),
                           
                           tabPanel("More sporty countries",
                                    sidebarPanel(id="sidebar",
                                                 sliderInput("date",
                                                             h5("Select a Year :"),
                                                             min = 1980,
                                                             max = 2012,
                                                             step = 4, #De 4 ans en 4 ans
                                                             sep='', #Pas de s?parateur pour les milliers
                                                             value = 2000, animate=T)),
                                    mainPanel(
                                        leafletOutput("medals"),
                                        br(),
                                        "You can click on the country circles to have more details."))),
                
                tabPanel("ON WHICH SPORT ?",
                         sidebarPanel(id="sidebar",
                                      selectInput("year",
                                                  label = h5("Select Year of Olympics"), 
                                                  choices = unique(spider_chart$Year),
                                                  selected='')),
                         
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
                             plotOutput("olympic_games"),
                             br(),
                             "Y stands for the Year the country hosted the Olympic Games (ex: 2008 for China).
                         The objective is to compare the evolution of tourism one year (Y+1) and two years (Y+2) after the event for each country"))
    )
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
            #Format des cercles et fen?tres pop-up lorsqu'on clique dessus
            addCircleMarkers(lng = pib$Longitude, 
                             lat = pib$Latitude, 
                             fillOpacity = 10,
                             radius = ~ sqrt(Population * 0.000001), #Taille des cercles
                             popup = ~paste(pib$Country, #Affichage fen?tre pop-up au clic
                                            "in 2018 :",
                                            br(),
                                            "GDP/capita :",
                                            prettyNum(pib$GDP, big.mark = ","), #Affichage du PIB avec une ',' en s?parateur des milliers
                                            "$",
                                            br(),
                                            "Population :",
                                            prettyNum(pib$Population, big.mark = ",")), #Affichage de la population avec une ',' en s?parateur des milliers
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
            #Format des cercles et fen?tres pop-up lorsqu'on clique dessus
            addCircleMarkers(lng = medals$Longitude, 
                             lat = medals$Latitude, 
                             fillOpacity = 10,
                             radius = medals$Total/(2050-input$date), #Comme c'est une somme cumul?e, on adapte l'?chelle en fonction de la date s?lectionn?e pour plus de lisibilit?
                             popup = ~paste("Number of medals for ",
                                            medals$Country_name, #Affichage fen?tre pop-up au clic
                                            "since",
                                            input$date,
                                            ":",
                                            br(),
                                            prettyNum(medals$Total, big.mark = ",")), #Affichage du PIB avec une ',' en s?parateur des milliers
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
                                  caxislabels=seq(0,100,10), cglwd=0.8,vlcex=0.8, title ='Number of medals per Sport and per Country' )
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
            theme_dark()
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
                   group=Country))+
            ggtitle("Evolution of tourism for countries hosting the Olympic Games")+
            theme_dark()+
            geom_line()
    })
}

#3. RUN THE APPLICATION

shinyApp(server = server, ui = ui)
