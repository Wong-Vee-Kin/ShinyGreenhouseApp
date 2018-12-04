library(shiny)
#library(shinythemes)
library(googleVis)
library(stringr)
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(shinydashboard)

#prepare DF for lineplot and scatterplot
myDF <- read.csv('data.csv')
names(myDF)[5:length(names(myDF))]<-gsub("X","",names(myDF)[5:length(names(myDF))])
#replicate DF for geomap, so that countries that have NA don't get published
myDF2 = myDF
myDF[is.na(myDF)]<-0
#create the year list
col_name<-as.data.frame(names(myDF))
year<-col_name[5:nrow(col_name),]
# transpose the data frame to fit into time series chart
#co2_t<-setNames(data.frame(t(co2[,3:57])), co2[,2])
co2_t<-melt(myDF,id=c("Country.Name","Country.Code", "Indicator.Name","Indicator.Code"), na.rm=T)
colnames(co2_t)[colnames(co2_t)=="variable"]<-"Year"
colnames(co2_t)[colnames(co2_t)=="value"]<-"Value"
co2_t$Year<-as.numeric(levels(co2_t$Year)[co2_t$Year])

#create gdp data for scatterplot
gdpDF <- read.csv('gdpdata.csv')
gdpDF[is.na(gdpDF)]<-0
names(gdpDF)[5:length(names(gdpDF))]<-gsub("X","",names(gdpDF)[5:length(names(gdpDF))])
gdp_t<-melt(gdpDF,id=c("Country.Name","Country.Code", "Indicator.Name","Indicator.Code"), na.rm=T)
colnames(gdp_t)[colnames(gdp_t)=="variable"]<-"Year"
colnames(gdp_t)[colnames(gdp_t)=="value"]<-"Value"
gdp_t$Year<-as.numeric(levels(gdp_t$Year)[gdp_t$Year])

body <- dashboardBody(
  fluidRow(
    tabBox(
      width=20,
      title = "Global Greenhouse Effect",
      id = "tabset1", 
      tabPanel("Global CO2 emission", h3(textOutput("year")),htmlOutput("geoplot")),
      tabPanel("Comparing CO2 emissions for selected countries", h3(textOutput("range")),plotOutput("lineplot")),
      tabPanel("GDP vs CO2", h3(textOutput("range2")),plotOutput("plot"))
      )
  )
)


ui1<-dashboardPage(
    dashboardHeader(title = "Input Panel"),
    dashboardSidebar(selectInput("country","Country :",choices=myDF$Country.Name,multiple=TRUE),
                     selectInput("year","Year: ",choices=year),
                     sliderInput("year_range",label = "Please select the range of years",min = 1960,max = 2014,value=c(1960,1975),step = 1, sep="")),
    body
)

ui2<-fluidPage(
  headerPanel("Global Greenhouse Effect By Year"),
  
  sidebarPanel(
    selectInput("variable","Select Year:",
                names(myDF)[5:62])
  )
)

server<-function(input, output){

  #create new DF for geomap to remove countries with NA from list
  #so that it won't show up as having 'good' emission in map
  newDF<-reactive({myDF2[!is.na(myDF2[input$year]),]})
  
  myYear <- reactive({input$year})
  output$year <- renderText({paste("Countries CO2 emission in ", myYear())})
  range<-reactive({input$year_range[1]})
  range2<-reactive({input$year_range[2]})
  co2_filtered<-reactive({co2_t %>% filter(Country.Name %in% input$country,between(Year,input$year_range[1],input$year_range[2]))})
  gdp_filtered<-reactive({gdp_t %>% filter(Country.Name %in% input$country,between(Year,input$year_range[1],input$year_range[2]))})

  #begin of all server outputs
  output$range<-renderText({paste("Countries CO2 emission in between",range(),"and",range2())})  
  output$range2<-renderText({paste("GDP vs CO2 emission for selected countries between",range(),"and",range2())})
  
  output$geoplot<-renderGvis({
    gvisGeoChart(newDF(), locationvar='Country.Name',options=list(dataMode="regions",width=700, height=400, colors="['green','orange','red']"), colorvar=input$year)
  })
  
  output$lineplot<-renderPlot({
    ggplot(co2_filtered(),aes(x=Year,y=Value))+ylab("CO2 emission (metric tons per capita)")+geom_line(aes(colour=Country.Name), size=2)
  })
  
  output$plot<-renderPlot({
    #ntable<-as.data.frame(cbind(as.character(co2_filtered()$Country.Name), format(co2_filtered()$Value, digits=2), format(gdp_filtered()$Value/1e9, digits=2)))
    ntable<-as.data.frame(cbind(as.character(co2_filtered()$Country.Name), co2_filtered()$Value, round(gdp_filtered()$Value/1e9, digits=2)))
    names(ntable)<-c("Country", "CO2", "GDP")
    ntable$CO2 <-as.numeric(levels(ntable$CO2))[ntable$CO2]
    ntable$GDP <-as.numeric(levels(ntable$GDP))[ntable$GDP]
    max=max(ntable$CO2)
    min=min(ntable$CO2)
    #yticks=format(seq(min, max, length=10), digits=2)
    yticks=round(seq(min, max, length=10), digits=2)
    ggplot(ntable, aes(x=GDP, y=CO2, color=Country))+geom_point(size=3)+xlab("GDP(USD billion)")+ylab("CO2 emission (metric tons per capita)")+ggtitle("CO2 emission vs. GDP")+scale_y_continuous(breaks=yticks)+theme(text=element_text(size=15),axis.text.x  = element_text(angle=90))
  })
    
}

shinyApp(ui1, server)