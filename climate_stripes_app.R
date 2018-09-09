#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This was written by Mark Higgins at EUMETSAT 
# and is based / insired by the Wamring Stripes by Ed Hawkins
# https://www.climate-lab-book.ac.uk/2018/warming-stripes/
#
# it uses the World Bank API data and as is a teaching tool 
# if your going to make statements about the climate you'll need to look more into the data 
# and what they really are!
#
#

library(shiny)
library(countrycode)
library(RColorBrewer)
library(jsonlite)
library(curl)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Explore annual mean temperature"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(label="Country",inputId="countryName",
                     choices=countrycode::codelist["country.name.en"], selected="Germany"),
         radioButtons(label="Plot linear regression line?",inputId="lineTF",c("Yes"=TRUE,"No"=FALSE)),
         radioButtons(label="Plot data points?",inputId="pointsTF",c("Yes"=TRUE,"No"=FALSE)),
         sliderInput("cols", "Number of bins in colour scale",min=3,max=11,value = 11),
         sliderInput("seed", "Random number seed for model",min=1,max=50,value = 42),
         sliderInput("binSize", "Bin size for histogram (C)",min=0.2,max=1,value = 0.5)
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Data" ,plotOutput("countryPlot"),htmlOutput("summary")),
          tabPanel("Model",plotOutput("modelPlot"),htmlOutput("summaryModel")),
          tabPanel("Compare Data & Model",plotOutput("histPlot")),
          tabPanel("Compare Decades",plotOutput("boxPlot"),plotOutput("boxPlotModel"))
        )
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  dataTreact <- reactive({ # get the Temp data
    # get country
    iso3 <- countrycode::countrycode(input$countryName,"country.name.en","iso3c")
    # get URL 
    urlWB <- paste(
      "http://climatedataapi.worldbank.org/climateweb/rest/v1/country/cru/tas/year/",iso3,".json", sep=""
    )
    # get data
  fromJSON(urlWB)
  })
  
  minT <- reactive( min(dataTreact()$data) )
  maxT <- reactive( max(dataTreact()$data) )
  minY <- reactive( min(dataTreact()$year) )
  maxY <- reactive( max(dataTreact()$year) )
  rangeT <- reactive( maxT()-minT() )
  
  # set up colour scale 
  nColBins <- reactive( input$cols ) 
  binWidth <- reactive( rangeT()/nColBins() )
  palette <- reactive(rev(brewer.pal(nColBins(),"RdBu")) )

  binCol <- function (Temp){ palette()[floor((Temp - minT())/binWidth())+1] }
  
  rectPlot <-  function(df.row){ # function to plot stripes
    y<-df.row["year"]
    lineCol<-binCol(df.row["data"])
    rect(y-0.5,minT(),y+0.5,maxT(),col=lineCol,border=NA,lwd=0)
  }
  
  modelT <- reactive ({
    dataT <- dataTreact()
    set.seed(input$seed)
    modelTemp <- rnorm(
                    n=nrow(dataT), 
                    mean=mean(unlist(dataT["data"])), 
                    sd=sd(unlist(dataT["data"]))
                    )
  
    dataT["data"] <- modelTemp
    dataT
  })
  
  output$summary <- renderUI({ # and set up all varliables used later ... 
    dataT <- dataTreact()
    txt.1 <- paste(
              "<tr><td>Slope:&nbsp </td>",
              "<td>",signif(coef(line(dataT))[2]*10,2)," C / Decade","</td></tr>"
              ,sep="")
    txt.2 <- paste(
              "<tr><td>Min:&nbsp </td><td>",signif(min(unlist(dataT["data"])),3),"</td></tr>","",
              "<tr><td>Mean:&nbsp </td><td>",signif(mean(unlist(dataT["data"])),3),"</td></tr>","",
              "<tr><td>Max:&nbsp </td><td>",signif(max(unlist(dataT["data"])),3),"</td></tr>","",
              "<tr><td>SD:&nbsp </td><td>",signif(sd(unlist(dataT["data"])),3),"</td></tr>","",
              "<tr><td>Q1:&nbsp </td><td>",signif(quantile(unlist(dataT["data"]))[2],3),"</td></tr>","",
              "<tr><td>Q3:&nbsp </td><td>",signif(quantile(unlist(dataT["data"]))[4],3),"</td></tr>",""
      ,sep="")
    
    HTML(paste("<table>",txt.1,txt.2,"</table>",sep = ''))
  })

  output$summaryModel <- renderUI({ # and set up all varliables used later ... 
    dataT <- modelT()
    txt.1 <- paste(
      "<tr><td>Slope:&nbsp </td>",
      "<td>",signif(coef(line(dataT))[2]*10,2)," C / Decade","</td></tr>"
      ,sep="")
    txt.2 <- paste(
      "<tr><td>Min:&nbsp </td><td>",signif(min(unlist(dataT["data"])),3),"</td></tr>","",
      "<tr><td>Mean:&nbsp </td><td>",signif(mean(unlist(dataT["data"])),3),"</td></tr>","",
      "<tr><td>Max:&nbsp </td><td>",signif(max(unlist(dataT["data"])),3),"</td></tr>","",
      "<tr><td>SD:&nbsp </td><td>",signif(sd(unlist(dataT["data"])),3),"</td></tr>","",
      "<tr><td>Q1:&nbsp </td><td>",signif(quantile(unlist(dataT["data"]))[2],3),"</td></tr>","",
      "<tr><td>Q3:&nbsp </td><td>",signif(quantile(unlist(dataT["data"]))[4],3),"</td></tr>",""
      ,sep="")
    
    HTML(paste("<table>",txt.1,txt.2,"</table>",sep = ''))
  })
  
    
  output$countryPlot <- renderPlot({
    dataT <- dataTreact()
    plot(NULL, # outline plot
         xlim=c(minY(),maxY()), 
         ylim=c(minT(),maxT()), 
         ylab="Temperatue (C)", xlab="Year",
         main="Observred Temperature (C)")
    apply(dataT,1,rectPlot) # plot stripes
    if (input$pointsTF) points(dataT,pch=21,bg="white")
    if (input$lineTF) abline(line(dataT),col="white")
  })
  
  output$modelPlot <- renderPlot({
    dataT <- modelT()
    plot(NULL, 
         xlim=c(minY(),maxY()), 
         ylim=c(minT(),maxT()), ylab="Temperatue (C)", xlab="Year",
         main="Modelled Temperature (C)")
    apply(dataT,1,rectPlot)
    if (input$pointsTF) points(dataT,pch=21,bg="white")
    if (input$lineTF) abline(line(dataT),col="white")
  })
  
  output$histPlot <- renderPlot({
    low <- floor(min(dataTreact()["data"],modelT()["data"]))
    high <- ceiling(max(dataTreact()["data"],modelT()["data"]))
    edges <- seq(low-0.5, high+0.5, by=input$binSize)
    binMax=ceiling(nrow(modelT())*4/length(edges)/10)*10 # 1/3 of sample but next higest 10
    hist(unlist(modelT()["data"]),breaks=edges,col=rgb(0,0,1,0.5),ylim=c(0,binMax),
         xlab="model (blue) and data (green)",main="Frequency of Temperature")
    hist(unlist(dataTreact()["data"]),breaks=edges,col=rgb(0,1,0,0.5),add=T)
  })
  
  output$boxPlot <- renderPlot({
    dataT <- dataTreact()
    dataT["decade"] <- floor(dataT["year"]/10)*10
    boxplot(data~decade,dataT,
            main="Observation Decadal variability",
            xlab="Temperature (C)",ylab="decade"
            )
  })
  output$boxPlotModel <- renderPlot({
    dataT <- modelT()
    dataT["decade"] <- floor(dataT["year"]/10)*10
    boxplot(data~decade,dataT,
            main="Model Decadal variability",
            xlab="Temperature (C)",ylab="decade"
    )
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

