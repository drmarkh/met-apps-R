# This is my EMA app built in R Shiny
library(shiny)

DF_na = data.frame(integer = c(NA, 2:10), 
                   logical = c(NA, rep(TRUE, 9)), 
                   character = c(NA, LETTERS[1:9]),
                   factor = c(NA, factor(letters[1:9])),
                   date = c(NA, seq(from = Sys.Date(), by = "days", 
                                    length.out = 9)),
                   stringsAsFactors = FALSE)

DF_na$factor_ch = as.character(DF_na$factor)
DF_na$date_ch = c(NA, as.character(seq(from = Sys.Date(), by = "days", 
                                       length.out = 9)))

rhandsontable(DF_na, width = 550, height = 300)

# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("TITLE Here"), # Application title
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(sliderInput("bins","Number of bins:",min = 1,max = 50, value = 30)), 
      mainPanel(plotOutput("distPlot"))
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlot({ # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   editTable(DF_na, outdir="~/Documents/", outfilename="newDF")
}

# Run the application 
shinyApp(ui = ui, server = server)

