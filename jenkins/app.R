#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hydrologic Connection Estimation Assistant"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("b",
                        "estimated saturated thickness (ft):",
                        min = 1,
                        max = 8000,
                        value = 30),
            numericInput("k",
                         "estimated hydraulic conductivity (ft/d):",
                         min = 0,
                         max = 100000,
                         value = 1),
            numericInput("a",
                         "distance from well to stream (ft):",
                         min = 0,
                         max = 1000000000000,
                         value = 1000),
            sliderInput("S",
                         "Aquifer Specific Yield (dimensionless):",
                         min = 0,
                         max = 1,
                         value = 0.15)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      sdf<-365.25*40
      S<-input$S
      sdf/S
      a<-seq(0,50000,5000)
      T<-(a^2*S)/sdf
      plot(a,T, 
           type="l", 
           ylab="Aquifer Transmissivity (ft2/d)", 
           xlab="Distance from well to stream (ft)" )
      Tprime<-input$b*input$k
      aprime<-input$a
      points(aprime,Tprime, pch="+", col="green", cex=3)
      sdfprime<-(aprime^2*S)/Tprime
    })
    output$text<-renderText({
      sdfprime<-(input$a^2*input$S)/(input$b*input$k)
      paste0("Stream depletion factor for this well is ",
                                   sdfprime,
                                   " days. If sdf is less than 14610 days, the well is hydrologically connected to the stream, under the 28/40 criteria.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
