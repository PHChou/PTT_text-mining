mydata <- read.csv("www/art.data.csv")






library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("ddata"),
      textOutput("output")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #source("helper.R")
  output$ddata <- renderTable({head(mydata)})
  #output$head_file1 <- renderTable({ head(file_1_csv()) })
  #output$output<- renderText({jiebaR::segment("每天都要吃早餐", cc)} )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

