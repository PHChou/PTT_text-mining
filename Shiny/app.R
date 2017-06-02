mydata <- read.csv("www/art.data.csv")

source("helper.R")



library(magrittr)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      checkboxGroupInput("art.type", "文章種類 (可複選)",
                         choiceNames =
                           list("家教", "上台" , "交換" ),
                         choiceValues =
                           list("家教", "上台" , "交換" ),
                         selected = "家教"
      ),
      selectInput("gender.type", "性別",
                  c("男" = "boy",
                    "女" = "girl",
                    "男&女" = "both"),
                  selected = "both"),
      
      selectInput("data.type", "資料類型",
                  c("科目" = "subject.type",
                    "地點" = "location.type"
                  )),
      h5(strong("排除關鍵字")),
      textInput("file2_search1",label = NA,value = '的'),
      textInput("file2_search2",label = NA,value = NA),
      textInput("file2_search3",label = NA,value = NA),
      textInput("file2_search4",label = NA,value = NA),
      textInput("file2_search5",label = NA,value = NA),
      textInput("file2_search6",label = NA,value = NA)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("f.word.colud"),
      tableOutput('ddata'),
      tableOutput("f.data"),
      textOutput("f.txt")
    )
  )
)


server <- function(input, output) {
  
  gender <- reactive({  switch( as.character(input$gender.type), "boy" = c("兄",'男','弟','哥'),
                                                             "girl" = c('女','妹','姐','姊'),
                                        'both' = c("兄",'男','弟','哥','女','妹','姐','姊')) })
  
  #output$ddata <- renderTable({ super_grepl( c( input$art.type ),rdata= mydata ,col = 1)  })
  
  
    ##output$output<- renderText({jiebaR::segment("每天都要吃早餐", cc)} )
  #output$f.txt <- renderText(gender())
  used.data <- reactive({ super_grepl( c( input$art.type ),rdata= mydata ,col = 1) %>%
                          super_grepl( gender(),rdata= . ,col = 3)  })
  output$ddata <- renderTable({ head(  used.data() )})

}

# Run the application 
shinyApp(ui = ui, server = server)



