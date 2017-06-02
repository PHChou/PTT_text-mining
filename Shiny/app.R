mydata <- read.csv("www/art.data.csv")

source("helper.R")


library(shiny)
#library(magrittr)


ui <- fluidPage(
  
  titlePanel("Old Faithful Geyser Data"),
  
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
      
      selectInput("data.typee", "資料類型",
                  c("科目" = "subject.type",
                    "地點" = "location.type"
                  )),
      h5(strong("排除關鍵字")),
      textInput("exclude.1",label = NA,value = '的'),
      textInput("exclude.2",label = NA,value = NA),
      textInput("exclude.3",label = NA,value = NA),
      textInput("exclude.4",label = NA,value = NA),
      textInput("exclude.5",label = NA,value = NA),
      textInput("exclude.6",label = NA,value = NA)
    ),
    

    mainPanel(
      wordcloud2Output("f.word.cloud"),
      tableOutput('ddata'),
      tableOutput("f.data"),
      textOutput("output")
    )
  )
)


server <- function(input, output) {
  
  gender <- reactive({  switch( as.character(input$gender.type), "boy" = c("兄",'男','弟','哥'),
                                                             "girl" = c('女','妹','姐','姊'),
                                        'both' = c("兄",'男','弟','哥','女','妹','姐','姊')) })
  
  #output$ddata <- renderTable({ head(super_grepl( c( input$art.type ),rdata= mydata ,col = 1))  })
  
  
  #output$output<- renderText({jiebaR::segment("每天都要吃早餐", cc)} )
  #output$f.txt <- renderText(gender())
  used.data <- reactive({ super_grepl( gender(),rdata= super_grepl( c( input$art.type ),rdata= mydata ,col = 1) 
                                       ,col = 3)  })
  #used.data <- reactive({ super_grepl( c( input$art.type ),rdata= mydata ,col = 1) %>%
  #                        super_grepl( gender(),rdata= . ,col = 3)  })
  #不知道為何shiny沒辦法library(magrittr)
  word.plot <- reactive({ super_jieba_cloud(rawdata  = used.data(),
                                            filters1 = c('地點'), filters2= c('科目'), col1 = 3,  
                                            stopword = c( 0:10,LETTERS,letters,'地點','附近',
                                                          "上","科目",'4.','的','及',"與","二",
                                                          '為主','中','科','請','安全','注意','自身',
                                                          '站','財物',"或",'可','的','近','路','中',
                                                          '等','區') ,
                                            output_select = "cloud"  )  })
  
  output$f.word.cloud <- renderWordcloud2( { word.plot() }) 
  
  #data.type1 <- reactive 

  
  
  
  #output$ddata <- renderTable({ head(  used.data() )})

}

# Run the application 
shinyApp(ui = ui, server = server)



