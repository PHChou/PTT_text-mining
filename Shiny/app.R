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
      
      selectInput("data.type", "資料類型",
                  c("科目" = "subject.type",
                    "地點" = "location.type"
                  )),
      h5(strong("排除關鍵字")),
      textInput("exclude.1",label = NA,value = '的'),
      textInput("exclude.2",label = NA,value = '-'),
      textInput("exclude.3",label = NA,value = '-'),
      textInput("exclude.4",label = NA,value = '-'),
      textInput("exclude.5",label = NA,value = '-'),
      textInput("exclude.6",label = NA,value = "-")
      ,width = 3 ),
    

    mainPanel(
      wordcloud2Output("f.word.cloud",width = "100%", height = "600px"),
      tableOutput('ddata'),
      tableOutput("f.data"),
      textOutput("output")
      ,width=9) 
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
  
  data.type1<- reactive ({ switch(input$data.type, "subject.type"="科目",'location.type'='地點')})
  data.type2<- reactive ({ switch(input$data.type, "subject.type"="上課時間",'location.type'='科目')})
  
  excludes  <- reactive ({  c(0:10,LETTERS,letters,'地點','附近',
                             "上","科目",'4.','的','及',"與","二",
                             '為主','中','科','請','安全','注意','自身',
                             '站','財物',"或",'可','的','近','路','中',
                             '等','區',input$exclude.1,input$exclude.2,input$exclude.3
                             ,input$exclude.4,input$exclude.5,input$exclude.6) })
  
  
  word.plot <- reactive({ super_jieba_cloud(rawdata  = used.data(),
                                            filters1 =  data.type1() , filters2= data.type2() , col1 = 3,  
                                            stopword =  excludes() ,
                                            output_select = "cloud"  )  })
  
  output$f.word.cloud <- renderWordcloud2( { word.plot() }) 
  
  word.table <- reactive({ w.table <- super_jieba_cloud(rawdata  = used.data(),
                                                        filters1 =  data.type1() , filters2= data.type2() , col1 = 3,  
                                                        stopword =  excludes() ,
                                                        output_select = "file"  )[1:10,]
                           colnames( w.table ) <- c("詞彙",'出現次數')
                           w.table  })
  
  output$f.data  <- renderTable({ word.table() } )
  
  #data.type1 <- reactive 

  
  
  
  #output$ddata <- renderTable({ head(  used.data() )})

}

# Run the application 
shinyApp(ui = ui, server = server)


