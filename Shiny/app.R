mydata <- read.csv("www/articles data.csv")

source("helper.R")


library(shiny)
#library(magrittr)


ui <- fluidPage(
  
  titlePanel("家教學生需求分析 The Analysis of Students' Tutoring Demand"),
  
  tabsetPanel(
    tabPanel('Help Text', 
             h4("目的"),
             h4("藉由"
                ,a(href ='https://www.ptt.cc/bbs/HomeTeach/index.html',"PTT家教版"),
                "發表的文章，分析台灣學生家教需求，進而找出可分類不同家教學生族群的特徵。"),
             
             h4("材料"),
             h4('PTT家教版2015/05/01至2017/05/01所發表徵求家教老師或交換才藝之文章。'),
             
             h4("方法"),
             h4('利用R進行網路爬蟲抓取PTT家教版文章後，使用',
                a(href = 'https://github.com/qinwf/jiebaR','juebaR套件'),
                '將各個完整的文章進行斷詞後，以',
                a(href = 'https://cran.r-project.org/web/packages/wordcloud2/wordcloud2.pdf','文字雲'),
                '呈現PTT家教版中常出現的字詞與頻率，以做為分類家教學生族群特徵的參考。'),
             
             br(),
             br(),
             
             h4('Aim'),
             h4("This analysis aim to detect the characteristics that can classify the students' tutoring demand from"
                ,a(href ='https://www.ptt.cc/bbs/HomeTeach/index.html',"PTT Tutor"),'.' ),
             h4('Material'),
             h4('The articles posted at PTT Tutor for tutoring demand or exchange skills from 2015/05/01 to 2017/05/01.'),
             h4("Method"),
             h4('By using R software, the articles were crawled from PTT Tutor firstly. After that, each article was segmented to vocabularies by ',
                a(href = 'https://github.com/qinwf/jiebaR','juebaR package'),
                '. To be the references of classification, the most used vocabularies and its frequencies are presented in the ',
                a(href = 'https://cran.r-project.org/web/packages/wordcloud2/wordcloud2.pdf','wordcloud'),
                ' and frequency table.')),
    tabPanel('Word Cloud & Frequency Table',sidebarLayout(
      sidebarPanel(
        h4(strong("1. 選擇欲分析的文章種類")),
        h4( checkboxGroupInput("art.type", "Choose the type of article.",
                               choiceNames =
                                 list("家教 Needed tutoring", "交換 Skill exchange" ),
                               choiceValues =
                                 list("家教", "交換" ),
                               selected = "家教"
        )),
        br(),
        h4(strong("2. 選擇學生性別")),
        h4( selectInput("gender.type", "Choose the gender of student.",
                        c("男 Male" = "boy",
                          "女 Female" = "girl",
                          "男&女 Both" = "both"),
                        selected = "both")),
        
        br(),
        h4(strong("3. 選擇欲分析之項目")),
        h4( selectInput("data.type", "Choose the object of analysis",
                        c("科目 Subject" = "subject.type",
                          "地點 Location" = "location.type"
                        ))),
        br(),
        h4( strong("4. 輸入分析中欲排除之常用字詞")),
        h4( strong("Input the commonly used vocabularies which are excluded from the analysis.")),
        h4( textInput("exclude.1",label = NA,value = '的')),
        h4( textInput("exclude.2",label = NA,value = '-')),
        h4( textInput("exclude.3",label = NA,value = '-')),
        h4( textInput("exclude.4",label = NA,value = '-')),
        h4( textInput("exclude.5",label = NA,value = '-')),
        h4( textInput("exclude.6",label = NA,value = "-"))
        ,width = 3 ),
      
      
      mainPanel(
        
        
        wordcloud2Output("f.word.cloud",width = "100%", height = "600px"),
        tableOutput('ddata'),
        h4( textOutput("f.txt")),
        div( tableOutput("f.data") , style = 'font-size:130%')
        
        ,width=9) 
    )
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
  
  data.type1<- reactive ({ switch(input$data.type, "subject.type"="科目",'location.type'='地點')})
  data.type2<- reactive ({ switch(input$data.type, "subject.type"="上課時間",'location.type'='科目')})
  
  excludes  <- reactive ({  c(0:10,LETTERS,letters,'地點','附近',
                             "上","科目",'4.','的','及',"與","二",'你','他',
                             '為主','中','科','請','安全','注意','自身','國',
                             '站','財物',"或",'可','的','近','路','中',"換","我",
                             '等','區',input$exclude.1,input$exclude.2,input$exclude.3
                             ,input$exclude.4,input$exclude.5,input$exclude.6) })
  
  
  word.plot <- reactive({ super_jieba_cloud(rawdata  = used.data(),
                                            filters1 =  data.type1() , filters2= data.type2() , col1 = 3,  
                                            excludeword =  excludes() ,
                                            output_select = "cloud"  )  })
  
  output$f.word.cloud <- renderWordcloud2( { word.plot() }) 
  
  word.table <- reactive({ w.table <- super_jieba_cloud(rawdata  = used.data(),
                                                        filters1 =  data.type1() , filters2= data.type2() , col1 = 3,  
                                                        excludeword =  excludes() ,
                                                        output_select = "file"  )[1:10,]
                           w.table <- t(w.table)
                           rownames( w.table ) <- c("詞彙",'出現次數')
                           w.table  })
  
  output$f.data  <- renderTable({ word.table() } ,rownames = T, colnames = F)
  output$f.txt   <- renderText({ paste("共有",nrow(used.data()),'篇文章符合篩選條件')})
  #data.type1 <- reactive 

  
  
  
  #output$ddata <- renderTable({ head(  used.data() )})

}

# Run the application 
shinyApp(ui = ui, server = server)


