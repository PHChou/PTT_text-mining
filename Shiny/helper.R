#library(magrittr)
library(jiebaR)
library(wordcloud2)


super_grepl <- function ( filters, rdata, excludes = NA, col ){
  data   <- rdata[ ,col]
  used   <- which( grepl( paste( filters, collapse = "|" ), x= data) ==T ) 
  
  if ( is.na(excludes)[1] ){ data3 <- rdata[used,] } else {
    
    rdata2  <- rdata [used,] 
    data2   <- rdata2[ ,col]
    exclude.data <- which( grepl( paste( excludes, collapse = "|" ), x= data2) ==T )
    if( length( exclude.data )==0 ){data3 <- rdata2 } else {
      data3  <- rdata2[-exclude.data,]  }  
  } 
  
  data3  }


cc<- jiebaR:: worker() #for PC
#cc<- jiebaR:: worker(dict="jieba.dict.utf8",hmm="hmm_model.utf8",user="user.dict.utf8") # for shiny server 

super_jieba_cloud <- function ( rawdata, filters1, filters2, col1,  
                                excludeword = c( 0:10,LETTERS,letters) ,
                                output_select = 'cloud'){
  
  #先載入會用到的function 
  #用jieba抓filter1到filter2之間的資料並斷詞
  jieba_segment <- function (data, begin, end){  
    d3<-substr( data ,start = regexpr(begin,data)[1]
                ,stop = regexpr(end,data)[1]  ) 
    jiebaR::segment(d3, cc) }
  #刪掉rdata中有excludewords的，再輸出剩下的資料
  excludeword_data <- function ( excludewords, rdata, col ){ data <- rdata[,col]
  used <- which( grepl( paste( na.exclude(excludewords), collapse = "|" ), x= data) ==T)
  rdata[-used,] }
  
  #抓符合filter1,filter2的資料
  d1 <- super_grepl( filters = filters1, rdata = rawdata, col= col1 )
  d2 <- super_grepl( filters = filters2, rdata = d1, col= col1 )
  
  # d2 <- super_grepl( filters = filters1, rdata = rawdata, col= col1 ) %>% 
  # super_grepl( filters = filters2, rdata = ., col= col1 )
  # Shiny 的 library(magrittr) 不知道為何
  
  #斷詞
  jieba.word  <- lapply( d2[,3],jieba_segment,begin = filters1, end = filters2 )
  
  #table好詞，並且由多到少排序
  word.table  <- freq( unlist(jieba.word) )
  word.table2 <- word.table[ order( word.table$freq,decreasing = T ), ]
  
  #刪除掉有excludeword的資料，剩下的就是我要的(畫cloud用的)
  f.word.table <- excludeword_data( excludewords = excludeword,
                                        rdata = word.table2,
                                        col = 1 )
  # 先選stop word
  if( output_select=="excludeword slecet" ){  word.table2  } else 
    # 畫cloud
    if( output_select=='cloud' ){  library(wordcloud2)
      
      wordcloud2::wordcloud2(data = f.word.table[1:round(nrow(f.word.table)/5,0),] )
      
      #輸出畫cloud用的資料
    } else if ( output_select == 'file' ) { f.word.table }
  
}




