
library(parallel)


cl <- parallel::makeCluster(3)
parallel::clusterEvalQ(cl, library(XML))
parallel::clusterEvalQ(cl, library(httr))
parallel::clusterEvalQ(cl, library(RCurl))
parallel::clusterEvalQ(cl, library(magrittr)) 
ttime <- Sys.time()

get_html_code <- function(url){ XML::htmlParse( RCurl::getURL( url ) ,encoding = "utf-8" )}


parallel::clusterEvalQ( cl,get_html_code <- function(url){ XML::htmlParse( RCurl::getURL( url ) ,encoding = "utf-8" )} )



url.list <- paste('https://www.ptt.cc/bbs/HomeTeach/index',c(1450:1723),'.html',sep="") #for RCurl::getURL

et_title_url <- function (index.url){ html <- get_html_code(index.url)

XML::xpathApply( html ,path = '//div[@class="title"]/a[@href]', xmlAttrs ) 

}


art.url <- paste( 'https://www.ptt.cc',
                  unlist( parallel::parLapply( cl, url.list, get_title_url ))
                  ,sep="" )


get_tdc <- function(url){ tryCatch( {
  res  <- rep(url,4)
  html <- get_html_code(url)
  
  title    <- as( XML::xpathApply( html, 
                                   path = '//span [@class="article-meta-value"]',xmlValue)[[3]] ,'character' )
  date     <- as( XML::xpathApply( html, 
                                   path = '//span [@class="article-meta-value"]',xmlValue)[[4]] ,'character' )
  contents <- as(XML::xpathApply( html, 
                                  path='//meta[@name="description"][@content]')[[1]] ,'character')
  res <- c(title,date,contents,url) }, error = function(e) {res })   
  res  }


system.time( art.xml <- parLapply( cl, art.url, get_tdc )  )


art.data <- t( do.call( "cbind", parLapply(cl,art.xml, as.data.frame)))

arallel::stopCluster(cl)


dim(art.data)

