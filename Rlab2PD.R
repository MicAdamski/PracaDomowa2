#install.packages( c("RSelenium","seleniumPipes","dplyr","gtools","stringr","xml2", "rvest") )
library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)
library(rvest)
remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port= 4444,
                  browserName = "chrome",
                  newSession = TRUE)

remDr %>% go("http:/otodom.pl/sprzedaz/mieszkanie")
wetkorLinkow <- c()
for (i in 1:10){
  newUrl<-paste0("https://www.otodom.pl/sprzedaz/mieszkanie/?page=",i)
  remDr%>%go(newUrl)
  elems <- remDr%>% findElements(using="tag name", "h3")
  for(j in 1:length(elems)){
    e<-findElementsFromElement(elems[[j]], using = "tag name", "a")
    if(length(e)>0){
      link<- e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
  
}

length(wektorLinkow)
wektorLinkowU<- wektorLinkow%>%unique()
length(wektorLinkowU)

for (i in 1:length(wektorLinkowU)){
  remDr%>%go(wektorLinkowU[i])
  szczegoly <- remDr%>%findElements("class name", "css-18h1kfv")
  listaSzczegolowOpis <- c()
  listaSzczegolowWartosci <- c()
  cena<-NA
  cena<- remDr%>%findElement("class name", "css-srd1q3")%>%getElementText()
  for (i in 1:length(szczegoly)){
    listaSzczegolowOpis <-c(listaSzczegolowOpis,szczegoly[[i]]%>%findElementsFromElement("class name", "css-o4i8bk"))
    listaSzczegolowWartosci<-c(listaSzczegolowWartosci,szczegoly[[i]]%>%findElementsFromElement("class name", "css-1ytkscc"))
    
  }
  nazwyKolumn<-lapply(listaSzczegolowOpis,getElementText)%>%str_replace_all(":","")%>%unlist()
  wartosci<-lapply(listaSzczegolowOpis,getElementText)%>%str_replace_all(":","")%>%unlist()
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<-cbind(cena,df1)
  View(df1)
  
}




zrobWiersz<-function(w,wektorLinkow,remDr){
  remDr%>%go(wektorLinkowU[w])
  cena<-NA
  cena<-remDr%>%findElement("class name", "css-srd1q3")%>%getElementText()
  szczegoly<-remDr%>%findElements("class name","css-18h1kfv" )
  listaSzczegolowOpis<-c()
  listaSzczegolowWartosci<-c()
  for ( i in 1: length(szczegoly)){
    listaSzczegolowOpis<- c(listaSzczegolowOpis,szczegoly[[i]]%>%findElementsFromElement("class name","css-o4i8bk") )
    listaSzczegolowWartosci<- c(listaSzczegolowWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","css-1ytkscc") )
  }
  nazwyKolumn<- lapply(listaSzczegolowOpis,getElementText)%>%str_replace_all(":","")%>%unlist()
  wartosci<- lapply(listaSzczegolowWartosci,getElementText)%>%unlist()
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<-cbind(cena,df1)
}

mieszkania<-NULL
for(w in 1: length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWiersz(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(mieszkania)){
    mieszkania<-df1
  }else{
    mieszkania<-smartbind(mieszkania,df1)
    View(mieszkania)
  }
}
zrobWierszRvest<-function(w,wektorLinkow,remDr){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".css-srd1q3")%>%html_text()
  v<-page %>% xml_find_all('/html/body/div[1]/main/div/div[3]/div[1]/*/*')%>%html_attr("title")%>%na.omit()
  print(v)
  indexy<-seq(1,length(v),1)
  nazwyKolumn<-v[indexy%%2==1]
  wartosci<-v[indexy%%2==0]
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<-cbind(cena,df1)
  df1
}

mieszkania<-NULL
for(w in 1: length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(mieszkania)){
    mieszkania<-df1
  }else{
    mieszkania<-smartbind(mieszkania,df1)
    View(mieszkania)
  }
}
