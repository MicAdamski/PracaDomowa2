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

#remDr %>% go("https://www.otomoto.pl/osobowe/volkswagen/passat/?search%5Bfilter_enum_generation%5D=gen-b7-2010-2014")
wektorLinkow <- c()
for (i in 1:24){ # tyle stron zawiera otomoto dla wybranych parametrów
  newUrl<-paste0("https://www.otomoto.pl/osobowe/volkswagen/passat/?search%5Bfilter_enum_generation%5D=gen-b7-2010-2014&search%5Border%5D=created_at%3Adesc&page=",i)
  remDr%>%go(newUrl)
  elems <- remDr%>% findElements(using="tag name", "h2")
  for(j in 1:length(elems)){
    e<-findElementsFromElement(elems[[j]], using = "tag name", "a")
    if(length(e)>0){
      link<- e[[1]]%>%getElementAttribute("href")
      #print(link)
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
  
}

length(wektorLinkow)
wektorLinkowU<- wektorLinkow%>%unique()
length(wektorLinkowU)



zrobWiersz<-function(w,wektorLinkow,remDr){
  remDr%>%go(wektorLinkowU[w])
  cena<-NA
  cena<-remDr%>%findElement("class name", "offer-price")%>%getElementAttribute("data-price")
  szczegoly<-remDr%>%findElements("class name","offer-params" )
  listaSzczegolowOpis<-c()
  listaSzczegolowWartosci<-c()
  for ( i in 1: length(szczegoly)){
    listaSzczegolowOpis<- c(listaSzczegolowOpis,szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__label") )
    listaSzczegolowWartosci<- c(listaSzczegolowWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__value") )
  }
  nazwyKolumn<- lapply(listaSzczegolowOpis,getElementText)%>%str_replace_all(":","")%>%unlist()
  wartosci<- lapply(listaSzczegolowWartosci,getElementText)%>%unlist()
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<-cbind(cena,df1)
}

passaty<-NULL
for(w in 1: length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWiersz(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(passaty)){
    passaty<-df1
  }else{
    passaty<-smartbind(passaty,df1)
    #View(passaty)
  }
}
View(passaty)
write.csv(passaty,paste0(getwd(), "/passaty.csv"), row.names = FALSE)

