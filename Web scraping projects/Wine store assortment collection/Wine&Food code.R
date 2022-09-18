
library(RSelenium)
library(rvest)
library(magrittr)
library(tidyverse)
library(readr)
library(dplyr)

#Data scraping ----  

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "chrome"
)

remDr$open()
remDr$maxWindowSize()

link<-"https://winefood.ua/vino/"

remDr$navigate(link)

pageState<-remDr$getPageSource()

pagehtml<-read_html(unlist(pageState))

WineBrand <- pagehtml %>% html_nodes(xpath = "//div[@id='mCSB_1_container']/div[@class='checkbox']//span[@class='bx-filter-param-text']/a") %>% html_text()
WineBrand



list1<-list()

for(i in 1:27){
  
  tryCatch({
    m2<-remDr$findElements("xpath","//div[@class='product-container']//*[self::div[@class='product-name'] or self::div[@class='product-price']]")
    m2_text<-m2 %>% sapply(., function(x) x$getElementText())
    
    
    if(i<27){
      nextPage<-remDr$findElement("xpath", "//li[@class='bx-pag-next']/a")
      nextPage$clickElement()
      Sys.sleep(5)
    }
    
    list1<-c(list1, m2_text)
    
    
  },
  error = function(c) {
    i<<-i-1
    #remDr$deleteAllCookies() 
    remDr$refresh()
    Sys.sleep(5)
  },
  finally={gc()}
  )
  
}




#Data processing ----

index<-seq(2,length(list1), by=2)

data <-  tibble('wine'=unlist(list1[-c(index)]), 'price'=unlist(list1[index]))

row1 <-  grepl("\n",data[[2]])

discout_p <-data[row1,]
regular_p <-data[!row1,]

regular_p$price<-parse_number(regular_p$price) # https://winefood.ua/search/index.php?q=+Sileni+%D0%A1hardonnay&s=

#Split a column with both prices into two separate ones
discout_p<-discout_p %>% separate(price,c("disc", "reg"), '\n')

discout_p$disc<-parse_number(discout_p$disc)
discout_p$reg<-parse_number(discout_p$reg)




#Save data in working directory ----

setwd("C:/Users/Work/Documents/RData")

listdock<-list(list1, data, discout_p, regular_p, WineBrand)
saveRDS(listdock, file = "winecatalogue_06.rds")




#Comparison ----

# (1) Check the existence of new brands
dataset_26<-readRDS("winecatalogue_26.rds")
dataset_06<-readRDS("winecatalogue_06.rds")

setdiff(dataset_06[[5]], dataset_26[[5]]) #[1] "Leleka Wines"

# (2) Check the existence of new types of wine

list_26<-dataset_26[[2]]
list_06<-dataset_06[[2]]

#As we can see, the length of the 2 sets is different, which means that new wines has been added.

setdiff(list_06$wine, list_26$wine) #Here is the list of those wines.

# (3) Check the price changes

v<-setdiff(list_06$wine, list_26$wine)

#removed new components
list_06_new<-list_06[!(list_06$wine %in% v),]

#compared order and presence of all fields
all(list_06_new$wine == list_26$wine)

Union<-cbind(list_26, "price_2"=list_06_new$price)

difference<-Union[Union$price!=Union$price_2, ]

                  