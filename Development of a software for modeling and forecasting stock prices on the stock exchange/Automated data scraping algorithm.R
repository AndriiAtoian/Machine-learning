
library(RSelenium)
library(magrittr)
library(readr)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "chrome"
)

symb="AAPL"
remDr$open()
remDr$maxWindowSize()

link<-paste0("https://finance.yahoo.com/quote/", symb,"?p=",symb)
remDr$navigate(link)

#Go to tab "Chart" ----
Tabs <- remDr$findElements(using = "xpath", "//a[starts-with(@class, 'Lh(50px)')]")

ChartIndex <- Tabs %>% sapply(., function(x) x$getElementText()) %>% grepl("^Chart$",.) %>% which

Tabs[[ChartIndex]]$clickElement()

Sys.sleep(10)

#Function that will scroll us to the element ----
scrollTo <- function(remDr, webElem){
  remDr$executeScript("arguments[0].scrollIntoView(false);", args = list(webElem))
  webElem$highlightElement()
}

Focus <- remDr$findElement(using = "xpath", "//div[starts-with(@class, 'stx-holder stx-panel-chart')]")
scrollTo(remDr, Focus)

#Set the 1 min Interval ----
BoxInstr <- remDr$findElements(using = "xpath", "//div[@class = 'Pos(r) D(ib) O(n):f Cur(p)']")

IntervalIndex <- BoxInstr %>% sapply(., function(x) x$getElementText()) %>% grepl("Interval",.) %>% which

BoxInstr[[IntervalIndex]]$clickElement()

#C($c-fuji-grey-j) Lh(15px) Fw(400) Fz(s) Py(8px) Px(20px) W(100%) Ta(start)
InterChoice <- remDr$findElements(using = "xpath", "//button[starts-with(@class, 'C($c-fuji-grey-j)')]")

IntervalIndex <- InterChoice %>% sapply(., function(x) x$getElementText()) %>% grepl("^1 min$",.) %>% which

InterChoice[[IntervalIndex]]$clickElement()

#Set the line type "Candle" ----

LineIndex <- BoxInstr %>% sapply(., function(x) x$getElementText()) %>% grepl("Line",.) %>% which

BoxInstr[[LineIndex]]$clickElement()

#C($c-fuji-grey-j) Lh(15px) Fw(400) Fz(s) Py(8px) Px(22px) W(100%) Ta(start)
LineChoice <- remDr$findElements(using = "xpath", "//button[starts-with(@class, 'C($c-fuji-grey-j)')]")

LineIndex<-LineChoice %>% sapply(., function(x) x$getElementText()) %>% grepl("^Candle$", .) %>% which

LineChoice[[LineIndex]]$clickElement()

# Chart block ----
ChartBox <- remDr$findElement(using = "class", "stx-subholder")
ChartBox$highlightElement()

# Extract block characteristics
BoxInfo <- ChartBox$getElementLocation()

#Determining the step between candles ----
v <- c()
n <- 0

repeat{
  remDr$mouseMoveToLocation(-BoxInfo$width/2 + n, 1, webElement = ChartBox)
  cross_x <- remDr$findElements(using = "xpath", "//div[@class = 'stx_crosshair stx_crosshair_x crossX']")
  left <- cross_x %>% sapply(., function(x) x$getElementValueOfCssProperty("left"))
  v <- append(v, left[[1]])
  if(n == 40){break}
  n = n + 1
}
freq <- table(table(v), dnn = NULL) #define frequency
step <- as.numeric(names(freq)[which.max(freq)])

#Data collection ----
n=0
i=1
ListContainer<-list()

while(n < BoxInfo$width){
  remDr$mouseMoveToLocation(-BoxInfo$width/2 + 1 + n, 1, webElement = ChartBox)
  ValueBox <- remDr$findElements(using = "xpath", "//div[@class = 'stx-tooltip']")
  values <- ValueBox %>% sapply(., function(x) x$getElementText())
  
  TimeFrame <- remDr$findElement(using = "xpath", "//div[@class = 'stx-float-date floatDate']")
  time <- TimeFrame$getElementText()
  t<- strsplit(values[[1]], "\n")
  t[[1]][c(2:7)]<-parse_number(t[[1]][c(2:7)])
  t[[1]][1] <- time[[1]]
  ListContainer[i]=t
  n = n + step
  i=i+1
}
#Dataframe creation ----
DataContainer<-data.frame(Reduce(rbind, ListContainer),row.names=NULL, stringsAsFactors = FALSE)
colnames(DataContainer) <- c("Time", "Open", "High", "Low", "Close", "Volume", "Change")


  