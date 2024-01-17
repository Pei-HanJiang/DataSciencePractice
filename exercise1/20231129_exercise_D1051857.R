#' 請用學到的方法讀取 GDP.csv 的資料、整理資料，並把最後的結果存到變數`gdp`。
#' 提示：GDP.csv 中的第一欄數據是年/季、第二欄數據是該季的GDP(百萬)。
#' 結果應該要有兩欄的數據，第一欄是年份，第二欄是我國每年的GDP，
#' 具體細節請參考最後的`stopifnot`的檢查事項。
#' 提示：拿掉數據中間的逗號，請用：`gsub(pattern = ",", replacement = "", x = <你的字串向量>)`
install.packages("dplyr")
library(dplyr)
gdp <- read.csv("GDP.txt")
gdp <- data.frame(gdp[4:135, 1:2])
colnames(gdp) <- c("year", "gdp")
gdp$gdp <- gsub(pattern = ",", replacement = "", x = gdp$gdp)
gdp$gdp <- as.numeric(gdp$gdp, na.rm = TRUE)
gdp$year <- substr(gdp$year,1,4)
gdp %>%
  group_by(year) %>%
  summarise(gdp = sum(gdp)) -> gdp
gdp$gdp <- gdp$gdp * 1000000

stopifnot(is.data.frame(gdp))
stopifnot(colnames(gdp) == c("year", "gdp"))
stopifnot(class(gdp$year) == "character")
stopifnot(class(gdp$gdp) == "numeric")
stopifnot(nrow(gdp) == 33)
stopifnot(range(gdp$year) == c("1981", "2013"))
stopifnot(range(gdp$gdp) == c(1810829,14564242) * 1000000)

#' cl_info_other.csv 的資料包含各家銀行的房貸餘額 (mortgage_bal) 資訊與資料建立的時間 (data_dt)。
#' 請用學到的方法整理cl_info的資料，並把最後的結果整理至 cl_info_year 
#' 結果應該要有兩欄的數據，第一欄是年份，第二欄是每年房貸餘額的值 (請以每年的一月份資料為準)。
#' 具體細節請參考最後的`stopifnot`檢查事項。

cl_info_year <- read.csv("cl_info_other.csv")
cl_info_year <- data.frame(cl_info_year)
yy <- unique(substring(cl_info_year$data_dt, 1, 4))

cl_info_year$data_dt <- substring(cl_info_year$data_dt, 1, 10)
cl_info_year$data_dt <- gsub(pattern = "-", replacement = "", x = cl_info_year$data_dt)
cl_info_year$data_dt <- as.numeric(cl_info_year$data_dt)
mm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
for(i in 1:length(cl_info_year$data_dt)){
  if(cl_info_year$data_dt[i]%%10000==101){
    mm[(cl_info_year$data_dt[i]/10000)-2005]<- mm[(cl_info_year$data_dt[i]/10000)-2005]+cl_info_year$mortgage_bal[i]
  }
}

cl_info_year <- data.frame(year=yy, mortgage_total_bal=mm)


stopifnot(is.data.frame(cl_info_year))
stopifnot(colnames(cl_info_year) == c("year", "mortgage_total_bal"))
stopifnot(class(cl_info_year$year) == "character")
stopifnot(class(cl_info_year$mortgage_total_bal) == "numeric")
stopifnot(nrow(cl_info_year) == 9)
stopifnot(range(cl_info_year$year) == c("2006", "2014"))
stopifnot(range(cl_info_year$mortgage_total_bal) == c(3.79632e+12, 5.726784e+12))

#' 最後，請同學用這門課程所學的技術整合 `gdp` 與 `cl_info` 的資料，
#' 並計算出房貸餘額與 gdp 的比率 (mortgage_total_bal / gdp)
#' 請將結果輸出到一個data.frame，第一欄是年份，第二欄則是房貸餘額的 GDP 佔有比率。
#' 細節請參考`stopifnot`的檢查。

gdp %>%
  left_join(cl_info_year) %>%
  filter(!is.na(mortgage_total_bal)) ->res

res <- mutate(res, index=(mortgage_total_bal/gdp))
res %>%
  select(year, index) -> mortgage2gdp

stopifnot(is.data.frame(mortgage2gdp))
stopifnot(nrow(mortgage2gdp) == 8)
stopifnot(colnames(mortgage2gdp) == c("year", "index"))
stopifnot(class(mortgage2gdp$year) == "character")
stopifnot(class(mortgage2gdp$index) == "numeric")
stopifnot(min(mortgage2gdp$index) > 0.3)
stopifnot(max(mortgage2gdp$index) < 0.4)

