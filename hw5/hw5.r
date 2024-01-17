# 我們將使用紐約機場起降的飛機資料來進行練習
install.packages("nycflights13")
library(nycflights13)
install.packages("dplyr")
library(dplyr)

# 請各位以 dplyr 以及 baseR 語法進行下列練習
# 1.取出當年上半年度 (1 月 ~ 6 月的資料) 

flights_first_half_baseR <- flights[flights$month >= 1 & flights$month <= 6,]
flights_first_half_dplyr <- filter(flights, month >= 1, month <= 6)

#請各位同學撈出資料表中班機編號 (tailnum) 以 "AA" 開頭的班機資料
flights_AA_dplyr <- filter(flights, grepl("AA", tailnum))
flights_AA_baseR <- flights[grepl("AA", flights$tailnum),]


# 請練習以 dplyr 語法以及 baseR 語法取出 flights 資料表中奇數列的資料
flights_odd_rows_dplyr <- slice(flights, seq(1, nrow(flights), 2))
flights_odd_rows_baseR <- flights[seq(1,nrow(flights),2),]


# 請練習以 dplyr 語法以集 baseR 語法取出 flights 資料表中 1000 到 2000 列 以及 2500 到 3500 列的資料
flights_selected_rows_baseR <- slice(flights, 1000:2000, 2500:3500)

# # 請參閱 arrange 與 order 的 help，試著由大至小進行排序
# 使用 dplyr 
flights_sorted_dplyr <- arrange(flights, desc(month), desc(day), desc(dep_time))

# 使用 base R 
flights_sorted_baseR <- with(flights, flights[order(desc(month), desc(day), desc(dep_time)),])

# 上述資料中第一列的 dep_time 為 517
# 代表了 1 月 1 日 當天最早的班機是 5:17 起飛的
# 那麼整個紀錄中起飛時刻最早是幾點呢？
earliest_arr_time <- flights$arr_time[min(flights$arr_time, na.rm = TRUE)]


# 以 %>% 指令改寫 group_by, summarise 流程會十分簡單
# 這邊我們做一個練習，請問美國航空公司 (carrier == "AA")，其班機尾標是否都有 AA 字眼，去掉 na
all_AA_tailnums <- flights %>%
  na.omit() %>%
  filter(carrier == "AA") %>%
  select(tailnum) %>%
  summarise(all_aa = all(grepl("AA", tailnum, ignore.case = TRUE)))


# 請嘗試計算各航空公司，其不同時段 (0000-0559, 0600-1159, 1200-1759, 1800-2359) 之航班的平均起飛遲延、以及平均降落遲延

library(dplyr)
library(nycflights13)
 
# 定義時段

flights <- flights %>%
  na.omit() %>%
  mutate(dep_time_group = cut(dep_time, breaks = c(0, 559, 1159, 1759, 2359)))
  
 

# 計算平均起飛和降落遲延
average_delays <- flights %>% 
  group_by(dep_time_group) %>%
  summarize(dep_delay = mean(dep_delay), arr_delay = mean(arr_delay))
  


stopifnot(nrow(flights_first_half_dplyr)==166158)
stopifnot(nrow(flights_first_half_baseR)==166158)
stopifnot(nrow(flights_AA_dplyr)==32645)
stopifnot(nrow(flights_AA_baseR)==32645)
stopifnot(nrow(flights_odd_rows_dplyr)==168388)
stopifnot(nrow(flights_odd_rows_baseR)==168388)
stopifnot(nrow(flights_selected_rows_baseR)==2002)
stopifnot(nrow(flights_sorted_dplyr)==336776)
stopifnot(nrow(flights_sorted_baseR)==336776)
stopifnot(earliest_arr_time==830)
stopifnot(all_AA_tailnums==TRUE)
stopifnot(average_delays$dep_delay[1]==139.838709677419)
stopifnot(average_delays$arr_delay[1]==129.5161290322581)

