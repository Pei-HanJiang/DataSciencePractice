# 我們將使用紐約機場起降的飛機資料來進行練習
install.packages("nycflights13")
library(nycflights13)
# 安裝 dplyr 套件 ---------
install.packages("dplyr")
# 安裝 dplyr 套件 ---------
library(dplyr)

# 練習：
# 我們現在要藉由資料表的結合，理解起飛機場的風速對於抵達時間的延遲是否有影響
# Step 1
# 結合 weather 與 flights 資料表
# 最終只留下 wind_speed 與 arr_delay 兩個欄位存入 delay_info
# hints: 過程中以 year:day, hour, origin 當成合併的key
#        記得要檢查並且清除其中的 NA 或 NaN
weather <- nycflights13::weather
flights <- nycflights13::flights

delay_info <- inner_join(weather, flights, by = c("year", "month", "day", "hour", "origin")) %>%
  select(wind_speed, arr_delay) %>%
  filter(!is.na(wind_speed), !is.na(arr_delay))

# 延伸練習：
# 假如風速希望可以分成十級，應該如何進行呢？
# 假如出發機場的風速對於出發延遲的影響，應該如何進行呢？

# 第一步：結合 weather 和 flights 數據表，並保留必要的欄位
delay_info <- inner_join(weather, flights, by = c("year", "month", "day", "hour", "origin")) %>%
  select(wind_speed, dep_delay) %>%
  filter(!is.na(wind_speed), !is.na(dep_delay))

# 第二步：將風速分為十個等級
breaks <- quantile(delay_info$wind_speed, probs = seq(0, 1, length.out = 10))
delay_info <- mutate(delay_info, wind_level10 = cut(delay_info$wind_speed, breaks = breaks))

# 第三步：分析每個風速等級對應的平均出發延遲時間
average_delay_by_wind_speed <- delay_info %>%
  group_by(wind_level10) %>%
  summarize(avg_dep_delay = mean(dep_delay))

stopifnot(nrow(delay_info) == 326915)
stopifnot(nrow(average_delay_by_wind_speed) == 10)
stopifnot(as.integer(average_delay_by_wind_speed$avg_dep_delay[1]) == 10)

