# 請依照指示完成資料處理
# 這是從 <http://data.gov.tw/node/7769> 下載的海盜通報資料，
# 請使用課堂所學的技巧，將此文件中的資訊整理成為表格。
# 首先，先將該檔案載入到R 之中。
pirate_info <- readLines(file("Desktop/data/pirate-info-2015-09.txt", encoding = "Big5"))
head(pirate_info, 20)
# 這份資料的格式，可以用`：`分割出資料的欄位與內容，
# 請同學利用`strsplit`將`pirate_info`進行切割，
# 並將結果儲存到`pirate_info_key_value`之中。
pirate_info_key_value <- strsplit(pirate_info,"：")
head(pirate_info_key_value, 20)
# 我們將`pirate_info_key`和`"經緯度"`做比較後，把結果存到變數`pirate_is_coordinate`中，
# 結果應該為一個布林向量，同時總共有11件海盜通報事件
pirate_is_coordinate <- sapply(pirate_info_key_value, "[", 1) == "經緯度"
pirate_is_coordinate_value <- pirate_info_key_value[pirate_is_coordinate]
pirate_coordinate = sapply(pirate_is_coordinate_value, '[', 2)

# 接著可以使用`substring`抓出經緯度的數字，
# 請先抓出緯度並忽略「分」的部份，並存入 pirate_coordinate_latitude
pirate_coordinate_latitude <- as.integer(substring(pirate_coordinate, 3, 4))

# 請用同樣的要領取出經度，忽略「分」的部份，並存入 pirate_coordinate_longitude
pirate_coordinate_longitude <- as.integer(substring(pirate_coordinate, 12, 14))

# 為了方便後續的分析，我們經常把非結構化的資料整理為結構化資料。
# 在R 中，結構化的資料結構就是data.frame。
# 請同學利用上述的數據，建立一個有11筆資料的data.frame，
# 其中有兩個欄位，一個是latitude, 另一個則是longitude。
# 這兩個欄位紀錄著海盜事件的位置。
pirate_df <- data.frame(
  latitude = pirate_coordinate_latitude,
  longitude = pirate_coordinate_longitude
)

# 下列兩項 check 都應該要是 True 答案才是對的
sum(pirate_df$latitude) == 43
sum(pirate_df$longitude) == 1151

