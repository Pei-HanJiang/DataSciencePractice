library(RSQLite)

# 首先準備存取 SQLite 所需的 DB driver

drv <- dbDriver("SQLite")

# 以適當的 driver 與 指定資料庫建立連線

db <- dbConnect(drv, "Desktop/data/example.db")

# 第一題  請嘗試取出範例資料庫中 TWII 資料表的資料，進而了解其日期範圍
twii_data <- dbReadTable(db, "TWII")
# 取出日期範圍
date_range <-range(twii_data$date)

#第二題 請將 iris 資料表中，setosa 物種的資料寫入範例資料庫，並且取名為 setosa
setosa_data2 <- iris[iris$Species == "setosa",]


# 將setosa_data寫入資料庫中，並取名為setosa
dbWriteTable(db, "setosa", setosa_data2)


# 關閉資料庫連接
dbDisconnect(db)



