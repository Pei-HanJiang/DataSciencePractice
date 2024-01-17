# 上傳時請將檔名的 Dxxxxxxxx 改換成為自己的學號
# 本次上機考總分 200p，最高採計到 99p
# 注意事項
# (1) 資料檔與程式碼請放在同一個資料夾下，執行時請記得使用 setwd() 指令，或使用工具列 Session > Set Working Directory 中的選項改換工作目錄
# (2) Open Everything 除了教室中的同學、線上的同學以及線上的 AI 夥伴
# (3) 所有題目皆須以程式碼完成，可以分段、可以使用其他變數先暫存後再行指入
# 但是請勿直接理解答案後將答案直接指入指定變數
# 例 請回答一加一等於多少，並將答案放入 answer 變數
# 答法一
#   answer <- 1+1 (正確)
# 答法二
#   a = 1
#   b = 1
#   answer <- a+b (正確)
# 答法三
#   answer <- 1 %>%
#             `+`(1) (正確)
# 答法四
#   answer <- 2 (錯誤)
#
# 答案變數一開始都會 assign 空值 (NULL) 請將 NULL 替換掉作答
# -----說明結束分隔線
#
# 1. (10p) 請在不修改原始檔案的前提下，將 hsb.csv 檔案讀入 hsb 變數中
data <- readBin("hsb.csv", what = "raw", n = 30)
stringi::stri_enc_detect(data)
hsb <- read.csv("hsb.csv", fileEncoding = "UTF-16BE",sep=",", head = TRUE, skip = 1)

# 2. (10p) hsb 資料中有若干成績缺漏值，請在 review 資料表後，撰寫程式將有缺漏成績的資料整筆刪去，存入 hsb.new 變數中
hsb %>%
  #filter(閱讀 == -99 | 寫作 == -99 | 數學 == -99 | 科學 == -99 | 社會科學 == -99) -> test
  filter(!(寫作 == -99)) -> hsb.new
hsb.new
# 3. (30p) 請分別計算 hsb.new 資料表中男生與女生的各科平均，並且將其整理成一個 data.frame 指入 hsb.split 變數
filter(hsb.new, 性別 ==  "male") -> Male
filter(hsb.new, 性別 ==  "female") -> Female

hsb.new %>%
  group_by(性別) %>%
  summarize(閱讀平均 = mean(閱讀, na.rm = TRUE),
            寫作平均 = mean(寫作, na.rm = TRUE),
            數學平均 = mean(數學, na.rm = TRUE),
            科學平均 = mean(科學, na.rm = TRUE),
            社會科學平均 = mean(社會科學, na.rm = TRUE)) -> hsb.split
hsb.split

stopifnot(isTRUE(all.equal(colnames(hsb.split), c("性別","閱讀平均","寫作平均","數學平均","科學平均","社會科學平均"))))

# 4. (20p) 其中我們發現男生與女生的寫作平均有顯著的差異，請以 ks.test 嘗試驗證男生與女生的寫作成績在分布上是否有顯著差異，並將答案指入 is.male_female_diff_in_writing 變數中，若有顯著差異答案為 TRUE 若沒有則為 FALSE
# 提示：以 ks.test 檢驗男生與女生的寫作成績，並檢查 p.value 是否小於 0.05
# p.value <- 0.03391
ks.test(Male$寫作, Female$寫作)
is.male_female_diff_in_writing <- TRUE

# 5. (30p) 除去編號與分類別，請嘗試以 "性別"、"種族"、"學校類型"、"學程類型"、"閱讀"、"數學"、"科學"、"社會科學" 等項建立線性模型預測寫作成績，並將建立出來模型的 R.square 值，存入變數 R2 中
# 提示：以 lm 建立線性模型，字串類別的資料須先轉換成為 R 認可的類別型資料
# 提示： summary 建好的模型可以獲得 R.square 值，可用 str 指令觀察該如何取出
#hsb2 <- select(hsb.new, -編號, -分類別)
g <- lm(寫作~性別+種族+學校類型+學程類型+閱讀+數學+科學+社會科學, hsb.new)
R2 <- summary(g)$r.squared

# 6. (20p) 嘗試以題 5 建立的線性模型，預測 hsb 資料中缺漏的若干筆閱讀成績是多少
# 提示：使用 predict 指令，利用題 5 建立的線性模型進行預測，詳細請參照 predict.lm 的 help
hsb %>%
  #filter(閱讀 == -99 | 寫作 == -99 | 數學 == -99 | 科學 == -99 | 社會科學 == -99) -> test
  filter((寫作 == -99)) -> hsb.lack

pred <- predict(model, newdata=hsb.lack)
# 7. (15p) 我們嘗試要計算不同產業的用電效率，此題目共有三個資料檔
# mapping.csv 中載明了不同產業類別的名稱，以及在 gdp 與 power 表單中分別的代碼
# gdp_industry.csv 中登載了不同產業類別及其子類別在不同年份的 gdp 數額
# power_industry.csv 中登載了不同產業類別及其子類別在不同年份的用電量
# 請分別將三張表單正確讀入，放置於 mapping、gdp_df、power_df 三個變數中
data <- "è¾²æ\u009e\u0097é­\u009aç\u0089§æç¤¦æ¥­å\u008f\u008aå\u009c\u009fç\u009f³æ\u008e¡å\u008f\u0096æ¥æ°´ã\u0080\u0081é\u009b»ã\u0080\u0081ç\u0087\u0083æ°£æ¥­"
stringi::stri_enc_detect(data)
mapping <- read.csv("mapping.csv", fileEncoding = "UTF-8")

data <- readBin("gdp_industry.csv", what = "raw", n = 30)
stringi::stri_enc_detect(data)
gdp_df <- read.csv("gdp_industry.csv", fileEncoding = "Big5", skip = 2)
gdp_df <- select(gdp_df, -gdp_codes)


stopifnot(isTRUE(all.equal(colnames(gdp_df), c("year", "industry_name", "gdp"))))

data <- readBin("power_industry.csv", what = "raw", n = 30)
stringi::stri_enc_detect(data)

power_df <- read.csv("power_industry.csv", fileEncoding = "Big5")
years = c(2007:2013) %>% as.character()
index <- sapply(years, function(y) which (gdp_df$year == y))
index = c(index, nrow(gdp_df))
for(i in 1:length(years)){
  gdp_df$year[index[i] : index[i+1]-1] = years[i]
}
power_df <- select(power_df, -power_codes)
colnames(power_df)[3] <- "power"
power_df <- select(power_df, year, industry_name, power)
stopifnot(isTRUE(all.equal(colnames(power_df), c("year", "industry_name", "power"))))

# 8. (40p) 根據 mapping 中的產業類別，濾出各產業逐年之 gdp 與 用電量
# 將 gdp_df 與 power_df 中年份重疊的部分 join 成 gdp_power_year 表單
# 計算各產業每年的用電效率值，即每單位電創造多少 gdp
splits <- strsplit(mapping$gdp_codes, "\\|") %>% unlist
gdp_df <- filter(gdp_df, gdp_codes %in% splits)
gdp_power_year <- NULL

stopifnot(isTRUE(all.equal(colnames(power_df), c("year", "industry_name", "gdp", "power", "eff"))))

# 9. (25p) 最終將計算各產業 gdp 與 power 逐年的相關係數以及其平均之 gdp、用電量與效率值，summarize 成 gdp_power 表單
gdp_power <- NULL

stopifnot(isTRUE(all.equal(colnames(power_df), c("industry_name", "cor", "gdp.avg", "power.avg", "eff.avg"))))
