# 請在不修改原始檔案的前提下，將 hsb.csv 檔案讀入 hsb 變數中

hsb <- read.csv("hsb.csv", fileEncoding = "UTF-16BE",sep=",", head = TRUE, skip = 1)


# 檢視 hsb 資料表，其中有若干成績缺漏值，請在 review 資料表後，撰寫程式將有缺漏成績的資料整筆刪去，存入 hsb.new 變數中

hsb %>%
  #filter(閱讀 == -99 | 寫作 == -99 | 數學 == -99 | 科學 == -99 | 社會科學 == -99) -> test
  filter(!(寫作 == -99)) -> hsb.new

# 請分別計算 hsb.new 資料表中男生與女生的各科平均，並且將其整理成一個 data.frame 指入 hsb.split 變數
filter(hsb.new, 性別 ==  "male") -> Male
filter(hsb.new, 性別 ==  "female") -> Female

hsb.new %>%
  group_by(性別) %>%
  summarize(閱讀平均 = mean(閱讀, na.rm = TRUE),
            寫作平均 = mean(寫作, na.rm = TRUE),
            數學平均 = mean(數學, na.rm = TRUE),
            科學平均 = mean(科學, na.rm = TRUE),
            社會科學平均 = mean(社會科學, na.rm = TRUE)) -> hsb.split

stopifnot(isTRUE(all.equal(colnames(hsb.split), c("性別","閱讀平均","寫作平均","數學平均","科學平均","社會科學平均"))))

# 其中我們發現男生與女生的寫作平均有顯著的差異，請以 ks.test 嘗試驗證男生與女生的寫作成績在分布上是否有顯著差異，並將答案指入 is.male_female_diff_in_writing 變數中，若有顯著差異答案為 TRUE 若沒有則為 FALSE
# 提示：以 ks.test 檢驗男生與女生的寫作成績，並檢查 p.value 是否小於 0.05
ks.test(Male$寫作, Female$寫作)
is.male_female_diff_in_writing <- TRUE

# 除去編號與分類別，請嘗試以 "性別"、"種族"、"學校類型"、"學程類型"、"閱讀"、"數學"、"科學"、"社會科學" 等項建立線性模型預測寫作成績，並將建立出來模型的 R.square 值，存入變數 R2 中
# 提示：以 lm 建立線性模型，字串類別的資料須先轉換成為 R 認可的類別型資料
# 提示： summary 建好的模型可以獲得 R.square 值，可用 str 指令觀察該如何取出
g <- lm(寫作~性別+種族+學校類型+學程類型+閱讀+數學+科學+社會科學, hsb.new)
R2 <- summary(g)$r.squared

# 嘗試以上面建立的線性模型，預測 hsb 資料中缺漏的若干筆閱讀成績是多少
# 提示：使用 predict 指令，利用題 5 建立的線性模型進行預測，詳細請參照 predict.lm 的 help
missing <- filter(hsb, 寫作 == -99)
writing.missing <- predict(g, newdata = missing)
