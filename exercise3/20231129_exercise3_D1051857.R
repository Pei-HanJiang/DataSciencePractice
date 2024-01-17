# 我們嘗試要計算不同產業的用電效率，此題目共有三個資料檔
# mapping.csv 中載明了不同產業類別的名稱，以及在 gdp 與 power 表單中分別的代碼
# gdp_industry.csv 中登載了不同產業類別及其子類別在不同年份的 gdp 數額
# power_industry.csv 中登載了不同產業類別及其子類別在不同年份的用電量
# 請分別將三張表單正確讀入，放置於 mapping、gdp_df、power_df 三個變數中
mapping <- NULL

gdp_df <- NULL

stopifnot(isTRUE(all.equal(colnames(gdp_df), c("year", "industry_name", "gdp"))))

power_df <- NULL

stopifnot(isTRUE(all.equal(colnames(power_df), c("year", "industry_name", "power"))))

# 接下來我們要根據 mapping 中的產業類別，濾出各產業逐年之 gdp 與 用電量
# mapping 中的 gdp_codes 欄位紀載了在 gdp_industry.csv 中與 industry_name 相呼應的產業代碼
# 有的產業涵蓋了多個代碼，例如水電燃氣業的 gdp_codes 為 D、E 代表在 gdp_industry.csv 中
# 代碼為 D 或 E 的皆歸屬為水電燃氣業，而 power_codes 則是 power_industry.csv 中的產業代碼
# 請將gdp_df、power_df 中的產業項目，根據 mapping 中有登載的項目進行過濾
# 換言之，僅需處理 mapping 中有紀載的代碼類別
# 然後將 gdp_df 與 power_df 中年份重疊的部分 join 成 gdp_power_year 表單
# 計算各產業每年的用電效率值，即每單位電創造多少 gdp

gdp_power_year <- NULL

stopifnot(isTRUE(all.equal(colnames(power_df), c("year", "industry_name", "gdp", "power", "eff"))))

# 最終將計算各產業 gdp 與 power 逐年的相關係數以及其平均之 gdp、用電量與效率值，summarize 成 gdp_power 表單
gdp_power <- NULL

stopifnot(isTRUE(all.equal(colnames(power_df), c("industry_name", "cor", "gdp.avg", "power.avg", "eff.avg"))))
