# 我們先將 iris 資料表中部分的資料取出

iris_function <- function() {
  sepal.len = iris$Sepal.Length # 花萼的長度
  petal.len = iris$Petal.Length # 花瓣的長度
  species = as.character(iris$Species) # 花的種類
  
  
  # 試回答下列問題
  # 資料集中，有幾筆樣本其品種為 setosa
  setosa_kind <- sum(species == "setosa")
  
  # 分別計算各品種之其花瓣(petal)與花萼(sepal)長度的平均數值，你發現了什麼？
  mean_sepal <- vector()
  for (i in unique(species)){
    mean_sepal <- c(mean_sepal, mean(sepal.len[species == i]))
    print(mean_petal)
  }
  # 各品種中，花萼長度超過 5 cm 的樣本各有幾株？
  sepal_len_exceeds_5 <- vector()
  for (i in unique(species)){
    sepal_len_exceeds_5 <- c(sepal_len_exceeds_5, sum(sepal.len[species == i] > 5))
  }
    
  # versicolor 品種的樣本中，其花萼長度大於 virginica 品種之平均花萼長度 的樣本有幾株？
  mean_sepal_virginica <- mean(sepal.len[species == "virginica"])
  versicolor_len_exceeds_mean <- sum(sepal.len[species == "versicolor"] > mean_sepal_virginica)
  
  # 資料集中，花辦與花萼之總長最長的一株樣本，是什麼品種的？
  #     (hint: 請查閱 which.max() 函式的 help，並且嘗試使用之)
  index <- which.max(sepal.len + petal.len)
  sepal_add_petal_max <- species[index]
  # 資料集中，各品種花瓣與花萼之長度總和 & 平均各為多少？
  species_mean <- vector()
  species_len <- vector()
  
  for(i in unique(species)){
    species_len <- c(species_len, sum(sepal.len[species == i], petal.len[species == i]))
    species_mean <- c(species_mean, mean(sepal.len[species == i]+petal.len[species == i]))
  }
  # 順便教兩個經常用於整理類別型變數的函式，請嘗試
  # unique(species)
  # table(species)
  # 請詳閱上述兩函式的 help 文件
  ans = c(setosa_kind , mean_sepal,sepal_len_exceeds_5 , versicolor_len_exceeds_mean , sepal_add_petal_max , species_mean , species_len  )
}




