iris_function <- function() {
    
    # 0. 載入 iris 資料集

    # 1. iris 資料表共有幾筆資料？
    nrow_iris <- 
    nrow_iris

    # 2. iris 資料表中個樣本記載了幾種不同的屬性？
    ncol_iris <- 
    ncol_iris

    # 3. iris 資料表中，有幾筆樣本其品種為 setosa
    n_setosa <- 
    n_setosa

    # 4. 分別計算各品種之其花瓣與花萼長度的平均數值
    mean_values <- 
    mean_values

    # 5. 各品種中，花萼長度超過 5 cm 的樣本各有幾株？
    count_over_5 <- 
    count_over_5

    # 6. versicolor 品種的樣本中，其花萼長度大於 virginica 品種之平均花萼長度 的樣本有幾株？
    
    n_versicolor_over_mean_virginica <- 
    n_versicolor_over_mean_virginica

    # 7. 在 iris 資料表中新增一個欄位記載各樣本花瓣與花萼的總長
    iris$Total.Length <- 

    # 8. iris 資料表中，花辦與花萼之總長最長的一株樣本，是什麼品種的？
    max_species <- 
    max_species

    # 9. 資料集中，各品種花瓣與花萼之長度總和的平均各為多少？
    mean_total_length_per_species <- 
    mean_total_length_per_species

    
    ans = list(
        nrow_iris = nrow_iris,
        ncol_iris = ncol_iris,
        n_setosa = n_setosa,
        mean_values = mean_values,
        count_over_5 = count_over_5,
        n_versicolor_over_mean_virginica = n_versicolor_over_mean_virginica,
        iris = iris,
        max_species = max_species,
        mean_total_length_per_species = mean_total_length_per_species
    )
    
    return(ans)



}
