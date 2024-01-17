hw2_function <- function() {


    #' 給定一個矩陣X，
    X <- cbind(x1 = 1, x2 = 1:10, x3 = sin(1:10))
    #' 以及一個長度為3 的向量 beta。
    beta <- c(0.5, -1, 4.3)

    #' 我們稱`X[,1] 為x1, X[,2] 為x2, X[,3] 為 x3
    #' 向量y 的值是 x1 * beta[1] + x2 * beta[2] + x3 * beta[3]，
    #' 請用矩陣乘法`%*%`算出向量y。
    #' dim(y) 應該是 c(10, 1)

    # 1. 計算 y
    x1 <- X[,1]
    x2 <- X[,2]
    x3 <- X[,3]
    
    ori_y <- X %*% beta #beta是X和y之間的關係

    #' epsilon 是一個隨機產生的雜訊向量（誤差）
    epsilon <- c(-1.24462014500259, 0.146172987456978, 1.56426869006839, -0.856920339050681,
        -1.15277300953772, 0.717919832604741, -0.270623615316431, -1.66281578024014,
        -1.15557078461633, -0.730253254897595)

    #' 我們讓y 參雜了雜訊。
    y <- ori_y + epsilon

    # 2. 根據公式算出 beta.hat 的估計值

    XtX <- t(X) %*% X
    Xty <- t(X) %*% y
    beta.hat <- solve(XtX, Xty)


    # 計算與比較差異
    x_difference <- beta.hat - beta


    # 3. 引入 CO2 資料表的資料
    X <- model.matrix(~ Type + Treatment + conc, CO2)

    #' 如此就建立了一個基於 Type、Treatment 和 conc 的矩陣

    #' 我們以 uptake 的值放入 y 之中，作為迴歸的目標
    y <- CO2$uptake

    # 4. 根據公式算出 co2_beta.hat 的估計值
    XtX <- t(X) %*% X
    Xty <- t(X) %*% y
    co2_beta.hat <- solve(XtX, Xty)

    y.hat <- X %*% co2_beta.hat

    # 5. 計算 correlation
    my.corr <- cor(X %*% co2_beta.hat, y)

    # 6. 用 lm() 函數進行線性迴歸
    g <- lm(uptake ~ Type + Treatment + conc, CO2)
    g.s <- summary(g)
    g.s$r.squared

    #' mode(g.s)顯示它是一個list。
    #' 請取出上述線性迴歸的 R-squared 數值，並且與 my.corr 的平方做比較

    # 提取 g.s 中的 R-squared 值
    R_squared_lm <- g.s$r.squared

    # 計算 my.corr^2 的值
    R_squared_calculated <- my.corr*my.corr

    # 比較兩者 提示可以使用 identical() 函數，然後取相同的 round ，例如 round(x, 10)
    R_squared_lm=round(R_squared_lm, 10)
    R_squared_calculated=round(R_squared_calculated, 10)
    R_squared_calculated <- as.numeric(R_squared_calculated)
    comparison <- identical(R_squared_lm, R_squared_calculated)


    # 使用 cars 資料表為例
    # 引入 cars 資料集
    data(cars)

    # 建立設計矩陣 X (這邊只有截距和 speed)
    X <- model.matrix(~speed, cars)
    
    # 將 dist 設為目標變數 y
    y <- cars$dist
    # 利用最小平方法的公式計算 beta 的估計值
    XtX <- t(X) %*% X
    Xty <- t(X) %*% y
    beta <- solve(XtX, Xty)

    # 使用估計的 beta 值來計算 y 的估計值
    y.hat <- X %*% beta
    # 計算 y 的估計值和實際 y 值之間的相關性
    my.corr_cars <- cor(X %*% beta, y)

    # 使用 R 的內建 lm() 函數進行線性回歸
    g_cars <- lm(dist ~ speed, cars)

    # 從 lm() 的結果中提取 R-squared
    g_cars.s <- summary(g_cars)
    R_squared_lm_cars <- g_cars.s$r.squared

    # 確認手動計算的 R-squared (即 my.corr_cars^2) 是否與 lm() 函數的結果相符
    rrcar <- as.numeric(my.corr_cars*my.corr_cars)
    rrcar <- round(rrcar, 10)
    R_squared_lm_cars <- round(R_squared_lm_cars, 10)
    is_identical <- identical(rrcar, R_squared_lm_cars)
    
    ans = list(
        ori_y = ori_y,
        beta_hat =  beta.hat,
        x_difference = x_difference,
        R_squared_lm = R_squared_lm,
        R_squared_calculated = R_squared_calculated,
        comparison = comparison,
        my_corr_cars = my.corr_cars,
        R_squared_lm_cars = R_squared_lm_cars,
        mean_total_length_per_species = is_identical
    )

}