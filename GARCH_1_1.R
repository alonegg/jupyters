#############################
##########- 实验3 -##########
#############################


# GARCH(1, 1)的数值模拟
# 本实验的主要目标是学习GARCH(1, 1)的估计方法，以及理解GARCH模型的估计精确度
# 过程如下
# # 1. 先生成一个GARCH(1, 1)模型的参数，由参数和随机项模拟出收益率序列
# # 2. 然后估计模拟收益率序列的GARCH(1, 1)参数
# # 3. 对比估计的参数和实际的参数，分析GARCH(1, 1)模型的估计精度

library(fGarch)
setwd("~/../OneDrive/Works/金融科技兴趣小组/项目/Lab3_单变量GARCH/")

#####- 1. 计算theta_hat -#####
# 使用S&P500指数的收益率序列得到用于数值模拟的参数(omega_hat, alpha_hat, beta_hat)

# 读取数据，并计算S&P500指数的收益率
raw_data <- read.csv("./data.csv", stringsAsFactors=F)
SP500 <- raw_data[, "SP500"]
SP500_rtn <- SP500[2: length(SP500)] / SP500[1: (length(SP500) - 1)] - 1

# 对S&P500指数收益率进行GARCH(1, 1)估计，得到参数(omega_hat, alpha_hat, beta_hat)
# 使用“fGarch”包的garchFit函数进行估计
# 第一个参数为GARCH模型形式，使用GARCH(1, 1)记性估计
# 第二个参数为收益率序列
# trace=F表示估计过程不打印在屏幕上
raw_fit <- garchFit(~garch(1, 1), data=SP500_rtn, trace=F) 
# 将估计结果中的参数作为原始值
# 估计出来的raw_fit是一个S4对象，因此使用“@”来从raw_fit中取出需要的参数list
omega_hat <- raw_fit@fit$coef["omega"]
alpha_hat <- raw_fit@fit$coef["alpha1"]
beta_hat <- raw_fit@fit$coef["beta1"]


#####- 2. 数值模拟 -#####
# 使用得到的(omega_hat, alpha_hat, beta_hat)和随机项模拟出收益率序列

# 参数设定
total_num <- 5000  # 收益率的个数
set.seed(666)  # 设定随机数种子
simu_rtn <- rep(NA, total_num)  # 定义收益率序列
sigma <- rep(NA, total_num)  # 定义sigma序列
epsilon <- rnorm(total_num, 0 ,1)  # epsilon~N(0, 1)，生成服从标准正态分布的随机残差项

# t=1时的初始值
sigma[1] <- sqrt(omega_hat / (1 - alpha_hat - beta_hat))   # 使用sigma的长期均值作为其t=1时的值
simu_rtn[1] <- sigma[1] * epsilon[1]  # t=1时的收益率值

# 根据GARCH(1, 1)模型，使用上面的参数值和随机项循环得到模拟收益率
for (i in 2: total_num) {
  sigma[i] <- sqrt(omega_hat + alpha_hat * (simu_rtn[i - 1]) ^ 2 + beta_hat * (sigma[i - 1]) ^ 2)
  simu_rtn[i] <- sigma[i] * epsilon[i]
}


#####- 3. 计算模拟收益率下的参数  -#####
# 使用第二步得到的收益率序列，进行GARCH(1, 1)的估计

# 分别使用250, 500, 750, 1000个收益率进行估计，分析估计参数的收益率个数和参数精确度之间的关系
cal_num <- c(250, 500, 750, 1000)

# 定义输出结果的格式
result <- matrix(NA, length(cal_num) + 1, 4)
result[1, ] <- raw_fit@fit$coef  # 原始参数作为对比放入

# 使用循环的方式估计不同个数收益率序列的GARCH(1, 1)参数
for (i in 1: length(cal_num)) {
  cal_num_i <- cal_num[i]
  result[i + 1, ] <- garchFit(~garch(1, 1), data=simu_rtn[(5000 - cal_num_i + 1): 5000], trace=F)@fit$coef  # 这里的估计方式和第一部分一致
}


#####- 4. 优化输出结果形式  -#####
# 我们关注omega, alpha1, beta1的估计差别
result <- result[, -1]
rownames(result) <- paste0("theta_", c("hat", as.character(cal_num)))
colnames(result) <- c("omega", "alpha1", "beta1")

# 计算数值模拟估计出来的参数值和原始参数值的差
result_diff <- matrix(NA, length(cal_num) + 1, 3)
colnames(result_diff) <- c("omega_diff", "alpha1_diff", "beta1_diff")
for(i in 1: length(cal_num)) {
  result_diff[i + 1, ] <- result[i + 1, ] - result[1, ]
}
result <- cbind(result, result_diff)  # 把结果合并在一起

# 输出打印
print(result)





