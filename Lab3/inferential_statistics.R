data <- c(1905, 3112, 2312, 2725, 2545, 2981, 2677, 2525, 2627, 2600, 2370, 2857, 2962, 2545, 2675, 2184, 2529, 2115, 2332, 2442)
# 导入数据

mean_estimate <- mean(data)
mean_estimate
# 平均年保费的点估计值即样本数据的均值，计算并输出

std_error <- sd(data)/sqrt(20)
# 计算标准误
ci_95 <- t.test(data, conf.level = 0.95)$conf.int
ci_99 <- t.test(data, conf.level = 0.99)$conf.int
# 分别计算置信区间
ci_95
ci_99
# 输出置信区间

install.packages("readxl")
library(readxl)
QAdata <- read_excel('QAdata.xlsx')
# 安装readxl包读取存储在表格中的数据

test_result <- list()
for (i in 1:4){
    test_result[[i]] <- t.test(QAdata[,i], mu = 12, sd = 0.21, conf.level = 0.99, alternative = 'two.sided')
}

statistic_pvalue_df <- data.frame(Statistic = NULL, P.Value = NULL)

for (i in 1:4) {
  # 提取检验统计量（t值）
  statistic <- test_result[[i]]$statistic
  # 提取显著性水平（p值）
  pvalue <- test_result[[i]]$p.value
  # 将提取的数据添加到数据框中
  statistic_pvalue_df <- rbind(statistic_pvalue_df, data.frame(Statistic = statistic, P.Value = pvalue, row.names = paste('Sample', i)))
}

write.csv(statistic_pvalue_df, file = 'statistic_pvalue.csv', fileEncoding = 'UTF-8')

