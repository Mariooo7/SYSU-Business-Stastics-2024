library(openxlsx)
DATA <- read.xlsx('DATA.xlsx')
DATA <- DATA[, colSums(is.na(DATA)) == 0]
DATA <- DATA[2:nrow(DATA),]
str(DATA)
DATA <- data.frame(lapply(DATA, as.numeric))
str(DATA)
data_returns <- as.data.frame(lapply(DATA[,-1], function(x) diff(log(x))))
data_returns

# 个股的β系数可以通过简单线性回归模型确定。因变量是股票超额收益率，自变量是股票市场超额收益率。本例以沪深300指数的收益率为股票市场超额收益率的测度，并且利用日度数据建立一个简单线性回归模型。所估计的回归方程斜率是股票的β系数，所估计的回归方程截距可作为股票的α收益近似估计。本例数据文件“数据-沪深300指数、成分股和SHIBOR2019年1月1日至2023年12月29日收盘价.xlsx”提供了沪深300指数及其成分股和SHIBOR隔夜的日收盘价序列。指数及成分股的收益率可通过收盘价序列的对数差分计算得到。

#（1）计算沪深300指数成份股日收益率之间的相关系数并进行简要分析。若以收益率之间相关系数最大的股票为配对交易股票，你会选择哪些股票对？
library(dplyr)
corMatrix <- cor(data_returns[,4:ncol(data_returns)])
df_pairs <- data.frame(stock1 = character(), stock2 = character(), cors = numeric(), stringsAsFactors = FALSE)
index = 0
for (i in 2:nrow(corMatrix)){
  for (j in 1:(i-1)){
    stock1 <- rownames(corMatrix)[i]
    stock2 <- colnames(corMatrix)[j]
    cors <- corMatrix[i,j]
    index = index + 1
    df_pairs <- rbind(df_pairs, data.frame(stock1 = stock1, stock2 = stock2, cors = cors))
  }
}
df_pairs_sorted <- df_pairs[order(-abs(df_pairs$cors)), ]
library(openxlsx)
write.xlsx(df_pairs_sorted, "pairs_correlation_sorted.xlsx")


#（2）计算沪深300指数所有成分股的α和β值，并进行简要分析。在一个上升的股票市场上，预期哪些股票将会有最好的业绩？在一个下跌的股票市场上，预期哪些股票保值将会最佳？若以最大α或最小β为选股准则，你会选择哪些股票构建投资组合？

for (i in 2:ncol(DATA)){
  DATA[, i] <- DATA[, i] - DATA$SHIBOR隔夜
}
DATA_excess <- cbind(DATA$沪深300, DATA[, 4:ncol(DATA)])
data_excess_returns <- as.data.frame(lapply(DATA_excess, function(x) diff(log(x))))


alpha_beta <- data.frame(stock = names(DATA_excess)[-1], alpha = numeric(128), beta = numeric(128))

# 对每个股票执行回归分析
for (i in 1:nrow(alpha_beta)) {
  stock_excess_returns <- data_excess_returns[, i + 1]
  index_excess_returns <- data_excess_returns$DATA.沪深300
  regression <- lm(stock_excess_returns ~ index_excess_returns)
  alpha_beta$alpha[i] <- coef(regression)[1]
  alpha_beta$beta[i] <- coef(regression)[2]
}

# 在上升的股票市场上，预期具有较高α值的股票将会有最好的业绩，因为它们在市场上涨时能够提供超额回报。
# 在下跌的股票市场上，预期具有较低β值的股票保值将会最佳，因为它们与市场的相关性较低，波动性较小。
write.xlsx(alpha_beta, "alpha_beta.xlsx")

#（3）评价股票收益率有多少能被股票市场收益率解释。
# 理论上，贝塔系数能够解释股票收益率中的系统性风险部分，这是与市场整体相关的风险，而非系统性风险则包括公司特有风险等，这部分风险通常不能通过市场收益率来解释。

r_squared_matrix <- matrix(NA, nrow = ncol(data_returns), ncol = 1)

# 循环计算每只股票的R²
for (i in 3:ncol(data_returns)) {
  # 建立回归模型
  model <- lm(data_returns[, i] ~ data_returns$沪深300)
  # 计算拟合优度
  r_squared_matrix[i, 1] <- summary(model)$r.squared
}

# 将R²矩阵转换成数据框
r_squared_df <- as.data.frame(r_squared_matrix)
r_squared_df <- r_squared_df %>% slice(-c(1, 2))
r_squared_df <- cbind(r_squared_df, names(data_returns)[-c(1:2)])
colnames(r_squared_df) <- c('r_squared', 'stock')
str(r_squared_df)

install.packages('latex2exp')
library(latex2exp)
# 使用直方图可视化 R² 值分布
library(ggplot2)
title <- TeX("各股票收益率被市场收益率解释的程度 $R^2$")
ggplot(r_squared_df, aes(x = stock, y = r_squared, fill = r_squared))+
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low = 'blue', high = 'red', name = TeX('$R^2$'))+
  labs(title = title, x = "股票", y = TeX('$R^2$')) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))

r_squared_sorted <- r_squared_df %>%
  arrange(desc(r_squared))
r_squared_sorted$stock <- factor(r_squared_sorted$stock, levels = r_squared_sorted$stock)
title1 <- TeX("各股票收益率被市场收益率解释的程度 $R^2$(降序）")
ggplot(r_squared_sorted, aes(x = stock, y = r_squared, fill = r_squared))+
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low = 'blue', high = 'red', name = TeX('$R^2$'))+
  labs(title = title, x = "股票", y = TeX('$R^2$')) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))
       

#（4）选择有代表性的股票，构建β对冲策略，评价该策略的绩效。
# 通过观察alpha_beta表，观察到所有股票的beta都为正，故选取beta较小的股票与beta较高的股票组合构建beta对冲策略，使投资组合的beta降低，即降低了市场风险。从表中选择有代表性的股票，beta最高的一支股票为 泸州老窖：1.4480788， 最低的为 长江电力：0.2672449，二者按1:1组合可以构建一个beta对冲策略，现评价该策略的绩效。
beta1 <- 1.4480788
beta2 <- 0.2672449
alpha1 <- 0.001072794
alpha2 <- 0.0003098278
# y1 <- beta1*x + alpha1
# y2 <- beta2*x + alpha2
beta <- (beta1 + beta2)/2
alpha <- (alpha1 + alpha2)/2
alpha
beta

# 计算被选取股票的超额收益率和风险
excess_returns1 <- data_excess_returns$泸州老窖
excess_returns2 <- data_excess_returns$长江电力
risk1 <- sd(excess_return1)
risk2 <- sd(excess_return2)
# 计算组合超额收益率和风险
portfolio_excess_returns <- 0.5*excess_returns1 + 0.5*excess_returns2
portfolio_risk <- sd(portfolio_excess_returns)
# 计算沪深300超额收益率和风险
hs_excess_returns <- data_excess_returns$DATA.沪深300
hs_risk <- sd(hs_excess_returns)
# 计算夏普比率
sharpe_ratio <- mean(portfolio_excess_returns) / portfolio_risk - mean(hs_excess_returns) / hs_risk

result <- data.frame(c(mean(portfolio_excess_returns), mean(hs_excess_returns), mean(excess_return1), mean(excess_return2)), c(portfolio_risk, hs_risk, risk1, risk2))
rownames(result) <- c('组合', '沪深300', '泸州老窖', '长江电力')
colnames(result) <- c('平均超额收益率', '风险')
print(result)

