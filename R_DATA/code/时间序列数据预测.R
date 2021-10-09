# 导入数据
revenue = read.csv('C:/Users/25001/Desktop/R_DATA/收入数据.csv', fileEncoding = 'UTF-8')
head(revenue)
# 转化为时间序列对象
revenue.ts = ts(revenue[,2], frequency = 12, start = c(2014,1))

# plot.ts(revenue.ts, xlab='时间', ylab='收入')
# abline(lm(revenue.ts~time(revenue.ts)), col='red', lty=2, lwd=2)

# 时间序列平稳性检验
library(fUnitRoots)
unitrootTest(revenue.ts) # p<0.05

# 进行一阶差分运算
revenue.ts.dif = diff(revenue.ts)
# 验证其平稳性
unitrootTest(revenue.ts.dif)

# 长期趋势，季节变动，周期变动，噪声变动时间序列的分解图
revenue.ts.decompose = decompose(revenue.ts)
plot(revenue.ts.decompose)

# 存在明显的趋势性和季节性，采用winters线性和季节性指数平滑建模
HoltWintersModel = HoltWinters(revenue.ts, alpha = TRUE, beta = TRUE, gamma = TRUE)
HoltWintersModel
plot(HoltWintersModel, lty=1, lty.predicted=2)  # 模型拟合图

# 预测未来6个月
library(forecast)
HoltWintersForecast =  forecast:::forecast.HoltWinters(HoltWintersModel, h=6,level=c(80,95))
HoltWintersForecast
# 对残差进行检验
# acf(HoltWintersForecast$residuals, lag.max = 20)
# 模型预测图
plot(HoltWintersForecast)

# 另外方法预测ARIMA（p,d,q)模型参数
library(forecast)
fit = auto.arima(revenue.ts)
fit
fit.forecast = forecast(fit, h=6)
fit.forecast
plot(fit.forecast)




















































































