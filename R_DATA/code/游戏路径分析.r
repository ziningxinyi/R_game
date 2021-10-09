data_click = read.csv("C:/Users/25001/Desktop/R_DATA/data_click.csv", header = F)
# 查看前6行
head(data_click)
# 提取有过11034点击行为的用户数据
targetuserid = unique(data_click[data_click$V3=='11034', 'V1'])
data_click_new = data_click[data_click$V1 %in% targetuserid,]

# arulesSequences包加载,将data_click_new数据换成事务型数据data_click_tran
library(arulesSequences)
tem_data = data.frame(click = data_click_new$V3)
tem_data$click = as.factor(tem_data$click)
data_click_tran = as(tem_data, 'transactions')
transactionInfo(data_click_tran)$sequenceID = data_click_new$V1
transactionInfo(data_click_tran)$eventID = data_click_new$V2
data_click_tran
# 查看事务型数据的前6行
inspect(data_click_tran[1:6])

#绘制商品的频率图
itemFrequencyPlot(data_click_tran, topN=20)

#找出所有开始打牌的路径
myrules = cspade(data_click_tran, parameter = list(support=0, maxlen=2), 
                 control = list(verbose=T))
myrules = sort(myrules, by = 'support') # 按照support进行排序
targetclick = paste0('.*click=11034', '[^\\}]*\\}>') #设置规则表达式
finalrules = myrules[grep(targetclick, as(myrules, 'data.frame')$sequence)]
inspect(finalrules[1:3]) #　查看序列前３条
nrow(finalrules)　#　计算序列个数

#　转换成数据框
finalrules.data.frame = as(finalrules[-1], 'data.frame')
# 查看前3行数据
head(finalrules.data.frame)
# 计算支持度的占比
finalrules.data.frame$percentage = finalrules.data.frame$support / 
  sum(finalrules.data.frame$support)
# 计算支持度的累计百分比
finalrules.data.frame$sum.percentage = cumsum(finalrules.data.frame$percentage)
# 筛选出累计百分比小于75%的序列数据
finalrules.data.frame = finalrules.data.frame[finalrules.data.frame$
                                                sum.percentage <= 0.75,]
# 查看符合结果额序列个数
nrow(finalrules.data.frame)
# 查看前六行数据
head(finalrules.data.frame)

# 提取关键点击按钮的事件id
clickid = substr(finalrules.data.frame$sequence, 9, 13)
# 查看关键事件id
clickid
# 计算关键点击按钮i的引导能力conf
conf = rep(1, length(clickid))
for (i in 1:length(clickid)) {
  n = myrules@info$nsequences
  nclickid_support = finalrules.data.frame[i, 'support']
  conf[i] = nclickid_support * n/
  nrow(data_click[data_click$V3==clickid[i],])
}
# 将关键事件id和支持度占比组成新的数据框result
result = data.frame(click = clickid,
                    percentage=round(finalrules.data.frame$percentage,3),conf=conf)
# 查看前6行
head(result)


barplot(result$percentage, names.arg=result$click, xlab='click', ylab = 'value', main = '引导用户进入开始打牌11034的重点事件id分析')

# 
# library(ggplot2)
# geom_bar(data = result)

# # 绘制支持度占比的垂直金字塔图
# library(reshape)
# md = melt(result, id='click') # 对result数据进行重组
# md$value[md$variable=='conf'] = md$value[md$variable=='conf']
# md = md[order(md$variable, decreasing = T),]
# # 绘制垂直金字塔图
# library(recharts)
# echartr(md, click, value, variable, type='vbar', subtype='stack' %>%
#           setTitle('引导用户进入开始打牌11034的重点事件id分析') %>%
#           setXAxis(axisLine=list(oneZero=T)) %>%
#           setYAxis(axisLable=list(formatter=JS('function(value) {return Math.abs(value);}'))))













