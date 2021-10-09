data = read.csv('C:/Users/25001/Desktop/R_DATA/玩家购物数据.csv', header = TRUE, fileEncoding = 'utf-8')
# 利用cast对数据进行重组
library(reshape)
data_matrix = cast(data, player_id~product_name, value='qty')
# 查看前3行5列数据
data_matrix[1:3,1:5]

# 进行替换将NA替换成0,其他数字为1
data_matrix_new = apply(data_matrix[,-1],2, function(x) {ifelse(is.na(x), 0, 1)})
# 对矩阵名称,列名称赋值
data_matrix_new = matrix(data_matrix_new, nrow = dim(data_matrix_new)[1],
                         ncol = dim(data_matrix_new)[2],
                         dimnames = list(data_matrix[,1],colnames(data_matrix)[-1]))
# 查看前3行5列数据
data_matrix_new[1:3,1:5]

# as函数将矩阵转成事务型
library(arules)
data_class = as(data_matrix_new,'transactions')
inspect(data_class[1:6])
# 汇总信息
summary(data_class)

# 设置一页多图
par(mfrow=c(2,1))
# 输出支持度support大于0.05的项集的支持度频率图
# itemFrequencyPlot(data_class,support=0.05, main='support大于0.05的项集支持度频率图')
# 输出支持度support最大的前20个项集的支持度频率图
itemFrequencyPlot(data_class,topN=20,main='support最大的前20个项集的支持度频率图')
par(mfrow=c(1,1))

# arules函数建立关联规则rules
rules = apriori(data_class, parameter = list(support=0.005,confidence=0.1,
                                             target='rules',minlen=2))
summary(rules)

# 相对提升度大于2的关联图形
rules_lift = subset(rules, subset=lift>2)
library(arulesViz)
plot(rules_lift,method='graph', control=list(nodeCol=grey.colors(10),
                                             edgeCol=grey(.7),alpha=1))


# 绘制分组矩阵
plot(rules_lift,method = 'grouped',control = list(col=grey.colors(10)))



# 将规则导出本地
write(rules, 'C:/Users/25001/Desktop/R_DATA/rules.txt',sep='|',row.names=F)











