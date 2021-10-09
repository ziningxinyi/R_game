# 关联分析1
dim(basket)
head(basket, 5)
# 数据按照用户切块后,转换成稀疏格式
basket_trans = as(split(basket[,'item_id'], basket[,'account_id']), 'transactions')
summary(basket_trans) # 从输出可以看到出现频数最多的5样物品
# 求关联规则,设置最低支持度0.02,最低置信度0.7, 最小项数2, 最大项数10,并且输出规则
rules = apriori(basket_trans, parameter=list(supp=0.02, conf=0.7, minlen=2,
                                             maxlen=10, target='rules'))
summary(rules)

# 关联分析2
inspect(rules[1:5])
inspect(sort(rules, by='support')[1:5]) # 按照支持度排序并且查看前5条规则
# 精确匹配rhs项包含item122物品的规则
choosel = subset(rules, subset=rhs %in% 'item122')
inspect(sort(choosel, by='support')[1:5])
# 模糊匹配rhs包含'11'字符串
choosel = subset(rules, subset=rhs %pin% '11' & confidence>0.9 & lift>=1.2)
inspect(sort(choose2, by='lift'))
# 关联可视化
library(arulesViz)
plot(rules, methods='graph', interactive=TRUE) # 可以自由移动摆布