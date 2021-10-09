w = read.csv('D:/R_DATA/玩家喜好分析数据.csv', T, encoding = 'UTF-8')
rownames(w) = w[,1] # 给道具名称赋予行名称
library(ca)
w.ca = ca(w[,-1]) # 建立简单对应分析
w.ca #　查看结果

names(w.ca) # 查看因子分析输出的对象列表
w.ca$colnames # 查看列名称
w.ca$colcoord # 查看列的标准坐标


# 绘制对应分析的散点图
plot(w.ca$rowcoord[, 1], w.ca$rowcoord[, 2], pch=16, col=rgb(0.6, 0.3, 0.2),
     xlim = c(min(w.ca$rowcoord[, 1]), max(w.ca$rowcoord[, 1]) + 0.3),
     main = '玩家购买物品偏好分析')
text(w.ca$rowcoord[, 1], w.ca$rowcoord[, 2], labels = w.ca$rownames,
     cex = 0.8, pos = 4, col = rgb(0.6, 0.3, 0.2))
points(w.ca$rowcoord[, 1], w.ca$rowcoord[, 2], pch = 17, col='#007e7e')
text(w.ca$rowcoord[, 1], w.ca$rowcoord[, 2], labels = w.ca$colnames,
     cex = 0.8, pos = 4, col = '#007e7e')
















































