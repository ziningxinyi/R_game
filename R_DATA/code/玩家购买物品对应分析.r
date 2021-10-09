library(ca)
# x = read.csv('C:/Users/25001/Desktop/R_DATA/玩家购买物品的数据.csv')
# head(x)
CA = ca(x)
CA
plot(CA)

# 购买物品偏好分析
# 查看对应分析结果的所有变量,并找到需要的变量
names(CA)
# 把物品对应的点及其名称在二维图中画出来
plot(CA$rowcoord[,1],CA$rowcoord[,2],pch=16,col='slateblue2', xlim=c(-3,2),
     xlab='Dim1(83.19%)',ylab='Dim2(15.23%)',main='玩家购买物品偏好分析')
text(CA$rowcoord[,1], CA$rowcoord[,2], lalels=CA$rownames, pos=4, cex=0.7)
# 用二级作图函数points把剩下的5类玩家标记出来
points(CA$colcoord[,1], CA$rolcoord[,2], pch=17, col='darkred', cex=1.2)
text(CA$colcoord[,1], CA$colcoord[,2], labels=CA$colnames, pos=4, col='darkred',
     cex=0.8)
# 添加分界线
abline(h=0, lty=2, col='deepskyblue')
abline(v=0, lty=2, col='deepskyblue')




























































































