logindata = read.csv('D:/R_DATA/logindata.csv')
str(logindata) #　逐行显示列内容
dim(logindata) # 数据维度

# 哑变量处理，所有变量变为数值型变量
library(caret)
dmy = dummyVars(~., data=logindata)
dmyTsrf = data.frame(predict(dmy, newdata = logindata))
dim(dmyTsrf)
str(dmyTsrf)


# 导入自定义的求相关系数函数
source('D:/R_DATA/CorrelationFunction.R')
corMasterList = flattenSquareMatrix(cor.prob(dmyTsrf))
# 按照相关系数的绝对值进行降序排列
corList = corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList, 10))

# 提取与‘是否付费.是’的相关系数大于0.04的记录
selectedSub = subset(corList, (abs(cor) > 0.04 & i %in% c('是否付费是')))
bestsub = as.character(selectedSub$j)
# 绘制相关系数图
library(corrplot)
corrplot.mixed(cor(dmyTsrf[,c('是否付费是',bestsub)]),
               lower = 'ellipse', upper = 'number', tl.pos='lt', diag='u')











































