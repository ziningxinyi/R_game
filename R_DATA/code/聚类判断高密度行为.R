# 数据探索
dim(Moder1)
head(Moder1)

# 进行时间类型转换
class(Moder1$object)
length(unique(Moder1$object))
class(Model1$action_time)
Moder1[1,2]-Moder1[2,2]
as.POSIXct(Moder1[,2]) = Moder1[,2] # 时间类型转换
Moder1[2,2]-Moder1[1,2] # 时间类型的变量之间可以进行运算

# 数据处理
# 分类统计各个object下的最小时间
min_time = aggregate(Moder1$action_time, by=list(Moder1$object), FUN=min)
names(min_time) = c('object', 'min_time')
summary(min_time)
#数据合并
Object_mintime = merge(Moder1, min_time, by='object')
Object_mintime[,4] = Object_mintime$action_time - Object_mintime$min_time
names(Object_mintime)[4] = 'scale_time'
head(Object_mintime, 10)

#　模型过程
# 对每一个对象的时间进行密度聚类
dsa_60 = tapply(Object_mintime$scale_time, Object_mintime$object, dbscan, 60, 5)
# dsa_60$U6

# 提取密度聚类结果
extract.cluster = function(x){x$cluster}
Object_mintime[,5] = unlist(lapply(dsa_60, extract.cluster))
names('Object_mintime')[5] = 'cluster'
head(Object_mintime,10)






























































































