# 孤立点分析
Object_num = data.frame(table(Object_mintime[, 'object']))
names(Object_num) = c('object', 'num')
isolated_points = data.frame(table(Object_mintime[which(
                  Object_mintime$cluster==0), 'object'])) # 每一个对象孤立点个数
names(isolated_points) = c('object', 'isolated_points')
isolated_points_pct = merge(Object_num, isolated_points, by='object', all.x=TRUE)
isolated_points_pct[which(is.na(isolated_points_pct$isolated_points                                                  )),'isolated_points'] = 0

isolated_points_pct$isolated_points_pct = round(isolated_points_pct[,3]/
                                                  isolated_points_pct[,2]*100,2) # 孤立点百分比运算
plot(density(isolated_points_pct[,4]), main='各对象孤立点百分比分布')
polygon(density(isolated_points_pct[,4]), col = 'lightsteelblue')



# 有效行为频率
# 统计所有对象各个cluster的最小时间与最大时间
all_object = aggregate(Object_mintime$scale_time,
                       by=list(Object_mintime$object, Object_mintime$cluster), FUN=min)
all_object[,4] = aggregate(Object_mintime$scale_time,
                           by=list(Object_mintime$object, Object_mintime$cluster), FUN=max)[,3]
all_object[,5] = as.numeric(all_object[,4]) - as.numeric(all_object[,3])
names(all_object) = c('object', 'cluster', 'cluster_mintime', 'cluster_maxtime', 'valid_time')
# 剔除孤立点
object_notzero = all_object[which(all_object$cluster!=0),]
# 统计每一个对象的有效时间
effective_time = aggregate(object_notzero$valid_time,
                           by=list(object_notzero$object), FUN=sum)
dim(effective_time)
# 计算有效行为频率
effective_time[,3] = data.frame(table(Object_mintime[which(Object_mintime$cluster!=0),'object']))[,2]
effective_time[,4] = effective_time[,2] / effective_time[,3]
names(effective_time) = c('object', 'sumtime', 'effective_actionnum', 'effective_fre')
plot(density(effective_time$effective_fre), main='有效的行为频率分布')
ploygon(density(effective_time$effective_fre), col='lightsalmon2')







































































































