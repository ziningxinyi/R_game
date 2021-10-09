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


# 数据转换
data_new = c()
for (i in 1:nrow(data_matrix_new)) {
  item.i = colnames(data_matrix_new)[which(data_matrix_new[i,]==1)]
  item.i.num=length(item.i)
  from = c();to = c()
  for (m in 1:(item.i.num-1)) {
    from = c(from, item.i[-c((item.i.num-m+1):item.i.num)])
    to = c(to, item.i[-c(1:m)])
  }
  data_new = rbind(data_new, matrix(c(from, to), ncol = 2))
}
data_new = data.frame(data_new)
head(data_new)

# sqldf汇总相同的记录,对统计后的数据按照频数进行降序排序
library(sqldf)
data_count = sqldf('select X1,X2,count(*) from data_new group by X1,X2')
colnames(data_count) = c('from', 'to', 'qty')
data_count = data_count[order(data_count$qty, decreasing = T),]
head(data_count)



# 选取data_count前100行进行社群结构分析
library(igraph)
G = graph.data.frame(data_count[1:100,], directed=F)
E(G)$weight = data_count[1:100, 'qty']

fc1 = multilevel.community(G) # 多层聚类
fc2 = edge.betweenness.community(G) # 边中间性聚类
fc3 = walktrap.community(G) # 随机游走聚类
fc4 = infomap.community(G) # infomap算法聚类
fc5 = spinglass.community(G) # 自旋转玻璃社群聚类
fc6 = label.propagation.community(G) # 标签传播

fc_list = list(fc1, fc2, fc3, fc4, fc5, fc6)
algorithm_list = c('多层聚类','边中间性聚类','随机游走聚类','infomap算法聚类'                       ,'自旋转玻璃社群聚类','标签传播')

par(mfrow=c(2,3))
for (i in 1:6) {
  plot(fc_list[[i]], G, main = algorithm_list[[i]])
}
par(mfrow=c(1,1))











































































