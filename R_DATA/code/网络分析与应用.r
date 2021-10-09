# 联通分支分类
g = erdos.renyi.game(20, 1/20)
clusters(g)


# 金钱转移网络关系图1
trade = data.frame(from=c(1:100, 101:105, 106), to=c(rep(c(101:105),20), rep(106,5),107))
trade_g = graph.data.frame(trade)
V(trade_g)$color = 'Skyblue2'
V(trade_g)[101:105]$color = 'gold'
V(trade_g)[106]$color = 'red'
V(trade_g)[107]$color = 'green'
V(trade_g)$size = c(rep(8,100), rep(12,7),)
tkplot(trade_g, vertex.label=c(rep(NA, 100),101:107), edge.arrow.size=1)

# 金钱转移网络关系图2
betweenness(trade_g)
round(evcent(trade_g)$vector,3)

# 金钱转移网络关系图3
trade2 = rbind(trade, data.frame(from=rep(107,15),to=201:215))
trade_g2 = graph.data.frame(trade2)
V(trade_g2)$color = 'Skyblue2'
V(trade_g)[101:105]$color = 'gold'
V(trade_g)[106:107]$color = 'red'
V(trade_g)[108:122]$color = 'green'
V(trade_g2)$size=c(rep(8,100), rep(12,7), rep(8,15))
tkplot(trade_g2, vertex.label=c(rep(NA,100), 101:107, rep(NA,15)), edge.arrow.size=1)
betweenness(trade_g2)
round(evcent(trade_g2)$vector, 3)

# 社交群体划分
G = graph.data.frame(trade2, directed=F)
fc1 = multilevel.community(G) # 多层聚类
fc2 = edge.betweenness.community(G) # 边中间性聚类
fc3 = walktrap.community(G) # 随机游走聚类
fc4 = infomap.community(G) # infomap算法聚类
fc5 = fastgreedy.community(G) # 快速贪婪聚类
fc6 = label.propagation.community(G) #　标签传播
fc_list = list(fc1, fc2, fc3, fc4, fc5, fc6)
algorithm_list = c('multilevel', 'edge.betweenness', 'walktrap',
                   'infomap', 'fastgreedy','label.propagation')
V(G)$label = c(rep(NA, 105), 106:107, rep(NA,15))
V(G)$size = c(rep(10, 105), 30, 30, rep(10, 15))
V(G)$label.color = 'white'
V(G)$label.font = 2
par(mfrow=c(2,3), mar=c(1,1,2,1))
for(i in 1:6) {
  plot(fc_list[[i]], G, main=algorithm_list[i])
}


#　以交易金币数作为边的权重
E(G)$weight = c(rep(100,100), rep(2000,5), 5000, rep(50,15)) # 模拟工作室打金及卖金
for(i in 1:6){
  plot(fc_list[[i]], G, main=algorithm_list[i])
}


# 添加8条边,并添加权重
GG = G + edges(c(1,2, 2,3, 3,4, 4,5, 6,7, 7,8, 8,9, 9,10)) # 新添加8条边
E(GG)[122:129]$weight = 100 # 给新增加的边添加权重

FC1 = multilevel.community(GG) # 多层聚类
FC2 = edge.betweenness.community(GG) # 边中间性聚类
FC3 = walktrap.community(GG) # 随机游走聚类
FC4 = infomap.community(GG) # infomap算法聚类
FC5 = fastgreedy.community(GG) # 快速贪婪聚类
FC6 = label.propagation.community(GG) #　标签传播

FC_list = list(FC1, FC2, FC3, FC4, FC5, FC6)
par(mfrow=c(2,3), mar=c(1, 1, 2, 1))
for (i in 1:6) {
  plot(FC_list[[i]], GG, main=algorithm_list[i])
}















