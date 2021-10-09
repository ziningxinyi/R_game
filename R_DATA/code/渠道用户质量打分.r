channel_score <- function(data,amount=T){
  
  # 进行指标的波动性打分  
  library(reshape)
  data <- cast(data,渠道名称~自然周)
  # 利用apply函数分渠道求出当前周与上周的差值
  x <- t(apply(data[,-1],1,diff))
  # 利用as.data.frame函将x转换成数据框形式
  x <- as.data.frame(x,row.names = as.character(data[,1]))
  # 利用colnames函数对x列名重新赋值
  colnames(x) <-  colnames(data[3:ncol(data)])
  # 找出最近四周的最大值
  # 自定义函数mystat求最近四周的最大值
  mystat <- function(x){
    m <- rep(0,(ncol(data)-1))
    for(i in 1:(ncol(data)-1)){
      if(i <=3){
        m[i] <- max(x[1:i])
      } else {
        m[i] <- max(x[(i-3):i])
      }
    }
    return(m)
  }
  # 利用apply函按分渠道求最近四周最大值
  y <- t(apply(data[,-1],1,mystat))
  # 利用as.data.frame函数将y转换成数据框形式
  y <- as.data.frame(y,row.names = as.character(data[,1]))
  # 利用colnames函数对y列名重新赋值
  colnames(y) <-  colnames(data[2:ncol(data)])
  # 计算波动变化得分
  reliability_score <- 5*round(x/y[,-1],3)
  reliability_score
  if(amount) {
    # 进行指标的量级打分
    # 利用colSums函数进行按列求和
    x <- colSums(data[,-1])
    x <- as.data.frame(matrix(rep(x,nrow(data)),nrow = nrow(data),byrow = T))
    amount_score <- 5*round(data[,-1]/x,3)
    rownames(amount_score) <- rownames(reliability_score)
    amount_score
    
    # 计算指标得分
    # 利用cumsum函数进行对波动变化值进行累积求和
    rs_cumsum <- apply(reliability_score,1,cumsum)
    rs_cumsum <- as.data.frame(t(rs_cumsum))
    score <- 10+rs_cumsum+amount_score[,-1] #起始分+波动变化得分+量级得分
    score[,colnames(data)[2]] <- 10
    score <- score[,c(ncol(score),1:ncol(score)-1)] # 改变列的顺序
  } else {
    # 利用cumsum函数进行对波动变化值进行累积求和
    rs_cumsum <- apply(reliability_score,1,cumsum)
    rs_cumsum <- as.data.frame(t(rs_cumsum))
    score <- 10+rs_cumsum #起始分+波动变化得分
    score[,colnames(data)[2]] <- 10
    score <- score[,c(ncol(score),1:ncol(score)-1)] # 改变列的顺序
  }
  return(score)
}


# 导入周渠道数据
load('D:/R_DATA/rawdata.RData')
str(rawdata)

# 对字段自然周转化为有序因子变量
levels = c(paste0('第', 1:length(unique(rawdata$自然周)), '周'))
rawdata$自然周 = factor(rawdata$自然周, levels = levels, ordered=T)
str(rawdata)

# 对A的渠道得分进行计算
channel_revenue = channel_score(rawdata[rawdata$游戏名称=='游戏A', c('渠道名称'                                        ,'自然周','周收入')])
#　计算周活跃用户得分
channel_active = channel_score(rawdata[rawdata$游戏名称=='游戏A', c('渠道名称',
                                                '自然周','周活跃用户数')])
#　计算第７日留存率得分
channel_newretation = channel_score(rawdata[rawdata$游戏名称=='游戏A',
                                        c('渠道名称','自然周','第7日留存率')])
# 计算周付费率得分
channel_payrate = channel_score(rawdata[rawdata$游戏名称=='游戏A',
                                        c('渠道名称','自然周','周付费率')])
channel_arppu = channel_score(rawdata[rawdata$游戏名称=='游戏A',
                                      c('渠道名称','自然周','周ARPPU')])

# 计算综合得分
channel_total = round(0.3*channel_revenue + 0.2*channel_active + 
                      0.2*channel_payrate + 
                      0.15*channel_arppu + 0.15*channel_newretation,2)
# 查看综合得分
channel_total

# 绘制A在各渠道的生存曲线
channel_total$渠道名称 = rownames(channel_total) # 增加渠道名称变量
library(reshape)
md = melt(channel_total, id='渠道名称') # 对数据进行重塑
md$week = ifelse(nchar(as.character(md$variable))==4,
                 substr(md$variable,2,3), substr(md$variable, 2, 2)) # 增加周数变量
md$week = as.numeric(md$week)
library(lattice)
xyplot(value~week|渠道名称, data=md, type=c('l', 'g'),
       lwd=2, layout=c(6,2)) # 绘制面板曲线图




















































































