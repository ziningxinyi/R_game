# 导入RFM数据
rfm_data = read.csv('C:/Users/25001/Desktop/R_DATA/RFM_data.csv', fileEncoding = 'UTF-8')
# 将最后付费日期转换为日期格式
rfm_data$last_date = paste(substr(rfm_data$last_date, 1, 4), 
                           substr(rfm_data$last_date, 5, 6),
                           substr(rfm_data$last_date, 7, 8), sep='/')
rfm_data$last_date = as.Date(rfm_data$last_date, '%Y/%m/%d')
# 增加距离统计日的相隔天数
rfm_data$time_internal = max(rfm_data$last_date) - rfm_data$last_date
# 查看前6行
head(rfm_data)

# 建立新的衍生指标
rfm_data$tagR = ifelse(rfm_data$time_internal <= 0, 1, 0)
rfm_data$tagF = ifelse(rfm_data$pay_cnt <= 1, 0, 1)
rfm_data$tagM = ifelse(rfm_data$pay_mnt <= 6, 0, 1)
head(rfm_data)

# 给用户打上标签
rfm_data$type = '一般挽留用户'
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==1, 'type']='重要保持客户'
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==1, 'type']='重要发展客户'
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==1, 'type']='一般保持客户'
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==1, 'type']='一般客户'
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==1, 'type']='重要挽留客户'
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==1, 'type']='一般客户'
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==1, 'type']='低价值客户'
head(rfm_data)

# 统计不同用户群的人数
library(sqldf)
rfm_data_sum = sqldf('select type, count(distinct player_id) as usr_cnt,
                     sum(pay_mnt) as pay_mnt_sum from rfm_data group by type')
# 按照付费金额进行降序排列
rfm_data_sum = rfm_data_sum[order(rfm_data_sum$pay_mnt_sum, decreasing = T),]
rfm_data_sum$usr_cnt_rate = paste0(round(rfm_data_sum$usr_cnt*100/
                                           sum(rfm_data_sum$usr_cnt), 2) ,'%')
rfm_data_sum$usr_mnt_sum_rate = paste0(round(rfm_data_sum$pay_mnt_sum*100/
                                                sum(rfm_data_sum$pay_mnt_sum),2),'%')
knitr::kable(rfm_data_sum)


# 绘制柱状图
library(RColorBrewer)
par(mfrow = c(2, 1))
barplot(rfm_data_sum$pay_mnt_sum, col=brewer.pal(12, 'Set3')[1:8],
        border = F, main = '付费金额统计')
barplot(rfm_data_sum$usr_cnt, col=brewer.pal(9, 'Set1')[1:8],
        border = F, names.arg = rfm_data_sum$type,
        main = '付费人数统计')
par(mfrow = c(1, 1))



















