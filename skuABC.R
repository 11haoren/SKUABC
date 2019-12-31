library(dplyr)
library(ggplot2)
library(gridExtra)
# 确定指标随机数的范围
{
  y <- 1:50
  z <- 1:10
  k <- 1:10
  p <- 1:50
  d_1 <-c("WWW")
  d_2 <-c("A","B","C","D")
}
# 生成数据表:
# dep_name_1：一级部门， dep_name_2：二级部门，GMV：成交金额；profit：利润；
# stock_amt：库存金额
  skudata<-data.frame(sku_id=seq(from=10001, to=20000, by=1),
                    dep_name_1 = sample(d_1,10000,replace=TRUE),
                    dep_name_2 = as.factor(sample(d_2,10000,replace=TRUE)),
                    GMV = sample(y,10000,replace=TRUE),
                    profit =sample(z,10000,replace=TRUE),
                    stock_amt = sample(k,10000,replace=TRUE),
                    pv = sample(p,10000,replace=TRUE))
# SKUABC分档：0-5%，5-10%，10-90%
    skudata<-group_by(skudata,dep_name_2)
    skudata_1 <- mutate(skudata,sku_rank = row_number(rank(desc(GMV))))
    freq <- count(skudata_1,dep_name_2)
    skudata_2 <- merge(skudata_1,freq,by.x = "dep_name_2",by.y = "dep_name_2",all.x = TRUE)%>%
      mutate(sku_p= sku_rank/n) %>%
      mutate(sku_ABC = ifelse(sku_p<=0.05,"TOP5%",ifelse(sku_p<=0.1,"TOP10%","BOTTOM90%")))
# 结果呈现
    v1 <- ggplot(data=skudata_2,aes(dep_name_2,GMV,fill=sku_ABC))+
      geom_bar(stat="identity",position="stack",width=0.7,size=0.25)
    v2 <- ggplot(data=skudata_2,aes(dep_name_2,pv,fill=sku_ABC))+
      geom_bar(stat="identity",position="stack",width=0.7,size=0.25)
    v3 <- ggplot(data=skudata_2,aes(dep_name_2,stock_amt,fill=sku_ABC))+
      geom_bar(stat="identity",position="stack",width=0.7,size=0.25)
    v4 <- ggplot(data=skudata_2,aes(dep_name_2,profit,fill=sku_ABC))+
      geom_bar(stat="identity",position="stack",width=0.7,size=0.25)
    v5 <- grid.arrange(v1,v2,v3,v4,ncol=2,nrow=2)
# 图像保存    
    ggsave("v5.png",plot=v5)
