library(dplyr)
library(Pareto)
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
# 生成数据表:如何分析WWW事业群商品经营表现情况，回顾一件商品的主要流程必然要经历的重要
# 环节是一个经营闭环，消费者浏览→仓储备货→下单成交→形成收益→仓储备货→……，这些
# 环节可以通过指标量化的方式，来体现了每一个商品的经营表现，在实际业务中结合了
# 产品ABC分类法，根据经营闭环，自号VSGP模型：pv、stock、GMV、profit。
# dep_name_1：一级部门， dep_name_2：二级部门，GMV：成交金额；profit：利润；
# stock_amt：库存金额
  skudata<-data.frame(sku_id=seq(from=10001, to=20000, by=1),
                    dep_name_1 = sample(d_1,10000,replace=TRUE),
                    dep_name_2 = as.factor(sample(d_2,10000,replace=TRUE)),
                    GMV = sample(y,10000,replace=TRUE),
                    profit =sample(z,10000,replace=TRUE),
                    stock_amt = sample(k,10000,replace=TRUE),
                    pv = sample(p,10000,replace=TRUE))
# 识别商品中主次，就像用考试分数排名l来区分出每个学生的情况一样，对于商品来说，
# 销量的大小就是区分他们排名前后的关键指标，而排名在前百分几的就是主要商品。因此，
# 首先将商品按照大小排序，并赋予排名序号，其次统计出每个序号在二级部门总SKU数量的第几百分位，
# 然后判断每个SKU的百分位落于哪个档次，将SKU分堆就会让我们在海量的商品SKU中能更看的清楚他们的经营效果了。
# 在产品ABC分类模型中，对商品进行分档是关键步骤，对商品区分主次需要有档次，假定商品SKU按照0-5%，5%-10%,10%-90%,分为三个档次,
# 分完档次后将VSGP聚合就可看到排名前百分几的商品对经营效果影响有多大，该方法又称为帕累托分析法，也就是我们常说的20%的人拥有80%
# 的财富，这里我们假定10%的SKU数量就能提供绝大部分的GMV，在互联网交易中由于长尾效应的存在，往往这个比例可能还会更小。
    skudata<-group_by(skudata,dep_name_2)
    skudata_1 <- mutate(skudata,sku_rank = row_number(rank(desc(GMV))))
    freq <- count(skudata_1,dep_name_2)
    skudata_2 <- merge(skudata_1,freq,by.x = "dep_name_2",by.y = "dep_name_2",all.x = TRUE)%>%
      mutate(sku_p= sku_rank/n) %>%
      mutate(sku_ABC = ifelse(sku_p<=0.05,"TOP5%",ifelse(sku_p<=0.1,"TOP10%","BOTTOM90%")))
#结果呈现
    v1 <- ggplot(data=skudata_2,aes(dep_name_2,GMV,fill=sku_ABC))+
      geom_bar(stat="identity",position="stack",width=0.7,size=0.25)
    v2 <- ggplot(data=skudata_2,aes(dep_name_2,pv,fill=sku_ABC))+
      geom_bar(stat="identity",position="stack",width=0.7,size=0.25)
    v3 <- ggplot(data=skudata_2,aes(dep_name_2,stock_amt,fill=sku_ABC))+
      geom_bar(stat="identity",position="stack",width=0.7,size=0.25)
    v4 <- ggplot(data=skudata_2,aes(dep_name_2,profit,fill=sku_ABC))+
      geom_bar(stat="identity",position="stack",width=0.7,size=0.25)
    grid.arrange(v1,v2,v3,v4,ncol=2,nrow=2)
# 由于数据是随机生成，现实状况中数据的规律并非像图中所显示的那样
# 在业务场景中，每种商品会拥有独立的pv转换特点，当消费者
# 下单后，仓库需要保证有充足的货源可以调配，这就涉及到了备货的库存金额stock_amt
# 我们可以衍生出相对值指标，PV现货率将他定义为衡量商品流量和是否有货关系的指标。
# 通常所统计出来的日均库存金额会大于成交金额GMV，但是二者之间的差值应该在一个合理的范围内
# ，太接近则备货风险会提升，太大则说明商品可能存在滞销积压的危险，因此又可以衍生出两个相对值
# 指标：售罄率和库存周转天数。这四个指标中，GMV和stock_amt最接近，利润profit远小于GMV，而pv作为
# 销售的最前端环节，下单才能转换成GMV，通常会远高于GMV值，形成第一个坡度，而每件商品还会有运营成本和费用，
# 这部分的开支也是不容小觑的，最后能留在口袋里的利润是第二个坡度。根据产品ABC分类模型，在现有的
# 资源条件下，就需要在10%与90%的商品中进行决策了，下游环节每减小一个坡度就可能让经营损益上升一个台阶。