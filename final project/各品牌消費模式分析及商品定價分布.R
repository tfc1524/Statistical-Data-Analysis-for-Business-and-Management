install.packages('data.table')
library(data.table)
data <- data.table::fread('Statistical-Data-Analysis-for-Business-and-Management\\final project\\final_project_data.csv', 
                          encoding='UTF-8',
                          header=T)
attach(data)

# 統計商品單價分布
library(readxl)
library(dplyr)
channels_product <- group_by(data, channel, name)
channels_product <-summarize(channels_product,
                  uniprice=mean(uniprice, na.rm=TRUE), 
                  totprice=mean(totprice, na.rm=TRUE), 
                  invo_price=mean(invo_price, na.rm=TRUE))
channels_product <- subset(channels_product, uniprice <= 500)
attach(channels_product)

library(ggplot2)
library(forcats)
library(dplyr)
# Overlaid histograms
# product uniprice distribution of channels_product
ggplot(channels_product, aes(x=uniprice, group=channel, fill=channel)) +
  geom_density(alpha=0.5, position="identity") +
  labs(title="Price density curve",x="Uniprice", y = "Density")+
  theme_minimal()

# 了解各品牌消費者輪廓
invo_aggregate <- group_by(data, channel, deviceid, invo_idx)
invo_aggregate <- summarize(invo_aggregate,
                            totprice=sum(totprice, na.rm=TRUE))

# 消費頻率
channels_freq <- group_by(invo_aggregate, channel, deviceid)
channels_freq <- summarize(channels_freq,
                           freq=n())
channels_freq <- group_by(channels_freq, channel)
channels_freq <- summarize(channels_freq,
                           avg_freq=mean(freq))

# 單次消費品項數
channels_goods_num <- group_by(data, channel, invo_idx)
channels_goods_num <- summarize(channels_goods_num,
                                size=n())
channels_goods_num <- group_by(channels_goods_num, channel)
channels_goods_num <- summarize(channels_goods_num,
                                avg_goods_num=mean(size))

# 單次消費總金額
channels_totprice <- group_by(invo_aggregate, channel)
channels_totprice <- summarize(channels_totprice,
                               med_totprice=median(totprice))

channels_consumer <- merge(channels_freq, channels_goods_num, by='channel')
channels_consumer <- merge(channels_consumer, channels_totprice, by='channel')
attach(channels_consumer)

write.csv(channels_consumer,file="Statistical-Data-Analysis-for-Business-and-Management\\final project\\消費模式分析.csv",row.names = FALSE)

# 消費頻次
mutate(channels_consumer, name = fct_reorder(channel, desc(avg_freq))) %>%
  ggplot(aes(x=name, y=avg_freq, fill=channel)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=avg_freq, label=round(avg_freq, 2)), vjust=1.6, 
            color="black", size=3.5)+
  scale_fill_manual(values = c('#E0426D', '#008E88', '#E8810F', '#D7006C'))+
  theme_minimal()

# 單次消費品項數
mutate(channels_consumer, name = fct_reorder(channel, desc(avg_goods_num))) %>%
  ggplot(aes(x=name, y=avg_goods_num, fill=channel)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=avg_goods_num, label=round(avg_goods_num, 2)), vjust=1.6, 
            color="black", size=3.5)+
  scale_fill_manual(values = c('#E0426D', '#008E88', '#E8810F', '#D7006C'))+
  theme_minimal()

# 單次消費總金額
mutate(channels_consumer, name = fct_reorder(channel, desc(med_totprice))) %>%
ggplot(aes(x=name, y=med_totprice, fill=channel)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=med_totprice, label=round(med_totprice, 2)), vjust=1.6, 
            color="black", size=3.5)+
  scale_fill_manual(values = c('#E0426D', '#008E88', '#E8810F', '#D7006C'))+
  theme_minimal()
