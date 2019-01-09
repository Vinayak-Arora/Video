#Please execute the file Code.R before executing this one.

library(dplyr)
library(ggplot2)

dest <- "C://Workspace//data.csv"
df <- read.csv(dest, stringsAsFactors = FALSE)

df <- na.omit(df)
#for connected = true, dataframe is named - df.true
df.true <- df[df$connected == 'true', ]
#p2p_cdn <- df.true$p2p/df.true$cdn
df.true$p2p_cdn <- df.true$p2p/(df.true$cdn+df.true$p2p)
#Remove p2p = 0 values from the dataset for quantile values analysis
df.true <- df.true[df.true$p2p_cdn > 0 , ]

df.true$X.stream <- as.factor(as.character(df.true$X.stream))

#Plotting boxplots for isp, browser and stream_id to represent p2p_cdn quantile values
print(ggplot(df.true, aes(browser, p2p_cdn))+stat_boxplot(geom ='errorbar') + geom_boxplot(aes(fill=(browser)),outlier.shape =NA))
print(ggplot(df.true, aes(isp, p2p_cdn))+stat_boxplot(geom ='errorbar') +  geom_boxplot(aes(fill=(isp)),outlier.shape =NA))
print(ggplot(df.true, aes(X.stream, p2p_cdn))+stat_boxplot(geom ='errorbar') +  geom_boxplot(aes(fill=(X.stream)),outlier.shape =NA) + guides(fill=FALSE) + labs( x = 'stream_id'))

#Plotting barplots for isp, browser and stream_id to represent percentage of p2p_cdn = 0 values
print(ggplot(browser.p2p0.df,aes(browser, as.numeric(as.character(Percentage))))+geom_bar(stat = 'identity', aes(fill= browser))+ geom_text(aes(label = Percentage), vjust = -.4) + labs(y = 'p2p0 Percentage'))
print(ggplot(isp.p2p0.df,aes(isp, as.numeric(as.character(Percentage))))+geom_bar(stat = 'identity', aes(fill= isp))+ geom_text(aes(label = Percentage), vjust = -.4) + labs(y = 'p2p0 Percentage'))
print(ggplot(X.stream.p2p0.df,aes(X.stream, as.numeric(as.character(Percentage))))+geom_bar(stat = 'identity', aes(fill= X.stream))+ geom_text(aes(label = Percentage), vjust = -.4) + labs(y = 'p2p0 Percentage', x = 'stream_id', colour = 'stream_id') + guides(fill=FALSE))
