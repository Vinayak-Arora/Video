library(dplyr)
df <- read.csv("C://Workspace//StreamRoot//data.csv", stringsAsFactors = FALSE)
#Remove empty rows
df <- na.omit(df)
#for connected = true, dataframe is named - df.true
df.true <- df[df$connected == 'true' 
              #& df$X.stream != 4
              , ]
#for connected = false, dataframe is named - df.false
df.false <- setdiff(df,df.true)

#Introduce a new column - p2p_cdn, whose value is - (p2p/p2p+cdn)
df.true$p2p_cdn <- df.true$p2p/(df.true$cdn+df.true$p2p)

#For columns - Stream, isp, browser, the percentage of rows having p2p
for(cname in colnames(df.true)[1:3]) {
  quantile.df <-  data.frame(matrix(nrow = 0, ncol = 4))
  for(value in as.factor(unique(df.true[ , cname]))) {
    total <-  df.true[df.true[,cname] == value, ]  %>%
      nrow()
    p2p0  <-  df.true[df.true$p2p == 0 & df.true[,cname] == value, ]  %>%
      nrow()

    percent <- round(((p2p0*100)/total), digits = 2) 
    quantile.df <- rbind(quantile.df , cbind(value,total, p2p0, percent))
  }
  rownames(quantile.df) <- NULL
  colnames(quantile.df) <- c(cname, 'Total_rows', 'p2p0_rows', 'Percentage')
  assign(paste(cname,'p2p0','df', sep = '.'), quantile.df)
}


df.true <- df.true[df.true$p2p_cdn > 0 , ]

for(cname in colnames(df.true)[1:3]) {
  quantile.df <-  data.frame(matrix(nrow = 0, ncol = 5))
    for(value in as.factor(unique(df.true[ , cname]))) {
      q <- round(quantile(df.true[df.true[ , cname] == value , 'p2p_cdn']), digits = 5)
      quantile.df <- rbind(quantile.df, cbind(value, q[2], q[3], q[4], sum(df.true[ , cname] == value)))
    }
  colnames(quantile.df) <- c(cname, 'Q1', 'Median', 'Q3', 'Number_of_rows')
  assign(paste(cname,'quantile','df', sep = '.'), quantile.df)
}