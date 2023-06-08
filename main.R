library(tidyverse)
library(dplyr)



#データはPWT10.01をExcel形式でダウンロードし、csvに変換したものを使用.
setwd(file.path("/", "Users", "ivanzinov", "Documents", "MATLAB", "q-macro-hw"))
sample.data<-read_csv("./q-macro-hw-upload/pwt1001.csv",locale = locale(encoding = "utf8"))

#ファイルのロードが上記の方法でできない時はこちらを使用して手作業で選択.
#fid<-file.choose()
#econ.data<-read_csv(fid,locale = locale(encoding = "utf8"))

#最終的に表示するデータフレームを作成.
Growth.Accounting.Chart <- data.frame(matrix(ncol = 6, nrow = 7))
colnames(Growth.Accounting.Chart) <- c("Country", "Growth Rate", "TFP Growth","Capital Deepening", "TFP Share", "Capital Share")
countries <- c("Canada", "France", "Germany", "Italy", "Japan", "UK", "US")
Growth.Accounting.Chart$Country <- countries



# 期間は1995~2019
start_year <- 1995
end_year <- 2019



#まずは国のデータだけを抽出する.
econ.data.CAN<-subset(econ.data,subset=country=="Canada")
econ.data.CAN<-subset(econ.data.CAN)
econ.data.CAN <- econ.data.CAN %>%
  filter(year >= start_year & year <= end_year)

#使用するデータをベクトルにまとめておく.
alpha <- 1-econ.data.CAN$labsh
hours <- econ.data.CAN$emp*econ.data.CAN$avh
y <- econ.data.CAN$rgdpna / hours
k <- econ.data.CAN$rkna / hours

#成長率を計算.
y.growth <- rep(0, 24)
for (i in 1:24) {
  y.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

k.growth <- rep(0, 24)
for (i in 1:24) {
  k.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

CapitalDeepning <- rep(0, 24)
for (i in 1:24) {
  CapitalDeepning[i] <- alpha[i]*k.growth[i]
}

#技術進歩は会計的な方法で算出.
A.growth <- rep(0,24)
for (i in 1:24) {
  A.growth[i] <- y.growth[i]-CapitalDeepning[i]
}

# パーセント表示に直す.
y.growth <- 100*y.growth
CapitalDeepning <- 100*CapitalDeepning
A.growth <- 100*A.growth

#今回は25期分の成長率(24コ)の平均値を最終的な表に使用する.
y.growth.average <- mean(y.growth)
A.growth.average <- mean(A.growth)
CapitalDeepning.average <- mean(CapitalDeepning)

TFPshare <- A.growth.average/(A.growth.average+CapitalDeepning.average)
CapitalShare <- CapitalDeepning.average/(A.growth.average+CapitalDeepning.average)

#データフレームへと代入.
n=1
  Growth.Accounting.Chart[n, "Growth Rate"] <- y.growth.average
  Growth.Accounting.Chart[n, "TFP Growth"] <- A.growth.average
  Growth.Accounting.Chart[n, "Capital Deepening"] <- CapitalDeepning.average
  Growth.Accounting.Chart[n, "TFP Share"] <- TFPshare
  Growth.Accounting.Chart[n, "Capital Share"] <- CapitalShare



#フランス
econ.data.FRA<-subset(econ.data,subset=country=="France")
econ.data.FRA<-subset(econ.data.FRA)
econ.data.FRA <- econ.data.FRA %>%
  filter(year >= start_year & year <= end_year)

#使用するデータをベクトルにまとめておく.
alpha <- 1-econ.data.FRA$labsh
hours <- econ.data.FRA$emp*econ.data.CAN$avh
y <- econ.data.FRA$rgdpna / hours
k <- econ.data.FRA$rkna / hours

#成長率を計算.
y.growth <- rep(0, 24)
for (i in 1:24) {
  y.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

k.growth <- rep(0, 24)
for (i in 1:24) {
  k.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

CapitalDeepning <- rep(0, 24)
for (i in 1:24) {
  CapitalDeepning[i] <- alpha[i]*k.growth[i]
}

#技術進歩は会計的な方法で算出.
A.growth <- rep(0,24)
for (i in 1:24) {
  A.growth[i] <- y.growth[i]-CapitalDeepning[i]
}

# パーセント表示に直す.
y.growth <- 100*y.growth
CapitalDeepning <- 100*CapitalDeepning
A.growth <- 100*A.growth

#今回は25期分の成長率(24コ)の平均値を最終的な表に使用する.
y.growth.average <- mean(y.growth)
A.growth.average <- mean(A.growth)
CapitalDeepning.average <- mean(CapitalDeepning)

TFPshare <- A.growth.average/(A.growth.average+CapitalDeepning.average)
CapitalShare <- CapitalDeepning.average/(A.growth.average+CapitalDeepning.average)

#データフレームへと代入.
n=2
Growth.Accounting.Chart[n, "Growth Rate"] <- y.growth.average
Growth.Accounting.Chart[n, "TFP Growth"] <- A.growth.average
Growth.Accounting.Chart[n, "Capital Deepening"] <- CapitalDeepning.average
Growth.Accounting.Chart[n, "TFP Share"] <- TFPshare
Growth.Accounting.Chart[n, "Capital Share"] <- CapitalShare



#ドイツ
econ.data.DEU<-subset(econ.data,subset=country=="Germany")
econ.data.DEU<-subset(econ.data.DEU)
econ.data.DEU <- econ.data.DEU %>%
  filter(year >= start_year & year <= end_year)

#使用するデータをベクトルにまとめておく.
alpha <- 1-econ.data.DEU$labsh
hours <- econ.data.DEU$emp*econ.data.CAN$avh
y <- econ.data.DEU$rgdpna / hours
k <- econ.data.DEU$rkna / hours

#成長率を計算.
y.growth <- rep(0, 24)
for (i in 1:24) {
  y.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

k.growth <- rep(0, 24)
for (i in 1:24) {
  k.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

CapitalDeepning <- rep(0, 24)
for (i in 1:24) {
  CapitalDeepning[i] <- alpha[i]*k.growth[i]
}

#技術進歩は会計的な方法で算出.
A.growth <- rep(0,24)
for (i in 1:24) {
  A.growth[i] <- y.growth[i]-CapitalDeepning[i]
}

# パーセント表示に直す.
y.growth <- 100*y.growth
CapitalDeepning <- 100*CapitalDeepning
A.growth <- 100*A.growth

#今回は25期分の成長率(24コ)の平均値を最終的な表に使用する.
y.growth.average <- mean(y.growth)
A.growth.average <- mean(A.growth)
CapitalDeepning.average <- mean(CapitalDeepning)

TFPshare <- A.growth.average/(A.growth.average+CapitalDeepning.average)
CapitalShare <- CapitalDeepning.average/(A.growth.average+CapitalDeepning.average)

#データフレームへと代入.
n=3
Growth.Accounting.Chart[n, "Growth Rate"] <- y.growth.average
Growth.Accounting.Chart[n, "TFP Growth"] <- A.growth.average
Growth.Accounting.Chart[n, "Capital Deepening"] <- CapitalDeepning.average
Growth.Accounting.Chart[n, "TFP Share"] <- TFPshare
Growth.Accounting.Chart[n, "Capital Share"] <- CapitalShare



#イタリア
econ.data.ITA<-subset(econ.data,subset=country=="Italy")
econ.data.ITA<-subset(econ.data.ITA)
econ.data.ITA <- econ.data.ITA %>%
  filter(year >= start_year & year <= end_year)

#使用するデータをベクトルにまとめておく.
alpha <- 1-econ.data.ITA$labsh
hours <- econ.data.ITA$emp*econ.data.CAN$avh
y <- econ.data.ITA$rgdpna / hours
k <- econ.data.ITA$rkna / hours

#成長率を計算.
y.growth <- rep(0, 24)
for (i in 1:24) {
  y.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

k.growth <- rep(0, 24)
for (i in 1:24) {
  k.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

CapitalDeepning <- rep(0, 24)
for (i in 1:24) {
  CapitalDeepning[i] <- alpha[i]*k.growth[i]
}

#技術進歩は会計的な方法で算出.
A.growth <- rep(0,24)
for (i in 1:24) {
  A.growth[i] <- y.growth[i]-CapitalDeepning[i]
}

# パーセント表示に直す.
y.growth <- 100*y.growth
CapitalDeepning <- 100*CapitalDeepning
A.growth <- 100*A.growth

#今回は25期分の成長率(24コ)の平均値を最終的な表に使用する.
y.growth.average <- mean(y.growth)
A.growth.average <- mean(A.growth)
CapitalDeepning.average <- mean(CapitalDeepning)

TFPshare <- A.growth.average/(A.growth.average+CapitalDeepning.average)
CapitalShare <- CapitalDeepning.average/(A.growth.average+CapitalDeepning.average)

#データフレームへと代入.
n=4
Growth.Accounting.Chart[n, "Growth Rate"] <- y.growth.average
Growth.Accounting.Chart[n, "TFP Growth"] <- A.growth.average
Growth.Accounting.Chart[n, "Capital Deepening"] <- CapitalDeepning.average
Growth.Accounting.Chart[n, "TFP Share"] <- TFPshare
Growth.Accounting.Chart[n, "Capital Share"] <- CapitalShare



#日本
econ.data.JPN<-subset(econ.data,subset=country=="Japan")
econ.data.JPN<-subset(econ.data.JPN)
econ.data.JPN <- econ.data.JPN %>%
  filter(year >= start_year & year <= end_year)

#使用するデータをベクトルにまとめておく.
alpha <- 1-econ.data.JPN$labsh
hours <- econ.data.JPN$emp*econ.data.CAN$avh
y <- econ.data.JPN$rgdpna / hours
k <- econ.data.JPN$rkna / hours

#成長率を計算.
y.growth <- rep(0, 24)
for (i in 1:24) {
  y.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

k.growth <- rep(0, 24)
for (i in 1:24) {
  k.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

CapitalDeepning <- rep(0, 24)
for (i in 1:24) {
  CapitalDeepning[i] <- alpha[i]*k.growth[i]
}

#技術進歩は会計的な方法で算出.
A.growth <- rep(0,24)
for (i in 1:24) {
  A.growth[i] <- y.growth[i]-CapitalDeepning[i]
}

# パーセント表示に直す.
y.growth <- 100*y.growth
CapitalDeepning <- 100*CapitalDeepning
A.growth <- 100*A.growth

#今回は25期分の成長率(24コ)の平均値を最終的な表に使用する.
y.growth.average <- mean(y.growth)
A.growth.average <- mean(A.growth)
CapitalDeepning.average <- mean(CapitalDeepning)

TFPshare <- A.growth.average/(A.growth.average+CapitalDeepning.average)
CapitalShare <- CapitalDeepning.average/(A.growth.average+CapitalDeepning.average)

#データフレームへと代入.
n=5
Growth.Accounting.Chart[n, "Growth Rate"] <- y.growth.average
Growth.Accounting.Chart[n, "TFP Growth"] <- A.growth.average
Growth.Accounting.Chart[n, "Capital Deepening"] <- CapitalDeepning.average
Growth.Accounting.Chart[n, "TFP Share"] <- TFPshare
Growth.Accounting.Chart[n, "Capital Share"] <- CapitalShare



#英国
econ.data.GBR<-subset(econ.data,subset=country=="United Kingdom")
econ.data.GBR<-subset(econ.data.GBR)
econ.data.GBR <- econ.data.GBR %>%
  filter(year >= start_year & year <= end_year)

#使用するデータをベクトルにまとめておく.
alpha <- 1-econ.data.GBR$labsh
hours <- econ.data.GBR$emp*econ.data.CAN$avh
y <- econ.data.GBR$rgdpna / hours
k <- econ.data.GBR$rkna / hours

#成長率を計算.
y.growth <- rep(0, 24)
for (i in 1:24) {
  y.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

k.growth <- rep(0, 24)
for (i in 1:24) {
  k.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

CapitalDeepning <- rep(0, 24)
for (i in 1:24) {
  CapitalDeepning[i] <- alpha[i]*k.growth[i]
}

#技術進歩は会計的な方法で算出.
A.growth <- rep(0,24)
for (i in 1:24) {
  A.growth[i] <- y.growth[i]-CapitalDeepning[i]
}

# パーセント表示に直す.
y.growth <- 100*y.growth
CapitalDeepning <- 100*CapitalDeepning
A.growth <- 100*A.growth

#今回は25期分の成長率(24コ)の平均値を最終的な表に使用する.
y.growth.average <- mean(y.growth)
A.growth.average <- mean(A.growth)
CapitalDeepning.average <- mean(CapitalDeepning)

TFPshare <- A.growth.average/(A.growth.average+CapitalDeepning.average)
CapitalShare <- CapitalDeepning.average/(A.growth.average+CapitalDeepning.average)

#データフレームへと代入.
n=6
Growth.Accounting.Chart[n, "Growth Rate"] <- y.growth.average
Growth.Accounting.Chart[n, "TFP Growth"] <- A.growth.average
Growth.Accounting.Chart[n, "Capital Deepening"] <- CapitalDeepning.average
Growth.Accounting.Chart[n, "TFP Share"] <- TFPshare
Growth.Accounting.Chart[n, "Capital Share"] <- CapitalShare



#米国
econ.data.USA<-subset(econ.data,subset=country=="United States")
econ.data.USA<-subset(econ.data.USA)
econ.data.USA <- econ.data.USA %>%
  filter(year >= start_year & year <= end_year)

#使用するデータをベクトルにまとめておく.
alpha <- 1-econ.data.USA$labsh
hours <- econ.data.USA$emp*econ.data.CAN$avh
y <- econ.data.USA$rgdpna / hours
k <- econ.data.USA$rkna / hours

#成長率を計算.
y.growth <- rep(0, 24)
for (i in 1:24) {
  y.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

k.growth <- rep(0, 24)
for (i in 1:24) {
  k.growth[i] <- (y[i + 1] - y[i]) / y[i]
}

CapitalDeepning <- rep(0, 24)
for (i in 1:24) {
  CapitalDeepning[i] <- alpha[i]*k.growth[i]
}

#技術進歩は会計的な方法で算出.
A.growth <- rep(0,24)
for (i in 1:24) {
  A.growth[i] <- y.growth[i]-CapitalDeepning[i]
}

# パーセント表示に直す.
y.growth <- 100*y.growth
CapitalDeepning <- 100*CapitalDeepning
A.growth <- 100*A.growth

#今回は25期分の成長率(24コ)の平均値を最終的な表に使用する.
y.growth.average <- mean(y.growth)
A.growth.average <- mean(A.growth)
CapitalDeepning.average <- mean(CapitalDeepning)

TFPshare <- A.growth.average/(A.growth.average+CapitalDeepning.average)
CapitalShare <- CapitalDeepning.average/(A.growth.average+CapitalDeepning.average)

#データフレームへと代入.
n=7
Growth.Accounting.Chart[n, "Growth Rate"] <- y.growth.average
Growth.Accounting.Chart[n, "TFP Growth"] <- A.growth.average
Growth.Accounting.Chart[n, "Capital Deepening"] <- CapitalDeepning.average
Growth.Accounting.Chart[n, "TFP Share"] <- TFPshare
Growth.Accounting.Chart[n, "Capital Share"] <- CapitalShare

#表の下部につける平均値の行を作る.
vector <- c("average", 0, 0, 0, 0, 0)
vector[2] <- mean(Growth.Accounting.Chart$`Growth Rate`)
vector[3] <- mean(Growth.Accounting.Chart$`TFP Growth`)
vector[4] <- mean(Growth.Accounting.Chart$`Capital Deepening`)
vector[5] <- mean(Growth.Accounting.Chart$`TFP Share`)
vector[6] <- mean(Growth.Accounting.Chart$`Capital Share`)

Growth.Accounting.Chart.final <- rbind(Growth.Accounting.Chart, vector)

#最後に成果を表示.
print(Growth.Accounting.Chart.final)
