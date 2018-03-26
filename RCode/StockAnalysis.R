library("quantmod")

##?quantmod

getSymbols('^FB',src='google')
summary(FB)
View(FB)
head(FB)
tail(FB)

class(FB)
# ?xts
# ?zoo

#multiple symbols
basket_symbols <- c('YELP', 'AMZN', 'AAPL')

getSymbols(basket_symbols, src='google')

dim(FB)
plot(FB)
names(FB)

lineChart(FB, line.type = 'h', theme='white')
barChart(FB, bar.type = 'hlc')
candleChart(FB,multi.col = TRUE, subset = '2017')

#time subset
#all january 2015
candleChart(FB,multi.col = TRUE, subset = '2017-01')
#everything after feb
candleChart(FB,multi.col = TRUE, subset = '2017-02::')
#january till June
candleChart(FB,multi.col = TRUE, subset = '2017-01::2017-06')

candleChart(FB,
            theme=chartTheme('white',up.col='blue',dn.col='darkred'),
            subset = '2015-01::2015-06')

#install.packages('TTR')
library(TTR)

chartSeries(FB, theme='white')
#adding simple moving Average
addSMA(n=200)
#adding the rate of change 
addROC(n=200)

#better or faster moving average
chartSeries(FB, theme='white')
addSMA(n=20)
addROC(n=20)

#adding Bolinger Bands and commodity channel index
chartSeries(FB,
            theme='white',
            TA='addVo();addBBands();addCCI()',
            subset='2015')


#custom technical indicators 
chartSeries(FB, theme=chartTheme('white'), up.col="black",
            dn.col="black")
#exponential moving average 
FB.EMA.20<- EMA(FB$FB.Close, n=20)
head(FB.EMA.20,n = 21)
tail(FB.EMA.20)
FB.EMA.100<- EMA(FB$FB.Close, n=100)
addTA(FB.EMA.20, on=1, col = "red")
addTA(FB.EMA.100, on=1, col = "blue")
addTA(FB.EMA.20 - FB.EMA.100, col = "blue",
      type='h', legend = '20-100MA')
