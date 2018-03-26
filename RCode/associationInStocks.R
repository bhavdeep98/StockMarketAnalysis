library(quantmod)
from="2012-05-17"
sym = c("AAPL", "ACN", "CSCO", "IBM", "INTC", "MSFT", "ORCL", "QCOM", "T", "TXN", "VZ")

prices = Map(function(n)
{
  print(n)
  tryCatch(getSymbols(n, src="google", env=NULL, from=from)[, 4], error = function(e) NA)
}, sym)
N = length(prices)
# identify symbols returning valid data
i = ! unlist(Map(function(i) is.na(prices[i]), seq(N)))
# combine returned prices list into a matrix, one column for each symbol with valid data
prices = Reduce(cbind, prices[i])
colnames(prices) = sym[i]

for(j in 1:ncol(prices)) prices[, j] = na.locf(prices[, j])       # fill in
prices = prices[, apply(prices, 2, function(x) ! any(is.na(x)))]  # omit stocks with missing data

log_returns = apply(prices, 2, function(x) diff(log(x)))

X = cor(log_returns)

library(corrplot)
corrplot(X,method = 'circle', type = 'upper')