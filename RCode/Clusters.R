library(quantmod)
library(igraph)
library(threejs)
library(crosstalk)
library(htmltools)
library(crosstool)
library(corrplot)

library(quantmod)
from="2012-05-17"
sym = c("AAPL", "ABBV", "ABT", "ACN", "AGN", "AIG", "ALL", "AMGN", "AMZN", "AXP",
        "BA", "BAC", "BIIB", "BK", "BLK", "BMY", "BRK.B", "C", "CAT", "CELG", "CL",
        "CMCSA", "COF", "COP", "COST", "CSCO", "CVS", "CVX", "DD", "DHR", "DIS", "DOW",
        "DUK", "EMR", "EXC", "F", "FB", "FDX", "FOX", "FOXA", "GD", "GE", "GILD", "GM",
        "GOOG", "GOOGL", "GS", "HAL", "HD", "HON", "IBM", "INTC", "JNJ", "JPM", "KHC",
        "KMI", "KO", "LLY", "LMT", "LOW", "MA", "MCD", "MDLZ", "MDT", "MET", "MMM",
        "MO", "MON", "MRK", "MS", "MSFT", "NEE", "NKE", "ORCL", "OXY", "PCLN", "PEP",
        "PFE", "PG", "PM", "PYPL", "QCOM", "RTN", "SBUX", "SLB", "SO", "SPG", "T",
        "TGT", "TWX", "TXN", "UNH", "UNP", "UPS", "USB", "UTX", "V", "VZ", "WBA",
        "WFC", "WMT", "XOM")

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
L = eigen(X, symmetric=TRUE)

plot(L$values, ylab="eigenvalues")
abline(v=10)

N = 10  # (use 1st 10 eigenvectors, set N larger to reduce regularization)
P = L$vectors[, 1:N] %*% ((1 / L$values[1:N]) * t(L$vectors[, 1:N]))
P = P / tcrossprod(sqrt(diag(P)))

threshold = 0.90
Q = P * (P > quantile(P, probs=threshold))                           # thresholded precision matrix
g = graph.adjacency(Q, mode="undirected", weighted=TRUE, diag=FALSE) # ...expressed as a graph

# The rest of the code lumps any singletons lacking edges into a single 'unassociated' group shown in gray
# (also assigning distinct colors to the other groups).
x = groups(cluster_louvain(g))
i = unlist(lapply(x, length))
d = order(i, decreasing=TRUE)
x = x[d]
i = i[d]
j = i > 1
s = sum(j)
names(x)[j] = seq(1, s)
names(x)[! j] = s + 1
grp = as.integer(rep(names(x), i))
clrs = c(rainbow(s), "gray")[grp[order(unlist(x))]]
g = set_vertex_attr(g, "color", value=clrs)

graphjs(g, vertex.size=0.2, vertex.shape=colnames(X), edge.alpha=0.5)