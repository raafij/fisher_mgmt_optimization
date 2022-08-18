############ 

library(quantmod)
library(dplyr)

### Fisher's Current Portfolio

AAPL_monthlyreturns <- monthlyReturn(getSymbols("AAPL", from="2016-8-22", auto.assign = FALSE)) 
MSFT_monthlyreturns <- monthlyReturn(getSymbols("MSFT", from="2016-8-22", auto.assign = FALSE))
AMZN_monthlyreturns <- monthlyReturn(getSymbols("AMZN", from="2016-8-22", auto.assign = FALSE))
GOOGL_monthlyreturns <- monthlyReturn(getSymbols("GOOGL", from="2016-8-22", auto.assign = FALSE))
VCIT_monthlyreturns <- monthlyReturn(getSymbols("VCIT", from="2016-8-22", auto.assign = FALSE))
ADBE_monthlyreturns <- monthlyReturn(getSymbols("ADBE", from="2016-8-22", auto.assign = FALSE))
CRM_monthlyreturns <- monthlyReturn(getSymbols("CRM", from="2016-8-22", auto.assign = FALSE))
ASML_monthlyreturns <- monthlyReturn(getSymbols("ASML", from="2016-8-22", auto.assign = FALSE))
V_monthlyreturns <- monthlyReturn(getSymbols("V", from="2016-8-22", auto.assign = FALSE))
NFLX_monthlyreturns <- monthlyReturn(getSymbols("NFLX", from="2016-8-22", auto.assign = FALSE))

joined_monthlyreturns <- merge.xts(AAPL_monthlyreturns, MSFT_monthlyreturns, AMZN_monthlyreturns,
                                   GOOGL_monthlyreturns, VCIT_monthlyreturns, ADBE_monthlyreturns,
                                   CRM_monthlyreturns, ASML_monthlyreturns, V_monthlyreturns,
                                   NFLX_monthlyreturns)

colnames(joined_monthlyreturns) <- c("AAPL_ROR", "MSFT_ROR", "AMZN_ROR",
                                     "GOOGL_ROR", "VCIT_ROR", "ADBE_ROR",
                                     "CRM_ROR", "ASML_ROR", "V_ROR",
                                     "NFLX_ROR", "PORT_ROR")

### Correlation between the portfolio's 10 securities

PORT_correlation <- cor(joined_monthlyreturns, use="complete.obs")

riskfree <- 0.0001

AAPL_sharpe <- (mean(joined_monthlyreturns$monthly.returns)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns)
MSFT_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.1)
AMZN_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.2)
GOOGL_sharpe <- (mean(joined_monthlyreturns$monthly.returns.3)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.3)
VCIT_sharpe <- (mean(joined_monthlyreturns$monthly.returns.4)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.4)
ADBE_sharpe <- (mean(joined_monthlyreturns$monthly.returns.5)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.5)
CRM_sharpe <- (mean(joined_monthlyreturns$monthly.returns.6)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.6)
ASML_sharpe <- (mean(joined_monthlyreturns$monthly.returns.7)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.7)
V_sharpe <- (mean(joined_monthlyreturns$monthly.returns.8)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.8)
NFLX_sharpe <- (mean(joined_monthlyreturns$monthly.returns.9)-riskfree)/
  sd(joined_monthlyreturns$monthly.returns.9)

#### Current Portfolio Allocation

## Weights:
AAPL_alloc <- (6.36/31.57) 
MSFT_alloc <- (5.06/31.57)
AMZN_alloc <- (4.04/31.57)
GOOGL_alloc <- (3.13/31.57)
VCIT_alloc <- (3.15/31.57) 
ADBE_alloc <- (2.14/31.57)
CRM_alloc <- (2.07/31.57)
ASML_alloc <- (1.91/31.57)
V_alloc <- (1.88/31.57)
NFLX_alloc <- (1.83/31.57) ## they add up to 1 

## Creating the portfolio

joined_monthlyreturns <- as.data.frame(joined_monthlyreturns)%>%
  mutate(portfolio_cur = 
           AAPL_alloc*AAPL_monthlyreturns +
           MSFT_alloc*MSFT_monthlyreturns +
           AMZN_alloc*AMZN_monthlyreturns+
           GOOGL_alloc*GOOGL_monthlyreturns+
           VCIT_alloc*VCIT_monthlyreturns+
           ADBE_alloc*ADBE_monthlyreturns+
           CRM_alloc*CRM_monthlyreturns+
           ASML_alloc*ASML_monthlyreturns+
           V_alloc*V_monthlyreturns+
           NFLX_alloc*NFLX_monthlyreturns)

### Expected return, Standard Deviation and Sharpe Ratio for Fisher's current portfolio

port_cur_sharpe <- (mean(joined_monthlyreturns$portfolio_cur)-riskfree)/
  sd(joined_monthlyreturns$portfolio_cur)


############# Portfolio Optimization (find out optimal weights) ################

library(quantmod)
library(tseries)
library(stats)
library(quadprog)

enddate <- "2022-1-1"
t<-1351 #The first time you run this, you'll see the error so adjust the t with the requested number

myvector <- c()
nstocks <- 3
pricinglist <- as.data.frame(matrix(ncol=nstocks, nrow=t))
colnames(pricinglist) <- c("MSFT", "ASML", "GOVT")

for (i in 1:(ncol(pricinglist))){
  current_ticker <- colnames(pricinglist)[i]
  newtable <- getSymbols(current_ticker, src = "yahoo", from="2016-8-22", to=enddate, auto.assign=FALSE)
  pricinglist[,i] <- newtable[,6]
}


newpricingdataset <- pricinglist

dailyROR <- as.data.frame(matrix(ncol=ncol(newpricingdataset), nrow=nrow(newpricingdataset)-25))
colnames(dailyROR) <- colnames(pricinglist)
for (c in 1:(ncol(newpricingdataset))){
  for (r in 1:(nrow(newpricingdataset)-25)){
    dailyROR[r,c] <- log(as.numeric(newpricingdataset[(r+25),c])/as.numeric(newpricingdataset[r,c]))
  }
}

#calculating Expected(R) for all securities 
averet <- as.matrix(dailyROR[nrow(dailyROR),], nrow=1)
#calculating covariance matrix
rcov <- cov(dailyROR[(nrow(dailyROR)-125):(nrow(dailyROR)),]) #125 stands for 6 trading months
target.r <- 1/1000
#using solver to get to optimal weights


effFrontier = function(averet, rcov, nports, shorts, wmax, wmin)
{
  mxret <- max(averet)
  mnret <- -mxret
  n.assets <- ncol(averet)
  reshigh <- rep(wmax, n.assets)
  reslow <- rep(wmin, n.assets)
  min.rets <- seq(mnret, mxret, length.out=nports)
  vol <- rep(NA, nports)
  ret <- rep(NA, nports)
  pw <- data.frame(matrix(ncol=nports, nrow=n.assets))
  for (i in 1:nports)
  {
    port.sol <- NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[i], covmat=rcov,   reshigh = reshigh, reslow= reslow, shorts=F)
        , silent=T)
    if(!is.null(port.sol))
    {
      vol[i] <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[i] <- averet %*% port.sol$pw
      pw[,i] <- port.sol$pw
    }
  }
  return(list(vol=vol, ret = ret, weights = pw))
  
}

maxSharpe <- function(averet, rcov, shorts=F, wmax=0.2, min.weight=0.01)
{
  optim.callback=function(param, averet, rcov, reshigh, reslow, shorts)
  { 
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=param, covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=shorts),silent=T)
    if(is.null(port.sol)) { ratio= 10^9} else 
    {
      m.return <- averet %*% port.sol$pw
      m.risk <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ratio <- m.return/m.risk
      assign("w", port.sol$pw, inherits=T)
    }
    return(ratio)
  }
  
  ef <- effFrontier(averet=averet, rcov=rcov, shorts=shorts, wmax=wmax, nports = 100, wmin=min.weight)
  n <- ncol(averet)
  reshigh <- rep(wmax, n)
  reslow <- rep(min.weight, n)
  
  max.sh <- which.max(ef$ret/ef$vol)
  
  
  if(is.na(ef$ret[max.sh-1])){lowerinterval<-ef$ret[max.sh]}else{lowerinterval <- ef$ret[max.sh-1]}
  if(is.na(ef$ret[max.sh+1])){upperinterval<-ef$ret[max.sh]}else{upperinterval <- ef$ret[max.sh+1]}
  
  w <- rep(0, ncol(averet))
  xmin <- optimize(f=optim.callback, interval = c(lowerinterval, upper=upperinterval), 
                   averet=averet, rcov=rcov, reshigh=reshigh, reslow=reslow, shorts=shorts)
  return(w)
  return(xmin)
}

z <- maxSharpe(averet, rcov, shorts=F, wmax=0.4)
z


#### New Portfolio ####

MSFT_new_returns <- monthlyReturn(getSymbols("MSFT", from="2016-8-22", auto.assign = FALSE))
ASML_new_returns <- monthlyReturn(getSymbols("ASML", from="2016-8-22", auto.assign = FALSE))
GOVT_new_returns <- monthlyReturn(getSymbols("GOVT", from="2016-8-22", auto.assign = FALSE))

joined_monthlyreturns_new <- merge.xts(MSFT_new_returns, ASML_new_returns, GOVT_new_returns)
colnames(joined_monthlyreturns_new) <- c("MSFT_ROR", "ASML_ROR", "GOVT_ROR")

MSFT_new_alloc <- 0.4000000 
ASML_new_alloc <- 0.2122124  
GOVT_new_alloc <- 0.3877876

## Correlation 
New_PF_correlation <- cor(joined_monthlyreturns_new, use="complete.obs")

## Sharpe Ratio:
riskfree <- 0.0001

## New Expected Return
MSFT_er <- (mean(joined_monthlyreturns_new$MSFT_ROR)-riskfree)
ASML_er <- (mean(joined_monthlyreturns_new$ASML_ROR)-riskfree)
GOVT_er <- (mean(joined_monthlyreturns_new$GOVT_ROR)-riskfree)

## New Standard Deviation
MSFT_sigma <- sd(joined_monthlyreturns_new$MSFT_ROR)
ASML_sigma <- sd(joined_monthlyreturns_new$ASML_ROR)
GOVT_sigma <- sd(joined_monthlyreturns_new$GOVT_ROR)

MSFT_sharpe <- MSFT_er/MSFT_sigma
ASML_sharpe <- ASML_er/ASML_sigma
GOVT_sharpe <- GOVT_er/GOVT_sigma

## Portfolio
joined_monthlyreturns_new <- as.data.frame(joined_monthlyreturns_new) %>% 
  mutate(portfolio_new = 
           MSFT_new_alloc*MSFT_ROR + 
           ASML_new_alloc*ASML_ROR+
           ifelse(is.na(GOVT_new_alloc*GOVT_ROR), 0, GOVT_new_alloc*GOVT_ROR))


PORT_new_er <- (mean(joined_monthlyreturns_new$portfolio_new)-riskfree)
PORT_new_sigma <- sd(joined_monthlyreturns_new$portfolio_new)
PORT_new_sharpe <- PORT_new_er/PORT_new_sigma




########### New Portfolio's Efficient Frontier ##################
library(data.table)
library(scales)
library(ggplot2)
library(quantmod)
library(reshape2)

# importing tickers

MSFT <- "MSFT"
ASML <- "ASML"
GOVT <- "GOVT"

mydf1 <- getSymbols(MSFT, from = "2016-8-22", auto.assign=FALSE)
#mydf1 <- as.data.frame(getSymbols(MSFT, from = "2016-8-22", auto.assign=FALSE))
mydf2 <- getSymbols(ASML, from = "2016-8-22", auto.assign=FALSE)
#mydf2 <- as.data.frame(getSymbols(ASML, from = "2016-8-22", auto.assign=FALSE))
mydf3 <- getSymbols(GOVT, from = "2016-8-22", auto.assign=FALSE)
#mydf3 <- as.data.frame(getSymbols(GOVT, from = "2016-8-22", auto.assign=FALSE))

combined_df <- cbind(mydf1[,4], mydf2[,4], mydf3[,4])

dt <- as.data.frame(combined_df)
colnames(dt) <- c(MSFT, ASML, GOVT)
dt$date = as.Date(index(mydf1))
dt <- melt(dt, id="date")
colnames(dt) <- c("date", "ticker","price")
dt <- data.table(dt)
dt[, idx_price := price/price[1], by = ticker]

ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")

# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.002)) +
  scale_x_continuous(label = percent, limits = c(0, 0.03))

################################################
#### Two risky assets portfolio
################################################
# load the data
AAPL_select <- "MSFT" 
MSFT_select <- "ASML"
AMZN_select <- "GOVT"

mydf1 <- as.data.frame(monthlyReturn(getSymbols(AAPL_select, from = "2016-8-22", auto.assign=FALSE)))
mydf2 <- as.data.frame(monthlyReturn(getSymbols(MSFT_select, from = "2016-8-22", auto.assign=FALSE)))
mydf3 <- as.data.frame(monthlyReturn(getSymbols(AMZN_select, from = "2016-8-22", auto.assign=FALSE)))


combined_df <- cbind(mydf1[,1], mydf2[,1], mydf3[,1])

df <- as.data.frame(combined_df)
colnames(df) <- c("x","y", "z")
df$date = as.Date(rownames(mydf1))
# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)

# III) covariance
cov_xy <- cov(df$x, df$y)

# create 1000 portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the two assets
two_assets <- data.table(wx = x_weights,
                         wy = 1 - x_weights)

# calculate the expected returns and standard deviations for the 1000 possible portfolios
two_assets[, ':=' (er_p = wx * er_x + wy * er_y,
                   sd_p = sqrt(wx^2 * sd_x^2 +
                                 wy^2 * sd_y^2 +
                                 2 * wx * (1 - wx) * cov_xy))]
two_assets



# lastly plot the values
ggplot() +
  geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier with MSFT and ASML") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x]), labels = percent)


######################################################
### Three risky assets portfolio:
#########################################################
# load the data

# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)

# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)

# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))

three_assets[, wz := 1 - wx - wy]


# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]

# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier with MSFT, ASML, and GOVT") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)


                        
                        