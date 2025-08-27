library(tidyverse)
library(tidyquant)
library(stringr)
library(rvest)
library(knitr)
library(kableExtra)
library(lubridate)
library(readr)
library(PerformanceAnalytics)
library(timetk)
library(corrplot)
library(scales)
library(cccp)
library(flexdashboard)
library(plotly)
library(RiskPortfolios)

symbols <- c("SPY", "QQQ", "IWM", "EEM", "EFA", "EWY", 
             "VGK", "AGG", "TIP", "DBC", "IEF", "TLT") 

start_date <- "2020-12-20"
end_date   <- "2025-05-31"

# 1. ETF 데이터 다운로드
etf_list <- map(symbols, ~ {
  tq_get(.x, get = "stock.prices", from = start_date, to = end_date) %>%
    select(date, adjusted) %>%
    rename(!! .x := adjusted)
})

# 2. 병합 (NA 포함)
ETF_agg <- reduce(etf_list, full_join, by = "date")

# 3. 가장 짧은 시계열 기준 날짜 계산
first_valid_date <- ETF_agg %>%
  pivot_longer(-date, names_to = "asset", values_to = "price") %>%
  group_by(asset) %>%
  summarise(first_date = min(date[!is.na(price)]), .groups = "drop") %>%
  summarise(first_valid_date = max(first_date, na.rm = TRUE)) %>%
  pull(first_valid_date)

# 4. 필터링 적용
ETF_agg <- ETF_agg %>%
  filter(date >= first_valid_date)

w <- c(0.3, 0.3, 0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0)

ETF_returns_tq <-
  ETF_agg %>% 
  gather(asset, prices, -date) %>% 
  group_by(asset) %>% 
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type = "arithmetic")             # "arithmetic", "log"

period.returns <- colnames(ETF_returns_tq[3])

ETF_returns_tq <- ETF_returns_tq %>%
  spread(asset, period.returns) %>%
  select(date, all_of(symbols)) %>%
  slice(-1)

ETF_returns_long <-
  ETF_returns_tq %>% 
  gather(asset, returns, -date) %>% 
  group_by(asset)

portfolio_returns <-
  ETF_returns_long %>% 
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")

ETF_rtn_xts <- ETF_returns_tq %>%
  tk_xts(date_var = date)

stack <- NULL


# Sharpe Ratio 계산 등 이용 risk-free rate (단기채권ETF 수익률로 대체 가능)
risk_free <- 0.03
for (m in 1:length(symbols)) {
  
  rtn_cum <- Return.cumulative(ETF_rtn_xts[, m]) # 누적수익률
  rtn_yr <- Return.annualized(ETF_rtn_xts[, m]) # 연율화 수익률(기하)
  
  std_yr <- StdDev.annualized(ETF_rtn_xts[, m]) # 연율화 변동성
  sr_yr <- SharpeRatio.annualized(ETF_rtn_xts[, m], Rf = risk_free, geometric = TRUE) # 기하평균 기준 연율화 수익률
  MDD <- maxDrawdown(ETF_rtn_xts[, m]) # 최대 낙폭
  cr_yr <- CalmarRatio(ETF_rtn_xts[, m])  # 연율화 수익률을 최대낙폭으로 나눈 값
  
  ETF_temp <- rbind(rtn_cum, rtn_yr, std_yr, sr_yr, MDD, cr_yr)
  stack <- cbind(stack, ETF_temp)
  
}

# 티블로 바꿔주고 테이블 정리
ETF_Perf <- stack %>% 
  tk_tbl(preserve_index = TRUE,
         rename_index = "Risk/Return") %>% 
  as_tibble()

stack2 <- NULL
for (m in 1:length(symbols)) {
  
  rtns_yr <- apply.yearly(ETF_rtn_xts[, m], Return.cumulative) # 연도별 수익률
  # apply.yearly() 함수 내에 적용될 함수를 Return.cumulative로 설정하면 연도별 수익률 계산 가능
  
  stack2 <- cbind(stack2, rtns_yr)
  
}

# 티블로 바꿔주고 테이블 정리
ETF_rtns_yr <- stack2 %>% 
  tk_tbl(preserve_index = TRUE,
         rename_index = "Year") %>% 
  as_tibble()


covmat <-
  cov(ETF_rtn_xts)
# calculate portfolio standard deviation
sd_portfolio <-
  sqrt(t(w) %*% covmat %*% w)
# calculate marginal contribution of each asset
marginal_contribution <-
  w %*% covmat / sd_portfolio[1, 1]
# multiply marginal by weights vector
component_contribution <-
  marginal_contribution * w
# divide by total standard deviation to get percentages
component_percentages <-
  component_contribution / sd_portfolio[1, 1] %>% 
  round(., 3)

component_percentages <-
  component_percentages %>% 
  as_tibble() %>% 
  gather(asset, contribution) %>% 
  mutate(weights = w) %>% 
  gather(type, percent, -asset) %>% 
  group_by(type)


# 평균 수익률 계산
mean_ret <- colMeans(ETF_rtn_xts)

# covariance matrix 계산, annualize
n_ann <- 252

cov_mat <- cov(ETF_rtn_xts) * n_ann

# random weights 만들기, uniform distribution 난수
wts <- runif(n = length(symbols)) 

# 합계가 1이 되도록 만들기
wts <- wts / sum(wts)

# annualized portfolio returns 계산
port_returns <- (sum(wts * mean_ret) + 1) ^ n_ann - 1

# annualized standard deviation 계산(연율화된 cov_mat 사용)
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

sharpe_ratio <- (port_returns - risk_free) / port_risk %>% 
  round(., 2)

# sharpe_ratio <- port_returns/port_risk

## n random portfolios
num_port <- 500
# num_port <- 500

# Creating a matrix to store the weights
all_wts <- matrix(nrow = num_port,
                  ncol = length(symbols))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)

# loop n times
for (i in seq_along(port_returns)) {
  
  # print("")
  
  wts <- runif(length(symbols))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1) ^ n_ann) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(ETF_rtn_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))


# create covariance matrix
covmat <-
  cov(ETF_rtn_xts)

# 위험기여도 계산
get_RC = function(w, covmat) {
  port_vol = t(w) %*% covmat %*% w
  port_std = sqrt(port_vol)
  
  MRC = (covmat %*% w) / as.numeric(port_std)
  RC = MRC * w
  RC = c(RC / sum(RC))
  
  return(RC)
}

# rp() 함수를 이용한 최적화
# slsqp()나 optimalPortfolio() 함수를 이용해 구현 가능
# 간혹 최적화된 값을 찾지 못할 때 있음
# cccp패키지의 rp() 함수 사용하면 매우 정확

opt = rp(x0 = rep(1/length(symbols), length(symbols)),       # 최적화를 위한 초기 입력값
         P = covmat,                                         # 분산-공분산 행렬 입력
         mrc = rep(1/length(symbols), length(symbols)))      # 목표로 하는 각 자산별 위험기여도 값 -> 동일하게
w = getx(opt) %>% drop()     # getx() 함수를 통해 해를 추출.. drop()을 통해 벡터 형태로 변환
w = (w / sum(w)) %>% 
  round(., 4) %>% 
  setNames(colnames(all_of(symbols)))


# create covariance matrix
covmat <-
  cov(ETF_rtn_xts)

w = optimalPortfolio(covmat,
                     control = list(type = 'maxdiv',
                                    constraint = 'lo')) %>% 
  round(., 4)
