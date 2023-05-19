setwd("C:/Users/Faye/桌面/assign")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Advanced Life insurance mathematics: #
#        Assignment - Jing Yang        #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

###### 0. Settings ######
start_year <- 1876
KULbg <- "#116E8A"

library(tidyverse)
library(demography)
library(forecast)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(astsa)

females<- read.table("females.csv", sep=",",head=TRUE)
males<- read.table("males.csv", sep=",",head=TRUE)
uni<- read.table("uni.csv", sep=",",head=TRUE)

names(females)# "Year" "Age"  "mx"   "qx"   "ax"   "lx"   "dx"   "Lx"   "Tx"   "ex"  

str(females)

min(females$Year)# 1876
max(females$Year)# 2021


min(females$Age)# 0
max(females$Age)# 110

###### 1. check the data in 2019 per gender ######

fem_2019<- filter(females, Year == 2019)
male_2019<- filter(males, Year == 2019)

#number of death dx----------------------
par(mfcol=c(2,2))

g_fem_dx <- ggplot(fem_2019, aes(Age, dx)) + 
  geom_step(col = KULbg) + 
  theme_bw() +
  ggtitle("Switzerland - females, 2019") 
g_fem_dx

g_male_dx <- ggplot(male_2019, aes(Age, dx)) + 
  geom_step(col = "red") + 
  theme_bw() +
  ggtitle("Switzerland - males, 2019")
g_male_dx

#combined
g_dx <- g_male_dx +
  geom_step(data = fem_2019, aes(Age, dx), col = KULbg) + 
  theme_bw() +
  ggtitle("Switzerland, 2019") +
  labs(y = "dx")
g_dx


#life expectancy on 2019 for all ages ex----------------------

g_fem_ex <- ggplot(fem_2019, aes(Age, ex)) + 
  geom_line(col = KULbg, size = 1) + 
  theme_bw() +
  ggtitle("Switzerland - females, 2019") +
  labs(y = "ex")
g_fem_ex

g_male_ex <- ggplot(male_2019, aes(Age, ex)) + 
  geom_line(col = "red", size = 1) + 
  theme_bw() +
  ggtitle("Switzerland - males, 2019") +
  labs(y = "ex")
g_male_ex

#combined
g_ex <- g_male_ex +
  geom_line(data = fem_2019, aes(Age, ex), col = KULbg, size = 1) + 
  theme_bw() +
  ggtitle("Switzerland, 2019") +
  labs(y = "ex")
g_ex

#log(qx)---------------------

g_fem_qx <- ggplot(fem_2019, aes(Age, log(qx))) + 
  geom_point(col = KULbg) + 
  geom_line(col = KULbg) + 
  theme_bw() +
  ggtitle("Switzerland - females, 2019") + 
  labs(y = bquote(ln(q[x])))
g_fem_qx

g_male_qx <- ggplot(male_2019, aes(Age, log(qx))) + 
  geom_point(col = "red") + 
  geom_line(col = "red") + 
  theme_bw() +
  ggtitle("Switzerland - males, 2019") + 
  labs(y = bquote(ln(q[x])))
g_male_qx

#combined
g_qx <- g_male_qx + 
  geom_point(data = fem_2019, aes(Age, log(qx)), col = KULbg) + 
  geom_line(data = fem_2019, aes(Age, log(qx)), col = KULbg)
  theme_bw() +
  ggtitle("Switzerland, 2019") + 
  labs(y = bquote(ln(q[x]))) 
g_qx

#period life expectancy at selected ages over years ex---------------------
females_0 <- females[females$Age == 0,]
females_20 <- females[females$Age == 20,]
females_40 <- females[females$Age == 40,]
females_60 <- females[females$Age == 60,]
females_80 <- females[females$Age == 80,]

g_fem_e0 <- ggplot(females_0, aes(Year, ex)) + 
  geom_line(col = "black", size = 0.8) +
  geom_line(data = females_20, aes(Year, ex, col ="blue"), size = 0.8) +
  geom_line(data = females_40, aes(Year, ex, col ="red"), size = 0.8) +
  geom_line(data = females_60, aes(Year, ex), col = "green", size = 0.8) +
  geom_line(data = females_80, aes(Year, ex), col = "yellow", size = 0.8) +
  theme_bw() +
  ggtitle("Switzerland - females") + 
  labs(y = "period life expectancy")
g_fem_e0



###### 2. Parameters Estimation - Lee Carter - LS approach ######

females_est <- filter(females, Year <= 2019 & Year >= start_year) 
males_est <- filter(males, Year <= 2019 & Year >= start_year)

###2.1 Estimation

## for females
## Estimate alphas ##
X <- model.matrix(
  ~ as.factor(females_est$Age) 
  - 1)
dim(X)
y <- log(females_est$mx)
y[is.infinite(y) & y < 0] <- 0#a few of values equals to 0 causes -Inf


alpha_fem_est_expl <- solve(
  crossprod(X)) %*% 
  t(X) %*% y

alpha_fem_est <- lm(
  y ~ -1 + 
    as.factor(females_est$Age))$coef 


data <- tibble(age = 0:110, fit = alpha_fem_est)
g_fem_alpha <- ggplot(data) + 
  geom_point(aes(age, fit)) + 
  geom_line(aes(age, fit), col = "black") +
  theme_bw() +
  ggtitle("Switzerland - females, 1876-2019") + 
  labs(y = bquote(hat(beta)[x]^"(1)")) 
g_fem_alpha



## Estimate kappas ##
# Create new response variable: Z = log(mx) - alpha
Z <- log(females_est$mx) - 
  alpha_fem_est[females_est$Age - 
              min(females_est$Age) + 1]
Z[is.infinite(Z) & Z < 0] <- 0#a few of values equals to 0 causes -Inf

X <- model.matrix(~ as.factor(females_est$Year) - 1)

kappa_fem_est_expl <- solve(crossprod(X)) %*% t(X) %*% Z

kappa_fem_est <- lm(
  Z ~ -1 + 
    as.factor(females_est$Year))$coef 


data <- tibble(year = start_year:2019, fit = kappa_fem_est)
g_fem_kappa <- ggplot(data) + geom_point(aes(year, kappa_fem_est)) + 
  geom_line(aes(year, kappa_fem_est), col = "black") +
  theme_bw() +
  ggtitle("Switzerland - females, 1876-2019, starting values") + 
  labs(y = bquote(hat(kappa)[t]^"(2)")) 
g_fem_kappa


## Estimate betas ##

var_fem_kappa <- kappa_fem_est[females_est$Year - 
                         min(females_est$Year) + 1]

X <- model.matrix(~ 
                    as.factor(females_est$Age):var_fem_kappa 
                  - 1)

beta_fem_est_expl  <- solve(crossprod(X)) %*% t(X) %*% Z 

beta_fem_est <- lm(
  Z ~ -1 + 
    as.factor(females_est$Age):var_fem_kappa)$coef 


data <- tibble(age = 0:110, fit = beta_fem_est)
g_fem_beta <- ggplot(data) + geom_point(aes(age, beta_fem_est)) + 
  geom_line(aes(age, beta_fem_est), col = "black") +
  theme_bw() +
  ggtitle("Switzerland - females, 1876-2019, starting value") + 
  labs(y = bquote(hat(beta)[x]^"(2)")) 
g_fem_beta


#### Convergence ####

converged = F
iter      = 1
&& iter <= 300
while(!converged){  
  beta_est_old  = beta_fem_est
  kappa_est_old = kappa_fem_est
  
  # (2): estimate kappa's
  var_beta = beta_fem_est[females_est$Age - min(females_est$Age) + 1]
  X        = model.matrix(~ as.factor(females_est$Year):var_beta - 1)
  kappa_est = solve(crossprod(X)) %*% t(X) %*% Z
  
  # (3): estimate beta's
  var_kappa = kappa_fem_est[females_est$Year - min(females_est$Year) + 1]
  X         = model.matrix(~ as.factor(females_est$Age):var_kappa - 1)
  beta_est   = solve(crossprod(X)) %*% t(X) %*% Z 
  
  # stopping criterion
  converged = 
    max(abs(beta_est - beta_est_old) / abs(beta_est_old), abs(kappa_est - kappa_est_old) / abs(kappa_est_old)) < 1
  iter = iter + 1
  if(iter %% 1e2 == 0)
    cat("\n\nIteration number", iter, "\n\n")
}

#Plot after convergence
data <- tibble(year = start_year:2019, fit = kappa_est, init = kappa_fem_est_expl)
g_fem_conv_kappa <- ggplot(data) + geom_point(aes(year, fit)) + 
  geom_line(aes(year, fit), col = "black") +
  geom_line(aes(year, init), col = "red") +
  theme_bw() +
  ggtitle("Switzerland - females, 1876-2019, starting + final values") + 
  labs(y = bquote(hat(kappa)[t]^"(2)")) 
g_fem_conv_kappa


data <- tibble(age = 0:110, fit = beta_est, init = beta_fem_est_expl)
g_fem_conv_beta <- ggplot(data) + geom_point(aes(age, fit)) + 
  geom_line(aes(age, fit), col = "black") +
  geom_line(aes(age, init), col = "red") +
  theme_bw() +
  ggtitle("Switzerland - females, 1876-2019, starting + final values") + 
  labs(y = bquote(hat(beta)[x]^"(2)")) 
g_fem_conv_beta


# apply constraints
beta_fem_est_LS  = beta_est / sum(beta_est)
kappa_fem_est_LS = (kappa_est - mean(kappa_est)) * sum(beta_est)
alpha_fem_est_LS = alpha_fem_est + beta_est * mean(kappa_est)
sum(beta_fem_est_LS) 
sum(kappa_fem_est_LS)


## for males

## Estimate alphas ##
X <- model.matrix(
  ~ as.factor(males_est$Age) 
  - 1)
dim(X)
y <- log(males_est$mx)
y[is.infinite(y) & y < 0] <- 0#a few of values equals to 0 causes -Inf


alpha_male_est_expl <- solve(
  crossprod(X)) %*% 
  t(X) %*% y

alpha_male_est <- lm(
  y ~ -1 + 
    as.factor(males_est$Age))$coef 


data <- tibble(age = 0:110, fit = alpha_male_est)
g_male_alpha <- ggplot(data) + 
  geom_point(aes(age, alpha_male_est)) + 
  geom_line(aes(age, alpha_male_est), col = "black") +
  theme_bw() +
  ggtitle("Switzerland - males, 1876-2019") + 
  labs(y = bquote(hat(beta)[x]^"(1)")) 
g_male_alpha



## Estimate kappas ##
# Create new response variable: Z = log(mx) - alpha
Z <- log(males_est$mx) - 
  alpha_male_est[males_est$Age - 
                  min(males_est$Age) + 1]
Z[is.infinite(Z) & Z < 0] <- 0#a few of values equals to 0 causes -Inf

X <- model.matrix(~ as.factor(males_est$Year) - 1)

kappa_male_est_expl <- solve(crossprod(X)) %*% t(X) %*% Z

kappa_male_est <- lm(
  Z ~ -1 + 
    as.factor(males_est$Year))$coef 


data <- tibble(year = start_year:2019, fit = kappa_male_est)
g_male_kappa <- ggplot(data) + geom_point(aes(year, kappa_male_est)) + 
  geom_line(aes(year, kappa_male_est), col = "black") +
  theme_bw() +
  ggtitle("Switzerland - males, 1876-2019, starting values") + 
  labs(y = bquote(hat(kappa)[t]^"(2)")) 
g_male_kappa


## Estimate betas ##

var_kappa <- kappa_male_est[males_est$Year - 
                             min(males_est$Year) + 1]

X <- model.matrix(~ 
                    as.factor(males_est$Age):var_kappa 
                  - 1)

beta_male_est_expl  <- solve(crossprod(X)) %*% t(X) %*% Z 

beta_male_est <- lm(
  Z ~ -1 + 
    as.factor(males_est$Age):var_kappa)$coef 


data <- tibble(age = 0:110, fit = beta_male_est)
g_male_beta <- ggplot(data) + geom_point(aes(age, beta_male_est)) + 
  geom_line(aes(age, beta_male_est), col = "black") +
  theme_bw() +
  ggtitle("Switzerland - males, 1876-2019, starting values") + 
  labs(y = bquote(hat(beta)[x]^"(2)")) 
g_male_beta


#### Convergence ####

converged = F
iter      = 1

while(!converged && iter <= 100){  
  beta_est_old  = beta_male_est
  kappa_est_old = kappa_male_est
  
  # (2): estimate kappa's
  var_beta = beta_male_est[males_est$Age - min(males_est$Age) + 1]
  X        = model.matrix(~ as.factor(males_est$Year):var_beta - 1)
  kappa_est = solve(crossprod(X)) %*% t(X) %*% Z
  
  # (3): estimate beta's
  var_kappa = kappa_male_est[males_est$Year - min(males_est$Year) + 1]
  X         = model.matrix(~ as.factor(males_est$Age):var_kappa - 1)
  beta_est   = solve(crossprod(X)) %*% t(X) %*% Z 
  
  # stopping criterion
  converged = 
    max(abs(beta_est - beta_est_old) / abs(beta_est_old), abs(kappa_est - kappa_est_old) / abs(kappa_est_old)) < 0.01
  iter = iter + 1
  if(iter %% 1e2 == 0)
    cat("\n\nIteration number", iter, "\n\n")
}

data <- tibble(year = start_year:2019, fit = kappa_est, init = kappa_male_est_expl)
g_male_conv_kappa <- ggplot(data) + geom_point(aes(year, fit)) + 
  geom_line(aes(year, fit), col = "black") +
  geom_line(aes(year, init), col = "red") +
  theme_bw() +
  ggtitle("Switzerland - males, 1876-2019, starting + final values") + 
  labs(y = bquote(hat(kappa)[t]^"(2)")) 
g_male_conv_kappa

data <- tibble(age = 0:110, fit = beta_est, init = beta_male_est_expl)
g_male_conv_beta <- ggplot(data) + geom_point(aes(age, fit)) + 
  geom_line(aes(age, fit), col = "black") +
  geom_line(aes(age, init), col = "red") +
  theme_bw() +
  ggtitle("Switzerland - males, 1876-2019, starting + final values") + 
  labs(y = bquote(hat(beta)[x]^"(2)")) 
g_male_conv_beta


# apply constraints
beta_male_est_LS  = beta_est / sum(beta_est)
kappa_male_est_LS = (kappa_est - mean(kappa_est)) * sum(beta_est)
alpha_male_est_LS = alpha_male_est + beta_est * mean(kappa_est)
sum(beta_male_est_LS) 
sum(kappa_male_est_LS)


kappa_male_1876 <- kappa_male_est_LS
kappa_fem_1876 <- kappa_fem_est_LS
beta_fem_1876 <- beta_fem_est_LS
beta_male_1876 <- beta_male_est_LS

kappa_male_1970 <- kappa_male_est_LS
kappa_fem_1970 <- kappa_fem_est_LS
beta_fem_1970 <- beta_fem_est_LS
beta_male_1970 <- beta_male_est_LS

kappa_male_1960 <- kappa_male_est_LS
kappa_fem_1960 <- kappa_fem_est_LS
beta_fem_1960 <- beta_fem_est_LS
beta_male_1960 <- beta_male_est_LS

###2.2 Projection###

### 1. by changing the ts model##
acf(time_series)
pacf(time_series)

auto.arima(kappa_fem_1960)

sarima(kappa_fem_1970, 0, 1, 0)
sarima(kappa_fem_1876, 0, 1, 1)

time_series_rw <- Arima(kappa_fem_1960, 
                     order = c(0, 1, 0), 
                     include.drift = TRUE)
time_series_rw$coef

time_series_11 <- Arima(kappa_fem_1876, 
                        order = c(0, 1, 1), 
                        include.drift = TRUE)
time_series_11


auto.arima(kappa_male_1960)

Arima(kappa_male_1970, 
      order = c(0, 1, 0), 
      include.drift = TRUE)

##2. using Random walking but change the start year until it reaches stability#

##for females
list1 <- list()

for (i in 1876:2000) {
  females_est <- filter(females, Year <= 2019 & Year >= i)
  ## for females
  ## Estimate alphas ##
  X <- model.matrix(
    ~ as.factor(females_est$Age) 
    - 1)
  dim(X)
  y <- log(females_est$mx)
  y[is.infinite(y) & y < 0] <- 0#a few of values equals to 0 causes -Inf
  
  
  alpha_fem_est_expl <- solve(
    crossprod(X)) %*% 
    t(X) %*% y
  
  alpha_fem_est <- lm(
    y ~ -1 + 
      as.factor(females_est$Age))$coef 
  
  
  
  ## Estimate kappas ##
  # Create new response variable: Z = log(mx) - alpha
  Z <- log(females_est$mx) - 
    alpha_fem_est[females_est$Age - 
                    min(females_est$Age) + 1]
  Z[is.infinite(Z) & Z < 0] <- 0#a few of values equals to 0 causes -Inf
  
  X <- model.matrix(~ as.factor(females_est$Year) - 1)
  
  kappa_fem_est_expl <- solve(crossprod(X)) %*% t(X) %*% Z
  
  kappa_fem_est <- lm(
    Z ~ -1 + 
      as.factor(females_est$Year))$coef 
  
  ## Estimate betas ##
  
  var_fem_kappa <- kappa_fem_est[females_est$Year - 
                                   min(females_est$Year) + 1]
  
  X <- model.matrix(~ 
                      as.factor(females_est$Age):var_fem_kappa 
                    - 1)
  
  beta_fem_est_expl  <- solve(crossprod(X)) %*% t(X) %*% Z 
  
  beta_fem_est <- lm(
    Z ~ -1 + 
      as.factor(females_est$Age):var_fem_kappa)$coef 
  
  
  #### Convergence ####
  
  converged = F
  iter      = 1
  
  while(!converged && iter <= 100){  
    beta_est_old  = beta_fem_est
    kappa_est_old = kappa_fem_est
    
    # (2): estimate kappa's
    var_beta = beta_fem_est[females_est$Age - min(females_est$Age) + 1]
    X        = model.matrix(~ as.factor(females_est$Year):var_beta - 1)
    kappa_est = solve(crossprod(X)) %*% t(X) %*% Z
    
    # (3): estimate beta's
    var_kappa = kappa_fem_est[females_est$Year - min(females_est$Year) + 1]
    X         = model.matrix(~ as.factor(females_est$Age):var_kappa - 1)
    beta_est   = solve(crossprod(X)) %*% t(X) %*% Z 
    
    # stopping criterion
    converged = 
      max(abs(beta_est - beta_est_old) / abs(beta_est_old), abs(kappa_est - kappa_est_old) / abs(kappa_est_old)) < 0.1
    iter = iter + 1
    if(iter %% 1e2 == 0)
      cat("\n\nIteration number", iter, "\n\n")
  }
  beta_fem_est_LS  = beta_est / sum(beta_est)
  kappa_fem_est_LS = (kappa_est - mean(kappa_est)) * sum(beta_est)
  
  time_series_rw <- Arima(kappa_fem_est_LS, 
                          order = c(0, 1, 0), 
                          include.drift = TRUE)
  
  list1 <- append(list1, time_series_rw$coef)
}
print(list1)

##for males

list2 <- list()

for (i in 1876:2000) {
  males_est <- filter(males, Year <= 2019 & Year >= i)
  ## for females
  ## Estimate alphas ##
  X <- model.matrix(
    ~ as.factor(males_est$Age) 
    - 1)
  dim(X)
  y <- log(males_est$mx)
  y[is.infinite(y) & y < 0] <- 0#a few of values equals to 0 causes -Inf
  
  
  alpha_male_est_expl <- solve(
    crossprod(X)) %*% 
    t(X) %*% y
  
  alpha_male_est <- lm(
    y ~ -1 + 
      as.factor(males_est$Age))$coef 
  
  
  ## Estimate kappas ##
  # Create new response variable: Z = log(mx) - alpha
  Z <- log(males_est$mx) - 
    alpha_male_est[males_est$Age - 
                     min(males_est$Age) + 1]
  Z[is.infinite(Z) & Z < 0] <- 0#a few of values equals to 0 causes -Inf
  
  X <- model.matrix(~ as.factor(males_est$Year) - 1)
  
  kappa_male_est_expl <- solve(crossprod(X)) %*% t(X) %*% Z
  
  kappa_male_est <- lm(
    Z ~ -1 + 
      as.factor(males_est$Year))$coef 
  
  ## Estimate betas ##
  
  var_kappa <- kappa_male_est[males_est$Year - 
                                min(males_est$Year) + 1]
  
  X <- model.matrix(~ 
                      as.factor(males_est$Age):var_kappa 
                    - 1)
  
  beta_male_est_expl  <- solve(crossprod(X)) %*% t(X) %*% Z 
  
  beta_male_est <- lm(
    Z ~ -1 + 
      as.factor(males_est$Age):var_kappa)$coef 
  
  
  #### Convergence ####
  
  converged = F
  iter      = 1
  
  while(!converged && iter <= 100){  
    beta_est_old  = beta_male_est
    kappa_est_old = kappa_male_est
    
    # (2): estimate kappa's
    var_beta = beta_male_est[males_est$Age - min(males_est$Age) + 1]
    X        = model.matrix(~ as.factor(males_est$Year):var_beta - 1)
    kappa_est = solve(crossprod(X)) %*% t(X) %*% Z
    
    # (3): estimate beta's
    var_kappa = kappa_male_est[males_est$Year - min(males_est$Year) + 1]
    X         = model.matrix(~ as.factor(males_est$Age):var_kappa - 1)
    beta_est   = solve(crossprod(X)) %*% t(X) %*% Z 
    
    # stopping criterion
    converged = 
      max(abs(beta_est - beta_est_old) / abs(beta_est_old), abs(kappa_est - kappa_est_old) / abs(kappa_est_old)) < 0.01
    iter = iter + 1
    if(iter %% 1e2 == 0)
      cat("\n\nIteration number", iter, "\n\n")
  }
  
  beta_male_est_LS  = beta_est / sum(beta_est)
  kappa_male_est_LS = (kappa_est - mean(kappa_est)) * sum(beta_est)
  
  
  time_series_rw <- Arima(kappa_male_est_LS, 
                          order = c(0, 1, 0), 
                          include.drift = TRUE)
  
  list2 <- append(list1, time_series_rw$coef)
}
print(list2)


coef_fem <- list1
coef_male <- list2


data <- tibble(year = 1876:2000, fit = as.numeric(list1))
data1 <- tibble(year = 1876:2000, fit = abs(as.numeric(list2)))
g_coef <- ggplot(data, aes(year, fit)) + 
  geom_line(col = "blue") +
  geom_point(col = "blue") + 
  geom_line(data = data1, aes(year, fit), col = "red") +
  geom_point(data = data1, aes(year, fit), col = "red") +
  theme_bw() +
  ggtitle("Switzerland - females, 1876-2019, starting + final values") + 
  labs(y = bquote(hat(beta)[x]^"(2)")) 
g_coef


#forcasting :: codes of lab 2: 2.5
auto.arima(kappa_fem_est_LS)
sarima(kappa_fem_est_LS, 0, 1, 1)

time_series <- Arima(LCfit701$kappa2, 
                     order = c(0, 1, 1), 
                     include.drift = TRUE)

time_series

plot(forecast(time_series, level = c(80, 85, 95)))



source('simModels.R')

sim_fem_LC = sim2001(
  xx = females_est$Age,
  yy = females_est$Year,
  beta1v = LCfit701$beta1,
  beta2v = beta_fem_1876,
  kappa2v = kappa_fem_1876,
  nsim = 10000,
  tmax = 50,
  nyears = length(1876:2019)
)

names(sim_fem_LC)

###2.3 Insight on calibrated model###

life expectancy                      




###### 3.  ######

