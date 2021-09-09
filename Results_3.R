library(tidyverse)
library(changepoint.np)
library(cpm)
library(strucchange)
library(dpseg)

#### Dataset 4
### Linear, 3 changepoints
##  Generate X
n = 1000
sigma = 3
m_true = 3

xi_1 = 21
xi_2 = 70
xi_3 = 85
xi = c(3,xi_1, xi_2, xi_3)

X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X>= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3]

y1 = 2*X1
y2 = 69 - 1.3*X2
y3 = rep(-22, length(X3))
y4 = X4 - 107


y_true = c(y1,y2,y3,y4)


eps = rnorm(n,0,sigma)
y = y_true + eps


df0 = tibble(X,y,y_true)
ggplot(df0) + geom_point(aes(X,y), color='gray') +
  geom_line(aes(X,y_true), color='red')
  

#### Simulation Study 


### Changepoints with changepoint.np
library(changepoint.np)
out <- cpt.np(y, penalty = "BIC",method="PELT",minseglen = 150)
cpts(out)/10
plot(out)


### Changepoints with strucchange
library(strucchange)
fit_bp = breakpoints(y ~ X, data = df0, breaks = 3)
summary(fit_bp)



## knot selection with aspline
k <- 40
knots <- seq(min(X), max(X), length = k + 2)[-c(1, k + 2)]
degree <- 1
pen <- 10 ^ seq(-4, 4, 0.25)
x_seq <- seq(min(X), max(X), length = 1000)
aridge <- aridge_solver(X, y, knots, pen, degree = degree)
aridge$knots_sel[[which.min(aridge$ebic)]]

## dpseg
seg_mod = dpseg(X,y,minl = 100)
seg_mod$segments
print(seg_mod)



## segmented
library(segmented)
fit_lm = lm(y ~  X, data = df0)  # intercept-only model
selgmented(fit_lm)

fit_segmented = segmented(fit_lm)  # Two change points along x
summary(fit_segmented)










