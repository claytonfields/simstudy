library(tidyverse)
library(aspline)
library(splines2)
library(dpseg)
library(segmented)

n = 1000
sigma = 3
m_true = 4

xi_1 = 8
xi_2 = 21
xi_3 = 44
xi_4 = 60
xi = c(4,xi_1, xi_2, xi_3, xi_4)

prop_correct = function(df){
  m = df['m']
  correct = m[m==4,]
  good = nrow(correct)
  total = nrow(m)
  good/total
}

X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3 & X < xi_4]
X5 = X[X >= xi_4]

y1 = rep(16, length(X1))
y2 = 2*X2
y3 = rep(42, length(X3))
y4 = -X4 + 86
y5 = rep(26, length(X5))

y_true = c(y1,y2,y3,y4,y5)
eps = rnorm(n,0,sigma)

y = y_true + eps
df0 = tibble(X,y,y_true)

ggplot(df0) + geom_point(aes(X,y),color='gray') +
  geom_line(aes(X,y_true),color='red') 

### Sim Study Data
# p.mut = .01
# max.itr = 200
# x.inc = 43
# df = read_csv('data/sim_study/4_changepoints/200_01_43/sim_study_v01_i_1_1000_0.01_200_43')


# Proportion of correct # of changepoints
prop_correct(df)
ggplot(df)+geom_histogram(aes(m))

# Visualize correct guesses
m = df['m']
correct = df[m==4,]
ggplot(correct) + geom_histogram(aes(c1))
ggplot(correct) + geom_histogram(aes(c2))
ggplot(correct) + geom_histogram(aes(c3))
ggplot(correct) + geom_histogram(aes(c4))

# Visualize incorrect guesses
incorrect = df[m==5,]
ggplot(incorrect) + geom_histogram(aes(c1))
ggplot(incorrect) + geom_histogram(aes(c2))
ggplot(incorrect) + geom_histogram(aes(c3))
ggplot(incorrect) + geom_histogram(aes(c4))
ggplot(incorrect) + geom_histogram(aes(c5))

# Visualize m
df.c5 = df[df['c5']!=0,]
m = c(df[['c1']],df[['c2']],df[['c3']],df[['c4']],df.c5[['c5']])
hist(m,breaks = seq(0, 100, length.out = 50))
# m = tibble(m)
# ggplot(m)+geom_histogram(aes(m))


### changepoint.np
##  comparison
library(changepoint.np)
out <- cpt.np(y, penalty = "BIC",method="PELT",minseglen = 100)
cpts(out)/10
plot(out)


## Changepoints with strucchange
library(strucchange)
fit_bp = breakpoints(y ~ X, data = df0, breaks =10)
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
seg_mod = dpseg(X,y,minl = 70)
seg_mod$segments


## segmented
fit_lm = lm(y ~  X, data = df0)  # intercept-only model
selgmented(fit_lm)

fit_segmented = segmented(fit_lm,npsi=4)  # Two change points along x
summary(fit_segmented)







# write_csv(dfB,'data/sim_study/200_01_43/sim_study_v01_i_1_1000_0.01_200_43',col_names = TRUE)

