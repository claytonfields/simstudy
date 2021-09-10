library(tidyverse)
library(segmented)
library(dpseg)
library(aspline)

#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This program applies the GA method to detect structural changes in a simple        ]*#
#*[            regression model setting.                                                          ]*#
#*[ Updated  : 07/30/2021                                                                         ]*#
#*[ Author   : Jaechoul Lee                                                                       ]*#
#*[-----------------------------------------------------------------------------------------------]*#


### Setup library, data, and output directories
WD.lib = c('')
WD.inp = c('')
### Load the proposed GA and fitted model likelihood funtion packages
source(file=paste(WD.lib,"ga_cont_01.R",sep=""))
source(file=paste(WD.lib,"mle_lin_01.R",sep=""))



#*[-----------------------------------------------------------------------------------------------]*#
### Case 1:Simulated Linear Data:
### 
#*[-----------------------------------------------------------------------------------------------]*#

#### Dataset 2
### Linear, 1 changepoints
##  Generate X
xi_1 = 32
xi = c(1,xi_1)
m_true = 1

n = 1000
sigma = 3
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1]

y1 = rep(26.3, length(X1))
y2 = .82*X2

y_true = c(y1,y2)
eps = rnorm(n,0,3)
y = y_true + eps


df0 = tibble(X,y,y_true)
ggplot(df0) + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,y_true), color='red')

### Find structural changes via the proposed GA method

### Utility Functions
get_df = function(){
  data.frame(
    i = integer(),
    seed_i = integer(),
    m = integer(),
    c1 = numeric(),
    c2 = numeric(),
    c3 = numeric(),
    c4 = numeric(),
    c5 = numeric(),
    c6 = numeric(),
    c7 = numeric(),
    c8 = numeric(),
    c9 = numeric(),
    c10 = numeric()
  )
}

# Data Structures
results_ga = get_df()
results_ar = get_df()
results_dp = get_df()
results_sg = get_df()

# Parameters for GA
p.mut = .01
max.itr = 150
x.inc = 45


# Parameters for aridge
k = 40
knots = seq(min(X), max(X), length = k + 2)[-c(1, k + 2)]
degree <- 1
pen = 10 ^ seq(-4, 4, 0.25)
x_seq = seq(min(X), max(X), length = 1000)
epsilon = .0001





start_pos = 701
end_pos = 750
for(i in start_pos:end_pos){
  begin = proc.time()  
  seed_i = 1000*(i-1)+543
  eps = rnorm(n,0,sigma)
  y = y_true + eps
  dfi = tibble(X,y)
  print(paste('Iteration',i))
  
  ## GA
  ga.out = ga.cpt_Norm(y=y, x=X,fitness=pnllik.MDL.M0Z, p.mut=p.mut, x.inc=x.inc,
                       max.itr=max.itr,seed=seed_i, is.print = FALSE)
  elapsed = proc.time() - begin
  print(elapsed)
  chrom.sol = ga.out$solution
  m = chrom.sol[1]
  row = c(i,seed_i,chrom.sol,rep(0,10-m))
  results_ga[nrow(results_ga)+1,] = row
  
  ## Segmented
  fit_lm = lm(y ~  X, data = dfi)  # intercept-only model
  temp_sg = selgmented(fit_lm, return.fit = TRUE)
  temp_sg2 = temp_sg$psi[,'Est.']
  m_sg = length(temp_sg2)
  row_sg = c(i,seed_i,m_sg,temp_sg2,rep(0,10-m_sg))
  results_sg[nrow(results_sg)+1,] = row_sg
  
  ## Aridge
  aridge <- aridge_solver(X, y,degree = degree,epsilon = epsilon)
  temp = aridge$knots_sel[[which.min(aridge$ebic)]]
  m_ar = length(temp)
  row_ar = c(i,seed_i,m_ar,temp,rep(0,10-m_ar))
  results_ar[nrow(results_ar)+1,] = row_ar
  
  ## dpseg
  seg_mod = dpseg(X,y,minl = 100)
  temp_dp = seg_mod$segments[-1,'x1']
  m_dp = length(temp_dp)
  row_dp = c(i,seed_i,m_dp,temp_dp,rep(0,10-m_dp))
  results_dp[nrow(results_dp)+1,] = row_dp
  
}




write_csv(results_ga, paste('results_ga_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
write_csv(results_ar, paste('results_ar_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
write_csv(results_dp, paste('results_dp_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
write_csv(results_sg, paste('results_sg_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))










