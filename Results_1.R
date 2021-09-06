library(tidyverse)


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

## Simulation study results: GA
# p.mut = .01
# max.itr = 150
# x.inc = 45

ga1 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_1_50_0.01_150_45')
ga2 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_51_100_0.01_150_45')
ga3 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_101_150_0.01_150_45')
ga4 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_151_200_0.01_150_45')
# ga5 = read_csv('data/sim_study/2_changepoints/ga/results_ga_1_v01_i_201_250_0.01_150_45')
ga6 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_251_300_0.01_150_45')
ga7 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_301_350_0.01_150_45')
ga8 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_351_400_0.01_150_45')
ga9 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_401_450_0.01_150_45')
# ga10 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_451_500_0.01_150_45')
ga11 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_501_550_0.01_150_45')
ga12 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_551_600_0.01_150_45')
ga13 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_601_650_0.01_150_45')
ga14 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_651_700_0.01_150_45')
# ga15 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_701_751_0.01_150_45')
ga16 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_751_800_0.01_150_45')
ga17 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_801_850_0.01_150_45')
ga18 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_851_900_0.01_150_45')
ga19 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_901_950_0.01_150_45')
ga20 = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_951_1000_0.01_150_45')

ga = rbind(ga1,ga2,ga3,ga4,ga6,ga7,ga8,ga9,ga11,
           ga12,ga13,ga14,ga16,ga17,ga18,ga19,ga20)




# Proportion where m==m_true
prop_correct = function(df,m_true){
  m = df['m']
  correct = m[m==m_true,]
  good = nrow(correct)
  total = nrow(m)
  good/total
}
prop_correct(ga,m_true)
ggplot(ga)+geom_histogram(aes(c1))

#
ggplot(df)+geom_histogram(aes(m))
ggplot(df)+geom_histogram(aes(c1))

## Simulation study results: dp

dp1 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_1_50_0.01_150_45')
dp2 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_51_100_0.01_150_45')
dp3 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_101_150_0.01_150_45')
dp4 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_151_200_0.01_150_45')
# dp5 = read_csv('data/sim_study/2_changepoints/dp/results_dp_1_v01_i_201_250_0.01_150_45')
dp6 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_251_300_0.01_150_45')
dp7 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_301_350_0.01_150_45')
dp8 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_351_400_0.01_150_45')
dp9 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_401_450_0.01_150_45')
# dp10 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_451_500_0.01_150_45')
dp11 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_501_550_0.01_150_45')
dp12 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_551_600_0.01_150_45')
dp13 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_601_650_0.01_150_45')
dp14 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_651_700_0.01_150_45')
# dp15 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_701_751_0.01_150_45')
dp16 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_751_800_0.01_150_45')
dp17 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_801_850_0.01_150_45')
dp18 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_851_900_0.01_150_45')
dp19 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_901_950_0.01_150_45')
dp20 = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_951_1000_0.01_150_45')

dp = rbind(dp1,dp2,dp3,dp4,dp6,dp7,dp8,dp9,dp11,
           dp12,dp13,dp14,dp16,dp17,dp18,dp19,dp20)
prop_correct(dp,m_true)
ggplot(dp)+geom_histogram(aes(c1))

## Simulation study results: sg

sg1 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_1_50_0.01_150_45')
sg2 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_51_100_0.01_150_45')
sg3 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_101_150_0.01_150_45')
sg4 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_151_200_0.01_150_45')
# sg5 = read_csv('data/sim_study/2_changepoints/sg/results_sg_1_v01_i_201_250_0.01_150_45')
sg6 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_251_300_0.01_150_45')
sg7 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_301_350_0.01_150_45')
sg8 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_351_400_0.01_150_45')
sg9 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_401_450_0.01_150_45')
# sg10 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_451_500_0.01_150_45')
sg11 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_501_550_0.01_150_45')
sg12 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_551_600_0.01_150_45')
sg13 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_601_650_0.01_150_45')
sg14 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_651_700_0.01_150_45')
# sg15 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_701_751_0.01_150_45')
sg16 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_751_800_0.01_150_45')
sg17 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_801_850_0.01_150_45')
sg18 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_851_900_0.01_150_45')
sg19 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_901_950_0.01_150_45')
sg20 = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_951_1000_0.01_150_45')

sg = rbind(sg1,sg2,sg3,sg4,sg6,sg7,sg8,sg9,sg11,
           sg12,sg13,sg14,sg16,sg17,sg18,sg19,sg20)
prop_correct(sg,m_true)

ggplot(sg)+geom_histogram(aes(c1))

