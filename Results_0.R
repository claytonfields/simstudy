library(tidyverse)

#### Dataset 1
### Linear, no changepoints
##  Generate X


n = 1000
X = seq(from=0, to=100, length.out = n)

y_true = 1.245*X
eps = rnorm(n,0,3)

y = y_true + eps


df2 = tibble(X,y,y_true)
ggplot(df2) + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,y_true), color='red')

#### Simulation study results
# p.mut = .01
# max.itr = 150
# x.inc = 45

df1 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_1_50_0.01_150_45')
df2 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_51_100_0.01_150_45')
df3 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_101_150_0.01_150_45')
df4 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_151_200_0.01_150_45')
df5 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_201_250_0.01_150_45')
df6 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_251_300_0.01_150_45')
df7 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_301_350_0.01_150_45')
df8 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_351_400_0.01_150_45')
df9 = read_csv('data/sim_study/0_changepoints/150_01_45/sim_study_0_v01_i_401_450_0.01_150_45')
df = rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)

# Proportion where m==0
m = df['m']
correct = m[m==0,]
good = nrow(correct)
total = nrow(m)
good/total

#
ggplot(df)+geom_histogram(aes(m))














