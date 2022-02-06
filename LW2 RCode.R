setwd("C:/Users/fatal/Desktop")
getwd()
dir()
A = read.table("Population.txt",header = T, sep=""); A
str(A)

# 1 task and # 2 task

summary(A)

set.seed(20201750)
sample(nrow(A),10)
S = A[sample(nrow(A), 250, replace = FALSE), ]; S
summary(S)
var(A$area)
var(S$area)

boxplot(A$area,S$area)
S[S$price == max(S$price), ]$area

# 3 task

mean(S$year)
mean(S$area)
mean(S$price)
t.test(S$year, mu = 14, conf.level = 0.95)
t.test(S$area, mu = 45, alternative = "greater")
t.test(S$price, mu = 33200, alternative = "two.sided")
t.test(S$price, mu = 33200, alternative = "less")
t.test(S$price, mu = 33200, alternative = "greater")
t.test(S$year, mu = 7, alternative = "two.sided")

# 4 task

par(mfrow = c(1, 3))
hist(S$year, freq=FALSE, main="edf for Year", xlab = "year")
arr = S$area
N = 7 
s = (max(arr) - min(arr)) / N; s
int = seq(min(arr), max(arr), s); int
afTable = table(cut(arr, int, include.lowest = TRUE, ordered_result = TRUE)); afTable
barplot(afTable, main = "edf for area")
pr = S$price
N = 37 
s = (max(pr) - min(pr)) / N; s
int = seq(min(pr), max(pr), s); int
pfTable = table(cut(pr, int, include.lowest = TRUE, ordered_result = TRUE)); pfTable
barplot(pfTable, main = "edf for price")

# 5 task

y = S$year
t = 10
s = (max(y) - min(y)) / t; s
int = seq(min(y), max(y), s); int
Ps = rep(1 / t, t)
yfTable = table(cut(y, int, include.lowest = TRUE, ordered_result = TRUE)); yfTable
chisq.test(yfTable, p = Ps)
barplot(yfTable / 250, p = Ps)
a = S$area
lambda = 1 / mean(a); lambda
t = 14
s = (max(a) - min(a)) / t; s
int = seq(min(a), max(a), s); int
m = length(int); m
epoins = pexp(c(int[2:(m - 1)], Inf), lambda); epoins
spoins = pexp(c(0,int[2:(m - 1)]), lambda); spoins
Ps = epoins - spoins; Ps
sum(Ps) == 1
fTable=table(cut(a, int, include.lowest = TRUE, ordered_result = TRUE)); fTable
chisq.test(spoins, p = Ps)
barplot(spoins / 250, p = Ps)
spoins
Ps * 250

# 6 task

y = S$price
x = S$year
cor(x, y)
lline = lm(y~x); lline
summary(lline)
par(mfrow = c(1, 1))
plot(y ~ x)
abline(lline, col="red")
pY = 35
prog = lline$coefficients[1] + lline$coefficients[2] * pY; prog
cor(x, y)

# 7 task

y = S$price
x = S$area
lline = lm(y ~ x); lline
summary(lline)
par(mfrow = c(1, 1))
plot(y ~ x)
abline(lline, col="red")
pArea = 160
prog = lline$coefficients[1] + lline$coefficients[2] * pArea; prog
p160 = lline$coefficients[1] + lline$coefficients[2] * 160;
p150 = lline$coefficients[1] + lline$coefficients[2] * 150;
p160 - p150
c = cor(x, y);
pct = c * c * 100; pct
