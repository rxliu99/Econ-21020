library(sandwhich)
library(lmtest)
library(AER)
library("readxl")

# Problem 4 ===================================================================
df1 <- read_excel('/Users/tutu/Desktop/UChicago/Junior/Spring/Econometrics/ps5_cps04.xlsx')
# part a-i
fit_lm <- lm(AHE ~ 1 + Female + College + HS, data = df1)
summary(fit_lm)
coeftest(fit_lm, vcoc = vcovHC(fit_lm, type = "HC1"))
upper <- -4.08314 + 1.96 * 0.072368
lower <- -4.08314 - 1.96 * 0.072368

# part a-iii
c <- 0
r <- c(0, 0, 1, -1)
t_n <- sqrt(linearHypothesis(fit_lm, r, c, vcov=vcovHC(fit_lm, type="HC1"), test="Chisq")$Chisq[2])

# part b-i
hs_col <- df1$HS + df1$College
df2 <- cbind(df1, hs_col)
fit_lm2 <- lm(AHE ~ 1 + Female + College + hs_col, data = df2)
summary(fit_lm2)

# part b-ii
t_n2 <- abs((13.25745 - 0) / 0.15255)

# part c
fem_col <- df1$Female * df1$College
fem_hs <- df1$Female * df1$HS
df3 <- cbind(df1, fem_col, fem_hs)
fit_lm3 <- lm(AHE ~ 1 + Female + College + fem_col + HS + fem_hs, data = df3)

c1 <- 0
r1 <- c(0, 0, 0, 1, 0, -1)
t_n3 <- sqrt(linearHypothesis(fit_lm3, r1, c1, vcov=vcovHC(fit_lm3, type="HC1"), test="Chisq")$Chisq[2])

# Problem 5 ===================================================================
df4 <- read_excel('/Users/tutu/Desktop/UChicago/Junior/Spring/Econometrics/ps5_fertility.xlsx')
# part a
fit_lm4 <- lm(weeksm1 ~ morekids, data = df4)
summary(fit_lm4)

# part c
fit_lm5 <- lm(morekids ~ samesex, data = df4)
summary(fit_lm5)

# part e
fit_iv <- ivreg(weeksm1 ~ morekids | samesex, data = df4)
summary(fit_iv)

# part f
#fit_iv2 <- ivreg(weeksm1 ~ 1 + morekids + agem1 + black + hispan + othrace | samesex, data = df4)
fit_iv2 <- lm(weeksm1 ~ 1 + morekids + agem1 + black + hispan + othrace, data = df4)
summary(fit_iv2)

