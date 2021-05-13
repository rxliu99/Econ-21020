library(sandwhich)
library(lmtest)
library("readxl")

df <- read_excel('/Users/tutu/Desktop/UChicago/Junior/Spring/Econometrics/ps4_birthweight_smoking.xlsx')

# part a
fit_lm <- lm(birthweight ~ smoker, data = df)
summary(fit_lm)

# part b
fit_lm_multiple <- lm(birthweight ~ smoker + alcohol + nprevist, data = df)
summary(fit_lm_multiple)

# part b-i
cor(df$smoker, df$alcohol)
cor(df$smoker, df$nprevist)
var(df$smoker)

# part b-iii
new <- data.frame(smoker=c(1), alcohol=c(0), nprevist=c(8))
predict(fit_lm_multiple, newdata = new)

# part b-iv
summary(fit_lm_multiple)$r.squared
summary(fit_lm_multiple)$adj.r.squared

# part c
fit_lm_fwt_step1 <- lm(smoker ~ alcohol + nprevist, data = df)
x_tilt <- summary(fit_lm_fwt_step1)$residuals
fit_lm_fwt_step2 <- lm(birthweight ~ alcohol + nprevist, data = df)
y_tilt <- summary(fit_lm_fwt_step2)$residuals
fwt_df <- data.frame(y_tilt, x_tilt)
fit_lm_fwt_step3 <- lm(y_tilt ~ x_tilt, data = fwt_df)
summary(fit_lm_fwt_step3)
