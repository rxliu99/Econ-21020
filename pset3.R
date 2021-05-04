# part a
library("readxl")
data <- read_excel('/Users/tutu/Desktop/ps3-caschool.xlsx')

# part b
income <- 1000 * data$avginc

mn <- mean(data$avginc)
std <- sd(data$avginc)

mn_income <- mean(income)
std_income <- sd(income)

# part c i-iv
mn_math <- mean(data$math_scr)

length(which(data$str <= 20)) / 420
sub_data <- data[data$str <= 20,]
mean(sub_data$math_scr)
sd(sub_data$math_scr)

length(which(data$str > 20)) / 420
sub_data_2 <- data[data$str > 20,]
mean(sub_data_2$math_scr)
sd(sub_data_2$math_scr)

# part c v-vii
cov(data$avginc, data$math_scr)
cov(income, data$math_scr)

cor(data$avginc, data$math_scr)
cor(income, data$math_scr)
