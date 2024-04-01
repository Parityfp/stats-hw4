## HW4

# 1
mean1 <- 21.3
mean2 <- 18.6
sd1 <- 3.2
sd2 <- 2.1
n1 <- 22
n2 <- 20
mean_diff <- mean1 - mean2
se_diff <- sqrt(sd1^2/n1 + sd2^2/n2)
df <- n1 + n2 - 2
alpha <- 0.02
t_critical <- qt(1 - alpha/2, df)
CI_lower <- mean_diff - t_critical * se_diff
CI_upper <- mean_diff + t_critical * se_diff
CI <- c(CI_lower, CI_upper)
CI


# 1 a
mean2 <- 18.6
sd2 <- 2.1
n2 <- 20
se_diff <- sd2 / sqrt(n2)
df <- n2 - 1
alpha <- 0.02
t_critical <- qt(1 - alpha / 2, df)
CI_lower <- mean2 - t_critical * se_diff
CI_upper <- mean2 + t_critical * se_diff
CI <- c(CI_lower, CI_upper)
CI

# 1 b2 b3 b4
mean1 <- 21.3
mean2 <- 18.6
sd1 <- 3.2
sd2 <- 2.1
n1 <- 22
n2 <- 20
alpha <- 0.01
mean_diff <- mean1 - mean2
se_diff <- sqrt(sd1^2/n1 + sd2^2/n2)
df <- n1 + n2 - 2
t_critical <- qt(1 - alpha, df)

t_statistic <- mean_diff / se_diff
t_statistic

p_value <- 1 - pt(t_statistic, df)
p_value

rejection_region <- t_critical
rejection_region


# 2 a2 a3 a4
before <- c(7, 7, 8, 9, 8, 10, 6, 7, 7, 5, 9, 7)
after <- c(7, 2, 3, 1, 4, 8, 5, 2, 3, 6, 6, 3)
alpha <- 0.001

diff <- before - after
mean_diff <- mean(diff)
sd_diff <- sd(diff)
n <- length(diff)
t_statistic <- mean_diff / (sd_diff / sqrt(n))
t_statistic

df <- n - 1
p_value <- 2 * (1 - pt(abs(t_statistic), df))
p_value

critical_t <- qt(1 - alpha / 2, df)
rejection_region <- c(-critical_t, critical_t)
rejection_region

# 2 b
alpha <- 0.2
critical_t <- qt(1 - alpha/2, df)
CI_lower <- mean_diff - critical_t * (sd_diff / sqrt(n))
CI_upper <- mean_diff + critical_t * (sd_diff / sqrt(n))
CI <- c(CI_lower, CI_upper)
CI

# 3 a
A <- c(5, 6, 7, 6)
B <- c(4, 3, 2, 3)
C <- c(5, 4, 6, 4)
D <- c(3, 3, 4, 5)
table4<-data.frame(A,B,C,D)
group_data <- stack(table4)
anova_result <- aov(values ~ ind, data = group_data)
summary(anova_result)

# 3 b
# From 3 a
df_ind <- 3
df_residuals <- 12

alpha <- 0.01

critical_F <- qf(1 - alpha, df_ind, df_residuals)
rejection_region <- c(-critical_F, critical_F)
rejection_region


# 4 b
cholesterol <- c(180, 185, 190, 195, 198, 200, 205, 210, 215, 220, 
                 205, 210, 215, 220, 225)
waist_circumference <- c(33, 30, 32, 34, 35, 36, 38, 40, 41, 42, 
                         38, 39, 40, 41, 42)

model <- lm(cholesterol ~ waist_circumference)
model

#4 c
plot(waist_circumference, cholesterol)
abline(model, col = "red")

#4 d
R <- cor(cholesterol, waist_circumference)
R

slope <- (sd(cholesterol)/sd(waist_circumference)) * R
slope

intercept <- mean(cholesterol) - slope * mean(waist_circumference)
intercept

#4 e
R <- cor(cholesterol, waist_circumference)
R

#4 f
R2 <- R^2
R2

