


options(scipen = 9999)


subscription.df <- read.csv("subscription.csv")
head(subscription.df)

# Drop rows which has NA 
subscription.df <- subset(subscription.df, rowSums(is.na(subscription.df)) == 0)
n_without_NA <- nrow(subscription.df)

# Get a matrix of scatterplots
pairs(~ month+tot_mem+avg_temp+deg_ht+pr_rain+pr_snow+age, data = subscription.df)
plot(subscription.df)

# Stepwise regression
null <- lm(tot_call ~ 1, data = subscription.df)
full <- lm(tot_call ~ month+tot_mem+avg_temp+deg_ht+pr_rain+pr_snow+age, data = subscription.df)

# Use AIC
step_AIC <- step(null, scope = list(lower = null, upper = full), direction = "both")
summary(step_AIC)##better

# Use BIC
step_BIC <- step(null, scope = list(lower = null, upper = full), direction = "both", k = log(n_without_NA))
summary(step_BIC)

# Detect outliers, we choose AIC because AIC is better than BIC
subscription.df$std_AIC <- rstandard(step_AIC)
subscription.df$cook_AIC <- cooks.distance(step_AIC)

# Drop outliers
subscription_without_outliers.df <- subset(subscription.df, std_AIC <= 2)[1:8]
n_without_outliers <- nrow(subscription_without_outliers.df)

# Stepwise regression after dropping outliers
null_without_outliers <- lm(tot_call ~ 1, data = subscription_without_outliers.df)
full_without_outliers <- lm(tot_call ~ month+tot_mem+avg_temp+deg_ht+pr_rain+pr_snow+age, 
                            data = subscription_without_outliers.df)

# Use AIC after dropping outliers
step_without_outliers_AIC <- step(null_without_outliers, 
                                  scope = list(lower = null_without_outliers, upper = full_without_outliers), 
                                  direction = "both")
summary(step_without_outliers_AIC)

# Use BIC after dropping outliers
step_without_outliers_BIC <- step(null_without_outliers, 
                                  scope = list(lower = null_without_outliers, upper = full_without_outliers), 
                                  direction = "both", k = log(n_without_outliers))
summary(step_without_outliers_BIC)##same as using AIC


# Testing Homoscedasticity
plot(step_without_outliers_AIC$fitted.values, step_without_outliers_AIC$residuals)
zres<-rstandard(step_without_outliers_AIC)
plot(step_without_outliers_AIC$fitted.values, zres)

# Testing Linearity

plot(subscription_without_outliers.df$deg_ht, zres)
plot(subscription_without_outliers.df$age, zres)
plot(subscription_without_outliers.df$avg_temp, zres)
plot(subscription_without_outliers.df$tot_mem, zres)
plot(subscription_without_outliers.df$month, zres)
plot(subscription_without_outliers.df$pr_snow, zres)

#Test for Normality
hist(step_without_outliers_AIC$residuals)
qqnorm(step_without_outliers_AIC$residuals)
qqline(step_without_outliers_AIC$residuals)
shapiro.test(step_without_outliers_AIC$residuals)

#Test of Independence
data<-data.frame(total_calls=c(1:n_without_outliers))
newsubscription_without_outliers.df <- cbind(subscription_without_outliers.df, data)
plot(newsubscription_without_outliers.df$total_calls, step_without_outliers_AIC$residuals)


library("lmtest")
dwtest(step_without_outliers_AIC, alternative = "two.sided")

## Factor Analysis
library(psych)

subscription_without_outliers.df$month <- as.numeric(subscription_without_outliers.df$month)

KCP <- data.frame(subscription_without_outliers.df$deg_ht, subscription_without_outliers.df$age, 
                  subscription_without_outliers.df$avg_temp, subscription_without_outliers.df$tot_mem, 
                  subscription_without_outliers.df$month, subscription_without_outliers.df$pr_snow)

Correlation_KCP <- cor(KCP, use = "complete.obs")

factor_find <- fa(r = KCP, nfactors = 3, rotate="cluster", fm="ml")
# ML1 = Long-term Value, ML2 = Short-term Value, ML3 = Satisfaction, ML4 = Reputation, ML5 = Pricing

factor_scores <- factor_find$scores

subscriptionreg.df <- data.frame(subscription_without_outliers.df,factor_scores)

subscriptionreg<-lm(tot_call ~ ML1+ML2+ML3, data=subscriptionreg.df)
summary(subscriptionreg)



