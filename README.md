# polyregression
df<- read.csv("Position_Salaries.csv")
df<- df[2:3]

lin_regressor <- lm(formula = Salary ~ .,
                      data = df)

#adding levels
df$Level2 <-  df$Level^2
df$Level3 <- df$Level^3
df$Level4 <- df$Level^4

poly_regressor <- lm(formula = Salary ~ .,
                     data = df )
summary(poly_regressor)
#visualizing both plots

install.packages("ggplot2")

# visualizing for linear vs polynomial regression
# linear regression
ggplot() +
  geom_point(aes(x = df$Level, y = df$Salary), color = "Red") +
  geom_line(aes(x = df$Level, y = predict(lin_regressor, newdata = df)), color ="blue") +
  ggtitle("level vs salary") +
  xlab("level") +
  ylab("salary")

#polynomial regression
ggplot() +
  geom_point(aes(x = df$Level, y = df$Salary), color = "Red") +
  geom_line(aes(x = df$Level, y = predict(poly_regressor, newdata = df)), color ="blue") +
  ggtitle("level vs salary") +
  xlab("level") +
  ylab("salary")

# predicting for level =  6.5

y_predict1 <- predict(lin_regressor, data.frame(Level = 6.5))
y_predict2 <- predict(poly_regressor, data.frame(Level = 6.5,
                                                 Level2 = 6.5^2,
                                                 Level3 = 6.5^3,
                                                 Level4 = 6.5^4))





