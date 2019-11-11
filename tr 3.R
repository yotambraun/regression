#שם:יותם בראון 309914646 שם:ענבר נחמיה 205434699
library(datasets)
View(mtcars)
cars=mtcars
model <- lm(mpg~disp+wt,data =cars)
summary(model)
model$coefficients 

x <- matrix(rep(1,32), nrow = 32, ncol = 3)
x[,2] <- mtcars$disp
x[,3] <- mtcars$wt
B1 <- model$coefficients[2]
B2 <- model$coefficients[3]

g <- solve(t(x)%*%x)
e <- c(resid(model))
s <- sqrt(as.numeric(t(e)%*%e)/(32-2-1))

T1 <- (B1 -0)/(s*(g[2,2])^0.5)
T2 <- (B2 -0)/(s*(g[3,3])^0.5)

 #3
a1 = 0.05
a2 = 0.1

abs(T1) >= qt((1-a1/2),(32-2-1))
abs(T2) >= qt((1-a1/2),(32-2-1))

abs(T1) >= qt((1-a2/2),(32-2-1))
abs(T2) >= qt((1-a2/2),(32-2-1))

#4

l <- sqrt(as.numeric(((t(e)%*%e))/qchisq((1-??1/2),32-2-1)))
u <- sqrt((as.numeric(((t(e)%*%e))/qchisq((??1/2),32-2-1))))







          