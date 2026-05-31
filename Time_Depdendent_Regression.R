# Packages

library(tidyverse)

#========================================================================
# Linear Regression

nyTemps <- data.frame(time = seq(1,nrow(airquality),1),
                      temp = airquality$Temp)

ggplot(data = nyTemps, aes(x = time, y = temp))+
  geom_line()


# Fit the model with time as a predictor
tempreg <- lm(temp ~ time, data = nyTemps)

# Check out the summary
summary(tempreg)

predict(tempreg)

plot(tempreg$residuals, predict(tempreg))

#========================================================================

nyTemps$timeSq <- nyTemps$time^2

tempreg2 <- lm(temp ~ time + timeSq, data = nyTemps)

summary(tempreg2)

hist(tempreg2$residuals)

acf(tempreg2$residuals)



# Add a lag-1 response to the data set
nyTemps$tempLag1 <- lag(nyTemps$temp,1)

# Get rid of the NA that was introduced
nyTemps <- nyTemps %>% na.omit(tempLag1)

# Fit our autoregressive regression model
tempregAuto <- lm(temp ~ time + timeSq + tempLag1, data = nyTemps)

# The lag is significant as expected!
summary(tempregAuto)

plot(tempregAuto$residuals, predict(tempregAuto))

acf(tempregAuto$residuals)

#========================================================================
# Lung Deaths Dataset

plot(ldeaths)
ld <- ldeaths

# Create a time variable in the ldeaths data
tmax<-length(ldeaths)
t <- 2:tmax

# Create a data frame with the sine and cosine terms
YX <- data.frame(ld=ld[2:tmax], ldpast=ld[1:(tmax-1)], t=t,
                 sin12=sin(2*pi*t/12), cos12=cos(2*pi*t/12))

# Fit the model
lunglm <- lm(ld ~ t + ldpast + sin12 + cos12, data=YX)

# The seasonal terms are significant!
summary(lunglm)

# Add predicted values
YX$preds <- predict(lunglm)

# Plot
ggplot(data = YX, aes(x = t))+
  geom_line(aes(y = ld), color = "lightblue", linewidth = 1.5)+
  geom_line(aes(y = preds), color = "indianred", linewidth = 1.5, linetype = 2)+
  theme_bw()
