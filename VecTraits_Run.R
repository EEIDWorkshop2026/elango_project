library(tidyverse)
library(rgl)

source("VecTraits_Dataset_Access.R")

list_df <- searchDatasets("ixodes scapularis")


for(i in 1:length(list_df)){
  name <- paste("df", i, sep = "")
  assign(name, list_df[[i]]$results)
  df <- list_df[[i]]$results
  # if(i > 1){
  #   main_dataframe <- rbind(main_dataframe, df)
  # }
  # else {
  #   main_dataframe <- df
  # }
}


rm(list = setdiff(ls(), c("df2", "df6")))

df2 <- df2 %>% 
  filter(OriginalTraitValue > 0)

df6 <- df6 %>% 
  filter(OriginalTraitValue > 0)

df <- rbind(df2, df6)

# Make a data set of the aggregated mean values
development_rate_mean2 <- df2 %>% 
  group_by(Interactor1Temp) %>%
  summarise(Trait = mean(1 / OriginalTraitValue), .groups = "drop") %>%
  mutate(curve_ID = factor(1), Temp = Interactor1Temp)



# Make a dataset of the individual-level values
development_rate_individuals2 <- df2 %>%
  mutate(curve_ID = factor(2),
         Temp = Interactor1Temp,
         Trait = 1 / OriginalTraitValue)


development_rate_mean6 <- df6 %>% 
  group_by(Interactor1Temp) %>%
  summarise(Trait = mean(1 / OriginalTraitValue), .groups = "drop") %>%
  mutate(curve_ID = factor(1), Temp = Interactor1Temp)



# Make a dataset of the individual-level values
development_rate_individuals6 <- df6 %>%
  mutate(curve_ID = factor(2),
         Temp = Interactor1Temp,
         Trait = 1 / OriginalTraitValue)



ggplot() +
  geom_jitter(data = development_rate_individuals2,
              aes(Temp, Trait),
              size = 2, shape = 21, fill = "black", col = "white",
              width = 0.12) +
  geom_point(data = development_rate_mean2,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "red") +
  geom_point(data = development_rate_mean6,
             aes(Temp, Trait),
             size = 3, shape = 23, colour = "black", fill = "black") +
  geom_point(data = development_rate_mean6,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "blue") +
  theme_bw()


briere2 <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
               start = list(a = 1, tmin = 10, tmax = 32),
               data = development_rate_individuals2)

summary(briere2)

briere6 <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
               start = list(a = 1, tmin = 2, tmax = 37),
               data = development_rate_individuals6)



# Generate predicted values to graph
tempDat2 <- data.frame(Temp = 
                         seq(min(development_rate_individuals2$Temp),
                             max(development_rate_individuals2$Temp),
                             length.out = 100))

d_preds2 <- predict(briere2, newdata = tempDat2)
tempDat2$preds <- d_preds2

tempDat6 <- data.frame(Temp = 
                         seq(min(development_rate_individuals6$Temp),
                             max(development_rate_individuals6$Temp),
                             length.out = 100))

d_preds6 <- predict(briere6, newdata = tempDat6)
tempDat6$preds <- d_preds6


# Graph
ggplot() +
  geom_jitter(data = development_rate_individuals2,
              aes(Temp, Trait),
              size = 2, shape = 21, fill = "black", col = "white",
              width = 0.12) +
  geom_point(data = development_rate_mean2,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "red") +
  geom_point(data = development_rate_mean6,
             aes(Temp, Trait),
             size = 3, shape = 23, colour = "black", fill = "black") +
  geom_point(data = development_rate_mean6,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "blue") +
  geom_line(data = tempDat2,
            aes(x = Temp, y = preds), color = "blue") +
  geom_line(data = tempDat6,
            aes(x = Temp, y = preds), color = "green") +
  theme_bw()

#================================================================================================

library(car)
library(gridExtra)

boot2 <- Boot(briere2, method = 'case', R = 100)

boot6 <- Boot(briere6, method = 'case', R = 100)

hist(boot2, c(2))

hist(boot6, c(2))
