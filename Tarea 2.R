library(tidyverse)
library(haven)
library(estimatr)
library(stargazer)
# Load flextable and modelsummary
library(modelsummary)
library(stats)
library(rdrobust)
library(tidyverse)
library(rddensity)
library(rdd)


read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

lmb_data <- read_data("lmb-data.dta")

lmb_subset <- lmb_data %>% 
  filter(lagdemvoteshare>.48 & lagdemvoteshare<.52)

lm_1 <- lm_robust(score ~ lagdemocrat, data = lmb_subset, clusters = id)
lm_2 <- lm_robust(score ~ democrat, data = lmb_subset, clusters = id)
lm_3 <- lm_robust(democrat ~ lagdemocrat, data = lmb_subset, clusters = id)

s1<-summary(lm_1)
s2<-summary(lm_2)
s3<-summary(lm_3)

stargazer(list(LM_1))

modelsummary(list(lm_1,lm_2,lm_3), stars = TRUE, statistic = 'std.error',
             fmt= '%.4f')


###################codigo 2##########

lm_1 <- lm_robust(score ~ lagdemocrat, data = lmb_data, clusters = id)
lm_2 <- lm_robust(score ~ democrat, data = lmb_data, clusters = id)
lm_3 <- lm_robust(democrat ~ lagdemocrat, data = lmb_data, clusters = id)

summary(lm_1)
summary(lm_2)
summary(lm_3)

modelsummary(list(lm_1,lm_2,lm_3), stars = TRUE, statistic = 'std.error',
             fmt= '%.4f')

################### Codigo 3#######################

lmb_data <- lmb_data %>% 
  mutate(demvoteshare_c = demvoteshare - 0.5)

lm_1 <- lm_robust(score ~ lagdemocrat + demvoteshare_c, data = lmb_data, clusters = id)
lm_2 <- lm_robust(score ~ democrat + demvoteshare_c, data = lmb_data, clusters = id)
lm_3 <- lm_robust(democrat ~ lagdemocrat + demvoteshare_c, data = lmb_data, clusters = id)

summary(lm_1)
summary(lm_2)
summary(lm_3)

modelsummary(list(lm_1,lm_2,lm_3), stars = TRUE, statistic = 'std.error',
             fmt= '%.4f')


######################codigo 4##############################

lm_1 <- lm_robust(score ~ lagdemocrat*demvoteshare_c, 
                  data = lmb_data, clusters = id)
lm_2 <- lm_robust(score ~ democrat*demvoteshare_c, 
                  data = lmb_data, clusters = id)
lm_3 <- lm_robust(democrat ~ lagdemocrat*demvoteshare_c, 
                  data = lmb_data, clusters = id)

summary(lm_1)
summary(lm_2)
summary(lm_3)

modelsummary(list(lm_1,lm_2,lm_3), stars = TRUE, statistic = 'std.error',
             fmt= '%.4f')

##################codigo 5######################

lmb_data <- lmb_data %>% 
  mutate(demvoteshare_sq = demvoteshare_c^2)

lm_1 <- lm_robust(score ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_sq, 
                  data = lmb_data, clusters = id)
lm_2 <- lm_robust(score ~ democrat*demvoteshare_c + democrat*demvoteshare_sq, 
                  data = lmb_data, clusters = id)
lm_3 <- lm_robust(democrat ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_sq, 
                  data = lmb_data, clusters = id)

summary(lm_1)
summary(lm_2)
summary(lm_3)



modelsummary(list(lm_1,lm_2,lm_3), stars = TRUE, statistic = 'std.error',
             fmt= '%.4f')


############ cODIGO 6###########

lmb_data <- lmb_data %>% 
  filter(demvoteshare > .45 & demvoteshare < .55) %>%
  mutate(demvoteshare_sq = demvoteshare_c^2)

lm_1 <- lm_robust(score ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_sq, 
                  data = lmb_data, clusters = id)
lm_2 <- lm_robust(score ~ democrat*demvoteshare_c + democrat*demvoteshare_sq, 
                  data = lmb_data, clusters = id)
lm_3 <- lm_robust(democrat ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_sq, 
                  data = lmb_data, clusters = id)

summary(lm_1)
summary(lm_2)
summary(lm_3)


modelsummary(list(lm_1,lm_2,lm_3), stars = TRUE, statistic = 'std.error',
             fmt= '%.4f')

###########cODIGO 7########################

categories <- lmb_data$lagdemvoteshare

demmeans <- split(lmb_data$score, cut(lmb_data$lagdemvoteshare, 100)) %>% 
  lapply(mean) %>% 
  unlist()

agg_lmb_data <- data.frame(score = demmeans, lagdemvoteshare = seq(0.01,1, by = 0.01))

#plotting
lmb_data <- lmb_data %>% 
  mutate(gg_group = case_when(lagdemvoteshare > 0.5 ~ 1, TRUE ~ 0))

fig1<-ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)

fig2<-ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "loess") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)

fig3<-ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)



ggsave(fig1, filename = "fig1.png",
       dpi = 800, width = 20, height = 12)
ggsave(fig2, filename = "fig2.png",
       dpi = 800, width = 20, height = 12)
ggsave(fig3, filename = "fig3.png",
       dpi = 800, width = 20, height = 12)

##########codigo 8###################




smooth_dem0 <- lmb_data %>% 
  filter(democrat == 0) %>% 
  select(score, demvoteshare)
smooth_dem0 <- as_tibble(ksmooth(smooth_dem0$demvoteshare, smooth_dem0$score, 
                                 kernel = "box", bandwidth = 0.1))
plot(smooth_dem0$score,smooth_dem0$demvoteshare)

smooth_dem1 <- lmb_data %>% 
  filter(democrat == 1) %>% 
  select(score, demvoteshare) %>% 
  na.omit()
smooth_dem1 <- as_tibble(ksmooth(smooth_dem1$demvoteshare, smooth_dem1$score, 
                                 kernel = "box", bandwidth = 0.1))

plot(smooth_dem1$y,smooth_dem1$x)


ggplot() + 
  geom_smooth(aes(demvoteshare,score), data = smooth_dem0,method = 'gam') +
  geom_smooth(aes(demvoteshare,score), data = smooth_dem1,method = 'gam') +
  geom_vline(xintercept = 0.5)

############codigo 9###############



rdr <- rdrobust(y = lmb_data$score,
                x = lmb_data$demvoteshare, c = 0.5)
summary(rdr)



###################codigo 10#################


DCdensity(lmb_data$demvoteshare, cutpoint = 0.5)

density <- rddensity(lmb_data$demvoteshare, c = 0.5)
rdplotdensity(density, lmb_data$demvoteshare)

