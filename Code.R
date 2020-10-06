# Load libraries
library(tidyverse)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(emmeans)
library(gridExtra)

# Read the data and explore the data
df <- read.csv("locomotiondata.csv")
head(df)
str(df)
df <- df %>%
  rename(Day=?..Day)


# Create small data frames and change from the wide to the tall format
vars <- c("TotalEvent", "TotalDistance", "TotalTime", "TotalAvgSpeed", 
         "FastEvent", "FastDistance", "FastTime", "FastAvgSpeed", "SlowEvent",
         "SlowDistance", "SlowTime", "SlowAvgSpeed")
for (var in vars) {
  list_var = c(paste0(var, c(1:10)))
  var_name = paste0(var, "_df")

  df_new <- df %>%
    select(Day, CricketID, StressLevel, list_var) %>%
    pivot_longer(list_var, names_to = var, values_to = paste0(var, "Value"))
  
  assign(var_name, df_new)
}


# Extract the last character which represents for the minute of the experiment
TotalEvent_df <- TotalEvent_df %>%
  mutate(Minute = as.numeric(str_sub(TotalEvent,-1))) %>%
  select(-TotalEvent)

TotalDistance_df <- TotalDistance_df %>%
  mutate(Minute = as.numeric(str_sub(TotalDistance,-1))) %>%
  select(-TotalDistance)

TotalTime_df <- TotalTime_df %>%
  mutate(Minute = as.numeric(str_sub(TotalTime,-1))) %>%
  select(-TotalTime)

TotalAvgSpeed_df <- TotalAvgSpeed_df %>%
  mutate(Minute = as.numeric(str_sub(TotalAvgSpeed,-1))) %>%
  select(-TotalAvgSpeed)

FastEvent_df <- FastEvent_df %>%
  mutate(Minute = as.numeric(str_sub(FastEvent,-1))) %>%
  select(-FastEvent)

FastDistance_df <- FastDistance_df %>%
  mutate(Minute = as.numeric(str_sub(FastDistance,-1))) %>%
  select(-FastDistance)

FastTime_df <- FastTime_df %>%
  mutate(Minute = as.numeric(str_sub(FastTime,-1))) %>%
  select(-FastTime)

FastAvgSpeed_df <- FastAvgSpeed_df %>%
  mutate(Minute = as.numeric(str_sub(FastAvgSpeed,-1))) %>%
  select(-FastAvgSpeed)

SlowEvent_df <- SlowEvent_df %>%
  mutate(Minute = as.numeric(str_sub(SlowEvent,-1))) %>%
  select(-SlowEvent)

SlowDistance_df <- SlowDistance_df %>%
  mutate(Minute = as.numeric(str_sub(SlowDistance,-1))) %>%
  select(-SlowDistance)

SlowTime_df <- SlowTime_df %>%
  mutate(Minute = as.numeric(str_sub(SlowTime,-1))) %>%
  select(-SlowTime)

SlowAvgSpeed_df <- SlowAvgSpeed_df %>%
  mutate(Minute = as.numeric(str_sub(SlowAvgSpeed,-1))) %>%
  select(-SlowAvgSpeed)


# Merge small dataframes together
df_new <- merge(TotalDistance_df, TotalTime_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, TotalAvgSpeed_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, TotalEvent_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, FastEvent_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, FastDistance_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, FastTime_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, FastAvgSpeed_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, SlowEvent_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, SlowDistance_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, SlowTime_df, by= c("Day", "CricketID", "StressLevel", "Minute"))
df_new <- merge(df_new, SlowAvgSpeed_df, by= c("Day", "CricketID", "StressLevel", "Minute"))

# Change the Minute variable
df_new <- df_new %>%
  mutate(Minute = Minute + 1)

# Create a data frame for the first 5 minutes
df_new_f <- df_new %>%
  filter(Minute %in% c(1:5))

# Create a data frame for the last 5 minutes
df_new_l <- df_new %>%
  filter(Minute %in% c(6:10))

# Create a new data frame to calcualte the average of variables by day, cricketID, and the stress level for the first 5 minutes
df_new_f <- df_new_f %>%
  group_by(Day, CricketID, StressLevel) %>%
  summarize(FAvgTotalDistanceValue = mean(TotalDistanceValue),
            FAvgTotalTimeValue = mean(TotalTimeValue),
            FAvgTotalAvgSpeedValue = mean(TotalAvgSpeedValue),
            FAvgTotalEventValue = mean(TotalEventValue),
            FAvgFastEventValue = mean(FastEventValue),
            FAvgFastDistanceValue = mean(FastDistanceValue),
            FAvgTotalEventValue = mean(TotalEventValue),
            FAvgFastTimeValue = mean(FastTimeValue),
            FAvgSlowDistanceValue = mean(SlowDistanceValue),
            FAvgSlowTimeValue = mean(SlowTimeValue),
            FAvgSlowAvgSpeedValue = mean(SlowAvgSpeedValue))

# Create a new dataframe to calcualte the average of variables by day, cricketID, and the stress level for the last 5 minutes
df_new_l <- df_new_l %>%
  group_by(Day, CricketID, StressLevel) %>%
  summarize(LAvgTotalDistanceValue = mean(TotalDistanceValue),
            LAvgTotalTimeValue = mean(TotalTimeValue),
            LAvgTotalAvgSpeedValue = mean(TotalAvgSpeedValue),
            LAvgTotalEventValue = mean(TotalEventValue),
            LAvgFastEventValue = mean(FastEventValue),
            LAvgFastDistanceValue = mean(FastDistanceValue),
            LAvgTotalEventValue = mean(TotalEventValue),
            LAvgFastTimeValue = mean(FastTimeValue),
            LAvgSlowDistanceValue = mean(SlowDistanceValue),
            LAvgSlowTimeValue = mean(SlowTimeValue),
            LAvgSlowAvgSpeedValue = mean(SlowAvgSpeedValue))

# Merge 2 data sets together
df_new_avg_fl <- merge(df_new_f, df_new_l, by=c("Day", "CricketID", "StressLevel"))

# Change variables to factor
df_new_avg_fl <- df_new_avg_fl %>%
  mutate(Day = factor(Day, levels=c("2", "7", "14")),
         CricketID = as.factor(CricketID),
         StressLevel = ifelse(StressLevel == 0, "NCS",
                              ifelse(StressLevel == 1, "CS 1h",
                                     "CS 6h")),
         StressLevel = factor(StressLevel, levels = c("NCS", "CS 1h", "CS 6h")))



########################
### First Model
# Dataframe for the first model
model1_df <- df_new_avg_fl %>%
  select(Day, CricketID, StressLevel, FAvgTotalAvgSpeedValue, LAvgTotalAvgSpeedValue) %>%
  pivot_longer(-c(Day, CricketID, StressLevel),
               names_to = "TimeInterval",
               values_to = "AvgTotalAvgSpeedValue") %>%
  mutate(TimeInterval = as.factor(ifelse(TimeInterval=="FAvgTotalAvgSpeedValue",
                                         "First 5 Minutes",
                                         "Last 5 Minutes")),
         Day_TimeInterval = as.factor(paste0(TimeInterval, " in day ", Day)),
         Day_TimeInterval = factor(Day_TimeInterval, levels=c("First 5 Minutes in day 2",
                                                              "Last 5 Minutes in day 2",
                                                              "First 5 Minutes in day 7",
                                                              "Last 5 Minutes in day 7",
                                                              "First 5 Minutes in day 14",
                                                              "Last 5 Minutes in day 14")))

# Source: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#:~:text=The%20repeated%2Dmeasures%20ANOVA%20is,are%20measured%20more%20than%20once.&text=The%20%E2%80%9Cwithin%2Dsubjects%E2%80%9D%20term,different%20time%20points%20or%20conditions.
# Get summary statistics of the average of the total average speed value
model1_df %>%
  group_by(StressLevel, Day, TimeInterval) %>%
  get_summary_stats(AvgTotalAvgSpeedValue, type = "mean_sd")

# Visualization
ggboxplot(
  model1_df, x = "StressLevel", y = "AvgTotalAvgSpeedValue",
  color = "TimeInterval", palette = "jco",
  facet.by = "Day", short.panel.labs = FALSE,
  ylab = "Average of the total \naverage speed value (mm/s)",
  xlab = "Stress Level",
  legend.title= "Time Interval"
)

# The Average of Total Average Speed by Stress Level, Day, and Time Interval across all Crickets
ggplot(model1_df, aes(x=StressLevel, y=AvgTotalAvgSpeedValue, group=TimeInterval, color=TimeInterval)) + 
  stat_summary(fun=mean, geom="line") + 
  stat_summary(fun=mean, geom="point") + 
  labs(x="Stress level",
       y="The average of total average speed (mm/s)",
       color="Time Interval") +
  facet_wrap(~Day,
             labeller = labeller(Day = c("2" = "Day 2", "7" = "Day 7", "14" = "Day 14"))) + 
  theme_bw()

# Check Outlier assumption
outlier_m1 <- model1_df %>%
  group_by(StressLevel, Day_TimeInterval) %>%
  identify_outliers(AvgTotalAvgSpeedValue)
outlier_m1

# Normality assumption: Compute Shapiro-Wilk test for each combinations of factor levels
model1_df %>%
  group_by(StressLevel, Day_TimeInterval) %>%
  shapiro_test(AvgTotalAvgSpeedValue)

# QQ Plot
ggqqplot(model1_df, "AvgTotalAvgSpeedValue", ggtheme = theme_bw()) +
  facet_grid(Day_TimeInterval ~ StressLevel, labeller = "label_both")

# Model 1 Construction
model1_anova <- aov(AvgTotalAvgSpeedValue ~ StressLevel*Day_TimeInterval + 
                      Error(CricketID/Day_TimeInterval), data=model1_df)
summary(model1_anova)

# Models without extreme outliers
# Create a dataframe with extreme outliers
outlier_m1 <- outlier_m1[which(outlier_m1$is.extreme == TRUE),]
selectedRows <- (   model1_df$StressLevel %in% outlier_m1$StressLevel &
                    model1_df$Day_TimeInterval %in% outlier_m1$Day_TimeInterval &
                    model1_df$Day %in% outlier_m1$Day &
                    model1_df$CricketID %in% outlier_m1$CricketID &
                    model1_df$TimeInterval %in% outlier_m1$TimeInterval &
                    model1_df$AvgTotalAvgSpeedValue %in% outlier_m1$AvgTotalAvgSpeedValue)
# Create a dataframe without extreme outliers
m1_df <- model1_df[!selectedRows,]
m1_df %>%
  group_by(StressLevel, Day_TimeInterval) %>%
  identify_outliers(AvgTotalAvgSpeedValue)

# Run an 2-way repeated measure anova without extreme outliers
m1_anova <- aov(AvgTotalAvgSpeedValue ~ StressLevel*Day_TimeInterval + 
                      Error(CricketID/Day_TimeInterval), data=m1_df)
summary(m1_anova)



# Multiple Comparison follow-ups
# Pairwise Comparison
model1.emm.s <- emmeans(model1_anova, "Day_TimeInterval")
model1.emm.s

# Customized matrix function 
# Source code: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
skip_comp.emmc <- function(levels, skip = 1, reverse = FALSE) {
  if((k <- length(levels)) < skip + 1)
    stop("Need at least ", skip + 1, " levels")
  coef <- data.frame()
  coef <- as.data.frame(lapply(seq_len(k - skip - 1), function(i) {
    sgn <- ifelse(reverse, -1, 1)
    sgn * c(rep(0, i - 1), 1, rep(0, skip), -1, rep(0, k - i - skip - 1))
  }))
  names(coef) <- sapply(coef, function(x)
    paste(which(x == 1), "-", which(x == -1)))
  attr(coef, "adjust") = "fdr"   # default adjustment method
  coef
}

# Compare the first 5 minutes and the last 5 minutes by individual days
mtrx1 <- skip_comp.emmc(1:6, skip = 0, reverse = FALSE)
mtrx1
# Only compare level 1 to 2, level 3 to 4, and level 5 to 6
mtrx1 <- mtrx1 %>%
  select(-c('2 - 3','4 - 5'))
mtrx1
contrast(model1.emm.s, mtrx1)
# 95% confidence interval of the pairwise comparison
confint(contrast(model1.emm.s, mtrx1)) 
plot(contrast(model1.emm.s, mtrx1))

# Compare the first 5 minutes and the last 5 minutes in all 3 days experiment
LF <- contrast(model1.emm.s, 
              list(lambda1 = c(1, -1, 1, -1, 1, -1)),
               offset = 0)
LF
# 95% confidence interval
confint(LF) 



########################
### Second Model
# Dataframe for the first model
# Data set for the second model
model2_df <- df_new_avg_fl %>%
  select(Day, CricketID, StressLevel, FAvgTotalDistanceValue, LAvgTotalDistanceValue) %>%
  pivot_longer(-c(Day, CricketID, StressLevel),
               names_to = "TimeInterval",
               values_to = "AvgTotalDistanceValue") %>%
  mutate(TimeInterval = as.factor(ifelse(TimeInterval=="FAvgTotalDistanceValue",
                                         "First 5 Minutes",
                                         "Last 5 Minutes")),
         Day_TimeInterval = paste0(TimeInterval, " in day ", Day),
         Day_TimeInterval = factor(Day_TimeInterval, levels=c("First 5 Minutes in day 2",
                                                              "Last 5 Minutes in day 2",
                                                              "First 5 Minutes in day 7",
                                                              "Last 5 Minutes in day 7",
                                                              "First 5 Minutes in day 14",
                                                              "Last 5 Minutes in day 14")))

# Source: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#:~:text=The%20repeated%2Dmeasures%20ANOVA%20is,are%20measured%20more%20than%20once.&text=The%20%E2%80%9Cwithin%2Dsubjects%E2%80%9D%20term,different%20time%20points%20or%20conditions.
# Get summary statistics of the average of the total average speed value
model2_df %>%
  group_by(StressLevel, Day_TimeInterval) %>%
  get_summary_stats(AvgTotalDistanceValue, type = "mean_sd")

# Visualization
ggboxplot(
  model2_df, x = "StressLevel", y = "AvgTotalDistanceValue",
  color = "TimeInterval", palette = "jco",
  facet.by = "Day", short.panel.labs = FALSE,
  ylab = "Average of the total \ndistance value (mm)",
  xlab = "Stress Level",
  legend.title = "Time Interval"
)

# The Average of Total Average Speed \nby StressLevel, Day, and Time Interval across 3 days
ggplot(model2_df, aes(x=StressLevel, y=AvgTotalDistanceValue, group=TimeInterval, color=TimeInterval)) + 
  stat_summary(fun=mean, geom="line") + 
  stat_summary(fun=mean, geom="point") + 
  labs(x="Stress level",
       y="The average of total average speed (mm/s)",
       color="Time Interval") +
  facet_wrap(~Day,
             labeller = labeller(Day = c("2" = "Day 2", "7" = "Day 7", "14" = "Day 14"))) + 
  theme_bw()

# Check outlier Assumption
outlier_m2 <- model2_df %>%
  group_by(StressLevel, Day_TimeInterval) %>%
  identify_outliers(AvgTotalDistanceValue)
outlier_m2

# Normality Assumption: Compute Shapiro-Wilk test for each combinations of factor levels
model2_df %>%
  group_by(StressLevel, Day_TimeInterval) %>%
  shapiro_test(AvgTotalDistanceValue)

# QQ Plot
ggqqplot(model2_df, "AvgTotalDistanceValue", ggtheme = theme_bw()) +
  facet_grid(Day_TimeInterval ~ StressLevel, labeller = "label_both")

# Model Construction
model2_anova <- aov(AvgTotalDistanceValue ~ StressLevel*Day_TimeInterval + 
                      Error(CricketID/Day_TimeInterval), data=model2_df)
summary(model2_anova)


# Multiple Comparison follow-ups
# Pairwise Comparison
model2.emm.s <- emmeans(model2_anova, "Day_TimeInterval")
model2.emm.s

# Compare the first 5 minutes and the last 5 minutes in each day of 3 days experiment
mtrx2<- skip_comp.emmc(1:6, skip = 0, reverse = FALSE)
mtrx2
# Only compare level 1 to 2, level 3 to 4, and level 5 to 6
mtrx2 <- mtrx2 %>%
  select(-c('2 - 3','4 - 5'))
mtrx2
contrast(model2.emm.s, mtrx2)
confint(contrast(model2.emm.s, mtrx2)) # 95% confidence interval
plot(contrast(model2.emm.s, mtrx2))

# Compare the first 5 minutes and the last 5 minutes in all 3 days experiment
LF1 <- contrast(model2.emm.s, 
               list(lambda1 = c(1, -1, 1, -1, 1, -1)),
               offset = 0)
LF1
confint(LF1) # 95% confidence interval



# Combine plots for presentation
# Combined plot 1: The mean values by stress level, day, and time interval (line plot)
# p1: The average of total average speed
p1<-ggplot(model1_df, aes(x=StressLevel, y=AvgTotalAvgSpeedValue, group=TimeInterval, color=TimeInterval)) + 
  stat_summary(fun=mean, geom="line") + 
  stat_summary(fun=mean, geom="point") + 
  labs(x="Stress level",
       y="The average of total average speed (mm/s)",
       color="Time Interval",
       title="The mean of the average of the total average speed") +
  facet_wrap(~Day,
             labeller = labeller(Day = c("2" = "Day 2", "7" = "Day 7", "14" = "Day 14"))) + 
  theme_bw()
# p2: The average of total average distance
p2<-ggplot(model2_df, aes(x=StressLevel, y=AvgTotalDistanceValue, group=TimeInterval, color=TimeInterval)) + 
  stat_summary(fun=mean, geom="line") + 
  stat_summary(fun=mean, geom="point") + 
  labs(x="Stress level",
       y="The average of total average distance (mm)",
       color="Time Interval",
       title="The mean of the average of the total distance ") +
  facet_wrap(~Day,
             labeller = labeller(Day = c("2" = "Day 2", "7" = "Day 7", "14" = "Day 14"))) + 
  theme_bw()

# Create the same legend
# Source: https://stackoverflow.com/questions/38559645/using-arrangegrob-to-add-a-sub-plot-as-a-legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(p1)
# Combine 2 plots: p1 + p2
grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2, heights=c(10, 2))


# Combined plot 2: The mean values by stress level, day, and time interval (box plot)
# p3: The average of the total average speed
p3<-ggboxplot(
  model1_df, x = "StressLevel", y = "AvgTotalAvgSpeedValue",
  color = "TimeInterval", palette = "jco",
  facet.by = "Day", short.panel.labs = FALSE,
  title="The average of the total average speed",
  ylab = "Average of the total \naverage speed(mm/s)",
  xlab = "Stress Level",
  legend.title= "Time Interval",
  legend="none"
)
# p4: The average of the total distance
p4<-ggboxplot(
  model2_df, x = "StressLevel", y = "AvgTotalDistanceValue",
  color = "TimeInterval", palette = "jco",
  facet.by = "Day", short.panel.labs = FALSE,
  title="The average of the total distance",
  ylab = "Average of the total \ndistance(mm)",
  xlab = "Stress Level",
  legend.title = "Time Interval",
  legend="none"
)
# Create the same legend
mylegend1<-g_legend(p3)
# Combine 2 plots: p3 + p4
grid.arrange(
  arrangeGrob(p3,
              p4,
              nrow=1),
  mylegend1, nrow = 2, heights=c(10, 1)
)
