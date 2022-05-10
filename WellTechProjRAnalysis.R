## Analyzing within R

```{r}
# install.packages("fpp3")
# install.packages("readr")
# install.packages("latex2exp")
# install.packages("seasonal")
# install.packages("feasts")
# install.packages("broom")
# install.packages("fable.prophet")
# install.packages("Mcomp")



library(fpp3)
library(readr)
library(latex2exp)
library(seasonal)
library(feasts)
library(broom)
library(fable)
library(ggpubr)
library(fable.prophet)
library(Mcomp)
library(readr)
library(latex2exp)
```

### Import my csv file

#### Character to date mm/dd/yyyy to yyyy-mm-dd

```{r}
DailyData <- read.csv("Data/DailyData/DailyData.csv")
DailyData <- DailyData %>% as_tibble
DailyData <- DailyData %>% mutate(ActivityDay = as.Date(ActivityDay, "%m/%d/%Y"),
                                  Id = as.character(Id) %>% as.factor())
DailyData
```

```{r}
DailyData %>% colnames()
```

```{r}
DailyData %>% summary()
```

```{r}
DailyDataLonger <- 
  cbind(
    DailyData %>% pivot_longer(cols=3:6, names_to = "ActivityMin", values_to = "Minutes") %>% select(Id, ActivityDay, ActivityMin, Minutes)
    ,
    DailyData %>% pivot_longer(cols = 7:10, names_to = "ActivityDist", values_to = "Distance" ) %>% select(ActivityDist, Distance)
  )

DailyDataLonger <- 
  DailyDataLonger %>% 
  mutate(ActivityMin = factor(ActivityMin, 
                              levels = c("SedentaryMinutes",  "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes" )),
         ActivityDist = factor(ActivityDist,
                               levels = c("SedentaryActiveDistance","LightActiveDistance","ModeratelyActiveDistance","VeryActiveDistance"))) 

DailyDataLonger
```

```{r}
DailyData %>% 
  mutate(TotalActiveMin = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes,
         TotalActiveDist = LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance) %>% 
  summarise(MeanActiveMin = mean(TotalActiveMin)) %>% 
  ggplot(aes(x="Total Avg Minutes", y = MeanActiveMin, fill = "Total Avg Minutes" )) + 
  geom_col(fill = "lightblue")+
  theme_minimal()+
  geom_text(aes(label = round(MeanActiveMin, 3)), vjust = 1.5) 

DailyData %>% 
  mutate(TotalActiveMin = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes,
         TotalActiveDist = LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance) %>% 
  summarise(MeanActiveDist = mean(TotalActiveDist)) %>% 
  ggplot(aes(x="Total Avg Distance", y = MeanActiveDist, fill = "Total Avg Distance" )) + 
  geom_col(fill = "lightgreen") +
  theme_minimal() + 
  geom_text(aes(label = round(MeanActiveDist,3)), vjust = 1.5) 

```

```{r}
DailyDataLonger %>% 
  # filter(ActivityMin != "SedentaryMinutes") %>%
  group_by(ActivityMin) %>%
  summarise(MeanMin = mean(Minutes)) %>%
  ggplot(aes(x = ActivityMin, y = MeanMin, fill = ActivityMin)) + 
  geom_col()+
  geom_text(aes(label = MeanMin %>% round(3)), vjust =.5)+
  # geom_text(aes(label = (MeanMin/60) %>% round(3) %>% paste("Hrs")), vjust =.5)+
  guides(fill = F)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 


DailyDataLonger %>%
  group_by(ActivityDist) %>%
  # filter(ActivityDist != "SedentaryActiveDistance") %>%
  summarise(MeanDist = mean(Distance)) %>%
  ggplot(aes(x = ActivityDist, y = MeanDist, fill = ActivityDist)) +
  geom_col()+
  geom_text(aes(label = MeanDist %>% round(3) ), vjust = .5)+
  guides(fill = F)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
# +  theme(axis.title.x=element_blank(), # If you dont x axis labels 
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
```

```{r}
DailyData %>% 
  mutate(TotalActiveMin = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes,
         TotalActiveDist = LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance) %>% 
  group_by(ActivityDay) %>% 
  summarise(MeanActiveMin = mean(TotalActiveMin)) %>% 
  ggplot(aes(x = ActivityDay, y = MeanActiveMin)) + 
  geom_line(col = "lightblue") +
  theme_minimal()


DailyData %>% 
  mutate(TotalActiveMin = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes,
         TotalActiveDist = LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance) %>% 
  group_by(ActivityDay) %>% 
  summarise(MeanActiveDist = mean(TotalActiveDist)) %>% 
  ggplot(aes(x = ActivityDay, y = MeanActiveDist)) + 
  geom_line(col = "lightgreen") +
  theme_minimal()
```

```{r}
DailyDataLonger %>% 
  # filter(ActivityMin != "SedentaryMinutes") %>% 
  group_by(ActivityDay, ActivityMin) %>%  
  summarise(meanMin = mean(Minutes)) %>%
  ggplot(aes(x = ActivityDay, y = meanMin, col = ActivityMin)) + 
  geom_line() + 
  theme_minimal()



DailyDataLonger %>% 
  # filter(ActivityDist != "SedentaryActiveDistance") %>% 
  group_by(ActivityDay, ActivityDist) %>%  
  summarise(meanDist = mean(Distance)) %>%
  ggplot(aes(x = ActivityDay, y = meanDist, col = ActivityDist)) + 
  geom_line() + 
  theme_minimal()
```

```{r}
DailyData %>% 
  mutate(TotalActiveMin = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes,
         TotalActiveDist = LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance,
         TotalHoursAsleep = (TotalMinutesAsleep/60) %>% round(2)) %>% 
  summarise(AvgHoursAsleep = mean(TotalHoursAsleep, na.rm = T))
```

-   Members on Average sleep 7 hours

```{r}
DailyData %>% 
  mutate(TotalActiveMin = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes,
         TotalActiveDist = LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance,
         TotalHoursAsleep = (TotalMinutesAsleep/60) %>% round(2)) %>% 
  GGally::ggpairs(columns = c(11,12,15,23,24,25))+
  theme_minimal()

DailyData %>% 
  mutate(TotalActiveMin = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes,
         TotalActiveDist = LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance,
         TotalHoursAsleep = (TotalMinutesAsleep/60) %>% round(2)) %>% 
  GGally::ggpairs(columns = c(3:10,25))+
  theme_minimal()
```

### Sleep and Their Predictors

It seems that Sleep does not have very strong Predictors

-   For obvious reasons, there is a strong correlation between TotalTimeInBed and TotalHoursAsleep

The rest of the Predictors that measure activity seem to have a slight negative correlation to TotalHoursAsleep

If we break down Distance and Minutes to there there degree of Intensities we see similar results with some anomalies

-   We see a strong negative correlation of Sedentary Minutes and Hours Slept

```{r}
# Relationship between Total calories or Step total vs Total Minutes asleep

# Trend line of TOTAL Members: Total calories or Step total vs     Total Minutes asleep
DailyData %>% 
  ggplot(aes(x = Calories, y = TotalMinutesAsleep)) +
  geom_point(show.legend = F) +
  geom_smooth(method = "lm",   show.legend = F, PI = F, se = F)

DailyData %>% 
  ggplot(aes(x = StepTotal, y = TotalMinutesAsleep)) + 
  geom_point(show.legend = F) +
  geom_smooth(method = "lm",   show.legend = F, PI = F, se = F)

# Trend line of INDIVIDUAL Members: Total calories or Step total vs     Total Minutes asleep
DailyData %>% 
  ggplot(aes(x = Calories, y = TotalMinutesAsleep, col = Id)) +
  # geom_point(show.legend = F) + 
  geom_smooth(method = "lm",   show.legend = F, PI = F, se = F)

DailyData %>% 
  ggplot(aes(x = StepTotal, y = TotalMinutesAsleep, col = Id)) + 
  # geom_point(show.legend = F) + 
  geom_smooth(method = "lm",   show.legend = F, PI = F, se = F)
```

```{r}
# The same pivot lonnger as before except all the data is Included
DailyDataLonger_v2 <- 
  cbind(
    DailyData %>% pivot_longer(cols=3:6, names_to = "ActivityMin", values_to = "Minutes")
    ,
    DailyData %>% pivot_longer(cols = 7:10, names_to = "ActivityDist", values_to = "Distance" ) %>% select(ActivityDist, Distance)
  )
```

```{r}

# Trend line of Distance travled given a the type of Intensity vs       Total Minutes asleep
DailyDataLonger_v2 %>% 
  mutate(TotalHoursAsleep = TotalMinutesAsleep/60) %>% 
  ggplot(aes(x = Distance, y = TotalHoursAsleep)) +
  geom_point(show.legend = F) + 
  geom_smooth(method = "lm",   
              # show.legend = F, 
              PI = F, se = F)
# Trend line of the Minutes Spent given a the type of Intensity vs       Total Minutes asleep
DailyDataLonger_v2 %>% 
  mutate(TotalHoursAsleep = TotalMinutesAsleep/60) %>% 
  ggplot(aes(x = Minutes, y = TotalHoursAsleep)) +
  geom_point(show.legend = F) +
  geom_smooth(method = "lm",   
              # show.legend = F, 
              PI = F, se = F)



DailyData %>% 
  ggplot(aes(x = StepTotal, y = TotalMinutesAsleep)) + 
  geom_point(show.legend = F) +
  geom_smooth(method = "lm",   show.legend = F, PI = F, se = F)

# Trend line of INDIVIDUAL Members: Total calories or Step total vs     Total Minutes asleep
DailyData %>% 
  ggplot(aes(x = Calories, y = TotalMinutesAsleep, col = Id)) +
  # geom_point(show.legend = F) + 
  geom_smooth(method = "lm",   
              # show.legend = F, 
              PI = F, se = F)

DailyData %>% 
  ggplot(aes(x = StepTotal, y = TotalMinutesAsleep, col = Id)) + 
  # geom_point(show.legend = F) + 
  geom_smooth(method = "lm",   show.legend = F, PI = F, se = F)
```
