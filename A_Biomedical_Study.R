
# Thesis Project!
# Investigation of Mobile Phones on the Human Brain
# on the Aspect of Temperature Change

setwd("~/Documents")

library(tidyverse)
library(lattice)
library(latticeExtra)

experiment <- read_csv("experiment_table.csv")

glimpse(experiment)

# Checking for missing data
experiment[!complete.cases(experiment),]

# Using tidyr to tidy up the dataset

predictorsXtransform <- names(experiment)[2:8]

experiment %>%
  gather("Minute", "Temperature", predictorsXtransform)  ->
  transformed

transformed %>%
  separate(Model, c("ModelType", "ProcessNo")) ->
  transformed


# wrangle the data set and set-up data types

transformed$ModelType <- gsub("H", "Human", transformed$ModelType)
transformed$ModelType <- gsub("P", "Phantom", transformed$ModelType)
transformed$Minute <- gsub("min", "", transformed$Minute)
transformed$Minute <- gsub("init", "0", transformed$Minute)

transformed %>% 
  mutate_at(vars(ModelType, ProcessNo), as.factor) %>%
  mutate_at(vars(Minute), as.numeric) ->
  transformed

# Explore the dataset

glimpse(transformed)

set.seed(1406)
sample_n(transformed, size = 20)

transformed %>%
  group_by(ModelType, Minute) %>%
  summarise(meanTemp = mean(Temperature),
            sdTemp = sd(Temperature)) ->
  groupedData

# Visualisation --- (featuring lattice)

pxy <- xyplot(Temperature ~ Minute | ModelType,
              data = transformed,
              type = c("p","g"),
              col = "gray20",
              alpha = .7,
              scales = list(y = "free"),
              panel = function(x, y, ...){
                panel.xyplot(x, y, ...)
                panel.loess(x, y, col = "red",
                            lwd = 1.5)
              })

pxy <- update(pxy,
              xlab = "Time (Minute)",
              ylab = "Temperature (Celcius)",
              main = "inVivo - inVitro Temperature Comparison")


# Similar Graphs --- (this time featuring ggplot2)
p1 <- ggplot(transformed,
             aes(x = Minute, y = Temperature,
                 color = ModelType)) +
  geom_point(size = 1.2, alpha = .5) +
  geom_smooth(method = "loess",
              se = FALSE, color = "navy", size = 1) +
  
  facet_wrap(~ ModelType, scales = "free_y") +
  scale_color_brewer(palette = "Set2") 
 
  
p1 <- p1 + labs(
  title = "A Comparison: in Vivo - in Vitro Average Temperature",
  x = "Time (minute)",
  y = "Temperature (celcius)",
  subtitle = "Phone talks conducted @900 MHz Band",
  caption = "figure")


p1 <- p1 + theme_light() +
  theme(
  plot.title = element_text(family = "Arial", size = 14),
  plot.subtitle = element_text(family = "Arial", size = 10),
  plot.caption = element_text(family = "Arial", size = 7))


# A simplified Graph Using Mean Values of Temperature
p2 <- ggplot(groupedData,
             aes(x = Minute,
                 y = meanTemp,
                 color = ModelType)) +
  theme_light() +
  geom_point(alpha = 1/2) + 
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~ ModelType, scales = "free_y") +
  theme(legend.position = "top")


# get some interactivity for graphs
# produced with ggplot2

library(plotly)
plotly::ggplotly(p1)
plotly::ggplotly(p2)

# Get Back To Data only containing Human Model
# Derive the approximate maximum temperature change
# on human cortex

approximatedChange <- transformed %>%
  filter(ModelType == "Human") %>%
  group_by(ProcessNo) %>%
  summarise(maxDegree = max(Temperature),
            minDegree = min(Temperature)) %>%
  mutate(maxDelta = maxDegree - minDegree,
         expHighChange = maxDelta * .0857) %>%
  arrange(desc(maxDelta))
  

approximatedChange

# Which process (i.e Experimental Process Number)
# generated maximum change on epidermis (Delta T)
library(ggthemes)

approximatedChange %>%
  mutate(hi_lo = case_when(
    maxDelta == max(maxDelta) ~ "extreme",
    maxDelta == min(maxDelta) ~ "extreme",
    TRUE ~ "not"
  )) -> approximatedChange
  
  
  
psurface <- ggplot(approximatedChange,
                   aes(x = ProcessNo, y = maxDelta,
                       color = hi_lo)) +
  theme_economist() +
  geom_col(aes(fill = hi_lo), color = "black") +
  scale_x_discrete(breaks = 1:20) +
  scale_y_continuous(breaks = seq(0, 1.8, by = .4)) 

psurface <- psurface +
  theme(plot.title = element_text(family = "Arial", size = 12)) +
  labs(x = "# Experimental Process",
       y = "Maximum Change on Epidermis",
       title = "Which Process Generated Max|Min Temperature Change?",
       subtitle = "Process Numbers and Maximum Delta T. (Celcius)")


# Approximated Highest Deviations on
# Human Brain Cortex

pcortex <- xyplot(expHighChange ~ maxDelta, 
       data = approximatedChange,
       grid = TRUE,
       col= "blue",
       type = c("p","l"),
       auto.key = TRUE,
       lwd = 1.4,
       ylim = c(0, 0.25),
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.abline(h = .2, lty = 2, lwd = 1.5, col ="red")}
       )

update(pcortex,
       xlab = "Max Temperature Diff.",
       ylab = "Highest Change (Approximation)",
       main = "Expected Maximum Temperature Change")

save.image(file = "thesisProject.RData")


