#-------------------------------------------------------------------------------
# PACKAGES & READ-IN FILE
#-------------------------------------------------------------------------------
library(tidyverse)
library(skimr)
library(ggpubr)              # customization ggplot2 & gg-arrange
library(RColorBrewer)
setwd("C:/Users/minhk/OneDrive/Data Science stuffs/PORTFOLIO/Movie Analysis")
options(digits = 3)
movies <- read.csv("movies.csv")

#-------------------------------------------------------------------------------
# DATA CLEANING 
#-------------------------------------------------------------------------------
sum(is.na(movies)) # look for NA values => no data cleaning is needed
summary(movies)
skim(movies)
#-------------------------------------------------------------------------------
# AT FIRST GLANCE
#-------------------------------------------------------------------------------
movies <- movies %>%
  mutate(revenue.mils = revenue/1000000,
         budget.mils = budget/1000000,
         profit.mils = (revenue - budget)/1000000)
n <- names(movies)


#-------------------------------------------------------------------------------
# TOP 10 DIRECTORS 
#-------------------------------------------------------------------------------
movies %>%
  select(director,
         revenue.mils) %>%
  group_by(director) %>%
  summarize(sum = sum(revenue.mils)) %>%
  arrange(desc(sum)) %>%
  head(10) 

#-------------------------------------------------------------------------------
# TOP 10 First Cast Member
#-------------------------------------------------------------------------------
movies %>%
  select(cast1,
         revenue.mils) %>%
  group_by(cast1) %>%
  summarize(sum = sum(revenue.mils)) %>%
  arrange(desc(sum)) %>%
  head(10) 

#-------------------------------------------------------------------------------
# genres that make the most profit in the course of 2012 - 2016
#-------------------------------------------------------------------------------
movies %>%
  select(year,
         genre,
         profit.mils) %>%
  group_by(year,genre) %>%
  summarize(sum = sum(profit.mils)) %>%
  arrange(desc(sum)) %>%
  head(5)
# just by profit (in millions) alone, action movies rake in the most profit. 

#-------------------------------------------------------------------------------
# Does Action movies rake in the most money on average?
#-------------------------------------------------------------------------------
movies %>%
  select(year,
         genre,
         profit.mils) %>%
  group_by(year,genre) %>%
  summarize(average = mean(profit.mils)) %>%
  arrange(desc(average)) %>%
  head(5)
# Interpretation: when analyzed by each year, action movies only rank 9 in terms
#                 of average profit. Adventure take its place in the top 5

#-------------------------------------------------------------------------------
# Maybe it just because there just more movies coming out
#-------------------------------------------------------------------------------
movies %>%
  select(year,
         genre,
         title) %>%
  group_by(year,genre) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(5)
# Interpretation: In general, action movies were released more than the other.
#                 that explains why they have more average. 

#-------------------------------------------------------------------------------
# How about budget and revenues?
#-------------------------------------------------------------------------------
movies %>%
  select(year,
         genre,
         budget.mils) %>%
  group_by(year,genre) %>%
  summarize(sum = sum(budget.mils)) %>%
  arrange(desc(sum)) %>%
  head(5)
# Action movies in general attract the most budgets 

movies %>%
  select(year,
         genre,
         revenue.mils) %>%
  group_by(year,genre) %>%
  summarize(sum = sum(revenue.mils)) %>%
  arrange(desc(sum)) %>%
  head(5)
# In terms of revenue, actions movies rake in the most as well

#-------------------------------------------------------------------------------
# Looking at a whole from 2012 - 2016
#-------------------------------------------------------------------------------
rev_all <- movies %>%
  select(genre,
         revenue.mils) %>%
  group_by(genre) %>%
  summarize(sum_rev = sum(revenue.mils)) %>%
  arrange(desc(sum_rev)) %>%
  head(5) %>%
  data.frame()         

bud_all <- movies %>%
  select(genre,
         budget.mils) %>%
  group_by(genre) %>%
  summarize(sum_bud = sum(budget.mils)) %>%
  arrange(desc(sum_bud)) %>%
  head(5) %>%
  data.frame()   # Action movies require the most budget 

count_all <- movies %>%
  select(genre) %>%
  group_by(genre) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  data.frame()              # Action movies were released the most 

profit_all <- movies %>%
  select(genre,
         profit.mils) %>%
  group_by(genre) %>% 
  summarize(sum_pro = sum(profit.mils)) %>%
  arrange(desc(sum_pro)) %>%
  head(5) %>%
  data.frame()             # Action movies generate most profits 

average_all <- movies %>%
  select(genre,
         profit.mils) %>%
  group_by(genre) %>%
  summarize(mean_profit = mean(profit.mils)) %>%
  arrange(desc(mean_profit)) %>%
  head(5) %>%
  data.frame()            
# Action movies not in top 5 in terms of average profit
# the most profitable genre of all time would be Adventure and family movies. 

#-------------------------------------------------------------------------------
# PLOTS
#-------------------------------------------------------------------------------
# revenue plot
rev_plot <- ggplot(data = rev_all,
               aes(x = genre,
                   y= sum_rev,
                   fill = genre)) +               
  geom_bar(mapping = aes(x=genre,     
                         y=sum_rev),
           stat = "identity",
           width = 0.5,
           position = "dodge",) +
  ylab("Revenues (in milions)") +
  xlab("Movie Genres") + 
  ggtitle("Revenue") +
  theme_minimal() +
  coord_flip()

# budget plot
bud_plot <- ggplot(data = bud_all,
       aes(x = genre,
           y= sum_bud,
           fill = genre))+               
  geom_bar(mapping = aes(x=genre,     
                         y= sum_bud),
           stat = "identity",
           width = 0.5,
           position = "dodge",) +
  ylab("Budget (in milions)") +
  xlab("Movie Genres") + 
  ggtitle("Budget") + 
  theme_minimal() +
  coord_flip()

# Count plot 
count_plot <- ggplot(data = count_all,
                   aes(x = genre,
                       y= count,
                       fill = genre))+               
  geom_bar(mapping = aes(x=genre,     
                         y= count),
           stat = "identity",
           width = 0.5,
           position = "dodge",) +
  ylab("Number of Movies Released") +
  xlab("Movie Genres") + 
  ggtitle("Movies Released") + 
  theme_minimal() +
  coord_flip()

# Profit plot
profit_plot <- ggplot(data = profit_all,
                   aes(x = genre,
                       y= sum_pro,
                       fill = genre))+               
  geom_bar(mapping = aes(x=genre,     
                         y= sum_pro),
           stat = "identity",
           width = 0.5,
           position = "dodge",) +
  ylab("Profit (in milions)") +
  xlab("Movie Genres") + 
  ggtitle("Profit") + 
  theme_minimal() +
  coord_flip()

# Average plot 
average_plot <- ggplot(data = average_all,
                   aes(x = genre,
                       y= mean_profit,
                       fill = genre))+               
  geom_bar(mapping = aes(x=genre,     
                         y= mean_profit),
           stat = "identity",
           width = 0.5,
           position = "dodge",) +
  ylab("Mean Profits (in milions)") +
  xlab("Movie Genres") + 
  ggtitle("Mean Profits") + 
  theme_minimal() +
  coord_flip()

genre_plot <- ggarrange(rev_plot,
          bud_plot,
          count_plot,
          profit_plot,
          average_plot,
          ncol = 2,nrow = 3,
          align = "hv",
          font.label = list(size = 10,
                            color = "black",
                            face = "bold",
                            family = NULL,
                            position = "top"))
annotate_figure(genre_plot, top = text_grob("
Top Five Movie Genre by Categories,from 2012 - 2016", 
                                             color = "black", face = "bold", size = 14))

#-------------------------------------------------------------------------------
# Confirm findings
#-------------------------------------------------------------------------------
movies %>%
  select(title,
         genre,
         year,
         revenue.mils) %>%
  group_by(genre) %>%
  arrange(desc(revenue.mils)) %>%
  head(10)
# from 2012 - 2016, because there are multiple action movies that go to the top
# 10, action movies generate the most profits, despite its needs for more budget 
# than other movies. 

#-------------------------------------------------------------------------------
# About how much revenues we can predict based on movies budget on average
#-------------------------------------------------------------------------------
# Linear Regression of the entire data table (N = 508)
summary(lm(data = movies, revenue.mils~budget.mils))
# b = 2.828, R^2 (1,506) = .576, p < .05

# Count and split movies into categories
movies <- movies %>%
  mutate(bud_group = ifelse(budget.mils >100,">100",ifelse(
    budget.mils <= 100 & budget.mils > 50,"<100",ifelse(
      budget.mils <=50 & budget.mils >25, "<50","<25"))))

movies %>% 
  select(bud_group,
         budget.mils) %>%
  group_by(bud_group) %>%
  count()

# Linear Regression with budgets > $100m (N = 74)
budget100more <- movies %>%
  filter(budget.mils >100) %>%
  select(budget.mils,
         genre,
         revenue.mils) %>%
  data.frame()
summary(lm(data = budget100more, revenue.mils~budget.mils))
# b = 2.915, R^2 (1,72) = .262, p< .05.
# slope = 2.915

# Linear Regression with budgets between $50 - $100m (N = 97)
budget100less <- movies %>%
  filter(budget.mils <=100 & budget.mils >50) %>%
  select(budget.mils,
         genre,
         revenue.mils) %>%
  arrange(desc(budget.mils)) %>% 
  data.frame()
summary(lm(data = budget100less, revenue.mils~budget.mils))
# b =2.08, R^2 (1,95) = .0253, p> .05

# Linear Regression with budgets between $25 - $50m (N = 107)
budget50less <- movies %>%
  filter(budget.mils <= 50 & budget.mils >25) %>%
  select(budget.mils,
         genre,
         revenue.mils) %>%
  arrange(desc(budget.mils)) %>% 
  data.frame()
summary(lm(data = budget50less, revenue.mils~budget.mils))
# b = 3.17, R^2 (1,105) = 3.17, p< .05

# Linear Regression with budgets < $25 (N = 230)
budget25less <- movies %>%
  filter(budget.mils <= 25) %>%
  select(budget.mils,
         genre,
         revenue.mils) %>%
  arrange(desc(budget.mils)) %>% 
  data.frame()
summary(lm(data = budget25less, revenue.mils~budget.mils))
# b = 2.107, R^2 = .058, p <.05


# graphing
bud100more <- ggplot(data = budget100more,
                     aes(x = budget.mils,
                         y = revenue.mils)) + 
  geom_point(size = 2) +
  theme_minimal() +
  geom_smooth(method = lm)+
  theme_minimal() +
  ylab("Revenues (in Millions") +
  xlab("Budgets (in Millions)") +
  ggtitle("Budget > $100m,
          y = 1.1 +2.9x")


bud100less <- ggplot(data = budget100less,
                     aes(x = budget.mils,
                         y = revenue.mils)) + 
  geom_point(size = 2) +
  theme_minimal() +
  geom_smooth(method = lm)+
  ylab("Revenues (in Millions") +
  xlab("Budgets (in Millions)") +
  ggtitle("Budget $50 - $100m (insignificant)")

bud50less <- ggplot(data = budget50less,
                    aes(x = budget.mils,
                        y = revenue.mils)) + 
  geom_point(size = 2) +
  theme_minimal() +
  geom_smooth(method = lm)+
  ylab("Revenues (in Millions") +
  xlab("Budgets (in Millions)") +
  ggtitle("Budget $25m - $50m,
          y = -18 +3.2x")

bud25less <- ggplot(data = budget25less,
                    aes(x = budget.mils,
                        y = revenue.mils)) + 
  geom_point(size = 2) +
  theme_minimal() +
  geom_smooth(method = lm)+
  ylab("Revenues (in Millions") +
  xlab("Budgets (in Millions)") +
  ggtitle("Budget < $25m
          y = 27 +2.1x")

budall <- ggplot(data = movies,
                 aes(x = budget.mils,
                     y = revenue.mils)) + 
  geom_point(size = 2) +
  theme_minimal() +
  geom_smooth(method = lm)+
  ylab("Revenues (in Millions") +
  xlab("Budgets (in Millions)") +
  ggtitle("Any Budgets
          y = 14 +2.8x")

budget_plot <- ggarrange(bud100more,
                         bud100less,
                         bud50less,
                         bud25less,
                         budall,
                         ncol = 2,nrow = 3,
                         widths = c(2,2.25),
                         align = "hv",
                         font.label = list(size = 10,
                                           color = "black",
                                           face = "bold",
                                           family = NULL,
                                           position = "top"))

annotate_figure(budget_plot, top = text_grob("Budget Predicts Revenues (in millions of Dollars),
                                             for movies released from 2012 - 2016", 
                                      color = "black", face = "bold", size = 14))

