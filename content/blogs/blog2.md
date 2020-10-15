---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Lorem Etiam Nullam
draft: false
image: pic09.jpg
keywords: ""
slug: magna
title: Magna
---


```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
library(rvest)    # scrape websites
library(purrr)  
library(lubridate) #to handle dates
```



# Where Do People Drink The Most Beer, Wine And Spirits?

```{r, load_alcohol_data}
library(fivethirtyeight)
data(drinks)


# or download directly
# alcohol_direct <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv")

```


What are the variable types? Any missing values we should worry about? 

```{r glimpse_skim_data}
# YOUR CODE GOES HERE
glimpse(drinks)
skim(drinks)

# There are no missing variables
```


Make a plot that shows the top 25 beer consuming countries

```{r beer_plot}
beer_plot <- drinks %>%
  select(beer_servings,country) %>%
  arrange(desc(beer_servings)) %>%
  head(25)
glimpse(beer_plot)

p <- ggplot(beer_plot, aes (x=beer_servings, y=reorder(country, beer_servings), fill=beer_servings)) + 
  geom_col() + 
  theme_bw()+
  
  scale_fill_gradient(low="#FFF897", # This function manually adds a gradient to the fill variable.
                      high="#EC9D00") +  # The codes are the hex colour codes for beer
  
  labs(title = "Namibia tops the beer chart", # add labels to the df
       subtitle = "Top 25 countries world's biggest beer drinkers",
       caption = "Source: fivethirtyeight.com - 2014", # Source
       fill = "Beer Servings" # Change legend name
       ) +
  ylab(NULL) +
  xlab("Average servings of beer per person")  # y-axis
p
```


Make a plot that shows the top 25 wine consuming countries

```{r wine_plot}

# YOUR CODE GOES HERE
wine_plot <- drinks %>%
  select(wine_servings,country) %>%
  arrange(desc(wine_servings)) %>%
  head(25)
glimpse(wine_plot)

plot2 <- ggplot(wine_plot, aes (x=wine_servings, y=reorder(country, wine_servings), fill=wine_servings)) + 
  geom_col()+
  theme_bw()+
  scale_fill_gradient(low="#790D03", # This function manually adds a gradient to the fill variable.
                      high="#B21226") +  # The codes are the hex colour codes for wine
  
    labs(title = "France tops the wine chart",
       subtitle = "Top 25 countries world's biggest wine drinkers",
       caption = "Source: fivethirtyeight.com - 2014", # Source
       fill = "Wine Servings")+
  ylab(NULL) +
  xlab("Average servings of wine per person")  # y-axis+

plot2

```

Finally, make a plot that shows the top 25 spirit consuming countries
```{r spirit_plot}
# YOUR CODE GOES HERE
spirit_plot <- drinks %>%
  select(spirit_servings,country) %>%
  arrange(desc(spirit_servings)) %>%
  head(25)
glimpse(spirit_plot)

plot3 <- ggplot(spirit_plot, aes (x=spirit_servings, y=reorder(country, spirit_servings), fill=spirit_servings)) + 
  geom_col() +
  theme_bw()+
  scale_fill_gradient(low="#ECEAE2",
                      high="#D3D3D3") + # hex colour codes for spirit
  
  labs(title = "Grenada tops the spirit chart!", 
       subtitle = "Top 25 countries world's biggest spirit drinkers",
       caption = "Source: fivethirtyeight.com - 2014",
       fill = "Spirit Servings"
       ) +
  ylab(NULL) +
  xlab("Average servings of spirit per person")

plot3
```




Now that we have a breakdown of biggest beer, wine and spirit drinking countries, it would be interesting to look at what drink is consumed the most on average across the globe.

```{r spirit_plot}
drinks %>% 
  summarise(Beer = mean(beer_servings), 
            Wine = mean(wine_servings),
            Spirits = mean(spirit_servings)) %>% 
  
  #get table into tidy format
  pivot_longer(c(Beer, Wine, Spirits), names_to ="categories") %>% 
  
  #bar plot
  ggplot(aes(x = categories, y = value, fill = categories)) +
  geom_col() + 
  
  #costum color
  scale_fill_manual(values = c("#EC9D00","#D3D3D3","#B21226"), labels=c("Beer", "Spirits", "Wine")) +
  
  #titles
  labs(title = "Beer is the most widely consumed alcoholic drink in the world",
       subtitles = "Average servings per person per year",
       caption = "Source: fivethirtyeight.com - 2014",
       y = "Number of servings",
       x = NULL) + 
  
  #remove legend
  theme(legend.position = "none")
````
From this graph, it can be inferred  that beer is the most widely consumed alcoholic drink in the world, followed by spirits and wine. Given that beer has a lower alcohol content in comparison to both spirits and wine,  it can be consumed on a more regular basis. Additionally, beer is the cheapest alternative by far - wine especially tend to be a lot more expensive.

It would be interesting to plot a graph showing how beer perform in comparison to water, coffe and tea.
