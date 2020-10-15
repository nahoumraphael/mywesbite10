---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Nullam et orci eu lorem consequat tincidunt vivamus et sagittis magna sed nunc rhoncus condimentum sem. In efficitur ligula tate urna. Maecenas massa sed magna lacinia magna pellentesque lorem ipsum dolor. Nullam et orci eu lorem consequat tincidunt. Vivamus et sagittis tempus.
draft: false
image: pic08.jpg
keywords: ""
slug: tempus
title: Tempus
---
---
title: 'Session 4: Homework 2'
author: "Your name goes here"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
  pdf_document:
    toc: yes
---


```{r, setup, include=FALSE}
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


```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
```


# General Social Survey (GSS)

The [General Social Survey (GSS)](http://www.gss.norc.org/) gathers data on American society in order to monitor and explain trends in attitudes, behaviours, and attributes. Many trends have been tracked for decades, so one can see the evolution of attitudes, etc in American Society.

```{r, read_gss_data, cache=TRUE}
gss <- read_csv(here::here("smallgss2016.csv"), 
                na = c("", "Don't know",
                       "No answer", "Not applicable"))

```

## Instagram

```{r, insta}
#We are calculating the proportion of instagram users

gss_instagram <- gss %>%
 filter(instagrm!="NA") %>% #we need to drop the NA values
  group_by(instagrm) %>% 
  summarise(count = n()) %>% 
  mutate(proportion_instagram = count/sum(count)) 
gss_instagram
```
```{r, instagram_visualisation}
gss_plot_instagram <- gss_instagram %>%
 ggplot(aes(
    x = instagrm,
    y = proportion_instagram)) +
  geom_col(fill = "#DD2A7B")+  #we use the color of instagram
 
#Adding title, subtitle and axis 
labs(title = "Usage of Instagram",
       subtitles = "31% of the survey participants use Instagram",
       x="",
       y ="Proportion") +

#Defining the size and color of the titles
    theme(text=, plot.title  = element_text (size=20, colour="#7D7D7D"),
       axis.title.y= element_text(size=12, colour="#002144"))
  gss_plot_instagram
```

## Snapchat

```{r, snapchat}
#We are calculating the proportion of snapcchat users
gss_snapchat <- gss %>%
 filter(snapchat!="NA") %>% #we need to drop the NA values
  group_by(snapchat) %>% 
  summarise(count = n()) %>% 
  mutate(proportion_snapchat = count/sum(count)) 
gss_snapchat

```
```{r, snapchat_visualisation}
gss_plot_snapchat <- gss_snapchat %>%
 ggplot(aes(
    x = snapchat,
    y = proportion_snapchat)) +
  geom_col(fill = "#FEFC00")+ #we use the color of snapchat

#Adding title, subtitle and axis  
labs(title = "Usage of Snapchat",
       subtitles = "22% of the survey participants use Snapchat",
       x="",
       y ="Proportion") +

#Defining the size and color of the titles
  theme(text=, plot.title  = element_text (size=20, colour="#7D7D7D"),
       axis.title.y= element_text(size=12, colour="#002144"))
  gss_plot_snapchat
```
## Twitter

```{r,twitter}
#We are calculating the proportion of twitter users
gss_twitter <- gss %>%
 filter(twitter!="NA") %>% #we need to drop the NA values
  group_by(twitter) %>% 
  summarise(count = n()) %>% 
  mutate(proportion_twitter = count/sum(count)) 
gss_twitter
```

```{r, twitter_visualisation}
gss_plot_twitter <- gss_twitter %>%
 ggplot(aes(
    x = twitter,
    y = proportion_twitter)) +
  geom_col(fill = "#36D8FF")+ #we use the color of Twitter
  
#Adding title, subtitle and axis
labs(title = "Usage of Twitter",
       subtitle = "19% of the survey participants use Twitter",
       x="",
       y ="Proportion")+
  
#Defining the size and color of the titles
  theme(text=, 
        plot.title  = element_text (size=20, colour="#7D7D7D"),
        axis.title.y= element_text(size=12, colour="#002144"))
gss_plot_twitter

```

Now that we have a breakdown of Instagram, Snapchat and Twitter users, it would be interesting to look at what social media platform is used the most on average.
```{r, most_users}
gss_instagram %>%
  Instagram <-proportion_instagram ==  "Yes"



```

## Twitter and Instagram by gender

```{r, gender}

gss_twitter_insta <- gss %>%
  # We create a new variable for twitter_insta
  mutate(twitter_insta = case_when(
    twitter == "Yes" & instagrm == "Yes" ~ "Yes", #Users need to use both Twitter and Insta to be recorded as a Yes
    twitter == "No" & instagrm == "No" ~ "No")) 
  
# calculate the proportion of users
gss_proportion <- gss_twitter_insta %>%
  filter(!is.na(twitter_insta)) %>% 
  group_by(twitter_insta) %>% 
  summarise(count = n()) %>% 
  mutate(proportion = count/sum(count)) 
gss_proportion

```

```{r,confidence_interval}
twitter_insta_ci <- gss_twitter_insta %>%
  group_by(sex) %>%
  # using the CI formula
  summarize(mean_twitter_insta = prop(twitter_insta == "Yes"), #We just want to include those who previously answered Yes
            count = n(),
            standard_error = sqrt((mean_twitter_insta * (1 - mean_twitter_insta)) / count),
            t_critical = qt(0.975, count-1),
            margin_of_error = t_critical*standard_error,
            lower_bound = mean_twitter_insta - margin_of_error,
            upper_bound = mean_twitter_insta + margin_of_error)
twitter_insta_ci
```

```{r, twitter_insta_visualisation}
gss_plot_twitter_insta <- twitter_insta_ci %>%
   ggplot(aes(
    x = sex,
    y = mean_twitter_insta))+

#Adding  errorbar to  visualise the confidence intervals for both gender
  geom_errorbar(aes(ymin=lower_bound, 
                    ymax=upper_bound, color=sex), 
                    width=0.1,
                    size=1)+
#Size  of  the  point
 geom_point(size = 5, aes(color = sex))+
  
#Adding title and axis
  labs(title = "Men are more likely to use Twitter and Instagram",
       x="",
       y ="Proportion using either Twitter or Instagram")+

#Defining the size and color of the titles
    theme(text=, plot.title  = element_text (size=20, colour="#7D7D7D"),
       axis.title.y= element_text(size=12, colour="#002144"))
  gss_plot_twitter_insta
```


