---
title: Lab 2 - Multiple Regression I
author: Drew Shives
date: '2022-02-05'
slug: lab-2-multiple-regression-i
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2022-02-05T22:44:07-05:00'
featured: no
type: book
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---


## Loading libraries.


```r
library(tidyverse)
library(ggrepel)
```

## Original Graph


```r
slamecka_design <- tibble(number_of_learning_trials = rep(c(2,4,8), each=6),
                          number_of_IL = rep(rep(c(2,4,8), 2), 3),
                          subjects = 1:18,
                          recall = c(35,21,6,
                                   39,31,8,
                                   40,34,18,
                                   52,42,26,
                                   61,58,46,
                                   73,66,52
                                   )
                          )

ggplot(slamecka_design,aes(x=number_of_IL,
                           group = number_of_learning_trials,
                           y=recall))+
  geom_line(stat = "summary", fun = "mean")+
  geom_point(stat = "summary", fun = "mean")+
  
  theme_classic()
```

<img src="/courses/PSYC7709G/Lab2_files/figure-html/unnamed-chunk-2-1.png" width="672" />


## Modified graph to match Figure 5.5 in the textbook.

Modifications include:

- Changing the x-axis title.
- Changing the y-axis title.
- Making the x-axis and y-axis ticks the same as in the textbook.
- Including different symbols to differentiate the lines.


```r
labels = c("2 learning trials", "4 learning trials", "8 learning trials")

slamecka_design <- tibble(number_of_learning_trials = rep(c(2,4,8), each=6),
                          number_of_IL = rep(rep(c(2,4,8), 2), 3),
                          subjects = 1:18,
                          recall = c(35,21,6,
                                   39,31,8,
                                   40,34,18,
                                   52,42,26,
                                   61,58,46,
                                   73,66,52
                                   )
                          )

slamecka_design <- slamecka_design %>%
  mutate(
    label = case_when(
      (number_of_IL == 8 & subjects == 6) ~ "2 learning trials",
      (number_of_IL == 8 & subjects == 12) ~ "4 learning trials",
      (number_of_IL == 8 & subjects == 18) ~ "8 learning trials"
    )
  )
           
ggplot(slamecka_design,aes(x=number_of_IL,
                           group = number_of_learning_trials,
                           y=recall)) +
  geom_line(stat = "summary", fun = "mean") +
  geom_text_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  geom_point(stat = "summary", fun = "mean", aes(shape = factor(number_of_learning_trials)), show.legend = FALSE) +
  xlab("Number of interpolated lists") +
  ylab("Number of words correct") +
  scale_x_continuous(breaks = c(2, 4, 8),
                   labels = c(2, 4, 8)) +
  ylim(0, 80) +
  expand_limits(x = 9) +
  theme_classic()
```

<img src="/courses/PSYC7709G/Lab2_files/figure-html/unnamed-chunk-3-1.png" width="672" />

## 3x3x3 Design and Graph


```r
labels = c("2 learning trials", "4 learning trials", "8 learning trials")

slamecka_design2 <- tibble(number_of_learning_trials = rep(c(2,4,8), each=18),
                          number_of_IL = rep(rep(c(2,4,8), 2), 9),
                          reward = rep(c(rep(0, 6), rep(50, 6), rep(1000, 6)), 3),
                          subjects = 1:54,
                          recall = sample(1:100, 54, replace = TRUE)
                          )

slamecka_design2 <- slamecka_design2 %>%
  mutate(
    label = case_when(
      (number_of_IL == 8 & subjects == 6) ~ "2 learning trials",
      (number_of_IL == 8 & subjects == 12) ~ "4 learning trials",
      (number_of_IL == 8 & subjects == 18) ~ "8 learning trials"
    )
  )
           
ggplot(slamecka_design2,aes(x=number_of_IL,
                           group = number_of_learning_trials,
                           y=recall)) +
  facet_wrap(~reward) +
  geom_line(stat = "summary", fun = "mean") +
  geom_point(stat = "summary", fun = "mean", aes(shape = factor(number_of_learning_trials))) +
  xlab("Number of interpolated lists") +
  ylab("Number of words correct") +
  theme(legend.title = element_text(size = 1), 
               legend.text = element_text(size = 1)) +
  labs(shape = "Number of learning trials") +
  scale_x_continuous(breaks = c(2, 4, 8),
                   labels = c(2, 4, 8),
                   sec.axis = sec_axis(~ . , name = "Reward", breaks = NULL, labels = NULL)) +
  ylim(0, 80) +
  expand_limits(x = 9) +
  theme_classic()
```

```
## Warning: Removed 11 rows containing non-finite values (stat_summary).
## Removed 11 rows containing non-finite values (stat_summary).
```

<img src="/courses/PSYC7709G/Lab2_files/figure-html/unnamed-chunk-4-1.png" width="672" />
