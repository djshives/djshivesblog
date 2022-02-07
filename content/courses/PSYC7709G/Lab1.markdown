---
title: Lab 1 - Shaping Data
author: Package Build
date: '2022-02-06'
slug: lab-1-shaping-data
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2022-02-06T23:27:47-05:00'
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
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
## ✓ readr   2.0.2     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(readxl)
library(tidyxl)
library(unpivotr)
```

```
## 
## Attaching package: 'unpivotr'
```

```
## The following objects are masked from 'package:tidyr':
## 
##     pack, unpack
```

```r
library(reshape2)
```

```
## 
## Attaching package: 'reshape2'
```

```
## The following object is masked from 'package:tidyr':
## 
##     smiths
```

## Loading data.


```r
data = read_excel("/Users/drewshives/Documents/GitHub/djshivesblog/content/courses/PSYC7709G/data/Lab1_data.xlsx", col_names = FALSE)
```

```
## New names:
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
## * ...
```

## Manipulating data into long format using unpivotr and tidyxl.


```r
data2 <- data %>% 
  as_cells() %>%
  behead("up-left", noise_level) %>%
  behead("up-left", time_of_day) %>%
  behead("up-left", a_b) %>%
  filter(a_b != "Participant") %>%
  rename(score = chr) %>%
  select(score, noise_level, time_of_day, a_b)

data2$participant <- rep(1:10, 12)

data2 <- data2 %>%
  relocate(participant, .before = score) %>%
  relocate(score, .after = a_b) %>%
  arrange(participant, decreasting = TRUE)

data2
```

```
## # A tibble: 120 × 5
##    participant noise_level time_of_day a_b   score
##          <int> <chr>       <chr>       <chr> <chr>
##  1           1 Noisy       Morning     A     61   
##  2           1 Noisy       Morning     B     77   
##  3           1 Noisy       Afternoon   A     97   
##  4           1 Noisy       Afternoon   B     97   
##  5           1 Noisy       Evening     A     89   
##  6           1 Noisy       Evening     B     94   
##  7           1 Quiet       Morning     A     87   
##  8           1 Quiet       Morning     B     87   
##  9           1 Quiet       Afternoon   A     65   
## 10           1 Quiet       Afternoon   B     53   
## # … with 110 more rows
```

## Manipulating data into long format using the tidyverse.


```r
data_headerless <- data[-c(1, 2), ] %>%
  select(!1)

colnames(data_headerless) <- data_headerless[1, ]

data_headerless <- data_headerless[-1, ]

data_headerless$participant <- 1:10

data_headerless <- pivot_longer(data_headerless, cols = c(A, B))

data_headerless$time_of_day <- rep(c(rep("Morning", 2), rep("Afternoon", 2), rep("Evening", 2)), 20)

data_headerless$noise_level <- rep(c(rep("Noisy", 6), rep("Quiet", 6)), 10)

data_headerless
```

```
## # A tibble: 120 × 5
##    participant name  value time_of_day noise_level
##          <int> <chr> <chr> <chr>       <chr>      
##  1           1 A     61    Morning     Noisy      
##  2           1 B     77    Morning     Noisy      
##  3           1 A     97    Afternoon   Noisy      
##  4           1 B     97    Afternoon   Noisy      
##  5           1 A     89    Evening     Noisy      
##  6           1 B     94    Evening     Noisy      
##  7           1 A     87    Morning     Quiet      
##  8           1 B     87    Morning     Quiet      
##  9           1 A     65    Afternoon   Quiet      
## 10           1 B     53    Afternoon   Quiet      
## # … with 110 more rows
```
