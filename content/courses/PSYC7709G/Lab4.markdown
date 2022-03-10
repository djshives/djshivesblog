---
title: Lab 4 - ANOVA
author: Drew Shives
date: '2022-02-03'
slug: lab-4-anova
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2022-03-10T12:49:22-05:00'
featured: no
type: book
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---





```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.6     ✓ dplyr   1.0.8
## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
## ✓ readr   2.1.2     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(papaja)
```

## Question (1) 

### Use R to conduct a t.test and ANOVA on the below example data. Then use R to prove that the results of both analyses are the same. For example, prove that the p-values are the same, and prove that the F-value and T-value are related.

Loading in the example data.


```r
example_data <- tibble(Group = rep(c("A","B"), each = 5),
                       DV = c(2,4,3,5,4,7,6,5,6,7))
```

Running an ANOVA on the example data, saving it to an object, and displaying the summary.


```r
example_anova <- aov(DV ~ Group, example_data)
summary(example_anova)
```

```
##             Df Sum Sq Mean Sq F value  Pr(>F)   
## Group        1   16.9    16.9    16.9 0.00339 **
## Residuals    8    8.0     1.0                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Running a two sample t-test on the sample data, saving it to an object, and displaying the summary.


```r
example_t_test <- t.test(DV ~ Group, example_data, var.equal = TRUE)
example_t_test
```

```
## 
## 	Two Sample t-test
## 
## data:  DV by Group
## t = -4.111, df = 8, p-value = 0.003386
## alternative hypothesis: true difference in means between group A and group B is not equal to 0
## 95 percent confidence interval:
##  -4.058445 -1.141555
## sample estimates:
## mean in group A mean in group B 
##             3.6             6.2
```

### Showing that the results of both analyses are the same.

The square of the t-statistic and the F-statistic are the same.


```r
round((example_t_test$statistic ^ 2), 5) == round(summary(example_anova)[[1]][1, 4], 5)
```

```
##    t 
## TRUE
```

The p-values for both are the same.


```r
round(example_t_test$p.value, 5) == round(summary(example_anova)[[1]][1, 5], 5)
```

```
## [1] TRUE
```

## Question (2)

### Look at the lab on ANOVA that I wrote for our undergraduate statistics OER lab manual https://crumplab.github.io/statisticsLab/lab-8-one-way-anova.html. That lab shows an example of obtaining data from a published paper in psych science where a one-factor ANOVA was used as a part of the analysis. Load the data, conduct the ANOVA, report a ggplot of the means, and use papaja to help you write a short results section reporting the ANOVA result.

Loading in the experimental data.


```r
data <- read_csv("https://raw.githubusercontent.com/CrumpLab/statisticsLab/master/data/Jamesetal2015Experiment2.csv")
```

```
## Rows: 72 Columns: 28
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (28): Condition, Time_of_Day, BDI_II, STAI_T, pre_film_VAS_Sad, pre_film...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

Creating the factors and levels for "Condition".


```r
data$Condition <- as.factor(data$Condition)

levels(data$Condition) <- c("Control", "Reactivation+Tetris", "Tetris_only", "Reactivation_only")
```

### Graphing the means, standard deviations, and individual scores.


```r
descriptive_df <- data %>% 
                    group_by(Condition) %>% 
                    summarise(means= mean(Days_One_to_Seven_Number_of_Intrusions),
                              SEs = sd(Days_One_to_Seven_Number_of_Intrusions)/sqrt(length(Days_One_to_Seven_Number_of_Intrusions)))

ggplot(descriptive_df, aes(x=Condition, y=means)) + 
  geom_bar(stat="identity", aes(fill=Condition)) + 
  geom_errorbar(aes(ymin=means-SEs,               
                    ymax=means+SEs), width=.1) +
  geom_point(data=data, aes(x=Condition, y=Days_One_to_Seven_Number_of_Intrusions), alpha=.5)+
  geom_point(alpha=.25)+
  ylab("Intrusive Memories (Mean for Week)")
```

<img src="/courses/PSYC7709G/Lab4_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Running an ANOVA on the experimental data, saving it to an object, and displaying the summary.


```r
anova <- aov(Days_One_to_Seven_Number_of_Intrusions ~ Condition, data)

summary(anova)
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)  
## Condition    3  114.8   38.27   3.795 0.0141 *
## Residuals   68  685.8   10.09                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Reporting the result using papaja.


```r
apa_output <- apa_print(anova)
```

The main effect of condition was significant, `\(F(3, 68) = 3.79\)`, `\(\mathit{MSE} = 10.09\)`, `\(p = .014\)`, `\(\hat{\eta}^2_G = .143\)`. 
