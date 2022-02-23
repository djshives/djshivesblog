---
title: Lab 3 - Multiple Regression II
author: Drew Shives
date: '2022-02-04'
slug: lab-3-multiple-regression-ii
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2022-02-04T19:08:14-05:00'
featured: no
type: book
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---



## Introduction

The mechanics of multivariate regression in R are relatively straightforward. By using the lm() function, building complex linear models is a quick and efficient affair.

### Data and Exploratory Data Analysis

Let's begin by loading in the necessary packages...


```r
library(MASS)
library(tidyverse)
library(ggrepel)
library(extraDistr)
library(loo)
library(bridgesampling)
library(brms)
library(rstan)
library(bayesplot)
library(tictoc)
library(hypr)
library(tidybayes)
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())
select <- dplyr::select
```

...And create a toy dataset. For this lab, we will be using the research design by Hulme et al. (1984) that investigates whether age and speech rate are involed with memory ability.


```r
data <- tibble(age = c(4,4,7,7,10,10),
               speech_rate = c(1,2,2,4,3,6),
               memory = c(14,23,30,50,39,67),
               id = c(1, 2, 3, 4, 5, 6))

data
```

```
## # A tibble: 6 × 4
##     age speech_rate memory    id
##   <dbl>       <dbl>  <dbl> <dbl>
## 1     4           1     14     1
## 2     4           2     23     2
## 3     7           2     30     3
## 4     7           4     50     4
## 5    10           3     39     5
## 6    10           6     67     6
```

As we can see from the above as speech rate increases, so does memory ability. The same, however, can be said for age and memory ability. This can be better illustrated visually.


```r
data %>%
  ggplot(aes(x = speech_rate, y = memory, group = age)) +
  geom_point() +
  facet_wrap(~age) +
  geom_label_repel(aes(label = memory), nudge_x = 1, na.rm = TRUE) +
  scale_x_continuous(
    limits = c(0, 7),
    sec.axis = sec_axis(~ . , name = "Age", breaks = NULL, labels = NULL)
    ) +
  ggtitle("Memory by Speech Rate") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="/courses/PSYC7709G/Lab3_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Unfortunately, it looks like we have an issue of independent variables being correlated with one another, known as multicollinearity.


```r
cor(data)
```

```
##                   age speech_rate    memory        id
## age         1.0000000   0.7500000 0.8027961 0.9561829
## speech_rate 0.7500000   1.0000000 0.9889517 0.8964215
## memory      0.8027961   0.9889517 1.0000000 0.9261501
## id          0.9561829   0.8964215 0.9261501 1.0000000
```

As we can see from the above, there is a 75% correlation between age and speech_rate. This means that a complete pooling regression model, where all of the data is pooled together, is not appropriate.

We can build the complete pooling model using the lm() function. The dependent variable, memory, is placed to the left of the "~" in the function, and the independent variables, age and speech_rate, to the right. The "data" parameter is filled by data.

The summary() function is then used to view the regression output.


```r
complete_pool <- lm(
  memory ~ 1 + speech_rate + age,
  data = data
  )

summary(complete_pool)
```

```
## 
## Call:
## lm(formula = memory ~ 1 + speech_rate + age, data = data)
## 
## Residuals:
##      1      2      3      4      5      6 
## -1.167 -1.667  2.333  3.333 -1.167 -1.667 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    1.667      3.598   0.463  0.67470   
## speech_rate    9.500      1.087   8.736  0.00316 **
## age            1.000      0.725   1.379  0.26162   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.877 on 3 degrees of freedom
## Multiple R-squared:  0.9866,	Adjusted R-squared:  0.9776 
## F-statistic: 110.1 on 2 and 3 DF,  p-value: 0.001559
```

There is a high R^2 for the mode, but when looking at the individual r^2 for the independent variables...


```r
cor(data) ^ 2
```

```
##                   age speech_rate    memory        id
## age         1.0000000   0.5625000 0.6444815 0.9142857
## speech_rate 0.5625000   1.0000000 0.9780254 0.8035714
## memory      0.6444815   0.9780254 1.0000000 0.8577539
## id          0.9142857   0.8035714 0.8577539 1.0000000
```

...They sum to greater than 1!

So what is to be done?

## No Pooling Models

One possible solution is to not pool any data and create an individual model for each age bucket...


```r
ggplot(
  data = data,
  aes(
    x = speech_rate,
    y = memory,
    col = age,
    group = age
  )
) +
  geom_point() +
  geom_smooth(
    method = lm
  )
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning in qt((1 - level)/2, df): NaNs produced

## Warning in qt((1 - level)/2, df): NaNs produced

## Warning in qt((1 - level)/2, df): NaNs produced
```

```
## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf
```

<img src="/courses/PSYC7709G/Lab3_files/figure-html/unnamed-chunk-7-1.png" width="672" />

But this looses the relationship between speech rate and age. In large data sets, this can become especially confusing and time consuming.

## Partial-Pooling Model: Multilevel

Rather than no pooling or complete pooling, we can create a partially-pooled multilevel model! Partial pooling is a process where population-level and individual-level effects are estimated simultaneously. By doing so, each individual's estimated effect is a reflection of the weighted combination of their own data and the population average. This allows us to account for the relationship between speech rate and age.

Using the brms package, we can easily fit a multilevel model in a very similar fashion to the lm() function. The dependent variable for the model again falls to the left of the "~" and the independent variables are placed to the right. What is different, however, is in the parentheses, "(speech_rate|age)". What this parameter does is specify that we want varying slopes and intercepts for speech rate for each age grouping.

(Aside: we also specify several other things unique to Bayesian models, most notably priors. Prior specification is a topic in it of itself, and for this model we can use vaguely informative priors based on the means and standard deviations of the data itself.)


```r
priors <- c(
  prior(normal(3, 2), class = b, coef = speech_rate),
  prior(normal(0, 2), class = Intercept),
  prior(normal(0, 2), class = sigma),
  prior(normal(0, 2), class = sd, coef = Intercept, group = age),
  prior(normal(3, 2), class = sd, coef = speech_rate, group = age)
)

partial_pool_model <- brm(
  memory ~ 1 + speech_rate + (speech_rate|age),
  data = data,
  prior= priors,
  iter = 4000,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 20
    )
)
```

Similar to using the "summary()" function for the complete pooling model, we can use it here for the multilevel model. 


```r
summary(partial_pool_model)
```

```
## Warning: There were 99 divergent transitions after warmup. Increasing
## adapt_delta above 0.99 may help. See http://mc-stan.org/misc/
## warnings.html#divergent-transitions-after-warmup
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: memory ~ 1 + speech_rate + (speech_rate | age) 
##    Data: data (Number of observations: 6) 
##   Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup draws = 8000
## 
## Group-Level Effects: 
## ~age (Number of levels: 3) 
##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
## sd(Intercept)                  2.63      1.63     0.12     6.13 1.00     1579
## sd(speech_rate)                6.23      1.23     4.03     8.79 1.00     3170
## cor(Intercept,speech_rate)     0.50      0.46    -0.71     0.99 1.00     1297
##                            Tail_ESS
## sd(Intercept)                  1890
## sd(speech_rate)                3659
## cor(Intercept,speech_rate)     1694
## 
## Population-Level Effects: 
##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept       3.59      3.67    -4.14     9.87 1.00     2187     3993
## speech_rate    -0.50      1.25    -2.76     2.11 1.00     2479     4096
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     1.77      1.03     0.36     4.28 1.01     1150     1232
## 
## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```


```r
mcmc_areas(
  partial_pool_model,
  pars = c(
    "b_Intercept",
    "b_speech_rate",
    "sd_age__Intercept",
    "sd_age__speech_rate"
  ),
  prob_outer = 0.99,
  prob = 0.95
)
```

<img src="/courses/PSYC7709G/Lab3_files/figure-html/unnamed-chunk-10-1.png" width="672" />

While the summary table is useful to get a sense of the model, and the subsequent chart visualizes the coefficients of the model for both the population and group levels, looking at the effect sizes for speech rate per age group is most illustrative.


```r
model_summary <- summary(partial_pool_model)
```

```
## Warning: There were 99 divergent transitions after warmup. Increasing
## adapt_delta above 0.99 may help. See http://mc-stan.org/misc/
## warnings.html#divergent-transitions-after-warmup
```

```r
effect_estimates <- as_tibble(ranef(partial_pool_model)[[1]]) %>%
  mutate(Estimate.speech_rate_with_fixed = Estimate.speech_rate + model_summary$fixed[2, 1])
  

ggplot(
  data = effect_estimates,
  aes(
    x = c(4, 7, 10),
    y = Estimate.speech_rate_with_fixed
  )
) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = Estimate.speech_rate_with_fixed - 2 * Est.Error.speech_rate,
      ymax = Estimate.speech_rate_with_fixed + 2 * Est.Error.speech_rate
    )
  ) +
  scale_x_continuous(
    breaks = c(4, 7, 10),
  ) +
  xlab("Age") +
  ylab("Estimated Effect of Speech Rate on Memory (-/+ 2sd)") +
  geom_label_repel(
    aes(
      label = round(Estimate.speech_rate_with_fixed, 2)
    ),
    nudge_y = 1
  )
```

<img src="/courses/PSYC7709G/Lab3_files/figure-html/unnamed-chunk-11-1.png" width="672" />

The above chart shows the estimated effect of speech rate on memory by age. We can see that, somewhat surprisingly, age 7 is where the effect of speech rate on memory is greatest while at age 4 it is the lowest.
