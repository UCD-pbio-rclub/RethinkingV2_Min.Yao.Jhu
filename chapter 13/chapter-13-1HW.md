---
title: "chpater 13-1HW"
author: "Min-Yao"
date: "2020/1/10"
output: 
  html_document: 
    keep_md: yes
---

## 13.7. Practice

###Easy.

### 12E1. Which of the following priors will produce more shrinkage in the estimates? (a) αtank ∼ Normal(0, 1); (b) αtank ∼ Normal(0, 2).

> I think (a) will produce more shrinkage in the estimates, as it is more 'regularising'(narrow because of lower sigma), thus I expect it forces estimates to be shifted to the mean more than (b). It results in stronger shrinkage.

### 12E2. Make the following model into a multilevel model.
y i ∼ Binomial(1, p i)
logit(p i) = αgroup[i] + βxi
αgroup ∼ Normal(0, 10)
β ∼ Normal(0, 1)

> multilevel model

y_i ~ Binomial(1,p_i)
logit(p_i) = a[i] + beta*x_i
αgroup ∼ Normal(α_bar, sigma)
α_bar ~ Normal(0, 10)
sigma ~ Exponential(1)
β ∼ Normal(0, 1)

### 12E3. Make the following model into a multilevel model.
y i ∼ Normal(µi, σ)
µi = αgroup[i] + βx i
αgroup ∼ Normal(0, 10)
β ∼ Normal(0, 1)
σ ∼ HalfCauchy(0, 2)

> multilevel model

y i ∼ Normal(µi, σ)
µi = αgroup[i] + βx i
αgroup ∼ Normal(α_bar, sigma)
α_bar ~ Normal(0, 10)
sigma ~ Exponential(1)
β ∼ Normal(0, 1)
σ ∼ HalfCauchy(0, 2)

### Medium.

### 12M1. Revisit the Reed frog survival data, data(reedfrogs), and add the predation and size treatment variables to the varying intercepts model. Consider models with either main effect alone, both main effects, as well as a model including both and their interaction. Instead of focusing on inferences about these two predictor variables, focus on the inferred variation across tanks. Explain why it changes as it does across models.


```r
## R code 13.1
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: StanHeaders
```

```
## Loading required package: ggplot2
```

```
## rstan (Version 2.19.2, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## For improved execution time, we recommend calling
## Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
## although this causes Stan to throw an error on a few processors.
```

```
## Loading required package: parallel
```

```
## Loading required package: dagitty
```

```
## rethinking (Version 1.90)
```

```
## 
## Attaching package: 'rethinking'
```

```
## The following object is masked from 'package:stats':
## 
##     rstudent
```

```r
data(reedfrogs)
d <- reedfrogs
str(d)
```

```
## 'data.frame':	48 obs. of  5 variables:
##  $ density : int  10 10 10 10 10 10 10 10 10 10 ...
##  $ pred    : Factor w/ 2 levels "no","pred": 1 1 1 1 1 1 1 1 2 2 ...
##  $ size    : Factor w/ 2 levels "big","small": 1 1 1 1 2 2 2 2 1 1 ...
##  $ surv    : int  9 10 7 10 9 9 10 9 4 9 ...
##  $ propsurv: num  0.9 1 0.7 1 0.9 0.9 1 0.9 0.4 0.9 ...
```

```r
head(d)
```

```
##   density pred  size surv propsurv
## 1      10   no   big    9      0.9
## 2      10   no   big   10      1.0
## 3      10   no   big    7      0.7
## 4      10   no   big   10      1.0
## 5      10   no small    9      0.9
## 6      10   no small    9      0.9
```


```r
# make the tank cluster variable
d$tank <- 1:nrow(d)
d$has_pred<- as.integer(d$pred=='pred')
d$is_big<- as.integer(d$size=='big')
str(d)
```

```
## 'data.frame':	48 obs. of  8 variables:
##  $ density : int  10 10 10 10 10 10 10 10 10 10 ...
##  $ pred    : Factor w/ 2 levels "no","pred": 1 1 1 1 1 1 1 1 2 2 ...
##  $ size    : Factor w/ 2 levels "big","small": 1 1 1 1 2 2 2 2 1 1 ...
##  $ surv    : int  9 10 7 10 9 9 10 9 4 9 ...
##  $ propsurv: num  0.9 1 0.7 1 0.9 0.9 1 0.9 0.4 0.9 ...
##  $ tank    : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ has_pred: int  0 0 0 0 0 0 0 0 1 1 ...
##  $ is_big  : int  1 1 1 1 0 0 0 0 1 1 ...
```

```r
dat <- list(
    S = d$surv,
    N = d$density,
    P = d$has_pred,
    B = d$is_big,
    tank = d$tank )

str(dat)
```

```
## List of 5
##  $ S   : int [1:48] 9 10 7 10 9 9 10 9 4 9 ...
##  $ N   : int [1:48] 10 10 10 10 10 10 10 10 10 10 ...
##  $ P   : int [1:48] 0 0 0 0 0 0 0 0 1 1 ...
##  $ B   : int [1:48] 1 1 1 1 0 0 0 0 1 1 ...
##  $ tank: int [1:48] 1 2 3 4 5 6 7 8 9 10 ...
```


### 12M2. Compare the models you fit just above, using WAIC. Can you reconcile the differences in WAIC with the posterior distributions of the models?




### 12H1. In 1980, a typical Bengali woman could have 5 or more children in her lifetime. By the year 200, a typical Bengali woman had only 2 or 3. You’re going to look at a historical set of data, when contraception was widely available but many families chose not to use it. These data reside in data(bangladesh) and come from the 1988 Bangladesh Fertility Survey. Each row is one of 1934 women. There are six variables, but you can focus on three of them for this practice problem:

(1) district: ID number of administrative district each woman resided in
(2) use.contraception: An indicator (0/1) of whether the woman was using contraception
(3) urban: An indicator (0/1) of whether the woman lived in a city, as opposed to living in a
rural area

The first thing to do is ensure that the cluster variable, district, is a contiguous set of integers. Recall that these values will be index values inside the model. If there are gaps, you’ll have parameters for which there is no data to inform them. Worse, the model probably won’t run. Look at the unique values of the district variable:

```r
## R code 13.40
#sort(unique(d$district))
```

District 54 is absent. So district isn’t yet a good index variable, because it’s not contiguous. Th is is
easy to fi x. Just make a new variable that is contiguous. Th is is enough to do it:

```r
## R code 13.41
#d$district_id <- as.integer(as.factor(d$district))
#sort(unique(d$district_id))
```

Now there are 60 values, contiguous integers 1 to 60.
Now, focus on predicting use.contraception, clustered by district_id. Do not include
urban just yet. Fit both (1) a traditional fi xed-eff ects model that uses dummy variables for district and
(2) a multilevel model with varying intercepts for district. Plot the predicted proportions of women
in each district using contraception, for both the fi xed-eff ects model and the varying-eff ects model.
Th at is, make a plot in which district ID is on the horizontal axis and expected proportion using con-
traception is on the vertical. Make one plot for each model, or layer them on the same plot, as you
prefer. How do the models disagree? Can you explain the pattern of disagreement? In particular, can
you explain the most extreme cases of disagreement, both why they happen where they do and why
the models reach diff erent inferences?
