---
title: "brms_intro_HW"
author: "Min-Yao"
date: "2020/8/16"
output: 
  html_document: 
    keep_md: yes
---

# Assignment



```r
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
## rstan (Version 2.21.2, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## Do not specify '-march=native' in 'LOCAL_CPPFLAGS' or a Makevars file
```

```
## Loading required package: parallel
```

```
## rethinking (Version 2.12)
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
library(brms)
```

```
## Loading required package: Rcpp
```

```
## Loading 'brms' package (version 2.13.5). Useful instructions
## can be found by typing help('brms'). A more detailed introduction
## to the package is available through vignette('brms_overview').
```

```
## 
## Attaching package: 'brms'
```

```
## The following objects are masked from 'package:rethinking':
## 
##     LOO, stancode, WAIC
```

```
## The following object is masked from 'package:rstan':
## 
##     loo
```

```
## The following object is masked from 'package:stats':
## 
##     ar
```

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## √ tibble  3.0.3     √ dplyr   1.0.1
## √ tidyr   1.1.1     √ stringr 1.4.0
## √ readr   1.3.1     √ forcats 0.5.0
## √ purrr   0.3.4
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x tidyr::extract() masks rstan::extract()
## x dplyr::filter()  masks stats::filter()
## x dplyr::lag()     masks stats::lag()
## x purrr::map()     masks rethinking::map()
```

Revisit the following homework problems and try to fit the with brms.  Make your first attempt without looking at the rethinking to brms translation, but if you get stuck definitely look!  Compare the coefficients or predictions that you obtain with brms and those with quap or ulam.

* 4H1, 4H2 (you probably need the function `posterior_predict()`)

### 4H1, The weights listed below were recorded in the !Kung census, but heights were not recorded for these individuals. Provide predicted heights and 89% intervals (either HPDI or PI) for each of these individuals. That is, fill in the table below, using model-based predictions.

Individual weight expected height 89% interval
1 46.95
2 43.72
3 64.78
4 32.59
5 54.63


```r
weights <- c(46.95,43.72,64.78,32.59,54.63)
```



```r
## change from R code 4.42
# load data again, since it's a long way back
library(rethinking)
data(Howell1)
d <- Howell1
str(d)
```

```
## 'data.frame':	544 obs. of  4 variables:
##  $ height: num  152 140 137 157 145 ...
##  $ weight: num  47.8 36.5 31.9 53 41.3 ...
##  $ age   : num  63 63 65 41 51 35 32 27 19 54 ...
##  $ male  : int  1 0 0 1 0 1 0 1 0 1 ...
```


```r
d2 <- d
str(d2)
```

```
## 'data.frame':	544 obs. of  4 variables:
##  $ height: num  152 140 137 157 145 ...
##  $ weight: num  47.8 36.5 31.9 53 41.3 ...
##  $ age   : num  63 63 65 41 51 35 32 27 19 54 ...
##  $ male  : int  1 0 0 1 0 1 0 1 0 1 ...
```

```r
m4H1brm <- brm(height ~ weight, data=d2,
               prior=c(
                 set_prior("normal(178, 20)", class = "Intercept"),
                 set_prior("lognormal(0,1)", class = "b",lb=0),
                 set_prior("exponential(1)", class = "sigma"))
               )
```

```
## Compiling Stan program...
```

```
## Start sampling
```

```
## 
## SAMPLING FOR MODEL '4f686d4abcd6152a27b0e5a2aad2c9a4' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.063 seconds (Warm-up)
## Chain 1:                0.06 seconds (Sampling)
## Chain 1:                0.123 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '4f686d4abcd6152a27b0e5a2aad2c9a4' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.078 seconds (Warm-up)
## Chain 2:                0.06 seconds (Sampling)
## Chain 2:                0.138 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '4f686d4abcd6152a27b0e5a2aad2c9a4' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.105 seconds (Warm-up)
## Chain 3:                0.055 seconds (Sampling)
## Chain 3:                0.16 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '4f686d4abcd6152a27b0e5a2aad2c9a4' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.079 seconds (Warm-up)
## Chain 4:                0.047 seconds (Sampling)
## Chain 4:                0.126 seconds (Total)
## Chain 4:
```

```r
m4H1brm
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: height ~ weight 
##    Data: d2 (Number of observations: 544) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 4000
## 
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    75.48      1.04    73.48    77.52 1.00     4166     2869
## weight        1.76      0.03     1.71     1.82 1.00     4304     2470
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     9.31      0.28     8.80     9.86 1.00     3731     2969
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
plot(m4H1brm)
```

![](brms_intro_HW_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
## predicted responses
p_m4H1brm <- predict(m4H1brm, newdata = data.frame(weight=weights))
head(p_m4H1brm)
```

```
##      Estimate Est.Error     Q2.5    Q97.5
## [1,] 158.5149  9.337546 140.4953 176.4701
## [2,] 152.4636  9.389424 134.2943 170.6030
## [3,] 189.4571  9.214452 172.2374 207.3911
## [4,] 132.7303  9.375878 114.1893 151.0093
## [5,] 171.6362  9.197070 153.2568 189.7668
```

```r
pp_m4H1brm <- posterior_predict(m4H1brm, newdata = data.frame(weight=weights))
head(pp_m4H1brm)
```

```
##          [,1]     [,2]     [,3]     [,4]     [,5]
## [1,] 154.4648 160.1035 196.6244 127.9117 171.3415
## [2,] 178.5702 140.9422 181.8364 129.3378 167.5227
## [3,] 171.5278 165.7688 199.4809 122.5525 173.5380
## [4,] 164.7647 153.7625 182.0530 145.0614 182.6350
## [5,] 159.2282 178.0705 202.1272 134.7726 160.5615
## [6,] 160.0418 160.2653 184.9159 119.2024 169.9193
```

### 4H2 Select out all the rows in the Howell1 data with ages below 18 years of age. If you do it right, you should end up with a new data frame with 192 rows in it.


```r
## change from R code 4.42
# load data again, since it's a long way back
library(rethinking)
data(Howell1)
d <- Howell1
d4H2 <- d[ d$age < 18 , ]
str(d4H2)
```

```
## 'data.frame':	192 obs. of  4 variables:
##  $ height: num  121.9 105.4 86.4 129.5 109.2 ...
##  $ weight: num  19.6 13.9 10.5 23.6 16 ...
##  $ age   : num  12 8 6.5 13 7 17 16 11 17 8 ...
##  $ male  : int  1 0 0 1 0 1 0 1 0 1 ...
```
#### (a) Fit a linear regression to these data, using quap. Present and interpret the estimates. For every 10 units of increase in weight, how much taller does the model predict a child gets?


```r
m4H2brm <- brm(height ~ weight, data=d4H2,
               prior=c(
                 set_prior("normal(178, 20)", class = "Intercept"),
                 set_prior("lognormal(0,1)", class = "b",lb=0),
                 set_prior("exponential(1)", class = "sigma"))
               )
```

```
## Compiling Stan program...
```

```
## recompiling to avoid crashing R session
```

```
## Start sampling
```

```
## 
## SAMPLING FOR MODEL '4f686d4abcd6152a27b0e5a2aad2c9a4' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0.001 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.058 seconds (Warm-up)
## Chain 1:                0.048 seconds (Sampling)
## Chain 1:                0.106 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '4f686d4abcd6152a27b0e5a2aad2c9a4' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.058 seconds (Warm-up)
## Chain 2:                0.039 seconds (Sampling)
## Chain 2:                0.097 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '4f686d4abcd6152a27b0e5a2aad2c9a4' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.065 seconds (Warm-up)
## Chain 3:                0.052 seconds (Sampling)
## Chain 3:                0.117 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '4f686d4abcd6152a27b0e5a2aad2c9a4' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.06 seconds (Warm-up)
## Chain 4:                0.049 seconds (Sampling)
## Chain 4:                0.109 seconds (Total)
## Chain 4:
```

```r
m4H2brm
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: height ~ weight 
##    Data: d4H2 (Number of observations: 192) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 4000
## 
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    58.37      1.42    55.66    61.14 1.00     4056     3061
## weight        2.72      0.07     2.58     2.85 1.00     4001     2964
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     8.35      0.42     7.59     9.21 1.00     3856     2588
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
plot(m4H2brm)
```

![](brms_intro_HW_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
> For every 10 units of increase in weight, the model predict a child gets 27.2 cm taller.

#### (b) Plot the raw data, with height on the vertical axis and weight on the horizontal axis. Super-impose the MAP regression line and 89% HPDI for the mean. Also superimpose the 89% HPDI for predicted heights.


```r
## change from R code 4.54
# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from= min(d4H2$weight) , to= max(d4H2$weight) , by=1 )

## predicted responses
p_m4H2brm <- predict(m4H2brm, newdata = data.frame(weight=weight.seq))
head(p_m4H2brm)
```

```
##      Estimate Est.Error     Q2.5    Q97.5
## [1,] 69.98755  8.329657 53.63433 86.39094
## [2,] 72.75514  8.547724 56.07933 89.78980
## [3,] 75.37592  8.523187 58.35190 92.22298
## [4,] 77.96535  8.590483 60.87102 95.02456
## [5,] 80.62253  8.456076 63.92399 96.95962
## [6,] 83.55276  8.399739 66.76190 99.65032
```

```r
df_p_m4H2brm <- data.frame(p_m4H2brm)
str(df_p_m4H2brm)
```

```
## 'data.frame':	41 obs. of  4 variables:
##  $ Estimate : num  70 72.8 75.4 78 80.6 ...
##  $ Est.Error: num  8.33 8.55 8.52 8.59 8.46 ...
##  $ Q2.5     : num  53.6 56.1 58.4 60.9 63.9 ...
##  $ Q97.5    : num  86.4 89.8 92.2 95 97 ...
```


```r
## change from R code 4.61
# plot raw data
plot( height ~ weight , d4H2 , col=col.alpha(rangi2,1) )
# draw MAP line
lines( weight.seq , df_p_m4H2brm$Estimate )
# draw HPDI region for line
lines( weight.seq , df_p_m4H2brm$Q2.5 )
lines( weight.seq , df_p_m4H2brm$Q97.5 )
```

![](brms_intro_HW_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


#### (c) What aspects of the model fit concern you? Describe the kinds of assumptions you would change, if any, to improve the model. You don’t have to write any new code. Just explain what the model appears to be doing a bad job of, and what you hypothesize would be a better model.

> I think a parabolic (second order) polynomial would be a better model.



* From chapter 8 I assigned a tomato problem from my data "Use the tomato.csv data set and evaluate whether hypocotyl length ("hyp") is affected by shade ("trt"), species ("species") and their interaction."

#### 1. Use the tomato.csv (attached) data set and evaluate whether hypocotyl length ("hyp") is affected by shade ("trt"), species ("species") and their interaction.


```r
tomato <- read.csv("Tomato.csv")
str(tomato)
```

```
## 'data.frame':	1008 obs. of  25 variables:
##  $ shelf   : chr  "Z" "Z" "Z" "Z" ...
##  $ flat    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ col     : chr  "B" "C" "D" "E" ...
##  $ row     : int  1 1 1 1 1 1 1 2 2 2 ...
##  $ acs     : chr  "LA2580" "LA1305" "LA1973" "LA2748" ...
##  $ trt     : chr  "H" "H" "H" "H" ...
##  $ days    : int  28 28 28 28 28 28 28 28 28 28 ...
##  $ date    : chr  "5/5/08" "5/5/08" "5/5/08" "5/5/08" ...
##  $ hyp     : num  19.5 31.3 56.6 35.2 35.3 ...
##  $ int1    : num  2.37 3.34 8.43 0.56 0.82 1.07 2.85 2.08 5.43 4.08 ...
##  $ int2    : num  1.59 0.01 2.39 0 0.02 6.69 0.41 0.53 0.81 3.26 ...
##  $ int3    : num  1.87 9.19 6.7 1.6 1.49 5.72 3.79 1.9 3.63 3.49 ...
##  $ int4    : num  0.51 1.62 3.69 0.61 0.46 4.76 3.25 NA 2.66 3.02 ...
##  $ intleng : num  6.34 14.16 21.21 2.77 2.79 ...
##  $ totleng : num  25.8 45.4 77.9 38 38.1 ...
##  $ petleng : num  15.78 12.36 13.05 8.08 7.68 ...
##  $ leafleng: num  30.5 22.9 46.7 26.8 22.4 ...
##  $ leafwid : num  34.4 14 43.8 33.3 23.6 ...
##  $ leafnum : int  5 4 5 5 5 5 5 4 5 5 ...
##  $ ndvi    : int  111 120 110 105 106 132 118 112 107 123 ...
##  $ lat     : num  -9.52 -13.38 -16.23 -20.48 -20.92 ...
##  $ lon     : num  -78 -75.4 -71.7 -70 -69.1 ...
##  $ alt     : int  740 3360 2585 1020 2460 2000 2920 480 75 3540 ...
##  $ species : chr  "S. pennellii" "S. peruvianum" "S. peruvianum" "S. chilense" ...
##  $ who     : chr  "Dan" "Dan" "Dan" "Dan" ...
```

```r
head(tomato)
```

```
##   shelf flat col row    acs trt days   date   hyp int1 int2 int3 int4 intleng
## 1     Z    1   B   1 LA2580   H   28 5/5/08 19.46 2.37 1.59 1.87 0.51    6.34
## 2     Z    1   C   1 LA1305   H   28 5/5/08 31.28 3.34 0.01 9.19 1.62   14.16
## 3     Z    1   D   1 LA1973   H   28 5/5/08 56.65 8.43 2.39 6.70 3.69   21.21
## 4     Z    1   E   1 LA2748   H   28 5/5/08 35.18 0.56 0.00 1.60 0.61    2.77
## 5     Z    1   F   1 LA2931   H   28 5/5/08 35.32 0.82 0.02 1.49 0.46    2.79
## 6     Z    1   G   1 LA1317   H   28 5/5/08 28.74 1.07 6.69 5.72 4.76   18.24
##   totleng petleng leafleng leafwid leafnum ndvi      lat      lon  alt
## 1   25.80   15.78    30.53   34.44       5  111  -9.5167 -78.0083  740
## 2   45.44   12.36    22.93   13.99       4  120 -13.3833 -75.3583 3360
## 3   77.86   13.05    46.71   43.78       5  110 -16.2333 -71.7000 2585
## 4   37.95    8.08    26.82   33.28       5  105 -20.4833 -69.9833 1020
## 5   38.11    7.68    22.40   23.61       5  106 -20.9167 -69.0667 2460
## 6   46.98   23.66    42.35   42.35       5  132 -13.4167 -73.8417 2000
##           species who
## 1    S. pennellii Dan
## 2   S. peruvianum Dan
## 3   S. peruvianum Dan
## 4     S. chilense Dan
## 5     S. chilense Dan
## 6 S. chmielewskii Dan
```


```r
tomato2 <- subset(tomato, select = c(hyp, trt, species))
str(tomato2)
```

```
## 'data.frame':	1008 obs. of  3 variables:
##  $ hyp    : num  19.5 31.3 56.6 35.2 35.3 ...
##  $ trt    : chr  "H" "H" "H" "H" ...
##  $ species: chr  "S. pennellii" "S. peruvianum" "S. peruvianum" "S. chilense" ...
```

```r
head(tomato2)
```

```
##     hyp trt         species
## 1 19.46   H    S. pennellii
## 2 31.28   H   S. peruvianum
## 3 56.65   H   S. peruvianum
## 4 35.18   H     S. chilense
## 5 35.32   H     S. chilense
## 6 28.74   H S. chmielewskii
```


```r
tomato2$SID <- as.factor(tomato2$species)
tomato2$TID <- as.numeric(as.factor(tomato2$trt))-1
str(tomato2)
```

```
## 'data.frame':	1008 obs. of  5 variables:
##  $ hyp    : num  19.5 31.3 56.6 35.2 35.3 ...
##  $ trt    : chr  "H" "H" "H" "H" ...
##  $ species: chr  "S. pennellii" "S. peruvianum" "S. peruvianum" "S. chilense" ...
##  $ SID    : Factor w/ 5 levels "S. chilense",..: 4 5 5 1 1 2 3 4 5 5 ...
##  $ TID    : num  0 0 0 0 0 0 0 0 0 0 ...
```

```r
head(tomato2)
```

```
##     hyp trt         species             SID TID
## 1 19.46   H    S. pennellii    S. pennellii   0
## 2 31.28   H   S. peruvianum   S. peruvianum   0
## 3 56.65   H   S. peruvianum   S. peruvianum   0
## 4 35.18   H     S. chilense     S. chilense   0
## 5 35.32   H     S. chilense     S. chilense   0
## 6 28.74   H S. chmielewskii S. chmielewskii   0
```

```r
summary(tomato2)
```

```
##       hyp            trt              species                       SID     
##  Min.   : 6.17   Length:1008        Length:1008        S. chilense    :207  
##  1st Qu.:26.81   Class :character   Class :character   S. chmielewskii:226  
##  Median :32.02   Mode  :character   Mode  :character   S. habrochaites:226  
##  Mean   :33.36                                         S. pennellii   :132  
##  3rd Qu.:38.56                                         S. peruvianum  :217  
##  Max.   :74.60                                                              
##       TID        
##  Min.   :0.0000  
##  1st Qu.:0.0000  
##  Median :1.0000  
##  Mean   :0.5089  
##  3rd Qu.:1.0000  
##  Max.   :1.0000
```
#### no interaction model
##### rethinking

```r
tomato_m1 <- quap(
  alist(
    hyp ~ dnorm( mu , sigma ) ,
    mu <- a[SID] + b*TID ,
    a[SID] ~ dnorm( 30 , 5 ) ,
    b ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )) ,
    data=tomato2 )
precis(tomato_m1, depth=2)
```

```
##            mean        sd      5.5%     94.5%
## a[1]  32.663559 0.6676654 31.596501 33.730617
## a[2]  29.985594 0.6454395 28.954057 31.017131
## a[3]  29.172750 0.6373990 28.154064 30.191437
## a[4]  26.651144 0.8173026 25.344936 27.957351
## a[5]  36.327638 0.6560625 35.279124 37.376153
## b      4.044714 0.4924035  3.257758  4.831670
## sigma  9.001190 0.1998889  8.681729  9.320651
```


```r
### interaction model
tomato_m2 <- quap(
  alist(
    hyp ~ dnorm( mu , sigma ) ,
    mu <- a[SID] + b*TID + bST[SID]*TID,
    a[SID] ~ dnorm( 30 , 5 ) ,
    b ~ dnorm( 0 , 1 ) ,
    bST[SID] ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )) ,
    data=tomato2 )

precis(tomato_m2, depth=2)
```

```
##               mean        sd       5.5%     94.5%
## a[1]   32.33627767 0.7512677 31.1356068 33.536949
## a[2]   30.36086681 0.7298618 29.1944066 31.527327
## a[3]   29.42443930 0.7093599 28.2907452 30.558133
## a[4]   26.01072289 0.9185976 24.5426265 27.478819
## a[5]   35.97875497 0.7413361 34.7939567 37.163553
## b       3.58254767 0.5852943  2.6471343  4.517961
## bST[1]  1.11978294 0.8112031 -0.1766763  2.416242
## bST[2] -0.27317965 0.8020539 -1.5550167  1.008657
## bST[3] -0.07743399 0.8023531 -1.3597492  1.204881
## bST[4]  1.66409540 0.8581254  0.2926452  3.035546
## bST[5]  1.14929528 0.8061866 -0.1391467  2.437737
## sigma   8.96049564 0.1992958  8.6419824  9.279009
```



##### brms



```r
get_prior(hyp ~ 1 + SID + TID, data=tomato2)
```

```
##                   prior     class              coef group resp dpar nlpar bound
## 1                               b                                              
## 2                               b SIDS.chmielewskii                            
## 3                               b SIDS.habrochaites                            
## 4                               b    SIDS.pennellii                            
## 5                               b   SIDS.peruvianum                            
## 6                               b               TID                            
## 7 student_t(3, 32, 8.5) Intercept                                              
## 8  student_t(3, 0, 8.5)     sigma
```

```r
get_prior(hyp ~ 0 + SID + TID, data=tomato2)
```

```
##                  prior class              coef group resp dpar nlpar bound
## 1                          b                                              
## 2                          b     SIDS.chilense                            
## 3                          b SIDS.chmielewskii                            
## 4                          b SIDS.habrochaites                            
## 5                          b    SIDS.pennellii                            
## 6                          b   SIDS.peruvianum                            
## 7                          b               TID                            
## 8 student_t(3, 0, 8.5) sigma
```


```r
tomato_m1_brms <-
  brm(data = tomato2, 
      family = gaussian,
      hyp ~ 1 + TID,
      prior = c(prior(normal(30 , 5), class = Intercept),
                prior(normal(0 , 1), class = b),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 8)
```

```
## Compiling Stan program...
```

```
## Start sampling
```

```r
tomato_m1_brms
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: hyp ~ 1 + TID 
##    Data: tomato2 (Number of observations: 1008) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 4000
## 
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    31.37      0.40    30.58    32.15 1.00     4041     3271
## TID           3.88      0.52     2.87     4.89 1.00     3703     2905
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     9.58      0.22     9.15    10.02 1.00     3763     3082
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
plot(tomato_m1_brms)
```

![](brms_intro_HW_files/figure-html/unnamed-chunk-22-1.png)<!-- -->



```r
tomato_m1_brms_2 <-
  brm(data = tomato2,
      family = gaussian,
      hyp ~ 0 + SID + TID,
      prior = c(prior(normal(30 , 5), class = b, coef = SIDS.chilense),
                prior(normal(30 , 5), class = b, coef = SIDS.chmielewskii),
                prior(normal(30 , 5), class = b, coef = SIDS.habrochaites),
                prior(normal(30 , 5), class = b, coef = SIDS.pennellii),
                prior(normal(30 , 5), class = b, coef = SIDS.peruvianum),
                prior(normal(0 , 1), class = b, coef = TID),
                prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 8)
```

```
## Compiling Stan program...
```

```
## Start sampling
```

```r
# tomato_m1_brms_2 <-
#   brm(data = tomato2, 
#       family = gaussian,
#       hyp ~ 0 + SID + TID,
#       prior = c(prior(normal(30 , 5), class = b , coef = SID),
#                 prior(normal(0 , 1), class = b, coef = TID),
#                 prior(exponential(1), class = sigma)),
#       iter = 2000, warmup = 1000, chains = 4, cores = 4,
#       seed = 8)
```

```r
tomato_m1_brms_2
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: hyp ~ 0 + SID + TID 
##    Data: tomato2 (Number of observations: 1008) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 4000
## 
## Population-Level Effects: 
##                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## SIDS.chilense        32.67      0.67    31.41    34.00 1.00     5349     2767
## SIDS.chmielewskii    30.00      0.65    28.69    31.25 1.00     5085     3203
## SIDS.habrochaites    29.17      0.65    27.88    30.45 1.00     6352     3449
## SIDS.pennellii       26.66      0.82    25.06    28.24 1.00     5394     3046
## SIDS.peruvianum      36.33      0.66    35.03    37.63 1.00     5493     3062
## TID                   4.03      0.50     3.05     5.02 1.00     3855     3188
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     9.04      0.20     8.66     9.45 1.00     6305     3057
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```


```r
plot(tomato_m1_brms_2)
```

![](brms_intro_HW_files/figure-html/unnamed-chunk-25-1.png)<!-- -->![](brms_intro_HW_files/figure-html/unnamed-chunk-25-2.png)<!-- -->


```r
tomato_m1_brms_2_I <- 
  brm(data = tomato2, 
      family = gaussian,
      bf(hyp ~ 0 + a + b * TID, 
         a ~ 0 + SID, 
         b ~ 0 + SID,
         nl = TRUE),
      prior = c(prior(normal(30 , 5), class = b, coef = SIDS.chilense, nlpar = a),
                prior(normal(30 , 5), class = b, coef = SIDS.chmielewskii, nlpar = a),
                prior(normal(30 , 5), class = b, coef = SIDS.habrochaites, nlpar = a),
                prior(normal(30 , 5), class = b, coef = SIDS.pennellii, nlpar = a),
                prior(normal(30 , 5), class = b, coef = SIDS.peruvianum, nlpar = a),
                prior(normal(30 , 5), class = b, coef = SIDS.chilense, nlpar = b),
                prior(normal(30 , 5), class = b, coef = SIDS.chmielewskii, nlpar = b),
                prior(normal(30 , 5), class = b, coef = SIDS.habrochaites, nlpar = b),
                prior(normal(30 , 5), class = b, coef = SIDS.pennellii, nlpar = b),
                prior(normal(30 , 5), class = b, coef = SIDS.peruvianum, nlpar = b),
                prior(exponential(1), class = sigma)),
      chains = 4, cores = 4,
      seed = 9)
```

```
## Compiling Stan program...
```

```
## Start sampling
```


```r
tomato_m1_brms_2_I
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: hyp ~ 0 + a + b * TID 
##          a ~ 0 + SID
##          b ~ 0 + SID
##    Data: tomato2 (Number of observations: 1008) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 4000
## 
## Population-Level Effects: 
##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## a_SIDS.chilense        30.78      0.86    29.03    32.46 1.00     3625     3044
## a_SIDS.chmielewskii    29.80      0.85    28.13    31.43 1.00     3997     2829
## a_SIDS.habrochaites    28.79      0.79    27.24    30.33 1.00     4343     2912
## a_SIDS.pennellii       22.91      1.11    20.71    25.13 1.00     4145     3210
## a_SIDS.peruvianum      34.45      0.86    32.81    36.15 1.00     4314     3394
## b_SIDS.chilense         7.80      1.21     5.47    10.21 1.00     3687     3238
## b_SIDS.chmielewskii     4.40      1.17     2.11     6.79 1.00     3824     3037
## b_SIDS.habrochaites     4.82      1.16     2.66     7.15 1.00     4433     3242
## b_SIDS.pennellii       11.07      1.47     8.11    13.95 1.00     4139     3078
## b_SIDS.peruvianum       7.73      1.20     5.42    10.04 1.00     3952     3451
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     8.99      0.20     8.62     9.40 1.00     4735     2940
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```


```r
plot(tomato_m1_brms_2_I)
```

![](brms_intro_HW_files/figure-html/unnamed-chunk-28-1.png)<!-- -->![](brms_intro_HW_files/figure-html/unnamed-chunk-28-2.png)<!-- -->![](brms_intro_HW_files/figure-html/unnamed-chunk-28-3.png)<!-- -->

* From chapter 9: 8M1 (remember that the problem numbers were offset it is actually called 9M1 in the Nov 24 PDF)

#### 8M1. Re-estimate the terrain ruggedness model from the chapter, but now using a uniform prior and an exponential prior for the standard deviation, sigma. The uniform prior should be dunif(0,10) and the exponential should be dexp(1). Do the different priors have any detectible influence on the posterior distribution?


```r
## R code 9.9
library(rethinking)
data(rugged)
d <- rugged
```



```r
d <- 
  d %>%
  mutate(log_gdp = log(rgdppc_2000))

dd <-
  d %>%
  drop_na(rgdppc_2000) %>% 
  mutate(log_gdp_std = log_gdp / mean(log_gdp),
         rugged_std  = rugged / max(rugged),
         cid         = ifelse(cont_africa == 1, "1", "2")) %>% 
  mutate(rugged_std_c = rugged_std - mean(rugged_std))
```

#### exponential prior


```r
b9.1 <- 
  brm(data = dd, 
      family = gaussian,
      bf(log_gdp_std ~ 0 + a + b * (rugged_std - 0.215), 
         a ~ 0 + cid, 
         b ~ 0 + cid,
         nl = TRUE),
      prior = c(prior(normal(1, 0.1), class = b, coef = cid1, nlpar = a),
                prior(normal(1, 0.1), class = b, coef = cid2, nlpar = a),
                prior(normal(0, 0.3), class = b, coef = cid1, nlpar = b),
                prior(normal(0, 0.3), class = b, coef = cid2, nlpar = b),
                prior(exponential(1), class = sigma)),
      chains = 4, cores = 4,
      seed = 9)
```

```
## Compiling Stan program...
```

```
## Start sampling
```

```r
b9.1
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: log_gdp_std ~ 0 + a + b * (rugged_std - 0.215) 
##          a ~ 0 + cid
##          b ~ 0 + cid
##    Data: dd (Number of observations: 170) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 4000
## 
## Population-Level Effects: 
##        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## a_cid1     0.89      0.02     0.85     0.92 1.00     4771     3531
## a_cid2     1.05      0.01     1.03     1.07 1.00     4634     3044
## b_cid1     0.13      0.08    -0.02     0.28 1.00     4427     2818
## b_cid2    -0.14      0.06    -0.26    -0.03 1.00     4898     3176
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     0.11      0.01     0.10     0.12 1.00     4605     3288
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
plot(b9.1)
```

![](brms_intro_HW_files/figure-html/unnamed-chunk-33-1.png)<!-- -->


#### The uniform prior should be dunif(0,10)


```r
b9.1_unif <- 
  brm(data = dd, 
      family = gaussian,
      bf(log_gdp_std ~ 0 + a + b * (rugged_std - 0.215), 
         a ~ 0 + cid, 
         b ~ 0 + cid,
         nl = TRUE),
      prior = c(prior(normal(1, 0.1), class = b, coef = cid1, nlpar = a),
                prior(normal(1, 0.1), class = b, coef = cid2, nlpar = a),
                prior(normal(0, 0.3), class = b, coef = cid1, nlpar = b),
                prior(normal(0, 0.3), class = b, coef = cid2, nlpar = b),
                prior(uniform(0,10), class = sigma)),
      chains = 4, cores = 4,
      seed = 9)
```

```
## Warning: It appears as if you have specified an upper bounded prior on a parameter that has no natural upper bound.
## If this is really what you want, please specify argument 'ub' of 'set_prior' appropriately.
## Warning occurred for prior 
## sigma ~ uniform(0, 10)
```

```
## Compiling Stan program...
```

```
## Start sampling
```

```r
b9.1_unif
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: log_gdp_std ~ 0 + a + b * (rugged_std - 0.215) 
##          a ~ 0 + cid
##          b ~ 0 + cid
##    Data: dd (Number of observations: 170) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 4000
## 
## Population-Level Effects: 
##        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## a_cid1     0.89      0.02     0.86     0.92 1.00     4622     3064
## a_cid2     1.05      0.01     1.03     1.07 1.00     4611     3005
## b_cid1     0.13      0.07    -0.01     0.28 1.00     4247     2849
## b_cid2    -0.14      0.06    -0.25    -0.03 1.00     5140     2960
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     0.11      0.01     0.10     0.12 1.00     4552     3271
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
plot(b9.1_unif)
```

![](brms_intro_HW_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

> It looks like the different priors have no detectible influence on the posterior distribution.
