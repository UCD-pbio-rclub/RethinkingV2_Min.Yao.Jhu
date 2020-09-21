---
title: "brms_week4_HW"
author: "Min-Yao"
date: "2020/9/10"
output: 
  html_document: 
    keep_md: yes
---


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
## -- Attaching packages ------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## √ tibble  3.0.3     √ dplyr   1.0.1
## √ tidyr   1.1.1     √ stringr 1.4.0
## √ readr   1.3.1     √ forcats 0.5.0
## √ purrr   0.3.4
```

```
## -- Conflicts ---------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x tidyr::extract() masks rstan::extract()
## x dplyr::filter()  masks stats::filter()
## x dplyr::lag()     masks stats::lag()
## x purrr::map()     masks rethinking::map()
```


```r
germ <- read_csv("light_round1_tall.csv") %>%
  filter(wps == 0) %>%
  select(pops, temps, total_seeds, germ, day, cumulative_germ)
```

```
## Parsed with column specification:
## cols(
##   pops = col_character(),
##   temps = col_double(),
##   wps = col_double(),
##   date = col_character(),
##   total_seeds = col_double(),
##   germ = col_double(),
##   start_date = col_date(format = ""),
##   census_date = col_date(format = ""),
##   day = col_double(),
##   cumulative_germ = col_double(),
##   cumulative_prop_germ = col_double()
## )
```

```r
germ
```

```
## # A tibble: 3,840 x 6
##    pops  temps total_seeds  germ   day cumulative_germ
##    <chr> <dbl>       <dbl> <dbl> <dbl>           <dbl>
##  1 CAAM      5          50     0     1               0
##  2 CAAM      5          50     0     2               0
##  3 CAAM      5          50     0     3               0
##  4 CAAM      5          50     0     4               0
##  5 CAAM      5          50     0     5               0
##  6 CAAM      5          50     0     6               0
##  7 CAAM      5          50     0     7               0
##  8 CAAM      5          50     0     8               0
##  9 CAAM      5          50     0     9               0
## 10 CAAM      5          50     0    10               0
## # ... with 3,830 more rows
```


```r
one_per_row <- function(df) {
  total_seed <- max(df$total_seeds, sum(df$germ))
  newdata <- tibble(id=1:total_seed, germ=0, day=max(df$day))
  df <- df %>% filter(germ>0)
  count <- 1
  if (nrow(df) > 0) {
    for (i in 1:nrow(df)) { # we look at each row of the df where germination occured
      for (j in 1:df$germ[i]) { # now update the newdata to reflect the germiantion of each seed
        newdata$germ[count] <- 1
        newdata$day[count]=df$day[i]
        count <- count+1 # count keeps track of which individual we are at in the new data
      } # for j
    } # for i
  } # if 
  return(newdata)
}
germone <- germ %>% group_by(pops, temps) %>%
  select(-cumulative_germ) %>% # not needed in this encoding (I think...in any case would need to be recalculated)
  nest() %>%
  mutate(newdata=map(data, one_per_row)) %>%
  select(-data) %>%
  unnest(newdata)
germone
```

```
## # A tibble: 9,120 x 5
## # Groups:   pops, temps [192]
##    pops  temps    id  germ   day
##    <chr> <dbl> <int> <dbl> <dbl>
##  1 CAAM      5     1     0    28
##  2 CAAM      5     2     0    28
##  3 CAAM      5     3     0    28
##  4 CAAM      5     4     0    28
##  5 CAAM      5     5     0    28
##  6 CAAM      5     6     0    28
##  7 CAAM      5     7     0    28
##  8 CAAM      5     8     0    28
##  9 CAAM      5     9     0    28
## 10 CAAM      5    10     0    28
## # ... with 9,110 more rows
```

```r
germ.stdi <- germone %>% filter(pops=="STDI") %>% select(-pops)
```

```
## Adding missing grouping variables: `pops`
```

```r
germ.stdi
```

```
## # A tibble: 396 x 5
## # Groups:   pops, temps [8]
##    pops  temps    id  germ   day
##    <chr> <dbl> <int> <dbl> <dbl>
##  1 STDI      5     1     1    24
##  2 STDI      5     2     0    28
##  3 STDI      5     3     0    28
##  4 STDI      5     4     0    28
##  5 STDI      5     5     0    28
##  6 STDI      5     6     0    28
##  7 STDI      5     7     0    28
##  8 STDI      5     8     0    28
##  9 STDI      5     9     0    28
## 10 STDI      5    10     0    28
## # ... with 386 more rows
```

Assignment:

You can start from my models if you want to.  The three relevant ones from my code are

### m1.1/m1.2: censored exponential (the worst)

#### m1.1: rethinking

exponential rate curve, censoring seed that don't germinate.


```r
d <- list(germ=germ.stdi$germ, 
          temps=as.numeric(as.factor(germ.stdi$temps)),
          day=germ.stdi$day)
m1.1 <- ulam(
  alist(
    day | germ==1 ~ exponential( lambda),
    day | germ==0 ~ custom(exponential_lccdf( !Y | lambda)),
    lambda <- 1.0 / mu,
    log(mu) <- a[temps],
    a[temps] ~ normal(0,1)),
  data=d,
  chains=4,
  cores = 4
)
```


```r
precis(m1.1, depth = 2)
```

```
##          mean        sd     5.5%    94.5%    n_eff     Rhat4
## a[1] 5.441308 0.3931014 4.859214 6.119978 2262.352 0.9988236
## a[2] 4.695024 0.2848085 4.272482 5.159395 2361.054 0.9992817
## a[3] 4.022022 0.2176538 3.683704 4.376322 2247.247 0.9995361
## a[4] 2.480859 0.1581518 2.242390 2.734211 2903.270 0.9994678
## a[5] 3.069373 0.1741638 2.805226 3.360603 2713.767 0.9993577
## a[6] 3.732955 0.1994819 3.431914 4.043900 2692.657 0.9988128
## a[7] 5.194464 0.3433344 4.687341 5.779571 3077.458 0.9988117
## a[8] 5.619692 0.4010675 5.027888 6.278359 2462.794 0.9994094
```

The above represent log(mean time to germination)

#### m1.2: brms censoring model

need to set up indicator for censoring.

```r
germ.stdi <- germ.stdi %>%
  mutate(cens=ifelse(germ==0, "right", "none"),
         tempsc=as.character(temps) %>% str_pad(width=2, pad="0"))
germ.stdi
```

```
## # A tibble: 396 x 7
## # Groups:   pops, temps [8]
##    pops  temps    id  germ   day cens  tempsc
##    <chr> <dbl> <int> <dbl> <dbl> <chr> <chr> 
##  1 STDI      5     1     1    24 none  05    
##  2 STDI      5     2     0    28 right 05    
##  3 STDI      5     3     0    28 right 05    
##  4 STDI      5     4     0    28 right 05    
##  5 STDI      5     5     0    28 right 05    
##  6 STDI      5     6     0    28 right 05    
##  7 STDI      5     7     0    28 right 05    
##  8 STDI      5     8     0    28 right 05    
##  9 STDI      5     9     0    28 right 05    
## 10 STDI      5    10     0    28 right 05    
## # ... with 386 more rows
```


```r
get_prior(day | cens(cens) ~ 0 + tempsc, family = exponential, data=germ.stdi)
```

```
##   prior class     coef group resp dpar nlpar bound
## 1           b                                     
## 2           b tempsc05                            
## 3           b tempsc10                            
## 4           b tempsc15                            
## 5           b tempsc20                            
## 6           b tempsc25                            
## 7           b tempsc30                            
## 8           b tempsc35                            
## 9           b tempsc40
```



```r
m1.2 <- brm(day | cens(cens) ~ 0 + tempsc,
            family = exponential(),
            set_prior("normal(0,1)", class="b"),
            data = germ.stdi, )
```

```
## Compiling Stan program...
```

```
## Start sampling
```

```
## 
## SAMPLING FOR MODEL '02cbacdf9a005d8fb72b95a50f9e274d' NOW (CHAIN 1).
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
## Chain 1:  Elapsed Time: 0.927 seconds (Warm-up)
## Chain 1:                0.995 seconds (Sampling)
## Chain 1:                1.922 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '02cbacdf9a005d8fb72b95a50f9e274d' NOW (CHAIN 2).
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
## Chain 2:  Elapsed Time: 0.985 seconds (Warm-up)
## Chain 2:                0.986 seconds (Sampling)
## Chain 2:                1.971 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '02cbacdf9a005d8fb72b95a50f9e274d' NOW (CHAIN 3).
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
## Chain 3:  Elapsed Time: 1.002 seconds (Warm-up)
## Chain 3:                1.005 seconds (Sampling)
## Chain 3:                2.007 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '02cbacdf9a005d8fb72b95a50f9e274d' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0.001 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
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
## Chain 4:  Elapsed Time: 0.986 seconds (Warm-up)
## Chain 4:                0.916 seconds (Sampling)
## Chain 4:                1.902 seconds (Total)
## Chain 4:
```


```r
summary(m1.2)
```

```
##  Family: exponential 
##   Links: mu = log 
## Formula: day | cens(cens) ~ 0 + tempsc 
##    Data: germ.stdi (Number of observations: 396) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup samples = 4000
## 
## Population-Level Effects: 
##          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## tempsc05     5.44      0.37     4.78     6.22 1.00     6660     3165
## tempsc10     4.70      0.29     4.18     5.29 1.00     6514     2943
## tempsc15     4.03      0.22     3.62     4.48 1.00     7399     3137
## tempsc20     2.48      0.16     2.18     2.79 1.00     6421     2849
## tempsc25     3.07      0.17     2.75     3.42 1.00     7660     2817
## tempsc30     3.74      0.19     3.36     4.14 1.00     7497     3024
## tempsc35     5.21      0.35     4.56     5.96 1.00     6695     2836
## tempsc40     5.62      0.39     4.92     6.45 1.00     5912     2542
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

### m1.5: ZI exponential

Try to write a model with different dormancy probs for the different temps.

```r
d <- list(N=nrow(germ.stdi),
          germ=germ.stdi$germ,
          temps=as.numeric(as.factor(germ.stdi$temps)),
          day=germ.stdi$day)
stanmodel1.5 <-
  "
data{
    int<lower=1> N;  // number of observations
    int germ[N];
    vector[N] day;
    int temps[N];
}
parameters{
    vector[8] a; // alpha for the exponential curve, one for each temp
    vector[8] ap; // alpha for the proportion dormant, one for each temp
}
model{
    vector[N] p;
    vector[N] lambda;
    vector[N] mu;
    a ~ normal( 0 , 1 );
    ap ~ normal( 0, 1 );
    for (i in 1:N) {
        p[i] = ap[temps[i]];
        p[i] = inv_logit(p[i]); // inverse link function
    }
    for ( i in 1:N ) {
        mu[i] = a[temps[i]];
        mu[i] = exp(mu[i]); // inverse link function
    }
    for ( i in 1:N ) {
        lambda[i] = 1/mu[i];
    }
    for ( i in 1:N ) 
        if ( germ[i] == 0 ) target += log_mix(p[i], 0, exponential_lccdf(day[i] | lambda[i]));
    for ( i in 1:N ) 
        if ( germ[i] == 1 ) target += log1m(p[i]) +  exponential_lpdf(day[i] | lambda[i]);
    for ( i in 1:N ) 
        if ( germ[i] == 1 ) day[i] ~ exponential( lambda[i] );
}
"
m1.5 <- stan(model_code=stanmodel1.5, data=d)
```

```
## 
## SAMPLING FOR MODEL '9064b37d517551d81d83ff3f0e4fe626' NOW (CHAIN 1).
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
## Chain 1:  Elapsed Time: 3.47 seconds (Warm-up)
## Chain 1:                2.884 seconds (Sampling)
## Chain 1:                6.354 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '9064b37d517551d81d83ff3f0e4fe626' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0.001 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
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
## Chain 2:  Elapsed Time: 3.594 seconds (Warm-up)
## Chain 2:                2.882 seconds (Sampling)
## Chain 2:                6.476 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '9064b37d517551d81d83ff3f0e4fe626' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0.001 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
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
## Chain 3:  Elapsed Time: 3.706 seconds (Warm-up)
## Chain 3:                4.513 seconds (Sampling)
## Chain 3:                8.219 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '9064b37d517551d81d83ff3f0e4fe626' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0.001 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
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
## Chain 4:  Elapsed Time: 4.426 seconds (Warm-up)
## Chain 4:                3.255 seconds (Sampling)
## Chain 4:                7.681 seconds (Total)
## Chain 4:
```


```r
precis(m1.5, depth = 2)
```

```
##             mean        sd       5.5%      94.5%     n_eff     Rhat4
## a[1]   2.7898745 0.5875093  1.9448791  3.8066779  4567.892 1.0001750
## a[2]   1.5293946 0.2631441  1.1298289  1.9685423  8708.865 0.9993337
## a[3]   1.3437965 0.1837119  1.0582902  1.6496333  7724.604 0.9994362
## a[4]   1.0058462 0.1166088  0.8244339  1.1980887  9783.916 0.9992434
## a[5]   1.1572450 0.1353861  0.9406932  1.3782561  8662.496 0.9994267
## a[6]   2.3433414 0.1782864  2.0696810  2.6375083  9337.374 0.9998187
## a[7]   2.4238360 0.4824830  1.7277458  3.2600938  5541.456 1.0006986
## a[8]   0.0123213 1.0368482 -1.6202884  1.6652033  8095.644 0.9995685
## ap[1]  2.4026043 0.5714404  1.4877315  3.2912951  5708.360 1.0002819
## ap[2]  1.6110297 0.3605727  1.0688242  2.2144860  7815.755 0.9996285
## ap[3]  0.7932142 0.2940609  0.3338250  1.2743708  8671.378 0.9996611
## ap[4] -0.9649042 0.3076405 -1.4727753 -0.4925721  9392.686 0.9993312
## ap[5] -0.2994623 0.2742653 -0.7383062  0.1420762 11752.846 0.9994431
## ap[6]  0.1695934 0.3026032 -0.3068404  0.6453503  9006.695 0.9994911
## ap[7]  2.2038830 0.4944290  1.4269446  3.0054120  6956.790 0.9996483
## ap[8]  2.9203917 0.5253023  2.1146524  3.8049876  7486.001 0.9992444
```

### m1.7/m1.7a : ZI Gamma (the best)

Look at gamma stancode from brms:


```r
make_stancode(day ~ tempsc, 
             family="Gamma",
             prior=set_prior("normal(0,1)"),
             data=germ.stdi,
             sample=FALSE)
```

```
## // generated with brms 2.13.5
## functions {
## }
## data {
##   int<lower=1> N;  // number of observations
##   vector[N] Y;  // response variable
##   int<lower=1> K;  // number of population-level effects
##   matrix[N, K] X;  // population-level design matrix
##   int prior_only;  // should the likelihood be ignored?
## }
## transformed data {
##   int Kc = K - 1;
##   matrix[N, Kc] Xc;  // centered version of X without an intercept
##   vector[Kc] means_X;  // column means of X before centering
##   for (i in 2:K) {
##     means_X[i - 1] = mean(X[, i]);
##     Xc[, i - 1] = X[, i] - means_X[i - 1];
##   }
## }
## parameters {
##   vector[Kc] b;  // population-level effects
##   real Intercept;  // temporary intercept for centered predictors
##   real<lower=0> shape;  // shape parameter
## }
## transformed parameters {
## }
## model {
##   // initialize linear predictor term
##   vector[N] mu = Intercept + Xc * b;
##   for (n in 1:N) {
##     // apply the inverse link function
##     mu[n] = shape * exp(-(mu[n]));
##   }
##   // priors including all constants
##   target += normal_lpdf(b | 0,1);
##   target += student_t_lpdf(Intercept | 3, 3.3, 2.5);
##   target += gamma_lpdf(shape | 0.01, 0.01);
##   // likelihood including all constants
##   if (!prior_only) {
##     target += gamma_lpdf(Y | shape, mu);
##   }
## }
## generated quantities {
##   // actual population-level intercept
##   real b_Intercept = Intercept - dot_product(means_X, b);
## }
```


Try developing a ZI gamma model:

stan uses that gamma(alpha, beta) parameterization where "alpha" is shape" and "beta" is "rate".  So try having the shape be the same for all temps.

```r
d <- list(N=nrow(germ.stdi),
          germ=germ.stdi$germ,
          temps=as.numeric(as.factor(germ.stdi$temps)),
          day=germ.stdi$day)
stanmodel1.7 <-
  "
data{
    int<lower=1> N;  // number of observations
    int germ[N];
    vector[N] day;
    int temps[N];
}
parameters{
    real<lower=0> shape; // should set lower bound
    vector[8] a; // alpha for the gamma curve, one for each temp
    vector[8] ap; // alpha for the proportion dormant, one for each temp
}
model{
    vector[N] p;
    vector[N] mu;
    a ~ normal( 0 , .5 ); //narrow priors to overcome divergent transitions
    ap ~ normal( 0, 1.5 ); 
    shape ~ normal(0, .5); //narrow priors to overcome divergent transitions
    for (i in 1:N) {
        p[i] = ap[temps[i]];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:N ) {
        mu[i] = a[temps[i]];
         // apply the inverse link function
        mu[i] = shape * exp(-(mu[i]));
    }
    for ( i in 1:N ) 
       if ( germ[i] == 0 ) target += log_mix(p[i], 0, gamma_lccdf(day[i] | shape, mu[i]));
    for ( i in 1:N ) 
       if ( germ[i] == 1 ) target += log1m(p[i]) + gamma_lpdf(day[i] | shape, mu[i]);
    for ( i in 1:N ) 
       if ( germ[i] == 1 ) day[i] ~ gamma( shape, mu[i] );
}
"
m1.7 <- stan(model_code=stanmodel1.7, data=d, chains=4, cores=4, control=list(adapt_delta=.99))
```



```r
precis(m1.7, depth = 2)
```

```
##                mean         sd       5.5%      94.5%    n_eff     Rhat4
## shape  2.353210e+00 0.20068741  2.0442091  2.6831005 4781.397 1.0000264
## a[1]   2.177421e+00 0.25167148  1.7954632  2.5973947 4920.073 1.0001622
## a[2]   1.436023e+00 0.15392840  1.1950092  1.6941747 4844.108 1.0001233
## a[3]   1.303146e+00 0.11783173  1.1177640  1.4969536 5564.022 0.9996409
## a[4]   9.929521e-01 0.07475170  0.8764939  1.1126569 5779.836 0.9996043
## a[5]   1.135649e+00 0.08258197  1.0062652  1.2697077 5228.105 0.9994393
## a[6]   2.211690e+00 0.09775070  2.0591970  2.3715131 6319.033 0.9993867
## a[7]   2.016053e+00 0.22843079  1.6540017  2.3904713 5397.419 1.0001817
## a[8]  -8.978115e-05 0.50213826 -0.7925012  0.7902565 5816.777 1.0001864
## ap[1]  3.146916e+00 0.65730749  2.1806813  4.2396253 4567.768 0.9998215
## ap[2]  1.747468e+00 0.38054992  1.1561457  2.3811398 5999.173 0.9997274
## ap[3]  8.370440e-01 0.29871046  0.3660776  1.3279054 5690.930 0.9998003
## ap[4] -1.023893e+00 0.32287702 -1.5471290 -0.5385074 4828.043 0.9992669
## ap[5] -3.160689e-01 0.27677625 -0.7623944  0.1214093 5541.410 1.0010999
## ap[6]  2.933310e-01 0.27671015 -0.1428672  0.7385646 4952.784 0.9999935
## ap[7]  2.689618e+00 0.56634946  1.8357740  3.6798437 5499.838 0.9994569
## ap[8]  3.641740e+00 0.76744104  2.5154665  4.9820588 3908.684 0.9996319
```



```r
shinystan::launch_shinystan(m1.7)
```

```
## 
## Launching ShinyStan interface... for large models this  may take some time.
```

```
## Loading required package: shiny
```

```
## 
## Listening on http://127.0.0.1:5176
```

playing with priors:

Make shape exponential

```r
d <- list(N=nrow(germ.stdi),
          germ=germ.stdi$germ,
          temps=as.numeric(as.factor(germ.stdi$temps)),
          day=germ.stdi$day)
stanmodel1.7a <-
  "
data{
    int<lower=1> N;  // number of observations
    int germ[N];
    vector[N] day;
    int temps[N];
}
parameters{
    real<lower=0> shape; // should set lower bound
    vector[8] a; // alpha for the gamma curve, one for each temp
    vector[8] ap; // alpha for the proportion dormant, one for each temp
}
model{
    vector[N] p;
    vector[N] mu;
    a ~ normal( 0 , .5 ); //narrow priors to overcome divergent transitions
    ap ~ normal( 0, 1.5 ); 
    shape ~ exponential(.5); 
    for (i in 1:N) {
        p[i] = ap[temps[i]];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:N ) {
        mu[i] = a[temps[i]];
         // apply the inverse link function
        mu[i] = shape * exp(-(mu[i]));
    }
    for ( i in 1:N ) 
       if ( germ[i] == 0 ) target += log_mix(p[i], 0, gamma_lccdf(day[i] | shape, mu[i]));
    for ( i in 1:N ) 
       if ( germ[i] == 1 ) target += log1m(p[i]) + gamma_lpdf(day[i] | shape, mu[i]);
    for ( i in 1:N ) 
       if ( germ[i] == 1 ) day[i] ~ gamma( shape, mu[i] );
}
"
m1.7a <- stan(model_code=stanmodel1.7a, data=d, chains=4, cores=4, control=list(adapt_delta=.99))
```



```r
precis(m1.7a, depth = 2)
```

```
##               mean         sd       5.5%      94.5%    n_eff     Rhat4
## shape  2.887889254 0.27349925  2.4664189  3.3236179 5367.130 1.0000498
## a[1]   2.278615352 0.23849675  1.9112899  2.6601871 5584.517 0.9999013
## a[2]   1.455577484 0.14119634  1.2342065  1.6848907 6572.584 0.9995436
## a[3]   1.317090293 0.10232128  1.1554069  1.4831579 6885.831 0.9992963
## a[4]   0.998160825 0.06939407  0.8883334  1.1102496 5458.607 1.0004525
## a[5]   1.142795810 0.07601275  1.0239752  1.2662805 6888.607 0.9999769
## a[6]   2.219390358 0.09002561  2.0782811  2.3634331 6104.004 0.9994170
## a[7]   2.085744726 0.21667242  1.7521865  2.4383559 5539.847 0.9997416
## a[8]   0.007793111 0.50416170 -0.7851663  0.8151926 6256.416 0.9995321
## ap[1]  3.141976675 0.65096383  2.1811192  4.2233416 5001.014 0.9995282
## ap[2]  1.747328401 0.39459560  1.1511560  2.4008299 4788.451 0.9995695
## ap[3]  0.831292857 0.29147222  0.3689738  1.3072105 6418.659 0.9993474
## ap[4] -1.014189931 0.32648554 -1.5450392 -0.4972164 6606.337 0.9995942
## ap[5] -0.328150328 0.30376524 -0.8165348  0.1586079 5850.944 0.9992194
## ap[6]  0.302686873 0.28995811 -0.1452210  0.7748388 5306.798 0.9998410
## ap[7]  2.690270419 0.56166607  1.8488740  3.6599969 4384.709 1.0002338
## ap[8]  3.672417472 0.76239872  2.5346251  4.9581408 4826.669 0.9997057
```


#### Choose at least one model from above (or your own) to work from. 
#### From the base model:

### 1) Try using temperature as a continuous predictor.  Perhaps as a quadratic, or perhaps using the equation that Rongkui showed. (Or try both).

### 2) Try incorporating multiple populations.
