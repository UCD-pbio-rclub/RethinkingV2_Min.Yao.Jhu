---
title: "chapter 3"
author: "Min-Yao"
date: "2019年4月12日"
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
## Warning: package 'rstan' was built under R version 3.5.3
```

```
## Loading required package: ggplot2
```

```
## Loading required package: StanHeaders
```

```
## Warning: package 'StanHeaders' was built under R version 3.5.3
```

```
## rstan (Version 2.18.2, GitRev: 2e1f913d3ca3)
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
## rethinking (Version 1.88)
```

# Sampling the Imaginary


```r
## R code 3.1
Pr_Positive_Vampire <- 0.95
Pr_Positive_Mortal <- 0.01
Pr_Vampire <- 0.001
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire +
               Pr_Positive_Mortal * ( 1 - Pr_Vampire )
( Pr_Vampire_Positive <- Pr_Positive_Vampire*Pr_Vampire / Pr_Positive )
```

```
## [1] 0.08683729
```

## 3.1. Sampling from a grid-approximate posterior


```r
## R code 3.2
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## R code 3.3
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## R code 3.4
plot( samples )
```

![](chapter_3_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## R code 3.5
library(rethinking)
dens( samples )
```

![](chapter_3_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

## 3.2. Sampling to summarize


```r
## R code 3.6
# add up posterior probability where p < 0.5
sum( posterior[ p_grid < 0.5 ] )
```

```
## [1] 0.1718746
```

```r
## R code 3.7
sum( samples < 0.5 ) / 1e4
```

```
## [1] 0.1704
```

```r
## R code 3.8
sum( samples > 0.5 & samples < 0.75 ) / 1e4
```

```
## [1] 0.6071
```

```r
## R code 3.9
quantile( samples , 0.8 )
```

```
##       80% 
## 0.7607608
```

```r
## R code 3.10
quantile( samples , c( 0.1 , 0.9 ) )
```

```
##       10%       90% 
## 0.4494494 0.8098098
```


```r
## R code 3.11
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

## R code 3.12
PI( samples , prob=0.5 )
```

```
##       25%       75% 
## 0.7017017 0.9299299
```

```r
## R code 3.13
HPDI( samples , prob=0.5 )
```

```
##      |0.5      0.5| 
## 0.8368368 0.9979980
```


```r
## R code 3.14
p_grid[ which.max(posterior) ]
```

```
## [1] 1
```

```r
## R code 3.15
chainmode( samples , adj=0.01 )
```

```
## [1] 0.986389
```

```r
## R code 3.16
mean( samples )
```

```
## [1] 0.7979829
```

```r
median( samples )
```

```
## [1] 0.8388388
```

```r
## R code 3.17
sum( posterior*abs( 0.5 - p_grid ) )
```

```
## [1] 0.3128752
```

```r
## R code 3.18
loss <- sapply( p_grid , function(d) sum( posterior*abs( d - p_grid ) ) )

## R code 3.19
p_grid[ which.min(loss) ]
```

```
## [1] 0.8408408
```



```r
## R code 3.20
dbinom( 0:2 , size=2 , prob=0.7 )
```

```
## [1] 0.09 0.42 0.49
```

```r
## R code 3.21
rbinom( 1 , size=2 , prob=0.7 )
```

```
## [1] 2
```

```r
## R code 3.22
rbinom( 10 , size=2 , prob=0.7 )
```

```
##  [1] 1 2 0 1 1 0 1 1 1 2
```

```r
## R code 3.23
dummy_w <- rbinom( 1e5 , size=2 , prob=0.7 )
table(dummy_w)/1e5
```

```
## dummy_w
##       0       1       2 
## 0.09171 0.42086 0.48743
```

```r
## R code 3.24
dummy_w <- rbinom( 1e5 , size=9 , prob=0.7 )
simplehist( dummy_w , xlab="dummy water count" )
```

![](chapter_3_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
## R code 3.25
w <- rbinom( 1e4 , size=9 , prob=0.6 )
simplehist(w)
```

![](chapter_3_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
## R code 3.26
w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist(w)
```

![](chapter_3_files/figure-html/unnamed-chunk-8-2.png)<!-- -->





## 3.5. Practice

### Easy. Th ese problems use the samples from the posterior distribution for the globe tossing example.This code will give you a specific set of samples, so that you can check your answers exactly.


```r
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
```

#### Use the values in samples to answer the questions that follow.

#### 3E1. How much posterior probability lies below p = 0.2?


```r
sum(samples < 0.2) / 1e4
```

```
## [1] 5e-04
```

#### 3E2. How much posterior probability lies above p = 0.8?


```r
sum(samples > 0.8) / 1e4
```

```
## [1] 0.1117
```

#### 3E3. How much posterior probability lies between p = 0:2 and p = 0:8?


```r
sum(samples > 0.2 & samples < 0.8) / 1e4
```

```
## [1] 0.8878
```


#### 3E4. 20% of the posterior probability lies below which value of p?


```r
quantile( samples , 0.2 )
```

```
##       20% 
## 0.5195195
```

#### 3E5. 20% of the posterior probability lies above which value of p?


```r
quantile( samples , 0.8 )
```

```
##       80% 
## 0.7567568
```

#### 3E6. Which values of p contain the narrowest interval equal to 66% of the posterior probability?

```r
HPDI( samples , prob=0.66 )
```

```
##     |0.66     0.66| 
## 0.5205205 0.7847848
```
> 0.52 < p < 0.78

#### 3E7. Which values of p contain 66% of the posterior probability, assuming equal posterior probability both below and above the interval?

```r
PI( samples , prob = 0.66 )
```

```
##       17%       83% 
## 0.5005005 0.7687688
```
> 0.50 < p < 0.77

### Medium.

#### 3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior distribution, using grid approximation. Use the same flat prior as before.

```r
# define grid
p_grid <- seq( from=0 , to=1 , length.out=100 )

# define prior
prior <- rep( 1 , 100 )

# compute likelihood at each value in grid
likelihood <- dbinom( 8 , size=15 , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot_posterior <- function(x, y) {
  plot(x = x, y = y, type="b", xlab = "Probability of Water", ylab = "Posterior Probability")
  title <- paste( length(x), "Points")
  mtext(title)
}
plot_posterior(x = p_grid, y = posterior)
```

![](chapter_3_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

#### 3M2. Draw 10,000 samples from the grid approximation from above. Th en use the samples to calculate the 90% HPDI for p.

```r
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
HPDI( samples , prob=0.9 )
```

```
##      |0.9      0.9| 
## 0.3535354 0.7272727
```
> 0.34<p<0.73

#### 3M3. Construct a posterior predictive check for this model and data. This means simulate the distribution of samples, averaging over the posterior uncertainty in p. What is the probability of observing 8 water in 15 tosses?

```r
w <- rbinom( 1e4 , size=15 , prob=samples )
simplehist(w)
```

![](chapter_3_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
sum(w == 8)/1e4
```

```
## [1] 0.1445
```
> 14.5% is the probability of observing 8 water in 15 tosses

#### 3M4. Using the posterior distribution constructed from the new (8/15) data, now calculate the probability of observing 6 water in 9 tosses.

```r
w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist(w)
```

![](chapter_3_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
sum(w == 6)/1e4
```

```
## [1] 0.1733
```
> 17.64% is the probability of observing 6 water in 9 tosses.

### Hard.

#### Introduction. The practice problems here all use the data below. These data indicate the gender (male=1, female=0) of officially reported first and second born children in 100 two-child families.


```r
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
0,0,0,1,1,1,0,0,0,0)
```

#### So for example, the first family in the data reported a boy (1) and then a girl (0). Th e second family reported a girl (0) and then a boy (1). Th e third family reported two girls. You can load these two vectors into R’s memory by typing:


```r
library(rethinking)
data(homeworkch3)
```

#### Use these vectors as data. So for example to compute the total number of boys born across all of these births, you could use:


```r
sum(birth1) + sum(birth2)
```

```
## [1] 111
```

#### 3H1. Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?


#### 3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.


#### 3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 numbers, each one a count of boys out of 200 births. Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births). Th ere are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case. Does it look like the model fi ts the data well? Th at is, does the distribution of predictions include the actual observation as a central, likely outcome?


#### 3H4. Now compare 10,000 counts of boys from 100 simulated fi rst borns only to the number of boys in the fi rst births, birth1. How does the model look in this light?


#### 3H5. Th e model assumes that sex of fi rst and second births are independent. To check this assumption, focus now on second births that followed female fi rst borns. Compare 10,000 simulated counts of boys to only those second births that followed girls. To do this correctly, you need to count the number of fi rst borns who were girls and simulate that many births, 10,000 times. Compare the counts of boys in your simulations to the actual observed count of boys following girls. How does the model look in this light? Any guesses what is going on in these data?



