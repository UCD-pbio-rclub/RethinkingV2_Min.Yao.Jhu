---
title: "chapter 3"
author: "Min-Yao"
date: "2019年4月12日"
output: 
  html_document: 
    keep_md: yes
---


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

## R code 3.5
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

![](chapter_3_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
dens( samples )
```

![](chapter_3_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

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
## [1] 0.1732
```

```r
## R code 3.8
sum( samples > 0.5 & samples < 0.75 ) / 1e4
```

```
## [1] 0.6025
```

```r
## R code 3.9
quantile( samples , 0.8 )
```

```
##     80% 
## 0.75996
```

```r
## R code 3.10
quantile( samples , c( 0.1 , 0.9 ) )
```

```
##       10%       90% 
## 0.4474474 0.8118118
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
## 0.7057057 0.9309309
```

```r
## R code 3.13
HPDI( samples , prob=0.5 )
```

```
##      |0.5      0.5| 
## 0.8408408 1.0000000
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
## [1] 0.9850705
```

```r
## R code 3.16
mean( samples )
```

```
## [1] 0.7998242
```

```r
median( samples )
```

```
## [1] 0.8408408
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
## [1] 0
```

```r
## R code 3.22
rbinom( 10 , size=2 , prob=0.7 )
```

```
##  [1] 2 2 0 2 2 2 1 2 2 1
```

```r
## R code 3.23
dummy_w <- rbinom( 1e5 , size=2 , prob=0.7 )
table(dummy_w)/1e5
```

```
## dummy_w
##       0       1       2 
## 0.09048 0.41970 0.48982
```

```r
## R code 3.24
dummy_w <- rbinom( 1e5 , size=9 , prob=0.7 )
simplehist( dummy_w , xlab="dummy water count" )
```

![](chapter_3_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
## R code 3.25
w <- rbinom( 1e4 , size=9 , prob=0.6 )

## R code 3.26
w <- rbinom( 1e4 , size=9 , prob=samples )

## R code 3.27
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## R code 3.28
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
0,0,0,1,1,1,0,0,0,0)

## R code 3.29
library(rethinking)
data(homeworkch3)

## R code 3.30
sum(birth1) + sum(birth2)
```

```
## [1] 111
```





## 3.5. Practice

### Easy. Th ese problems use the samples from the posterior distribution for the globe tossing example.Th is code will give you a specifi c set of samples, so that you can check your answers exactly.


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

#### 3E1. How much posterior probability lies below p = 0:2?

#### 3E2. How much posterior probability lies above p = 0:8?

#### 3E3. How much posterior probability lies between p = 0:2 and p = 0:8?

#### 3E4. 20% of the posterior probability lies below which value of p?

#### 3E5. 20% of the posterior probability lies above which value of p?

#### 3E6. Which values of p contain the narrowest interval equal to 66% of the posterior probability?

#### 3E7. Which values of p contain 66% of the posterior probability, assuming equal posterior probability both below and above the interval?


### Medium.

#### 3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior distribution, using grid approximation. Use the same fl at prior as before.

#### 3M2. Draw 10,000 samples from the grid approximation from above. Th en use the samples to calculate the 90% HPDI for p.

#### 3M3. Construct a posterior predictive check for this model and data. Th is means simulate the distribution of samples, averaging over the posterior uncertainty in p. What is the probability of observing 8 water in 15 tosses?

#### 3M4. Using the posterior distribution constructed from the new (8/15) data, now calculate the probability of observing 6 water in 9 tosses.

### Hard.

#### Introduction. Th e practice problems here all use the data below. Th ese data indicate the gender (male=1, female=0) of offi cially reported fi rst and second born children in 100 two-child families.


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

#### So for example, the fi rst family in the data reported a boy (1) and then a girl (0). Th e second family reported a girl (0) and then a boy (1). Th e third family reported two girls. You can load these two vectors into R’s memory by typing:


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

