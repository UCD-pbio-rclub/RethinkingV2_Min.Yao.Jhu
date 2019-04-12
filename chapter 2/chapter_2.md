---
title: "chapter 2"
author: "Min-Yao"
date: "2019¦~4¤ë11¤é"
output: 
  html_document: 
    keep_md: yes
---


```r
library("rstan") # observe startup messages
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

```r
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
```

# Chapter 0

## R code 0.1

```r
print( "All models are wrong, but some are useful." )
```

```
## [1] "All models are wrong, but some are useful."
```

## R code 0.2

```r
x <- 1:2
x
```

```
## [1] 1 2
```

```r
x <- x*10
x
```

```
## [1] 10 20
```

```r
x <- log(x)
x
```

```
## [1] 2.302585 2.995732
```

```r
x <- sum(x)
x
```

```
## [1] 5.298317
```

```r
x <- exp(x)
x
```

```
## [1] 200
```

## R code 0.3

```r
( log( 0.01^200 ) )
```

```
## [1] -Inf
```

```r
( 200 * log(0.01) )
```

```
## [1] -921.034
```

## R code 0.4
#### Load the data:
#### car braking distances in feet paired with speeds in km/h
#### see ?cars for details

```r
data(cars)
```


#### fit a linear regression of distance on speed

```r
m <- lm( dist ~ speed , data=cars )
```

#### estimated coefficients from the model

```r
coef(m)
```

```
## (Intercept)       speed 
##  -17.579095    3.932409
```


#### plot residuals against speed

```r
plot( resid(m) ~ speed , data=cars )
```

![](chapter_2_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


## R code 0.5

```r
#install.packages(c("coda","mvtnorm","devtools"))
library(devtools)
#devtools::install_github("rmcelreath/rethinking",ref="Experimental")
```

# Chapter 2

## R code 2.1


```r
ways <- c( 0 , 3 , 8 , 9 , 0 )
ways/sum(ways)
```

```
## [1] 0.00 0.15 0.40 0.45 0.00
```

## R code 2.2

```r
dbinom( 6 , size=9 , prob=0.5 )
```

```
## [1] 0.1640625
```


## R code 2.3
#### define grid

```r
p_grid <- seq( from=0 , to=1 , length.out=20 )
```


#### define prior

```r
prior <- rep( 1 , 20 )
```


#### compute likelihood at each value in grid

```r
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
```

#### compute product of likelihood and prior

```r
unstd.posterior <- likelihood * prior
```

#### standardize the posterior, so it sums to 1

```r
posterior <- unstd.posterior / sum(unstd.posterior)
```

## R code 2.4

```r
plot( p_grid , posterior , type="b" ,
    xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```

![](chapter_2_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

## R code 2.5

```r
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
prior <- exp( -5*abs( p_grid - 0.5 ) )
```

## R code 2.6

```r
library(rethinking)
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.88)
```

```r
library(parallel)
globe.qa <- quap(
    alist(
        W ~ dbinom( W+L ,p) ,  # binomial likelihood
        p ~ dunif(0,1)     # uniform prior
    ) ,
    data=list(W=6,L=3) )
```

# display summary of quadratic approximation

```r
precis( globe.qa )
```

```
##        mean        sd     5.5%     94.5%
## p 0.6666518 0.1571373 0.415516 0.9177875
```


## R code 2.7


```r
# analytical calculation
W <- 6
L <- 3
curve( dbeta( x , W+1 , L+1 ) , from=0 , to=1 )

# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )
```

![](chapter_2_files/figure-html/unnamed-chunk-21-1.png)<!-- -->


## R code 2.8

```r
n_samples <- 1000
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
    p_new <- rnorm( 1 , p[i-1] , 0.1 )
    if ( p_new < 0 ) p_new <- abs( p_new )
    if ( p_new > 1 ) p_new <- 2 - p_new
    q0 <- dbinom( W , W+L , p[i-1] )
    q1 <- dbinom( W , W+L , p_new )
    p[i] <- ifelse( runif(1) < q1/q0 , p_new , p[i-1] )
}
```

## R code 2.9

```r
dens( p , xlim=c(0,1) )
curve( dbeta( x , W+1 , L+1 ) , lty=2 , add=TRUE )
```

![](chapter_2_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

### 2E1 Which of the expressions below correspond to the statement: the probability of rain on Monday?

(1) Pr(rain)


### 2E2

(3) Th e probability that it is Monday, given that it is raining.


### 2E3

(1) Pr(Mondayjrain)
(4) Pr(rainjMonday) Pr(Monday)= Pr(rain)


### 2M1



### 2M2

### 2M3

### 2M4

### 2M5





