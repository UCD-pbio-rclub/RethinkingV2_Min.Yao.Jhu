---
title: "chapter15-1"
author: "Min-Yao"
date: "2020/7/12"
output: 
  html_document: 
    keep_md: yes
---


```r
## R code 15.1
# simulate a pancake and return randomly ordered sides
sim_pancake <- function() {
    pancake <- sample(1:3,1)
    sides <- matrix(c(1,1,1,0,0,0),2,3)[,pancake]
    sample(sides)
}

# sim 10,000 pancakes
pancakes <- replicate( 1e4 , sim_pancake() )
up <- pancakes[1,]
down <- pancakes[2,]

# compute proportion 1/1 (BB) out of all 1/1 and 1/0
num_11_10 <- sum( up==1 )
num_11 <- sum( up==1 & down==1 )
num_11/num_11_10
```

```
## [1] 0.6623351
```

## 15.1. Measurement error

```r
## R code 15.2
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
## rstan (Version 2.19.3, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## For improved execution time, we recommend calling
## Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
## although this causes Stan to throw an error on a few processors.
```

```
## Loading required package: parallel
```

```
## Loading required package: dagitty
```

```
## rethinking (Version 2.01)
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
data(WaffleDivorce)
d <- WaffleDivorce

# points
plot( d$Divorce ~ d$MedianAgeMarriage , ylim=c(4,15) ,
    xlab="Median age marriage" , ylab="Divorce rate" )

# standard errors
for ( i in 1:nrow(d) ) {
    ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
    x <- d$MedianAgeMarriage[i]
    lines( c(x,x) , ci )
}
```

![](chapter15-1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### 15.1.1. Error on the outcome.

```r
## R code 15.3
dlist <- list(
    D_obs = standardize( d$Divorce ),
    D_sd = d$Divorce.SE / sd( d$Divorce ),
    M = standardize( d$Marriage ),
    A = standardize( d$MedianAgeMarriage ),
    N = nrow(d)
)

m15.1 <- ulam(
    alist(
        D_obs ~ dnorm( D_true , D_sd ),
        vector[N]:D_true ~ dnorm( mu , sigma ),
        mu <- a + bA*A + bM*M,
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        bM ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=dlist , chains=4 , cores=4 )

## R code 15.4
precis( m15.1 , depth=2 )
```

```
##                   mean         sd        5.5%        94.5%     n_eff     Rhat4
## D_true[1]   1.17208322 0.36739351  0.59759566  1.759797988 2556.5060 0.9998244
## D_true[2]   0.68548170 0.54906867 -0.16428478  1.578064180 2844.5988 0.9997248
## D_true[3]   0.42591645 0.34611617 -0.13243117  0.986429463 3241.9615 1.0007665
## D_true[4]   1.42273239 0.45978027  0.70496214  2.156133271 1698.2923 1.0019802
## D_true[5]  -0.90078478 0.12842773 -1.10534334 -0.700322610 2948.5643 0.9997399
## D_true[6]   0.66246352 0.39831577  0.03959602  1.307494301 2386.8667 1.0001046
## D_true[7]  -1.37674468 0.34728789 -1.92308338 -0.823342592 2996.5256 0.9990869
## D_true[8]  -0.35426010 0.47894486 -1.12597328  0.397316835 2403.2967 0.9995526
## D_true[9]  -1.90122298 0.56914779 -2.81691074 -0.961703501 1554.5526 0.9992997
## D_true[10] -0.61680233 0.17424383 -0.90319666 -0.342104258 3035.7250 0.9983337
## D_true[11]  0.77499855 0.29264764  0.30709288  1.240139678 2742.5584 0.9998273
## D_true[12] -0.54762279 0.47600395 -1.30574610  0.191803310 1869.5709 0.9998124
## D_true[13]  0.16162307 0.49257275 -0.61680925  0.926181017 1326.8773 1.0030589
## D_true[14] -0.87155377 0.22549738 -1.23233138 -0.510188480 3433.3135 0.9995453
## D_true[15]  0.55305392 0.31025132  0.06844027  1.042418589 2631.5689 0.9999440
## D_true[16]  0.28896411 0.39408838 -0.35807791  0.921179117 2871.5617 0.9997493
## D_true[17]  0.49530387 0.43869861 -0.19681591  1.172681986 2982.5923 0.9987905
## D_true[18]  1.26347126 0.35987785  0.70048377  1.850638007 2575.2880 0.9994888
## D_true[19]  0.43756383 0.37749256 -0.14998890  1.053579312 2823.9615 0.9984569
## D_true[20]  0.40570473 0.52962519 -0.39802906  1.291804693 1875.1516 0.9999326
## D_true[21] -0.54975225 0.32074319 -1.05150634 -0.040497757 3509.5436 0.9985214
## D_true[22] -1.08802682 0.26685575 -1.51585965 -0.652100716 2594.9415 0.9987378
## D_true[23] -0.27455209 0.26905117 -0.70660641  0.175740839 3159.9768 0.9986168
## D_true[24] -0.99964683 0.30053244 -1.48668947 -0.533177888 2754.2226 0.9990893
## D_true[25]  0.43298741 0.40351489 -0.18899354  1.079650657 2885.1627 0.9997284
## D_true[26] -0.01698713 0.30355466 -0.51895519  0.466880459 3332.6765 1.0000561
## D_true[27] -0.01939452 0.53902431 -0.90243721  0.820099463 3802.0655 1.0008309
## D_true[28] -0.15824334 0.39052931 -0.78542933  0.444210049 2341.6368 0.9998566
## D_true[29] -0.24745131 0.50578325 -1.01969771  0.577452373 2617.4053 0.9981144
## D_true[30] -1.80753683 0.23043358 -2.18042670 -1.449833487 2090.4033 1.0000993
## D_true[31]  0.17094844 0.40627721 -0.45508833  0.826407117 2927.7869 1.0009093
## D_true[32] -1.66227861 0.16865695 -1.92849443 -1.395109828 3067.8831 0.9989708
## D_true[33]  0.11893095 0.23756637 -0.27557273  0.499472781 4084.3632 0.9993516
## D_true[34] -0.08082964 0.54278898 -0.95748970  0.747962799 2085.4987 0.9986062
## D_true[35] -0.12258595 0.21753322 -0.47077588  0.226499521 2433.9316 0.9993687
## D_true[36]  1.28152622 0.40378354  0.67116046  1.923359888 2459.1098 0.9988293
## D_true[37]  0.22692244 0.35243759 -0.32697638  0.803972316 2943.6434 0.9998206
## D_true[38] -1.02259973 0.21972496 -1.36679879 -0.679682660 3287.7446 0.9989808
## D_true[39] -0.91657308 0.57008660 -1.82214992 -0.021168908 2502.5021 0.9992281
## D_true[40] -0.67911415 0.31382715 -1.18633582 -0.188453850 2888.5900 0.9986884
## D_true[41]  0.26186555 0.56182268 -0.64414593  1.162971610 2871.8937 0.9991247
## D_true[42]  0.74738957 0.34692549  0.20856128  1.295213289 2649.4478 0.9992375
## D_true[43]  0.19038291 0.17388142 -0.08273207  0.458792671 3687.2045 0.9988055
## D_true[44]  0.78432425 0.44430078  0.05934907  1.491242162 2185.8337 0.9995667
## D_true[45] -0.41267375 0.52603612 -1.25195187  0.427159334 2568.2319 0.9993085
## D_true[46] -0.38919179 0.24816933 -0.78316668  0.007661327 3765.6032 0.9992978
## D_true[47]  0.13020654 0.31330546 -0.37443624  0.629826194 2502.1053 0.9994645
## D_true[48]  0.55608944 0.48486769 -0.20703999  1.336218614 2296.4395 0.9988348
## D_true[49] -0.63765149 0.26587728 -1.08107628 -0.214896982 2823.1352 0.9994118
## D_true[50]  0.85308017 0.59996253 -0.11256014  1.802021242 1826.3385 0.9993055
## a          -0.05405237 0.09514588 -0.20575272  0.100709101 1667.1502 1.0005873
## bA         -0.61568604 0.16000884 -0.86941516 -0.351271499  905.9842 1.0022912
## bM          0.05086642 0.17555479 -0.22550786  0.325358193  918.6388 1.0021573
## sigma       0.59060433 0.10725350  0.43551339  0.770191964  734.4818 1.0095779
```

### 15.1.2. Error on both outcome and predictor.

```r
## R code 15.5
dlist <- list(
    D_obs = standardize( d$Divorce ),
    D_sd = d$Divorce.SE / sd( d$Divorce ),
    M_obs = standardize( d$Marriage ),
    M_sd = d$Marriage.SE / sd( d$Marriage ),
    A = standardize( d$MedianAgeMarriage ),
    N = nrow(d)
)

m15.2 <- ulam(
    alist(
        D_obs ~ dnorm( D_est , D_sd ),
        vector[N]:D_est ~ dnorm( mu , sigma ),
        mu <- a + bA*A + bM*M_est[i],
        M_obs ~ dnorm( M_est , M_sd ),
        vector[N]:M_est ~ dnorm( 0 , 1 ),
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        bM ~ dnorm(0,0.5),
        sigma ~ dexp( 1 )
    ) , data=dlist , chains=4 , cores=4 )

## R code 15.6
post <- extract.samples( m15.2 )
D_est <- apply( post$D_est , 2 , mean )
M_est <- apply( post$M_est , 2 , mean )
plot( dlist$M_obs , dlist$D_obs , pch=16 , col=rangi2 ,
    xlab="marriage rate (std)" , ylab="divorce rate (std)" )
points( M_est , D_est )
for ( i in 1:nrow(d) )
    lines( c( dlist$M_obs[i] , M_est[i] ) , c( dlist$D_obs[i] , D_est[i] ) )
```

![](chapter15-1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### 15.1.3. Measurement terrors.

```r
## R code 15.7
N <- 500
A <- rnorm(N)
M <- rnorm(N,-A)
D <- rnorm(N,A)
A_obs <- rnorm(N,A)
```

## 15.2. Missing data
### 15.2.1. DAG ate my homework.

```r
## R code 15.8
N <- 100
S <- rnorm( N )
H <- rbinom( N , size=10 , inv_logit(S) )

## R code 15.9
D <- rbern( N ) # dogs completely random
Hm <- H
Hm[D==1] <- NA

## R code 15.10
D <- ifelse( S > 0 , 1 , 0 )
Hm <- H
Hm[D==1] <- NA

## R code 15.11
set.seed(501)
N <- 1000
X <- rnorm(N)
S <- rnorm(N)
H <- rbinom( N , size=10 , inv_logit( 2 + S - 2*X ) )
D <- ifelse( X > 1 , 1 , 0 )
Hm <- H
Hm[D==1] <- NA

## R code 15.12
dat_list <- list(
    H = H,
    S = S )

m15.3 <- ulam(
    alist(
        H ~ binomial( 10 , p ),
        logit(p) <- a + bS*S,
        a ~ normal( 0 , 1 ),
        bS ~ normal( 0 , 0.5 )
    ), data=dat_list , chains=4 )
```

```
## 
## SAMPLING FOR MODEL '23aa98f3ed8699c2ba8ae951caa2fdf8' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 1.055 seconds (Warm-up)
## Chain 1:                1.077 seconds (Sampling)
## Chain 1:                2.132 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '23aa98f3ed8699c2ba8ae951caa2fdf8' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 1.126 seconds (Warm-up)
## Chain 2:                1.032 seconds (Sampling)
## Chain 2:                2.158 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '23aa98f3ed8699c2ba8ae951caa2fdf8' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0.001 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 3: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 3: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 3: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 3: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 3: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 3: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 3: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 3: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 3: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 3: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 3: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 1.164 seconds (Warm-up)
## Chain 3:                1.174 seconds (Sampling)
## Chain 3:                2.338 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '23aa98f3ed8699c2ba8ae951caa2fdf8' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 4: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 4: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 4: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 4: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 4: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 4: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 4: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 4: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 4: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 4: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 4: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 1.275 seconds (Warm-up)
## Chain 4:                1.101 seconds (Sampling)
## Chain 4:                2.376 seconds (Total)
## Chain 4:
```

```r
precis( m15.3 )
```

```
##         mean         sd      5.5%     94.5%     n_eff    Rhat4
## a  1.1138049 0.02579002 1.0732201 1.1554344  940.5712 1.001608
## bS 0.6895852 0.02603040 0.6484871 0.7301994 1035.3165 1.003775
```


```r
## R code 15.13
dat_list0 <- list(
    H = H[D==0],
    S = S[D==0] )

m15.4 <- ulam(
    alist(
        H ~ binomial( 10 , p ),
        logit(p) <- a + bS*S,
        a ~ normal( 0 , 1 ),
        bS ~ normal( 0 , 0.5 )
    ), data=dat_list0 , chains=4 )
```

```
## 
## SAMPLING FOR MODEL 'b0ffcbbcd522526a2407e59b1b1b4f2c' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 1.024 seconds (Warm-up)
## Chain 1:                1.018 seconds (Sampling)
## Chain 1:                2.042 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL 'b0ffcbbcd522526a2407e59b1b1b4f2c' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 0 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 1.168 seconds (Warm-up)
## Chain 2:                0.91 seconds (Sampling)
## Chain 2:                2.078 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL 'b0ffcbbcd522526a2407e59b1b1b4f2c' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 0 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 3: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 3: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 3: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 3: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 3: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 3: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 3: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 3: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 3: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 3: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 3: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.973 seconds (Warm-up)
## Chain 3:                0.913 seconds (Sampling)
## Chain 3:                1.886 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL 'b0ffcbbcd522526a2407e59b1b1b4f2c' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 0 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 4: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 4: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 4: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 4: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 4: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 4: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 4: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 4: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 4: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 4: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 4: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 1.024 seconds (Warm-up)
## Chain 4:                1.045 seconds (Sampling)
## Chain 4:                2.069 seconds (Total)
## Chain 4:
```

```r
precis( m15.4 )
```

```
##         mean         sd      5.5%     94.5%     n_eff    Rhat4
## a  1.7959583 0.03507498 1.7393783 1.8524947 1034.9354 1.000515
## bS 0.8286333 0.03541430 0.7738497 0.8862532  993.2018 1.001287
```


```r
## R code 15.14
D <- ifelse( abs(X) < 1 , 1 , 0 )

## R code 15.15
N <- 100
S <- rnorm(N)
H <- rbinom( N , size=10 , inv_logit(S) )
D <- ifelse( H < 5 , 1 , 0 )
Hm <- H
Hm[D==1] <- NA
```

### 15.2.2. Imputing primates.

```r
## R code 15.16
library(rethinking)
data(milk)
d <- milk
d$neocortex.prop <- d$neocortex.perc / 100
d$logmass <- log(d$mass)

## R code 15.17
dat_list <- list(
    K = standardize( d$kcal.per.g ),
    B = standardize( d$neocortex.prop ),
    M = standardize( d$logmass )
)

m15.3 <- ulam(
    alist(
        K ~ dnorm( mu , sigma ),
        mu <- a + bB*B + bM*M,
        B ~ dnorm( nu , sigma_B ),
        c(a,nu) ~ dnorm( 0 , 0.5 ),
        c(bB,bM) ~ dnorm( 0, 0.5 ),
        sigma_B ~ dexp( 1 ),
        sigma ~ dexp( 1 )
    ) , data=dat_list , chains=4 , cores=4 )
```

```
## Found 12 NA values in B and attempting imputation.
```

```r
## R code 15.18
precis( m15.3 , depth=2 )
```

```
##                     mean        sd        5.5%      94.5%    n_eff     Rhat4
## nu           -0.04368485 0.2089562 -0.36766534  0.2858699 1736.483 1.0016063
## a             0.03054252 0.1612742 -0.22493887  0.2810272 2200.639 0.9993779
## bM           -0.53716048 0.2056274 -0.85772693 -0.2019582 1085.239 1.0031449
## bB            0.49293141 0.2410612  0.08981309  0.8545625  725.841 1.0062578
## sigma_B       1.01386339 0.1713830  0.78661136  1.3069266 1292.821 1.0020987
## sigma         0.84615472 0.1481048  0.63940410  1.1056416 1047.627 1.0036267
## B_impute[1]  -0.57191935 0.9034350 -1.93551961  0.9593654 2048.685 1.0000945
## B_impute[2]  -0.70703639 0.9760477 -2.20638421  0.8824175 1699.314 1.0049426
## B_impute[3]  -0.71358403 1.0013145 -2.26369832  0.8543068 2290.988 1.0029735
## B_impute[4]  -0.29132332 0.8748787 -1.66288168  1.0581652 2437.105 1.0010534
## B_impute[5]   0.46361239 0.9093612 -0.94440294  1.8446707 2384.664 0.9996933
## B_impute[6]  -0.16926372 0.8931844 -1.56905745  1.3089676 2608.736 0.9996702
## B_impute[7]   0.18554776 0.8723654 -1.23276997  1.5174643 2568.280 0.9991635
## B_impute[8]   0.27195223 0.8840276 -1.13556458  1.6761046 2960.539 0.9988263
## B_impute[9]   0.52730809 0.9266440 -0.92136965  1.9646207 2332.782 1.0003682
## B_impute[10] -0.41434132 0.9099458 -1.85496634  0.9849306 2342.857 0.9998743
## B_impute[11] -0.29847972 0.8972953 -1.73847012  1.1018060 2211.445 0.9999470
## B_impute[12]  0.13988787 0.9272784 -1.34981496  1.5647464 2765.257 0.9995123
```


```r
## R code 15.19
obs_idx <- which( !is.na(d$neocortex.prop) )
dat_list_obs <- list(
    K = dat_list$K[obs_idx],
    B = dat_list$B[obs_idx],
    M = dat_list$M[obs_idx]
)
m15.4 <- ulam(
    alist(
        K ~ dnorm( mu , sigma ),
        mu <- a + bB*B + bM*M,
        B ~ dnorm( nu , sigma_B ),
        c(a,nu) ~ dnorm( 0 , 0.5 ),
        c(bB,bM) ~ dnorm( 0, 0.5 ),
        sigma_B ~ dexp( 1 ),
        sigma ~ dexp( 1 )
    ) , data=dat_list_obs , chains=4 , cores=4 )
precis( m15.4 )
```

```
##                 mean        sd       5.5%      94.5%    n_eff     Rhat4
## nu      -0.002082801 0.2218455 -0.3641984  0.3451365 2024.998 0.9986361
## a        0.100267709 0.1972432 -0.2172196  0.4177692 1362.972 1.0019343
## bM      -0.626927522 0.2574989 -1.0259886 -0.2001869 1274.660 0.9999130
## bB       0.590589753 0.2825382  0.1342167  1.0329160 1319.317 0.9995301
## sigma_B  1.036199499 0.1869313  0.7822177  1.3594704 1309.046 0.9993124
## sigma    0.884059742 0.1897155  0.6366597  1.2190512 1322.718 1.0002338
```

```r
## R code 15.20
plot( coeftab(m15.3,m15.4) , pars=c("bB","bM") )
```

![](chapter15-1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
## R code 15.21
post <- extract.samples( m15.3 )
B_impute_mu <- apply( post$B_impute , 2 , mean )
B_impute_ci <- apply( post$B_impute , 2 , PI )

# B vs K
plot( dat_list$B , dat_list$K , pch=16 , col=rangi2 ,
    xlab="neocortex percent (std)" , ylab="kcal milk (std)" )
miss_idx <- which( is.na(dat_list$B) )
Ki <- dat_list$K[miss_idx]
points( B_impute_mu , Ki )
for ( i in 1:12 ) lines( B_impute_ci[,i] , rep(Ki[i],2) )
```

![](chapter15-1_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
# M vs B
plot( dat_list$M , dat_list$B , pch=16 , col=rangi2 ,
    ylab="neocortex percent (std)" , xlab="log body mass (std)" )
Mi <- dat_list$M[miss_idx]
points( Mi , B_impute_mu )
for ( i in 1:12 ) lines( rep(Mi[i],2) , B_impute_ci[,i] )
```

![](chapter15-1_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

```r
## R code 15.22
m15.5 <- ulam(
    alist(
       # K as function of B and M
        K ~ dnorm( mu , sigma ),
        mu <- a + bB*B_merge + bM*M,

       # M and B correlation
        MB ~ multi_normal( c(muM,muB) , Rho_BM , Sigma_BM ),
        matrix[29,2]:MB <<- append_col( M , B_merge ),

       # define B_merge as mix of observed and imputed values
        vector[29]:B_merge <- merge_missing( B , B_impute ),

       # priors
        c(a,muB,muM) ~ dnorm( 0 , 0.5 ),
        c(bB,bM) ~ dnorm( 0, 0.5 ),
        sigma ~ dexp( 1 ),
        Rho_BM ~ lkj_corr(2),
        Sigma_BM ~ exponential(1)
    ) , data=dat_list , chains=4 , cores=4 )
```

```
## Warning: The largest R-hat is NA, indicating chains have not mixed.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#r-hat
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```

```r
precis( m15.5 , depth=3 , pars=c("bM","bB","Rho_BM" ) )
```

```
##                   mean           sd       5.5%      94.5%     n_eff     Rhat4
## bM          -0.6476228 2.178477e-01 -0.9879225 -0.3009558 1250.8742 0.9993527
## bB           0.5953814 2.495106e-01  0.2019971  0.9862815 1088.0474 0.9992318
## Rho_BM[1,1]  1.0000000 0.000000e+00  1.0000000  1.0000000       NaN       NaN
## Rho_BM[1,2]  0.6075401 1.342893e-01  0.3592014  0.7903052 1655.9751 0.9996355
## Rho_BM[2,1]  0.6075401 1.342893e-01  0.3592014  0.7903052 1655.9751 0.9996355
## Rho_BM[2,2]  1.0000000 7.010242e-17  1.0000000  1.0000000  519.8581 0.9979980
```

```r
## R code 15.23
B_missidx <- which( is.na( dat_list$B ) )
```

## 15.5. Practice

### 15E1.Rewrite the Oceanic tools model (from Chapter 11) below so that it assumes measured error on the log population sizes of each society.
Ti ∼ Poisson(μi)
log μi = α + β log Pi
α ∼ Normal(0, 10)
β ∼ Normal(0, 1)

> new model

Ti ∼ Poisson(μi)
log μi = α + β log P_true_i
log P_observed_i ~ Normal(log P_true_i, log P_se_i)
α ∼ Normal(0, 10)
β ∼ Normal(0, 1)


### 15E2. Rewrite the same model so that it allows imputation of missing values for log population. There aren’t any missing values in the variable, but you can still write down a model formula that would imply imputation, if any values were missing.
Ti ∼ Poisson(μi)
log μi = α + β log Pi
α ∼ Normal(0, 10)
β ∼ Normal(0, 1)

> new model. The simplest model will simply impute log Pi from its own normal distribution.

Ti ∼ Poisson(μi)
log μi = α + β log Pi
log Pi ~ Normal(ν, σ_logP)
α ∼ Normal(0, 10)
β ∼ Normal(0, 1)
ν ∼ Normal(0.5, 1)
σ_logP ∼ Exponential(1)

### 15M2. In earlier chapters, we threw away cases from the primate milk data, so we could use the neocortex variable. Now repeat the WAIC model comparison example from Chapter 6, but use imputation on the neocortex variable so that you can include all of the cases in the original data. The simplest form of imputation is acceptable. How are the model comparison results affected by being able to include all of the cases?

> he kind of did 15M2 in the chapter.  Sub 15M3 for 15M2


### 15M3. Repeat the divorce data measurement error models, but this time double the standard errors. Can you explain how doubling the standard errors impacts inference?


```r
## R code 15.2
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
```


```r
## R code 15.5
dlist <- list(
    D_obs = standardize( d$Divorce ),
    D_sd = d$Divorce.SE / sd( d$Divorce ),
    M_obs = standardize( d$Marriage ),
    M_sd = d$Marriage.SE / sd( d$Marriage ),
    A = standardize( d$MedianAgeMarriage ),
    N = nrow(d)
)

m15.2 <- ulam(
    alist(
        D_obs ~ dnorm( D_est , D_sd ),
        vector[N]:D_est ~ dnorm( mu , sigma ),
        mu <- a + bA*A + bM*M_est[i],
        M_obs ~ dnorm( M_est , M_sd ),
        vector[N]:M_est ~ dnorm( 0 , 1 ),
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        bM ~ dnorm(0,0.5),
        sigma ~ dexp( 1 )
    ) , data=dlist , chains=4 , cores=4 )
```

```
## recompiling to avoid crashing R session
```

```r
precis( m15.2 )
```

```
## 100 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##              mean        sd       5.5%      94.5%     n_eff     Rhat4
## a     -0.04060498 0.0983836 -0.1929647  0.1161032 2002.8088 0.9998635
## bA    -0.54426724 0.1612741 -0.7915661 -0.2818097  986.8621 1.0013794
## bM     0.19696425 0.2074347 -0.1347760  0.5276694  692.8134 1.0053220
## sigma  0.56580648 0.1106100  0.3983102  0.7484597  712.5323 1.0020740
```

> doubling the standard errors


```r
## modify from R code 15.5
dlist2 <- list(
    D_obs = standardize( d$Divorce ),
    D_sd = 2*d$Divorce.SE / sd( d$Divorce ),
    M_obs = standardize( d$Marriage ),
    M_sd = 2*d$Marriage.SE / sd( d$Marriage ),
    A = standardize( d$MedianAgeMarriage ),
    N = nrow(d)
)

m15.2_double <- ulam(
    alist(
        D_obs ~ dnorm( D_est , D_sd ),
        vector[N]:D_est ~ dnorm( mu , sigma ),
        mu <- a + bA*A + bM*M_est[i],
        M_obs ~ dnorm( M_est , M_sd ),
        vector[N]:M_est ~ dnorm( 0 , 1 ),
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        bM ~ dnorm(0,0.5),
        sigma ~ dexp( 1 )
    ) , data=dlist2 , chains=4 , cores=4 )
```

```
## recompiling to avoid crashing R session
```

```
## Warning: There were 67 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: There were 3 chains where the estimated Bayesian Fraction of Missing Information was low. See
## http://mc-stan.org/misc/warnings.html#bfmi-low
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Warning: The largest R-hat is 1.42, indicating chains have not mixed.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#r-hat
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```

```r
precis( m15.2_double )
```

```
## 100 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##              mean         sd        5.5%       94.5%      n_eff    Rhat4
## a     -0.08377867 0.09974541 -0.24647627  0.08321848 429.648299 1.004657
## bA    -0.58990573 0.15214805 -0.82723680 -0.33495239 264.733601 1.013613
## bM     0.36511482 0.20370922  0.06547382  0.69808458 147.252508 1.051624
## sigma  0.14116656 0.09666459  0.04099062  0.31549739   9.062279 1.338498
```

```r
plot(precis( m15.2 ))
```

```
## 100 vector or matrix parameters hidden. Use depth=2 to show them.
```

![](chapter15-1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
plot(precis( m15.2_double ))
```

```
## 100 vector or matrix parameters hidden. Use depth=2 to show them.
```

![](chapter15-1_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

> effective number of samples are small.

> larger deviance of observations makes inference harder

### 15H1. The data in data(elephants) are counts of matings observed for bull elephants of differing ages. There is a strong positive relationship between age and matings. However, age is not always assessed accurately. First, fit a Poisson model predicting MATINGS with AGE as a predictor. Second, assume that the observed AGE values are uncertain and have a standard error of±5 years. Re-estimate the relationship between MATINGS and AGE, incorporating this measurement error. Compare the inferences of the two models.


```r
data(elephants)
d <- elephants
str(d)
```

```
## 'data.frame':	41 obs. of  2 variables:
##  $ AGE    : int  27 28 28 28 28 29 29 29 29 29 ...
##  $ MATINGS: int  0 1 1 1 3 0 0 0 2 2 ...
```

```r
summary(d)
```

```
##       AGE           MATINGS     
##  Min.   :27.00   Min.   :0.000  
##  1st Qu.:29.00   1st Qu.:1.000  
##  Median :34.00   Median :2.000  
##  Mean   :35.85   Mean   :2.683  
##  3rd Qu.:42.00   3rd Qu.:3.000  
##  Max.   :52.00   Max.   :9.000
```

```r
plot(d$MATINGS, d$AGE)
```

![](chapter15-1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
dlist <- list(
    A = standardize( d$AGE ),
    M = d$MATINGS,
    N = nrow(d)
)
str(dlist)
```

```
## List of 3
##  $ A: num [1:41] -1.35 -1.19 -1.19 -1.19 -1.19 ...
##   ..- attr(*, "scaled:center")= num 35.9
##   ..- attr(*, "scaled:scale")= num 6.58
##  $ M: int [1:41] 0 1 1 1 3 0 0 0 2 2 ...
##  $ N: int 41
```

```r
summary(dlist)
```

```
##   Length Class  Mode   
## A 41     -none- numeric
## M 41     -none- numeric
## N  1     -none- numeric
```

```r
plot(dlist$M, dlist$A)
```

![](chapter15-1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
> fit a Poisson model using age to predict matings


```r
m15H1.base <- ulam(
  alist(
    M ~ dpois(lambda),
    log(lambda) <- a + bA*A,
    a ~ dnorm(0,1),
    bA ~ dnorm(0,1)
    ), data = dlist, chains = 4, cores = 4)

precis(m15H1.base)
```

```
##         mean         sd      5.5%     94.5%    n_eff     Rhat4
## a  0.8623536 0.10380846 0.6957450 1.0296437 750.9403 1.0037875
## bA 0.4539435 0.09065786 0.3109031 0.5994287 911.6769 0.9993912
```

```r
plot(m15H1.base)
```

![](chapter15-1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

> Second, assume that the observed AGE values are uncertain and have a standard error of±5 years


```r
m15H1.se <- ulam(
  alist(
    M ~ dpois(lambda),
    log(lambda) <- a + bA*A_est[i],
    A ~ dnorm(A_est, 5),
    vector[N]:A_est ~ dnorm(0,1),
    a ~ dnorm(0,1),
    bA ~ dnorm(0,1)
    ), data = dlist, chains = 4, cores = 4)
```

```
## Warning: The largest R-hat is 1.29, indicating chains have not mixed.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#r-hat
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```

```r
precis(m15H1.se)
```

```
## 41 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##         mean        sd       5.5%     94.5%      n_eff    Rhat4
## a  0.8172438 0.1445623  0.5824883 1.0359291 1488.03404 1.000392
## bA 0.4021913 0.4251880 -0.5908905 0.8000926    4.61595 2.016090
```

```r
plot(m15H1.se)
```

```
## 41 vector or matrix parameters hidden. Use depth=2 to show them.
```

![](chapter15-1_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


### 15H2. Repeat the model fitting problem above, now increasing the assumed standard error on AGE. How large does the standard error have to get before the posterior mean for the coefficient on AGE reaches zero?

> increasing the assumed standard error on AGE to 10


```r
m15H2.se10 <- ulam(
  alist(
    M ~ dpois(lambda),
    log(lambda) <- a + bA*A_est[i],
    A ~ dnorm(A_est, 10),
    vector[N]:A_est ~ dnorm(0,1),
    a ~ dnorm(0,1),
    bA ~ dnorm(0,1)
    ), data = dlist, chains = 4, cores = 4)
```

```
## Warning: The largest R-hat is 1.51, indicating chains have not mixed.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#r-hat
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```

```r
precis(m15H2.se10)
```

```
## 41 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##         mean        sd       5.5%     94.5%       n_eff     Rhat4
## a  0.8186754 0.1456028  0.5797071 1.0397571 1317.365413 0.9991667
## bA 0.0737264 0.5765481 -0.7200405 0.7801531    2.953309 2.1798822
```

```r
plot(m15H2.se10)
```

```
## 41 vector or matrix parameters hidden. Use depth=2 to show them.
```

![](chapter15-1_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

> increasing the assumed standard error on AGE to 50


```r
m15H2.se50 <- ulam(
  alist(
    M ~ dpois(lambda),
    log(lambda) <- a + bA*A_est[i],
    A ~ dnorm(A_est, 50),
    vector[N]:A_est ~ dnorm(0,1),
    a ~ dnorm(0,1),
    bA ~ dnorm(0,1)
    ), data = dlist, chains = 4, cores = 4)
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```

```r
precis(m15H2.se50)
```

```
## 41 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##          mean        sd       5.5%     94.5%      n_eff    Rhat4
## a   0.8233270 0.1510617  0.5714822 1.0534912 1056.04633 1.002142
## bA -0.4686015 0.3518378 -0.8215803 0.4659315   21.67972 1.095363
```

```r
plot(m15H2.se50)
```

```
## 41 vector or matrix parameters hidden. Use depth=2 to show them.
```

![](chapter15-1_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

> AGE estimates become very close to zero

> increasing the assumed standard error on AGE to 100


```r
m15H2.se100 <- ulam(
  alist(
    M ~ dpois(lambda),
    log(lambda) <- a + bA*A_est[i],
    A ~ dnorm(A_est, 100),
    vector[N]:A_est ~ dnorm(0,1),
    a ~ dnorm(0,1),
    bA ~ dnorm(0,1)
    ), data = dlist, chains = 4, cores = 4)
```

```
## Warning: The largest R-hat is 1.55, indicating chains have not mixed.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#r-hat
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```

```r
precis(m15H2.se100)
```

```
## 41 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##          mean        sd       5.5%     94.5%       n_eff    Rhat4
## a  0.80766660 0.1584468  0.5465027 1.0459757 1279.123983 1.002232
## bA 0.01092038 0.5984624 -0.7651453 0.8018481    5.448706 2.145244
```

```r
plot(m15H2.se100)
```

```
## 41 vector or matrix parameters hidden. Use depth=2 to show them.
```

![](chapter15-1_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

> AGE estimates become negative (close to zero)
