---
title: "chapter 4"
author: "Min-Yao"
date: "2019年4月21日"
output: 
  html_document: 
    keep_md: yes
---
# Geocentric Models

## 4.1. Why normal distributions are normal
### 4.1.1. Normal by addition.


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



```r
## R code 4.1
pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
hist(pos)
```

![](chapter_4_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
plot(density(pos))
```

![](chapter_4_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
dens(pos, norm.comp=TRUE)
```

![](chapter_4_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

### 4.1.2. Normal by multiplication.


```r
## R code 4.2
prod( 1 + runif(12,0,0.1) )
```

```
## [1] 1.681588
```

```r
## R code 4.3
growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
dens( growth , norm.comp=TRUE )
```

![](chapter_4_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## R code 4.4
big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
dens( big , norm.comp=TRUE )
```

![](chapter_4_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )
dens( small , norm.comp=TRUE )
```

![](chapter_4_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

### 4.1.3. Normal by log-multiplication.


```r
## R code 4.5
log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )
dens( log.big , norm.comp=TRUE )
```

![](chapter_4_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### 4.1.4. Using Gaussian distributions.
## 4.2. A language for describing models
### 4.2.1. Re-describing the globe tossing model.


```r
## R code 4.6
w <- 6; n <- 9;
p_grid <- seq(from=0,to=1,length.out=100)
posterior <- dbinom(w,n,p_grid)*dunif(p_grid,0,1)
posterior <- posterior/sum(posterior)
```

## 4.3. A Gaussian model of height

### 4.3.1. The data.


```r
## R code 4.7
library(rethinking)
data(Howell1)
d <- Howell1

## R code 4.8
str( d )
```

```
## 'data.frame':	544 obs. of  4 variables:
##  $ height: num  152 140 137 157 145 ...
##  $ weight: num  47.8 36.5 31.9 53 41.3 ...
##  $ age   : num  63 63 65 41 51 35 32 27 19 54 ...
##  $ male  : int  1 0 0 1 0 1 0 1 0 1 ...
```

```r
## R code 4.9
precis( d )
```

```
##               mean         sd      5.5%     94.5%     histogram
## height 138.2635963 27.6024476 81.108550 165.73500 ▁▁▁▁▁▁▁▂▁▇▇▅▁
## weight  35.6106176 14.7191782  9.360721  54.50289 ▁▂▃▂▂▂▂▅▇▇▃▂▁
## age     29.3443934 20.7468882  1.000000  66.13500     ▇▅▅▃▅▂▂▁▁
## male     0.4724265  0.4996986  0.000000   1.00000    ▇▁▁▁▁▁▁▁▁▇
```

```r
## R code 4.10
d$height
```

```
##   [1] 151.7650 139.7000 136.5250 156.8450 145.4150 163.8300 149.2250
##   [8] 168.9100 147.9550 165.1000 154.3050 151.1300 144.7800 149.9000
##  [15] 150.4950 163.1950 157.4800 143.9418 121.9200 105.4100  86.3600
##  [22] 161.2900 156.2100 129.5400 109.2200 146.4000 148.5900 147.3200
##  [29] 137.1600 125.7300 114.3000 147.9550 161.9250 146.0500 146.0500
##  [36] 152.7048 142.8750 142.8750 147.9550 160.6550 151.7650 162.8648
##  [43] 171.4500 147.3200 147.9550 144.7800 121.9200 128.9050  97.7900
##  [50] 154.3050 143.5100 146.7000 157.4800 127.0000 110.4900  97.7900
##  [57] 165.7350 152.4000 141.6050 158.8000 155.5750 164.4650 151.7650
##  [64] 161.2900 154.3050 145.4150 145.4150 152.4000 163.8300 144.1450
##  [71] 129.5400 129.5400 153.6700 142.8750 146.0500 167.0050 158.4198
##  [78]  91.4400 165.7350 149.8600 147.9550 137.7950 154.9400 160.9598
##  [85] 161.9250 147.9550 113.6650 159.3850 148.5900 136.5250 158.1150
##  [92] 144.7800 156.8450 179.0700 118.7450 170.1800 146.0500 147.3200
##  [99] 113.0300 162.5600 133.9850 152.4000 160.0200 149.8600 142.8750
## [106] 167.0050 159.3850 154.9400 148.5900 111.1250 111.7600 162.5600
## [113] 152.4000 124.4600 111.7600  86.3600 170.1800 146.0500 159.3850
## [120] 151.1300 160.6550 169.5450 158.7500  74.2950 149.8600 153.0350
## [127]  96.5200 161.9250 162.5600 149.2250 116.8400 100.0760 163.1950
## [134] 161.9250 145.4150 163.1950 151.1300 150.4950 141.6050 170.8150
## [141]  91.4400 157.4800 152.4000 149.2250 129.5400 147.3200 145.4150
## [148] 121.9200 113.6650 157.4800 154.3050 120.6500 115.6000 167.0050
## [155] 142.8750 152.4000  96.5200 160.0000 159.3850 149.8600 160.6550
## [162] 160.6550 149.2250 125.0950 140.9700 154.9400 141.6050 160.0200
## [169] 150.1648 155.5750 103.5050  94.6150 156.2100 153.0350 167.0050
## [176] 149.8600 147.9550 159.3850 161.9250 155.5750 159.3850 146.6850
## [183] 172.7200 166.3700 141.6050 142.8750 133.3500 127.6350 119.3800
## [190] 151.7650 156.8450 148.5900 157.4800 149.8600 147.9550 102.2350
## [197] 153.0350 160.6550 149.2250 114.3000 100.9650 138.4300  91.4400
## [204] 162.5600 149.2250 158.7500 149.8600 158.1150 156.2100 148.5900
## [211] 143.5100 154.3050 131.4450 157.4800 157.4800 154.3050 107.9500
## [218] 168.2750 145.4150 147.9550 100.9650 113.0300 149.2250 154.9400
## [225] 162.5600 156.8450 123.1900 161.0106 144.7800 143.5100 149.2250
## [232] 110.4900 149.8600 165.7350 144.1450 157.4800 154.3050 163.8300
## [239] 156.2100 153.6700 134.6200 144.1450 114.3000 162.5600 146.0500
## [246] 120.6500 154.9400 144.7800 106.6800 146.6850 152.4000 163.8300
## [253] 165.7350 156.2100 152.4000 140.3350 158.1150 163.1950 151.1300
## [260] 171.1198 149.8600 163.8300 141.6050  93.9800 149.2250 105.4100
## [267] 146.0500 161.2900 162.5600 145.4150 145.4150 170.8150 127.0000
## [274] 159.3850 159.4000 153.6700 160.0200 150.4950 149.2250 127.0000
## [281] 142.8750 142.1130 147.3200 162.5600 164.4650 160.0200 153.6700
## [288] 167.0050 151.1300 147.9550 125.3998 111.1250 153.0350 139.0650
## [295] 152.4000 154.9400 147.9550 143.5100 117.9830 144.1450  92.7100
## [302] 147.9550 155.5750 150.4950 155.5750 154.3050 130.6068 101.6000
## [309] 157.4800 168.9100 150.4950 111.7600 160.0200 167.6400 144.1450
## [316] 145.4150 160.0200 147.3200 164.4650 153.0350 149.2250 160.0200
## [323] 149.2250  85.0900  84.4550  59.6138  92.7100 111.1250  90.8050
## [330] 153.6700  99.6950  62.4840  81.9150  96.5200  80.0100 150.4950
## [337] 151.7650 140.6398  88.2650 158.1150 149.2250 151.7650 154.9400
## [344] 123.8250 104.1400 161.2900 148.5900  97.1550  93.3450 160.6550
## [351] 157.4800 167.0050 157.4800  91.4400  60.4520 137.1600 152.4000
## [358] 152.4000  81.2800 109.2200  71.1200  89.2048  67.3100  85.0900
## [365]  69.8500 161.9250 152.4000  88.9000  90.1700  71.7550  83.8200
## [372] 159.3850 142.2400 142.2400 168.9100 123.1900  74.9300  74.2950
## [379]  90.8050 160.0200  67.9450 135.8900 158.1150  85.0900  93.3450
## [386] 152.4000 155.5750 154.3050 156.8450 120.0150 114.3000  83.8200
## [393] 156.2100 137.1600 114.3000  93.9800 168.2750 147.9550 139.7000
## [400] 157.4800  76.2000  66.0400 160.7000 114.3000 146.0500 161.2900
## [407]  69.8500 133.9850  67.9450 150.4950 163.1950 148.5900 148.5900
## [414] 161.9250 153.6700  68.5800 151.1300 163.8300 153.0350 151.7650
## [421] 132.0800 156.2100 140.3350 158.7500 142.8750  84.4550 151.9428
## [428] 161.2900 127.9906 160.9852 144.7800 132.0800 117.9830 160.0200
## [435] 154.9400 160.9852 165.9890 157.9880 154.9400  97.9932  64.1350
## [442] 160.6550 147.3200 146.7000 147.3200 172.9994 158.1150 147.3200
## [449] 124.9934 106.0450 165.9890 149.8600  76.2000 161.9250 140.0048
## [456]  66.6750  62.8650 163.8300 147.9550 160.0200 154.9400 152.4000
## [463]  62.2300 146.0500 151.9936 157.4800  55.8800  60.9600 151.7650
## [470] 144.7800 118.1100  78.1050 160.6550 151.1300 121.9200  92.7100
## [477] 153.6700 147.3200 139.7000 157.4800  91.4400 154.9400 143.5100
## [484]  83.1850 158.1150 147.3200 123.8250  88.9000 160.0200 137.1600
## [491] 165.1000 154.9400 111.1250 153.6700 145.4150 141.6050 144.7800
## [498] 163.8300 161.2900 154.9000 161.3000 170.1800 149.8600 123.8250
## [505]  85.0900 160.6550 154.9400 106.0450 126.3650 166.3700 148.2852
## [512] 124.4600  89.5350 101.6000 151.7650 148.5900 153.6700  53.9750
## [519] 146.6850  56.5150 100.9650 121.9200  81.5848 154.9400 156.2100
## [526] 132.7150 125.0950 101.6000 160.6550 146.0500 132.7150  87.6300
## [533] 156.2100 152.4000 162.5600 114.9350  67.9450 142.8750  76.8350
## [540] 145.4150 162.5600 156.2100  71.1200 158.7500
```

```r
## R code 4.11
d2 <- d[ d$age >= 18 , ]
precis( d2 )
```

```
##             mean         sd     5.5%     94.5%       histogram
## height 154.59709  7.7423321 142.8750 167.00500       ▁▃▇▇▅▇▂▁▁
## weight  44.99049  6.4567081  35.1375  55.76588         ▁▅▇▇▃▂▁
## age     41.13849 15.9678551  20.0000  70.00000 ▂▅▇▅▃▇▃▃▂▂▂▁▁▁▁
## male     0.46875  0.4997328   0.0000   1.00000      ▇▁▁▁▁▁▁▁▁▇
```

### 4.3.2. The model.


```r
dens(d2$height)
```

![](chapter_4_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
## R code 4.12
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
```

![](chapter_4_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
## R code 4.13
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )
```

![](chapter_4_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```r
## R code 4.14
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )
```

![](chapter_4_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

```r
## R code 4.15
sample_mu <- rnorm( 1e4 , 178 , 100 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )
```

![](chapter_4_files/figure-html/unnamed-chunk-7-5.png)<!-- -->

### 4.3.3. Grid approximation of the posterior distribution.


```r
## R code 4.16
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
                d2$height ,
                mean=post$mu[i] ,
                sd=post$sigma[i] ,
                log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
    dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

## R code 4.17
contour_xyz( post$mu , post$sigma , post$prob )
```

![](chapter_4_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
## R code 4.18
image_xyz( post$mu , post$sigma , post$prob )
```

![](chapter_4_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

### 4.3.4. Sampling from the posterior.


```r
## R code 4.19
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
    prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

## R code 4.20
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )
```

![](chapter_4_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
## R code 4.21
dens( sample.mu )
```

![](chapter_4_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
dens( sample.sigma )
```

![](chapter_4_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
## R code 4.22
HPDI( sample.mu )
```

```
##    |0.89    0.89| 
## 153.8693 155.1759
```

```r
HPDI( sample.sigma )
```

```
##    |0.89    0.89| 
## 7.291457 8.195980
```

```r
## R code 4.23
d3 <- sample( d2$height , size=20 )

## R code 4.24
mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
    sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
    log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
    dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
    prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
    col=col.alpha(rangi2,0.1) ,
    xlab="mu" , ylab="sigma" , pch=16 )
```

![](chapter_4_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

```r
## R code 4.25
dens( sample2.sigma , norm.comp=TRUE )
```

![](chapter_4_files/figure-html/unnamed-chunk-9-5.png)<!-- -->

### 4.3.5. Finding the posterior distribution with quap.


```r
## R code 4.26
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

## R code 4.27
flist <- alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 20 ) ,
    sigma ~ dunif( 0 , 50 )
)

## R code 4.28
m4.1 <- quap( flist , data=d2 )

## R code 4.29
precis( m4.1 )
```

```
##             mean        sd       5.5%      94.5%
## mu    154.612791 0.4119718 153.954380 155.271201
## sigma   7.730884 0.2913439   7.265261   8.196508
```

```r
precis( m4.1, prob = 0.95 )
```

```
##             mean        sd       2.5%      97.5%
## mu    154.612791 0.4119718 153.805341 155.420241
## sigma   7.730884 0.2913439   7.159861   8.301908
```

```r
## R code 4.30
start <- list(
    mu=mean(d2$height),
    sigma=sd(d2$height)
)
m4.1 <- quap( flist , data=d2 , start=start )

## R code 4.31
m4.2 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu ~ dnorm( 178 , 0.1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d2 )
precis( m4.2 )
```

```
##            mean        sd      5.5%     94.5%
## mu    177.86375 0.1002354 177.70356 178.02395
## sigma  24.51756 0.9289234  23.03296  26.00216
```

### 4.3.6. Sampling from a quap. 


```r
## R code 4.32
vcov( m4.1 )
```

```
##                 mu        sigma
## mu    0.1697396109 0.0002180307
## sigma 0.0002180307 0.0849058224
```

```r
## R code 4.33
diag( vcov( m4.1 ) )
```

```
##         mu      sigma 
## 0.16973961 0.08490582
```

```r
cov2cor( vcov( m4.1 ) )
```

```
##                mu       sigma
## mu    1.000000000 0.001816174
## sigma 0.001816174 1.000000000
```

```r
## R code 4.34
library(rethinking)
post <- extract.samples( m4.1 , n=1e4 )
head(post)
```

```
##         mu    sigma
## 1 154.5491 7.520431
## 2 155.2088 7.684656
## 3 154.7984 7.692010
## 4 153.2333 7.478963
## 5 155.6721 7.819995
## 6 155.0498 7.522178
```

```r
## R code 4.35
precis(post)
```

```
##             mean        sd      5.5%      94.5%   histogram
## mu    154.605349 0.4151979 153.94028 155.276149    ▁▁▁▅▇▂▁▁
## sigma   7.729075 0.2916053   7.26325   8.202198 ▁▁▁▂▅▇▇▃▁▁▁
```

```r
## R code 4.36
library(MASS)
post <- mvrnorm( n=1e4 , mu=coef(m4.1) , Sigma=vcov(m4.1) )
```

## 4.4. Adding a predictor


```r
## R code 4.37
plot( d2$height ~ d2$weight )
```

![](chapter_4_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### 4.4.1. The linear model strategy.


```r
## R code 4.38
set.seed(2971)
N <- 100                   # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )

## R code 4.39
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
    xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
    from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
    col=col.alpha("black",0.2) )
```

![](chapter_4_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
## R code 4.40
b <- rlnorm( 1e4 , 0 , 1 )
dens( b , xlim=c(0,5) , adj=0.1 )
```

![](chapter_4_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
## R code 4.41
set.seed(2971)
N <- 100                   # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rlnorm( N , 0 , 1 )

## R code 4.39
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
    xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
    from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
    col=col.alpha("black",0.2) )
```

![](chapter_4_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

```r
## R code 4.40
b <- rlnorm( 1e4 , 0 , 1 )
dens( b , xlim=c(0,5) , adj=0.1 )
```

![](chapter_4_files/figure-html/unnamed-chunk-13-4.png)<!-- -->

### 4.4.2. Finding the posterior distribution.


```r
## R code 4.42
# load data again, since it's a long way back
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# define the average weight, x-bar
xbar <- mean(d2$weight)

# fit model
m4.3 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d2 )

## R code 4.43
#m4.3b <- quap(
#    alist(
#        height ~ dnorm( mu , sigma ) ,
#        mu <- a + exp(log_b)*( weight - xbar ),
#        a ~ dnorm( 178 , 100 ) ,
#        log_b ~ dnorm( 0 , 1 ) ,
#        sigma ~ dunif( 0 , 50 )
#    ) ,
#    data=d2 )
```

## 4.7. Practice

### Easy.

#### 4E1. In the model definition below, which line is the likelihood?

yi ~ Normal(u, sigma)

> the likelihood

u ~ Normal(0, 10)

> the prior for u (mean)

sigma ~ Uniform(0, 10)

> the prior for sigma (SD)

#### 4E2. In the model definition just above, how many parameters are in the posterior distribution?

> Two, u and sigma are parameters. yi is observed data.

#### 4E3. Using the model definition above, write down the appropriate form of Bayes’ theorem that includes the proper likelihood and priors.

$\Pr(μ,σ|y)=\prod_iNormal(yi|μ,σ)Normal(μ|0,10)Uniform(σ|0,10)/\int\int\prod_iNormal(hi|μ,σ)Normal(μ|0,10)Uniform(σ|0,10)dμdσ$

> Pr(μ,σ|y)=∏iNormal(yi|μ,σ)Normal(μ|0,10)Uniform(σ|0,10)/∫∫∏iNormal(hi|μ,σ)Normal(μ|0,10)Uniform(σ|0,10)dμdσ

#### 4E4. In the model definition below, which line is the linear model?
yi ~ Normal(u, sigma)

> the likelihood

ui = a + bxi
> the linear model

a ~ Normal(0, 10)

> the prior for a

b ~ Normal(0, 1)

> the prior for b

sigma ~ Uniform(0, 10)

> the prior for sigma

#### 4E5. In the model definition just above, how many parameters are in the posterior distribution?

> Three, a, b, and sigma are parameters.


### Medium.

#### 4M1. For the model definition below, simulate observed heights from the prior (not the posterior).
#### yi ~ Normal(u, sigma)
#### u ~ Normal(0, 10)
#### sigma ~ Uniform(0, 10)


```r
sample_mu <- rnorm( 1e4 , 0 , 10 )
sample_sigma <- runif( 1e4 , 0 , 10 )
prior_y <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_y )
```

![](chapter_4_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


#### 4M2. Translate the model just above into a quap formula.


```r
## R code 4.27
formula4M2 <- alist(
    y ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 10 )
)
```


#### 4M3. Translate the quap model formula below into a mathematical model definition.


```r
flist <- alist(
y ~ dnorm( mu , sigma ),
mu <- a + b*x,
a ~ dnorm( 0 , 50 ),
b ~ dunif( 0 , 10 ),
sigma ~ dunif( 0 , 50 )
)
```

> yi ~ Normal(mu, sigma)

> mu <- a + b*xi

> a ~ Normal(0, 50)

> b ~ Uniform(0, 10)

> sigma ~ Uniform(0, 50)
