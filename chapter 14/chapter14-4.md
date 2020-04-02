---
title: "chapter14-4"
author: "Min-Yao"
date: "2020/4/2"
output: 
  html_document: 
    keep_md: yes
---


```r
## R code 14.29
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
## rethinking (Version 1.93)
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
data(KosterLeckie)
```


```r
head(kl_dyads)
```

```
##   hidA hidB did giftsAB giftsBA offset drel1 drel2 drel3 drel4 dlndist  dass
## 1    1    2   1       0       4  0.000     0     0     1     0  -2.790 0.000
## 2    1    3   2       6      31 -0.003     0     1     0     0  -2.817 0.044
## 3    1    4   3       2       5 -0.019     0     1     0     0  -1.886 0.025
## 4    1    5   4       4       2  0.000     0     1     0     0  -1.892 0.011
## 5    1    6   5       8       2 -0.003     1     0     0     0  -3.499 0.022
## 6    1    7   6       2       1  0.000     0     0     0     0  -1.853 0.071
##   d0125
## 1     0
## 2     0
## 3     0
## 4     0
## 5     0
## 6     0
```



```r
## R code 14.30
kl_data <- list(
    N = nrow(kl_dyads),
    N_households = max(kl_dyads$hidB),
    did = kl_dyads$did,
    hidA = kl_dyads$hidA,
    hidB = kl_dyads$hidB,
    giftsAB = kl_dyads$giftsAB,
    giftsBA = kl_dyads$giftsBA
)

m14.4 <- ulam(
    alist(
        giftsAB ~ poisson( lambdaAB ),
        giftsBA ~ poisson( lambdaBA ),
        log(lambdaAB) <- a + gr[hidA,1] + gr[hidB,2] + d[did,1] ,
        log(lambdaBA) <- a + gr[hidB,1] + gr[hidA,2] + d[did,2] ,
        a ~ normal(0,1),

       ## gr matrix of varying effects
        vector[2]:gr[N_households] ~ multi_normal(0,Rho_gr,sigma_gr),
        Rho_gr ~ lkj_corr(4),
        sigma_gr ~ exponential(1),

       ## dyad effects
        transpars> matrix[N,2]:d <-
                compose_noncentered( rep_vector(sigma_d,2) , L_Rho_d , z ),
        matrix[2,N]:z ~ normal( 0 , 1 ),
        cholesky_factor_corr[2]:L_Rho_d ~ lkj_corr_cholesky( 8 ),
        sigma_d ~ exponential(1),

       ## compute correlation matrix for dyads
        gq> matrix[2,2]:Rho_d <<- Chol_to_Corr( L_Rho_d )
    ), data=kl_data , chains=4 , cores=4 , iter=2000 )

## R code 14.31
precis( m14.4 , depth=3 , pars=c("Rho_gr","sigma_gr") )
```

```
##                   mean           sd       5.5%       94.5%     n_eff      Rhat
## Rho_gr[1,1]  1.0000000 0.000000e+00  1.0000000  1.00000000       NaN       NaN
## Rho_gr[1,2] -0.3976935 1.997039e-01 -0.6940614 -0.06149018 1452.1320 1.0007749
## Rho_gr[2,1] -0.3976935 1.997039e-01 -0.6940614 -0.06149018 1452.1320 1.0007749
## Rho_gr[2,2]  1.0000000 8.309187e-17  1.0000000  1.00000000 3892.9752 0.9989995
## sigma_gr[1]  0.8342686 1.374892e-01  0.6393336  1.07398996 2123.3340 1.0012447
## sigma_gr[2]  0.4306055 9.209264e-02  0.2998512  0.58830816  951.6251 1.0014473
```


```r
## R code 14.32
post <- extract.samples( m14.4 )
g <- sapply( 1:25 , function(i) post$a + post$gr[,i,1] )
r <- sapply( 1:25 , function(i) post$a + post$gr[,i,2] )
Eg_mu <- apply( exp(g) , 2 , mean )
Er_mu <- apply( exp(r) , 2 , mean )
```


```r
## R code 14.33
plot( NULL , xlim=c(0,8.6) , ylim=c(0,8.6) , xlab="generalized giving" ,
    ylab="generalized receiving" , lwd=1.5 )
abline(a=0,b=1,lty=2)

# ellipses
library(ellipse)
```

```
## Warning: package 'ellipse' was built under R version 3.6.3
```

```
## 
## Attaching package: 'ellipse'
```

```
## The following object is masked from 'package:rethinking':
## 
##     pairs
```

```
## The following object is masked from 'package:graphics':
## 
##     pairs
```

```r
for ( i in 1:25 ) {
    Sigma <- cov( cbind( g[,i] , r[,i] ) )
    Mu <- c( mean(g[,i]) , mean(r[,i]) )
    for ( l in c(0.5) ) {
        el <- ellipse( Sigma , centre=Mu , level=l )
        lines( exp(el) , col=col.alpha("black",0.5) )
    }
}
# household means
points( Eg_mu , Er_mu , pch=21 , bg="white" , lwd=1.5 )
```

![](chapter14-4_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
## R code 14.34
precis( m14.4 , depth=3 , pars=c("Rho_d","sigma_d") )
```

```
##                 mean         sd      5.5%     94.5%     n_eff     Rhat
## Rho_d[1,1] 1.0000000 0.00000000 1.0000000 1.0000000       NaN      NaN
## Rho_d[1,2] 0.8823712 0.03415023 0.8239274 0.9316875  745.1854 1.001234
## Rho_d[2,1] 0.8823712 0.03415023 0.8239274 0.9316875  745.1854 1.001234
## Rho_d[2,2] 1.0000000 0.00000000 1.0000000 1.0000000       NaN      NaN
## sigma_d    1.1047574 0.05609478 1.0173639 1.1973230 1136.2245 1.005166
```


```r
## R code 14.35
dy1 <- apply( post$d[,,1] , 2 , mean )
dy2 <- apply( post$d[,,2] , 2 , mean )
plot( dy1 , dy2 )
```

![](chapter14-4_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

