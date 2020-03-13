---
title: "chapter14-2"
author: "Min-Yao"
date: "2020/3/12"
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
library(MASS)
```


```r
## R code 14.18
data(chimpanzees)
d <- chimpanzees
d$block_id <- d$block
d$treatment <- 1L + d$prosoc_left + 2L*d$condition

dat <- list(
    L = d$pulled_left,
    tid = d$treatment,
    actor = d$actor,
    block_id = as.integer(d$block_id) )

m14.2 <- ulam(
    alist(
        L ~ binomial(1,p),
        logit(p) <- g[tid] + alpha[actor,tid] + beta[block_id,tid],

        # adaptive priors
        vector[4]:alpha[actor] ~ multi_normal(0,Rho_actor,sigma_actor),
        vector[4]:beta[block_id] ~ multi_normal(0,Rho_block,sigma_block),

        # fixed priors
        g[tid] ~ dnorm(0,1),
        sigma_actor ~ dexp(1),
        Rho_actor ~ dlkjcorr(4),
        sigma_block ~ dexp(1),
        Rho_block ~ dlkjcorr(4)
    ) , data=dat , chains=4 , cores=4 )
```

```
## Warning: There were 18 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: There were 4 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Warning: The largest R-hat is 1.05, indicating chains have not mixed.
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
## R code 14.19
m14.3 <- ulam(
    alist(
        L ~ binomial(1,p),
        logit(p) <- g[tid] + alpha[actor,tid] + beta[block_id,tid],

        # adaptive priors - non-centered
        transpars> matrix[actor,4]:alpha <-
                compose_noncentered( sigma_actor , L_Rho_actor , z_actor ),
        transpars> matrix[block_id,4]:beta <-
                compose_noncentered( sigma_block , L_Rho_block , z_block ),
        matrix[4,actor]:z_actor ~ normal( 0 , 1 ),
        matrix[4,block_id]:z_block ~ normal( 0 , 1 ),

        # fixed priors
        g[tid] ~ normal(0,1),
        vector[4]:sigma_actor ~ dexp(1),
        cholesky_factor_corr[4]:L_Rho_actor ~ lkj_corr_cholesky( 2 ),
        vector[4]:sigma_block ~ dexp(1),
        cholesky_factor_corr[4]:L_Rho_block ~ lkj_corr_cholesky( 2 ),

        # compute ordinary correlation matrixes from Cholesky factors
        gq> matrix[4,4]:Rho_actor <<- multiply_lower_tri_self_transpose(L_Rho_actor),
        gq> matrix[4,4]:Rho_block <<- multiply_lower_tri_self_transpose(L_Rho_block)
    ) , data=dat , chains=4 , cores=4 , log_lik=TRUE )
```



```r
## R code 14.20
# extract n_eff values for each model
neff_nc <- precis(m14.3,3,pars=c("alpha","beta"))$n_eff
neff_c <- precis(m14.2,3,pars=c("alpha","beta"))$n_eff
plot( neff_c , neff_nc , xlab="centered (default)" ,
    ylab="non-centered (cholesky)" , lwd=1.5 )
abline(a=0,b=1,lty=2)
```

![](chapter14-2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
## R code 14.21
precis( m14.2 , depth=2 , pars=c("sigma_actor","sigma_block") )
```

```
##                     mean        sd       5.5%     94.5%     n_eff     Rhat
## sigma_actor[1] 1.3841987 0.5113749 0.80779532 2.2807450  664.4278 1.006191
## sigma_actor[2] 0.9114280 0.4245672 0.39238580 1.6637022  623.0577 1.009010
## sigma_actor[3] 1.8511300 0.5775114 1.12221315 2.8491017 1101.8327 1.001155
## sigma_actor[4] 1.5957079 0.6507045 0.86741875 2.7807044  575.9586 1.001588
## sigma_block[1] 0.4321708 0.3100782 0.06401890 0.9897692  407.3245 1.003161
## sigma_block[2] 0.4159335 0.3155206 0.04786570 0.9881891  278.3955 1.006305
## sigma_block[3] 0.2943339 0.2682470 0.02410769 0.7685520  281.4468 1.016096
## sigma_block[4] 0.5072097 0.3896404 0.06257776 1.1796562  199.4472 1.019242
```

```r
precis( m14.3 , depth=2 , pars=c("sigma_actor","sigma_block") )
```

```
##                     mean        sd       5.5%     94.5%     n_eff      Rhat
## sigma_actor[1] 1.4030374 0.4703800 0.80920340 2.2292551  959.1682 0.9994056
## sigma_actor[2] 0.9141453 0.4036680 0.40268194 1.6548898 1406.2909 1.0007352
## sigma_actor[3] 1.8623147 0.5837334 1.11978135 2.8475209 1271.2526 1.0015408
## sigma_actor[4] 1.5816136 0.6261912 0.83713204 2.7226177 1144.9613 0.9986853
## sigma_block[1] 0.4105534 0.3218220 0.03926324 1.0160577  934.2576 0.9997610
## sigma_block[2] 0.4438367 0.3565338 0.03695063 1.0760632  800.8017 1.0041905
## sigma_block[3] 0.2937417 0.2602322 0.02224211 0.7750928 1490.4543 0.9990829
## sigma_block[4] 0.4573700 0.3647852 0.03589410 1.1118910 1023.5617 1.0047709
```

```r
WAIC(m14.3)
```

```
##       WAIC      lppd  penalty  std_err
## 1 545.2385 -245.5763 27.04299 19.73106
```


```r
## R code 14.22
# compute mean for each actor in each treatment
pl <- by( d$pulled_left , list( d$actor , d$treatment ) , mean )

# generate posterior predictions using link
datp <- list(
    actor=rep(1:7,each=4) ,
    tid=rep(1:4,times=7) ,
    block_id=rep(5,times=4*7) )
p_post <- link( m14.3 , data=datp )
p_mu <- apply( p_post , 2 , mean )
p_ci <- apply( p_post , 2 , PI )

# set up plot
plot( NULL , xlim=c(1,28) , ylim=c(0,1) , xlab="" ,
    ylab="proportion left lever" , xaxt="n" , yaxt="n" )
axis( 2 , at=c(0,0.5,1) , labels=c(0,0.5,1) )
abline( h=0.5 , lty=2 )
for ( j in 1:7 ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
for ( j in 1:7 ) text( (j-1)*4+2.5 , 1.1 , concat("actor ",j) , xpd=TRUE )

xo <- 0.1 # offset distance to stagger raw data and predictions
# raw data
for ( j in (1:7)[-2] ) {
    lines( (j-1)*4+c(1,3)-xo , pl[j,c(1,3)] , lwd=2 , col=rangi2 )
    lines( (j-1)*4+c(2,4)-xo , pl[j,c(2,4)] , lwd=2 , col=rangi2 )
}
points( 1:28-xo , t(pl) , pch=16 , col="white" , cex=1.7 )
points( 1:28-xo , t(pl) , pch=c(1,1,16,16) , col=rangi2 , lwd=2 )

yoff <- 0.175
text( 1-xo , pl[1,1]-yoff , "R/N" , pos=1 , cex=0.8 )
text( 2-xo , pl[1,2]+yoff , "L/N" , pos=3 , cex=0.8 )
text( 3-xo , pl[1,3]-yoff , "R/P" , pos=1 , cex=0.8 )
text( 4-xo , pl[1,4]+yoff , "L/P" , pos=3 , cex=0.8 )

# posterior predictions
for ( j in (1:7)[-2] ) {
    lines( (j-1)*4+c(1,3)+xo , p_mu[(j-1)*4+c(1,3)] , lwd=2 )
    lines( (j-1)*4+c(2,4)+xo , p_mu[(j-1)*4+c(2,4)] , lwd=2 )
}
for ( i in 1:28 ) lines( c(i,i)+xo , p_ci[,i] , lwd=1 )
points( 1:28+xo , p_mu , pch=16 , col="white" , cex=1.3 )
points( 1:28+xo , p_mu , pch=c(1,1,16,16) )
```

![](chapter14-2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## 14.3. Instrumental variables and front doors
### 14.3.1. Instrumental variables.


```r
## R code 14.23
set.seed(73)
N <- 500
U_sim <- rnorm( N )
Q_sim <- sample( 1:4 , size=N , replace=TRUE )
E_sim <- rnorm( N , U_sim + Q_sim )
W_sim <- rnorm( N , U_sim + 0*E_sim )
dat_sim <- list(
    W=standardize(W_sim) ,
    E=standardize(E_sim) ,
    Q=standardize(Q_sim) )
```


```r
## R code 14.24
m14.4 <- ulam(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- aW + bEW*E,
        aW ~ dnorm( 0 , 0.2 ),
        bEW ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp( 1 )
    ) , data=dat_sim , chains=4 , cores=4 )
precis( m14.4 )
```

```
##               mean         sd       5.5%      94.5%    n_eff      Rhat
## aW    -0.000242611 0.04064180 -0.0661639 0.06426641 1951.946 1.0026192
## bEW    0.398419108 0.04085636  0.3348046 0.46060691 1944.176 0.9992465
## sigma  0.918903898 0.03026494  0.8719642 0.96936890 1382.326 0.9995344
```


```r
## R code 14.25
m14.5 <- ulam(
    alist(
        c(W,E) ~ multi_normal( c(muW,muE) , Rho , Sigma ),
        muW <- aW + bEW*E,
        muE <- aE + bQE*Q,
        c(aW,aE) ~ normal( 0 , 0.2 ),
        c(bEW,bQE) ~ normal( 0 , 0.5 ),
        Rho ~ lkj_corr( 2 ),
        Sigma ~ exponential( 1 )
    ), data=dat_sim , chains=4 , cores=4 )
precis( m14.5 , depth=3 )
```

```
##                   mean           sd        5.5%      94.5%     n_eff      Rhat
## aE       -0.0009462924 3.649156e-02 -0.05861398 0.05569336 1378.3726 1.0005029
## aW       -0.0001773620 4.443351e-02 -0.07158054 0.07050074 1563.2355 0.9989986
## bQE       0.5871297537 3.500801e-02  0.53035407 0.64314854 1111.2260 1.0006733
## bEW      -0.0512556884 7.579855e-02 -0.17192313 0.06818140  716.1019 1.0041687
## Rho[1,1]  1.0000000000 0.000000e+00  1.00000000 1.00000000       NaN       NaN
## Rho[1,2]  0.5434407821 5.193702e-02  0.45610859 0.62520270  730.5277 1.0035508
## Rho[2,1]  0.5434407821 5.193702e-02  0.45610859 0.62520270  730.5277 1.0035508
## Rho[2,2]  1.0000000000 7.243836e-17  1.00000000 1.00000000 1843.4778 0.9979980
## Sigma[1]  1.0244726392 4.702611e-02  0.95610416 1.10487092  729.9842 1.0030199
## Sigma[2]  0.8084673357 2.591288e-02  0.76870509 0.85030113 1586.1319 0.9989154
```


```r
## R code 14.26
m14.4x <- ulam( m14.4 , data=dat_sim , chains=4 , cores=4 )
m14.5x <- ulam( m14.5 , data=dat_sim , chains=4 , cores=4 )

## R code 14.27
set.seed(73)
N <- 500
U_sim <- rnorm( N )
Q_sim <- sample( 1:4 , size=N , replace=TRUE )
E_sim <- rnorm( N , U_sim + Q_sim )
W_sim <- rnorm( N , -U_sim + 0.2*E_sim )
dat_sim <- list(
    W=standardize(W_sim) ,
    E=standardize(E_sim) ,
    Q=standardize(Q_sim) )
```


```r
## R code 14.24
m14.4_2 <- ulam(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- aW + bEW*E,
        aW ~ dnorm( 0 , 0.2 ),
        bEW ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp( 1 )
    ) , data=dat_sim , chains=4 , cores=4 )
```

```
## recompiling to avoid crashing R session
```

```r
precis( m14.4_2 )
```

```
##                mean         sd        5.5%       94.5%    n_eff     Rhat
## aW     8.972189e-05 0.04347848 -0.07012275  0.06980247 1819.994 1.001898
## bEW   -1.140880e-01 0.04335801 -0.18218471 -0.04707266 1793.423 1.001890
## sigma  9.962962e-01 0.03283347  0.94538285  1.04936165 1828.929 1.000204
```


```r
## R code 14.25
m14.5_2 <- ulam(
    alist(
        c(W,E) ~ multi_normal( c(muW,muE) , Rho , Sigma ),
        muW <- aW + bEW*E,
        muE <- aE + bQE*Q,
        c(aW,aE) ~ normal( 0 , 0.2 ),
        c(bEW,bQE) ~ normal( 0 , 0.5 ),
        Rho ~ lkj_corr( 2 ),
        Sigma ~ exponential( 1 )
    ), data=dat_sim , chains=4 , cores=4 )
```

```
## recompiling to avoid crashing R session
```

```r
precis( m14.5_2 , depth=3 )
```

```
##                   mean           sd        5.5%       94.5%    n_eff      Rhat
## aE        0.0008725869 3.395860e-02 -0.05436436  0.05524612 1653.225 0.9999025
## aW       -0.0008395781 4.654696e-02 -0.07509974  0.07289912 1695.251 0.9999092
## bQE       0.5891700544 3.611664e-02  0.53007611  0.64609509 1631.651 1.0009537
## bEW       0.2870970675 8.289196e-02  0.15915598  0.42220343 1023.494 1.0028381
## Rho[1,1]  1.0000000000 0.000000e+00  1.00000000  1.00000000      NaN       NaN
## Rho[1,2] -0.4633926156 5.959401e-02 -0.55671748 -0.36375344 1025.656 1.0022117
## Rho[2,1] -0.4633926156 5.959401e-02 -0.55671748 -0.36375344 1025.656 1.0022117
## Rho[2,2]  1.0000000000 8.533531e-17  1.00000000  1.00000000 1683.612 0.9979980
## Sigma[1]  1.0762408256 4.666720e-02  1.00663944  1.15370109 1073.198 0.9994651
## Sigma[2]  0.8099578676 2.662630e-02  0.76900216  0.85455436 1517.785 0.9994101
```


```r
## R code 14.28
library(dagitty)
dagIV <- dagitty( "dag{
    E -> W
    E <- U -> W
    Q -> E
}")
instrumentalVariables( dagIV , exposure="E" , outcome="W" )
```

```
##  Q
```

### 14.3.2. Front-door criterion.

## 14.7. Practice

#### 14M3. Re-estimate the varying slopes model for the UCBadmit data, now using a non-centered parameterization. Compare the efficiency of the forms of the model, using n_eff. Which is better? Which chain sampled faster?


```r
## modify from R code 12.2
library(rethinking)
data(UCBadmit)
d <- UCBadmit
d$gid <- ifelse( d$applicant.gender=="male" , 1L , 2L )
d$dept_id <- coerce_index( d$dept )

dat <- list( A=d$admit , App=d$applications, gid=d$gid , did=d$dept_id )
head(dat)
```

```
## $A
##  [1] 512  89 353  17 120 202 138 131  53  94  22  24
## 
## $App
##  [1] 825 108 560  25 325 593 417 375 191 393 373 341
## 
## $gid
##  [1] 1 2 1 2 1 2 1 2 1 2 1 2
## 
## $did
##  [1] 1 1 2 2 3 3 4 4 5 5 6 6
```

```r
m14M3 <- ulam(
    alist(
        A ~ dbinom( App, p ),
        logit(p) <- a[gid]+a_dept[did],
        a[gid] ~ dnorm( 0 , 1.5 ),
        a_dept[did] ~ dnorm( 0 , 1.5 )
    ), data=dat , chains=4 , cores=4 , log_lik = TRUE)
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
precis(m14M3, depth=2)
```

```
##                 mean        sd       5.5%      94.5%    n_eff     Rhat
## a[1]      -0.5064077 0.5231652 -1.3193195  0.4317579 113.8004 1.038284
## a[2]      -0.4105844 0.5219980 -1.2148134  0.5062906 111.7309 1.038882
## a_dept[1]  1.0880336 0.5245530  0.1609037  1.9034814 113.5110 1.038709
## a_dept[2]  1.0432687 0.5268083  0.1190381  1.8683348 113.1264 1.039081
## a_dept[3] -0.1717109 0.5231228 -1.1253761  0.6364937 116.1088 1.036874
## a_dept[4] -0.2065568 0.5284228 -1.1511796  0.6327985 113.6295 1.038662
## a_dept[5] -0.6448683 0.5266484 -1.5646862  0.1759418 113.7115 1.038536
## a_dept[6] -2.2054594 0.5320180 -3.1369072 -1.3926510 119.8059 1.038085
```

```r
m14M3_noncentered <- ulam(
    alist(
        A ~ dbinom( App, p ),
        logit(p) <- a_bar + a[gid]*sigma_a + a_dept[did]*sigma_d,
        a_bar ~ dnorm( 0 , 1.5 ),
        a[gid] ~ dnorm( 0 , 1.5 ),
        a_dept[did] ~ dnorm( 0 , 1.5 ),
        sigma_a ~ dexp(1),
        sigma_d ~ dexp(1)
    ), data=dat , chains=4 , cores=4 , log_lik = TRUE)
```

```
## Warning: There were 6 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```r
precis(m14M3_noncentered, depth=2)
```

```
##                  mean        sd        5.5%      94.5%     n_eff      Rhat
## a_bar     -0.50906245 0.6314060 -1.51748480  0.4704567  371.5326 1.0043960
## a[1]      -0.41210670 1.1440757 -2.27340587  1.3585117 1057.6661 1.0012691
## a[2]       0.21209378 1.0794520 -1.44466023  1.9226006  959.8455 0.9999065
## a_dept[1]  1.37575940 0.7355592  0.24581919  2.5814415  359.5858 1.0134822
## a_dept[2]  1.32324110 0.7274844  0.21441305  2.4916422  354.0231 1.0132348
## a_dept[3] -0.09937764 0.5660124 -0.98579987  0.7954230  363.1063 1.0036413
## a_dept[4] -0.13783920 0.5689404 -1.04697091  0.7691425  383.6665 1.0034284
## a_dept[5] -0.65763529 0.5935557 -1.64628183  0.2754358  399.7298 1.0027363
## a_dept[6] -2.48774648 0.9149094 -4.02167917 -1.0838275  443.9321 1.0109449
## sigma_a    0.27250973 0.3534447  0.01191694  0.9801347  642.4252 1.0010184
## sigma_d    0.94132295 0.3449272  0.54805740  1.5787329  406.2374 1.0197050
```


```r
compare(m14M3,m14M3_noncentered)
```

```
##                       WAIC       SE    dWAIC       dSE    pWAIC    weight
## m14M3_noncentered 107.7078 16.13788 0.000000        NA 8.879626 0.6807714
## m14M3             109.2224 15.94570 1.514638 0.8457569 9.801726 0.3192286
```

```r
plot(compare(m14M3,m14M3_noncentered))
```

![](chapter14-2_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
plot(coeftab(m14M3,m14M3_noncentered))
```

![](chapter14-2_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

> Models look very similar between these two. The non-centered model samples much more efficiently, evidenced by the `n_eff` counts.

#### 1. Revisit the Bangladesh fertility data, data(bangladesh). Fit a model with both varying intercepts by district_id and varying slopes of urban (as a 0/1 indicator variable) by district_id. You are still predicting use.contraception. Inspect the correlation between the intercepts and slopes. Can you interpret this correlation, in terms of what it tells you about the pattern of contraceptive use in the sample? It might help to plot the varying effect estimates for both the intercepts and slopes, by district. Then you can visualize the correlation and maybe more easily think through what it means to have a particular correlation. Plotting predicted proportion of women using contraception, in each district, with urban women on one axis and rural on the other, might also help.

> (1) district: ID number of administrative district each woman resided in

> (2) use.contraception: An indicator (0/1) of whether the woman was using contraception

> (3) urban: An indicator (0/1) of whether the woman lived in a city, as opposed to living in a
rural area


```r
data(bangladesh)
d <- bangladesh
head(d)
```

```
##   woman district use.contraception living.children age.centered urban
## 1     1        1                 0               4      18.4400     1
## 2     2        1                 0               1      -5.5599     1
## 3     3        1                 0               3       1.4400     1
## 4     4        1                 0               4       8.4400     1
## 5     5        1                 0               1     -13.5590     1
## 6     6        1                 0               1     -11.5600     1
```

```r
summary(d)
```

```
##      woman           district     use.contraception living.children
##  Min.   :   1.0   Min.   : 1.00   Min.   :0.0000    Min.   :1.000  
##  1st Qu.: 484.2   1st Qu.:14.00   1st Qu.:0.0000    1st Qu.:1.000  
##  Median : 967.5   Median :29.00   Median :0.0000    Median :3.000  
##  Mean   : 967.5   Mean   :29.35   Mean   :0.3925    Mean   :2.652  
##  3rd Qu.:1450.8   3rd Qu.:45.00   3rd Qu.:1.0000    3rd Qu.:4.000  
##  Max.   :1934.0   Max.   :61.00   Max.   :1.0000    Max.   :4.000  
##   age.centered            urban       
##  Min.   :-13.560000   Min.   :0.0000  
##  1st Qu.: -7.559900   1st Qu.:0.0000  
##  Median : -1.559900   Median :0.0000  
##  Mean   :  0.002198   Mean   :0.2906  
##  3rd Qu.:  6.440000   3rd Qu.:1.0000  
##  Max.   : 19.440000   Max.   :1.0000
```

```r
d$did <- as.integer( as.factor(d$district) )

dat_list <- list(
C = d$use.contraception,
D = d$did,
U = d$urban)
summary(dat_list)
```

```
##   Length Class  Mode   
## C 1934   -none- numeric
## D 1934   -none- numeric
## U 1934   -none- numeric
```

```r
str(dat_list)
```

```
## List of 3
##  $ C: int [1:1934] 0 0 0 0 0 0 0 0 0 0 ...
##  $ D: int [1:1934] 1 1 1 1 1 1 1 1 1 1 ...
##  $ U: int [1:1934] 1 1 1 1 1 1 1 1 1 1 ...
```

```r
m14_1 <- ulam(
  alist(
    C ~ dbinom( 1 , p ),
    logit(p) <- a[D] + b[D]*U,
    c(a,b)[D] ~ multi_normal( c(a_bar,b_bar) , Rho , Sigma ),
    a_bar ~ normal(0,1),
    b_bar ~ normal(0,1),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
    ) , data=dat_list , chains=4 , cores=4 , log_lik = TRUE)
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
precis(m14_1, depth=3 , pars=c("Rho","Sigma"))
```

```
##                mean           sd       5.5%      94.5%     n_eff      Rhat
## Rho[1,1]  1.0000000 0.000000e+00  1.0000000  1.0000000       NaN       NaN
## Rho[1,2] -0.6554806 1.551907e-01 -0.8648101 -0.3750117  499.1297 1.0058996
## Rho[2,1] -0.6554806 1.551907e-01 -0.8648101 -0.3750117  499.1297 1.0058996
## Rho[2,2]  1.0000000 5.716652e-17  1.0000000  1.0000000 1859.7799 0.9979980
## Sigma[1]  0.5812115 9.626065e-02  0.4398744  0.7447574  592.7369 0.9999326
## Sigma[2]  0.7900806 1.994913e-01  0.4860201  1.1241375  242.7275 1.0129656
```


```r
## modify from R code 14.14

# extract posterior means of partially pooled estimates
post <- extract.samples(m14_1)
summary(post)
```

```
##       Length Class  Mode   
## b     120000 -none- numeric
## a     120000 -none- numeric
## a_bar   2000 -none- numeric
## b_bar   2000 -none- numeric
## Rho     8000 -none- numeric
## Sigma   4000 -none- numeric
```

```r
a2 <- apply( post$a , 2 , mean )
b2 <- apply( post$b , 2 , mean )

# plot both and connect with lines
plot( a2 , b2 , xlab="intercept" , ylab="slope" ,
    pch=16 , col=rangi2 )

## R code 14.15
# compute posterior mean bivariate Gaussian
Mu <- c( mean(post$a_bar) , mean(post$b_bar) )
rho <- apply( post$Rho , 2:3 , mean )
s <- apply( post$Sigma , 2 , mean )
S <- diag(s) %*% rho %*% diag(s)

# draw contours
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
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
    lines(ellipse(S,centre=Mu,level=l),
        col=col.alpha("black",0.5))
```

![](chapter14-2_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

> negative correlation


```r
u0 <- inv_logit( a2 )
u1 <- inv_logit( a2 + b2 )
## change from R code 14.9
plot( u0 , u1 , xlim=c(0,1) , pch=16 , col=rangi2 , ylim=c(0,1) , xlab="urban: 0" , ylab="urban: 1" )
```

![](chapter14-2_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

> Urban areas centered at 0.5, rural areas are mostly below 0.5

#### 2. Now consider the predictor variables age.centered and living.children, also contained in data(bangladesh). Suppose that age influences contraceptive use (changing attitudes) and number of children (older people have had more time to have kids). Number of children may also directly influence contraceptive use. Draw a DAG that reflects these hypothetical relationships. Then build models needed to evaluate the DAG. You will need at least two models. Retain district and urban, as in Problem 1. What do you conclude about the causal influence of age and children?


```r
library(rethinking)
library(dagitty)
library(ggdag)
```

```
## 
## Attaching package: 'ggdag'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     expand_scale
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```r
# age = A, number of children = N, contraceptive use = C
dag <- dagitty("dag{A -> N -> C <- A}")

ggdag(dag, layout = "circle")
```

![](chapter14-2_files/figure-html/unnamed-chunk-22-1.png)<!-- -->


```r
dat_list$children <- scale( d$living.children )
dat_list$age <- scale( d$age.centered )
str(dat_list)
```

```
## List of 5
##  $ C       : int [1:1934] 0 0 0 0 0 0 0 0 0 0 ...
##  $ D       : int [1:1934] 1 1 1 1 1 1 1 1 1 1 ...
##  $ U       : int [1:1934] 1 1 1 1 1 1 1 1 1 1 ...
##  $ children: num [1:1934, 1] 1.08 -1.33 0.28 1.08 -1.33 ...
##   ..- attr(*, "scaled:center")= num 2.65
##   ..- attr(*, "scaled:scale")= num 1.24
##  $ age     : num [1:1934, 1] 2.046 -0.617 0.16 0.936 -1.505 ...
##   ..- attr(*, "scaled:center")= num 0.0022
##   ..- attr(*, "scaled:scale")= num 9.01
```


```r
m14_2_A <- ulam(
  alist(
    C ~ dbinom( 1 , p ),
    logit(p) <- a[D] + b[D]*U + bA*age,
    c(a,b)[D] ~ multi_normal( c(a_bar,b_bar) , Rho , Sigma ),
    a_bar ~ normal(0,1),
    b_bar ~ normal(0,1),
    bA ~ normal(0,1),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
    ) , data=dat_list , chains=4 , cores=4 , log_lik = TRUE)
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
precis(m14_2_A)
```

```
## 126 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##              mean         sd         5.5%      94.5%     n_eff      Rhat
## a_bar -0.70706097 0.10115705 -0.869891748 -0.5453902 1600.0940 0.9991011
## b_bar  0.69346026 0.16944793  0.423520461  0.9643340  941.4413 1.0028867
## bA     0.08577172 0.04906308  0.006448286  0.1636043 3479.7745 0.9998846
```


```r
m14_2_N <- ulam(
  alist(
    C ~ dbinom( 1 , p ),
    logit(p) <- a[D] + b[D]*U + bN*children,
    c(a,b)[D] ~ multi_normal( c(a_bar,b_bar) , Rho , Sigma ),
    a_bar ~ normal(0,1),
    b_bar ~ normal(0,1),
    bN ~ normal(0,1),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
    ) , data=dat_list , chains=4 , cores=4 , log_lik = TRUE)
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
precis(m14_2_N)
```

```
## 126 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##             mean        sd       5.5%      94.5%    n_eff      Rhat
## a_bar -0.7329938 0.1046680 -0.9052016 -0.5717538 1485.275 0.9992384
## b_bar  0.7307053 0.1712786  0.4585480  1.0013288 1059.176 0.9988646
## bN     0.3318769 0.0528917  0.2465572  0.4173497 3125.838 0.9990371
```


```r
m14_2_AN <- ulam(
  alist(
    C ~ dbinom( 1 , p ),
    logit(p) <- a[D] + b[D]*U + bA*age + bN*children,
    c(a,b)[D] ~ multi_normal( c(a_bar,b_bar) , Rho , Sigma ),
    a_bar ~ normal(0,1),
    b_bar ~ normal(0,1),
    bA ~ normal(0,1),
    bN ~ normal(0,1),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
    ) , data=dat_list , chains=4 , cores=4 , log_lik = TRUE)
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
precis(m14_2_AN)
```

```
## 126 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##             mean         sd       5.5%      94.5%     n_eff      Rhat
## a_bar -0.7384125 0.10773434 -0.9109091 -0.5718907  992.2761 1.0010241
## b_bar  0.7549581 0.16636825  0.4929528  1.0287640  683.9958 1.0065616
## bA    -0.2765666 0.07102950 -0.3910823 -0.1613399 1873.4339 1.0008842
## bN     0.5276055 0.07322961  0.4109051  0.6487713 1946.8364 0.9988404
```


```r
compare(m14_2_A,m14_2_N,m14_2_AN)
```

```
##              WAIC       SE    dWAIC       dSE    pWAIC       weight
## m14_2_AN 2412.492 30.69770  0.00000        NA 54.93706 9.993846e-01
## m14_2_N  2427.277 29.96797 14.78520  7.589644 54.25824 6.154123e-04
## m14_2_A  2467.395 28.12411 54.90305 14.596464 53.13880 1.195876e-12
```

```r
plot(compare(m14_2_A,m14_2_N,m14_2_AN))
```

![](chapter14-2_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

