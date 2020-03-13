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
## Warning: There were 21 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
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
##                     mean        sd       5.5%    94.5%    n_eff     Rhat
## sigma_actor[1] 1.4130699 0.5070820 0.79378483 2.287950 873.8630 1.002899
## sigma_actor[2] 0.9071626 0.4073926 0.39303427 1.641304 658.3038 1.003607
## sigma_actor[3] 1.8502142 0.5722425 1.10494639 2.841244 782.0665 1.011019
## sigma_actor[4] 1.5984014 0.6084718 0.83224354 2.625827 730.3286 1.003706
## sigma_block[1] 0.4484111 0.3186458 0.06365281 1.003705 366.6279 1.012968
## sigma_block[2] 0.4587676 0.3841681 0.05535773 1.071415 134.6001 1.030067
## sigma_block[3] 0.3080470 0.2723701 0.02746726 0.800175 237.6404 1.003646
## sigma_block[4] 0.5076786 0.3703358 0.06624749 1.157953 323.7654 1.009450
```

```r
precis( m14.3 , depth=2 , pars=c("sigma_actor","sigma_block") )
```

```
##                     mean        sd       5.5%     94.5%     n_eff      Rhat
## sigma_actor[1] 1.3652226 0.4833670 0.76576220 2.2433966  847.0636 1.0021813
## sigma_actor[2] 0.9092769 0.4027059 0.40940633 1.6392240  897.8854 1.0020970
## sigma_actor[3] 1.8355830 0.5839931 1.11166414 2.8705234 1328.7774 0.9992336
## sigma_actor[4] 1.5527642 0.6082554 0.81787373 2.6108107 1237.8075 1.0013100
## sigma_block[1] 0.4148750 0.3436741 0.03244216 1.0355886  917.7320 1.0010286
## sigma_block[2] 0.4488272 0.3462758 0.04893881 1.0554674  994.4951 1.0011377
## sigma_block[3] 0.2988193 0.2707048 0.02123616 0.8072268 1599.2003 1.0012349
## sigma_block[4] 0.4811025 0.3812228 0.04046248 1.1802591  970.5590 1.0030107
```

```r
WAIC(m14.3)
```

```
##       WAIC      lppd  penalty  std_err
## 1 544.9845 -245.5176 26.97462 19.71861
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
dat <- list( A=d$admit , N=d$applications , gid=d$gid )
m12.1 <- ulam(
    alist(
        A ~ dbetabinom( N , pbar , theta ),
        logit(pbar) <- a[gid],
        a[gid] ~ dnorm( 0 , 1.5 ),
        theta ~ dexp(1)
    ), data=dat , chains=4 )
```

```
## 
## SAMPLING FOR MODEL '379c3c9629d2ce418bfe29904221f6bc' NOW (CHAIN 1).
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
## Chain 1:  Elapsed Time: 0.076 seconds (Warm-up)
## Chain 1:                0.06 seconds (Sampling)
## Chain 1:                0.136 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '379c3c9629d2ce418bfe29904221f6bc' NOW (CHAIN 2).
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
## Chain 2:  Elapsed Time: 0.079 seconds (Warm-up)
## Chain 2:                0.059 seconds (Sampling)
## Chain 2:                0.138 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '379c3c9629d2ce418bfe29904221f6bc' NOW (CHAIN 3).
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
## Chain 3:  Elapsed Time: 0.073 seconds (Warm-up)
## Chain 3:                0.065 seconds (Sampling)
## Chain 3:                0.138 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '379c3c9629d2ce418bfe29904221f6bc' NOW (CHAIN 4).
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
## Chain 4:  Elapsed Time: 0.081 seconds (Warm-up)
## Chain 4:                0.065 seconds (Sampling)
## Chain 4:                0.146 seconds (Total)
## Chain 4:
```


#### 1. Revisit the Bangladesh fertility data, data(bangladesh). Fit a model with both varying intercepts by district_id and varying slopes of urban (as a 0/1 indicator variable) by district_id. You are still predicting use.contraception. Inspect the correlation between the intercepts and slopes. Can you interpret this correlation, in terms of what it tells you about the pattern of contraceptive use in the sample? It might help to plot the varying eff ect estimates for both the intercepts and slopes, by district. Th en you can visualize the correlation and maybe more easily think through what it means to have a particular correlation. Plotting predicted proportion of women using contraception, in each district, with urban women on one axis and rural on the other, might also help.




#### 2. Now consider the predictor variables age.centered and living.children, also contained in data(bangladesh). Suppose that age infl uences contraceptive use (changing attitudes) and number of children (older people have had more time to have kids). Number of children may also directly infl uence contraceptive use. Draw a DAG that refl ects these hypothetical relationships. Th en build models needed to evaluate the DAG. You will need at least two models. Retain district and urban, as in Problem 1. What do you conclude about the causal infl uence of age and children?



