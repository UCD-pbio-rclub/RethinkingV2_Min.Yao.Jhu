---
title: "chapter14-6"
author: "Min-Yao"
date: "2020/6/21"
output: 
  html_document: 
    keep_md: yes
---

### 14.5.2. Example: Phylogenetic distance.


```r
## R code 14.46
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
data(Primates301)
data(Primates301_nex)

# plot it using ape package - install.packages('ape') if needed
library(ape)
```

```
## 
## Attaching package: 'ape'
```

```
## The following object is masked from 'package:dagitty':
## 
##     edges
```

```r
plot( ladderize(Primates301_nex) , type="fan" , font=1 , no.margin=TRUE ,
    label.offset=1 , cex=0.5 )
```

![](chapter14-6_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


```r
## R code 14.47
d <- Primates301
d$name <- as.character(d$name)
dstan <- d[ complete.cases( d$group_size , d$body , d$brain ) , ]
spp_obs <- dstan$name
```


```r
## R code 14.48
dat_list <- list(
    N_spp = nrow(dstan),
    M = standardize(log(dstan$body)),
    B = standardize(log(dstan$brain)),
    G = standardize(log(dstan$group_size)),
    Imat = diag( nrow(dstan) )
)

m14.8 <- ulam(
    alist(
        B ~ multi_normal( mu , SIGMA ),
        mu <- a + bM*M + bG*G,
        matrix[N_spp,N_spp]: SIGMA <- Imat * sigma_sq,
        a ~ normal( 0 , 1 ),
        c(bM,bG) ~ normal( 0 , 0.5 ),
        sigma_sq ~ exponential( 1 )
    ), data=dat_list , chains=4 , cores=4 )
precis( m14.8 )
```

```
##                  mean          sd        5.5%      94.5%    n_eff     Rhat4
## a        0.0001686134 0.016961917 -0.02674190 0.02753967 1785.938 1.0003152
## bG       0.1243065005 0.023603290  0.08689848 0.16007944 1539.803 0.9989565
## bM       0.8920718555 0.023225421  0.85351254 0.92992531 1392.665 0.9990645
## sigma_sq 0.0471773892 0.005515466  0.03923991 0.05703267 1455.366 0.9996308
```


```r
## R code 14.49
library(ape)
tree_trimmed <- keep.tip( Primates301_nex, spp_obs )
Rbm <- corBrownian( phy=tree_trimmed )
V <- vcv(Rbm)
```

```
## Warning in Initialize.corPhyl(phy, dummy.df): No covariate specified, species
## will be taken as ordered in the data frame. To avoid this message, specify a
## covariate containing the species names with the 'form' argument.
```

```r
Dmat <- cophenetic( tree_trimmed )
plot( Dmat , V , xlab="phylogenetic distance" , ylab="covariance" )
```

![](chapter14-6_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
image(V)
```

![](chapter14-6_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
image(Dmat)
```

![](chapter14-6_files/figure-html/unnamed-chunk-4-3.png)<!-- -->


```r
## R code 14.50
# put species in right order
dat_list$V <- V[ spp_obs , spp_obs ]
# convert to correlation matrix
dat_list$R <- dat_list$V / max(V)

# Brownian motion model
m14.9 <- ulam(
    alist(
        B ~ multi_normal( mu , SIGMA ),
        mu <- a + bM*M + bG*G,
        matrix[N_spp,N_spp]: SIGMA <- R * sigma_sq,
        a ~ normal( 0 , 1 ),
        c(bM,bG) ~ normal( 0 , 0.5 ),
        sigma_sq ~ exponential( 1 )
    ), data=dat_list , chains=4 , cores=4 )
precis( m14.9 )
```

```
##                 mean         sd        5.5%      94.5%    n_eff     Rhat4
## a        -0.19861857 0.16252594 -0.45149861 0.06397288 2388.262 0.9988464
## bG       -0.01205195 0.01949293 -0.04296826 0.01985504 2304.391 1.0003526
## bM        0.69851185 0.03629908  0.63945205 0.75606019 2072.027 1.0024928
## sigma_sq  0.16186282 0.01929674  0.13409615 0.19608579 2069.636 1.0000592
```


```r
## R code 14.51
# add scaled and reordered distance matrix
dat_list$Dmat <- Dmat[ spp_obs , spp_obs ] / max(Dmat)

m14.10 <- ulam(
    alist(
        B ~ multi_normal( mu , SIGMA ),
        mu <- a + bM*M + bG*G,
        matrix[N_spp,N_spp]: SIGMA <- cov_GPL1( Dmat , etasq , rhosq , 0.01 ),
        a ~ normal(0,1),
        c(bM,bG) ~ normal(0,0.5),
        etasq ~ half_normal(1,0.25),
        rhosq ~ half_normal(3,0.25)
    ), data=dat_list , chains=4 , cores=4 )
precis( m14.10 )
```

```
##              mean          sd        5.5%      94.5%    n_eff     Rhat4
## a     -0.06522133 0.075999931 -0.18966837 0.05699875 2214.216 0.9996907
## bG     0.05065639 0.023211474  0.01388543 0.08809617 2176.415 0.9988262
## bM     0.83318011 0.029733867  0.78618944 0.88209869 1980.906 1.0000068
## etasq  0.03516917 0.006623013  0.02597261 0.04667312 1893.130 0.9993151
## rhosq  2.80089286 0.241190199  2.41805568 3.17905417 2141.191 1.0013954
```


```r
## R code 14.52
post <- extract.samples(m14.10)
plot( NULL , xlim=c(0,max(dat_list$Dmat)) , ylim=c(0,1.5) ,
    xlab="phylogenetic distance" , ylab="covariance" )

# posterior
for ( i in 1:30 )
    curve( post$etasq[i]*exp(-post$rhosq[i]*x) , add=TRUE , col=rangi2 )

# prior mean and 89% interval
eta <- abs(rnorm(1e3,1,0.25))
rho <- abs(rnorm(1e3,3,0.25))
d_seq <- seq(from=0,to=1,length.out=50)
K <- sapply( d_seq , function(x) eta*exp(-rho*x) )
lines( d_seq , colMeans(K) , lwd=2 )
shade( apply(K,2,PI) , d_seq )
```

```
## Warning in if (class(object) == "formula") {: 條件的長度 > 1，因此只能用其第一元
## 素
```

```
## Warning in if (class(object) == "density") {: 條件的長度 > 1，因此只能用其第一元
## 素
```

```
## Warning in if (class(object) == "matrix" & length(dim(object)) == 2) {: 條件的長
## 度 > 1，因此只能用其第一元素
```

```
## Warning in if (class(object) == "matrix") {: 條件的長度 > 1，因此只能用其第一元
## 素
```

```r
text( 0.5 , 0.5 , "prior" )
text( 0.2 , 0.1 , "posterior" , col=rangi2 )
```

![](chapter14-6_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## 14.7. Practice

### 14M5. Modify the phylogenetic distance example to use group size as the outcome and brain size as a predictor. Assuming brain size influences group size, what is your estimate of the effect? How does phylogeny influence the estimate?

### 14H2. Varying effects models are useful for modeling time series, as well as spatial clustering. In a time series, the observations cluster by entities that have continuity through time, such as individuals. Since observations within individuals are likely highly correlated, the multilevel structure can help quite a lot. You’ll use the data in data(Oxboys), which is 234 height measurements on 26 boys from an Oxford Boys Club (I think these were like youth athletic leagues?), at 9 different ages (centered and standardized) per boy. You’ll be interested in predicting height, using age, clustered by Subject (individual boy).
### Fit a model with varying intercepts and slopes (on age), clustered by Subject. Present and interpret the parameter estimates. Which varying effect contributes more variation to the heights, the intercept or the slope?

### 14H3. Now consider the correlation between the varying intercepts and slopes. Can you explain its value? How would this estimated correlation influence your predictions about a new sample of boys?


```r
## R code 14.53
#S <- matrix( c( sa^2 , sa*sb*rho , sa*sb*rho , sb^2 ) , nrow=2 )
```

