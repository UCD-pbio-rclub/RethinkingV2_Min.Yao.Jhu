---
title: "chapter14-3"
author: "Min-Yao"
date: "2020/3/19"
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
library(foreign)
qob = read.dta("NEW7080.dta")
head(qob)
```

```
##   v1    v2 v3 v4 v5 v6 v7        v8       v9 v10 v11 v12 v13 v14 v15 v16 v17
## 1 40 40.50  1 11  0  0 13  8.955383 5.023558   1   0   0   0   0   0  70  54
## 2 41 41.00  1 12  0  0 14  8.993365 5.061540   1   0   0   0   0   0  70  10
## 3 41 41.50  1 12  0  0 14  9.310141 5.378315   1   0   0   0   0   0  70  39
## 4 46 46.25  1 12  0  0 14  9.110465 5.178639   1   0   0   0   0   0  70  42
## 5 46 46.00  1 16  0  0 18 10.310601 6.378776   1   0   0   0   0   0  70  47
## 6 47 47.00  1 12  0  0 14  8.929236 4.997411   0   0   0   0   0   0  70  42
##   v18 v19 v20 v21 v22 v23 v24 v25 v26  v27
## 1   3   0   1   1  10   5   0   0   0 1929
## 2   1   0   0   1  10   5   0   0   0 1929
## 3   3   0   0   1  10   5   0   0   0 1928
## 4   4   0   0   1  10   5   0   0   0 1923
## 5   1   0   0   1  10   5   0   0   1 1924
## 6   1   0   1   1  10   5   0   0   1 1923
```


```r
library(tidyverse)
```

```
## -- Attaching packages ---------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## √ tibble  2.1.3     √ dplyr   0.8.3
## √ tidyr   1.0.0     √ stringr 1.4.0
## √ readr   1.3.1     √ forcats 0.4.0
## √ purrr   0.3.3
```

```
## -- Conflicts ------------------------------------------------------------------- tidyverse_conflicts() --
## x tidyr::extract() masks rstan::extract()
## x dplyr::filter()  masks stats::filter()
## x dplyr::lag()     masks stats::lag()
## x purrr::map()     masks rethinking::map()
```

```r
library(dplyr)
qob <- as_tibble(qob)
head(qob)
```

```
## # A tibble: 6 x 27
##      v1    v2    v3    v4    v5    v6    v7    v8    v9   v10   v11   v12   v13
##   <int> <dbl> <int> <int> <int> <int> <int> <dbl> <dbl> <int> <int> <int> <int>
## 1    40  40.5     1    11     0     0    13  8.96  5.02     1     0     0     0
## 2    41  41       1    12     0     0    14  8.99  5.06     1     0     0     0
## 3    41  41.5     1    12     0     0    14  9.31  5.38     1     0     0     0
## 4    46  46.2     1    12     0     0    14  9.11  5.18     1     0     0     0
## 5    46  46       1    16     0     0    18 10.3   6.38     1     0     0     0
## 6    47  47       1    12     0     0    14  8.93  5.00     0     0     0     0
## # ... with 14 more variables: v14 <int>, v15 <int>, v16 <int>, v17 <int>,
## #   v18 <int>, v19 <int>, v20 <int>, v21 <int>, v22 <int>, v23 <int>,
## #   v24 <int>, v25 <int>, v26 <int>, v27 <int>
```

```r
qob_new = qob %>% 
  rename(
    AGE = v1,
    AGEQ = v2,
    EDUC = v4,
    ENOCENT = v5,
    ESOCENT = v6,
    LWKLYWGE = v9,
    MARRIED = v10,
    MIDATL = v11,
    MT = v12,
    NEWENG = v13,
    CENSUS = v16,
    QOB = v18,
    RACE = v19,
    SMSA = v20,
    SOATL = v21,
    WNOCENT = v24,
    WSOCENT = v25,
    YOB = v27
    )
head(qob_new)
```

```
## # A tibble: 6 x 27
##     AGE  AGEQ    v3  EDUC ENOCENT ESOCENT    v7    v8 LWKLYWGE MARRIED MIDATL
##   <int> <dbl> <int> <int>   <int>   <int> <int> <dbl>    <dbl>   <int>  <int>
## 1    40  40.5     1    11       0       0    13  8.96     5.02       1      0
## 2    41  41       1    12       0       0    14  8.99     5.06       1      0
## 3    41  41.5     1    12       0       0    14  9.31     5.38       1      0
## 4    46  46.2     1    12       0       0    14  9.11     5.18       1      0
## 5    46  46       1    16       0       0    18 10.3      6.38       1      0
## 6    47  47       1    12       0       0    14  8.93     5.00       0      0
## # ... with 16 more variables: MT <int>, NEWENG <int>, v14 <int>, v15 <int>,
## #   CENSUS <int>, v17 <int>, QOB <int>, RACE <int>, SMSA <int>, SOATL <int>,
## #   v22 <int>, v23 <int>, WNOCENT <int>, WSOCENT <int>, v26 <int>, YOB <int>
```

```r
summary(qob_new)
```

```
##       AGE             AGEQ               v3             EDUC      
##  Min.   :30.00   Min.   :  40.25   Min.   :0.000   Min.   : 0.00  
##  1st Qu.:35.00   1st Qu.:1930.50   1st Qu.:2.000   1st Qu.:12.00  
##  Median :40.00   Median :1935.25   Median :2.000   Median :12.00  
##  Mean   :39.94   Mean   :1498.73   Mean   :1.882   Mean   :12.84  
##  3rd Qu.:45.00   3rd Qu.:1942.00   3rd Qu.:2.000   3rd Qu.:15.00  
##  Max.   :50.00   Max.   :1950.00   Max.   :3.000   Max.   :20.00  
##                                                                   
##     ENOCENT          ESOCENT              v7              v8        
##  Min.   :0.0000   Min.   :0.00000   Min.   : 0.00   Min.   : 1.609  
##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:14.00   1st Qu.: 9.211  
##  Median :0.0000   Median :0.00000   Median :14.00   Median : 9.681  
##  Mean   :0.2022   Mean   :0.06375   Mean   :15.02   Mean   : 9.541  
##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:18.00   3rd Qu.:10.035  
##  Max.   :1.0000   Max.   :1.00000   Max.   :22.00   Max.   :11.225  
##                                                                     
##     LWKLYWGE         MARRIED           MIDATL             MT         
##  Min.   :-2.342   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
##  1st Qu.: 5.303   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.00000  
##  Median : 5.790   Median :1.0000   Median :0.0000   Median :0.00000  
##  Mean   : 5.680   Mean   :0.8429   Mean   :0.1651   Mean   :0.04817  
##  3rd Qu.: 6.126   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.00000  
##  Max.   :11.225   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000  
##                                                                      
##      NEWENG             v14                v15             CENSUS     
##  Min.   :0.00000   Min.   :0.000000   Min.   :0.0000   Min.   :70.00  
##  1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:80.00  
##  Median :0.00000   Median :0.000000   Median :0.0000   Median :80.00  
##  Mean   :0.05563   Mean   :0.006792   Mean   :0.1279   Mean   :77.68  
##  3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:80.00  
##  Max.   :1.00000   Max.   :1.000000   Max.   :1.0000   Max.   :80.00  
##                                                                       
##       v17            QOB             RACE              SMSA       
##  Min.   : 1.0   Min.   :1.000   Min.   :0.00000   Min.   :0.0000  
##  1st Qu.:19.0   1st Qu.:2.000   1st Qu.:0.00000   1st Qu.:0.0000  
##  Median :34.0   Median :3.000   Median :0.00000   Median :0.0000  
##  Mean   :30.7   Mean   :2.516   Mean   :0.08175   Mean   :0.2128  
##  3rd Qu.:42.0   3rd Qu.:3.000   3rd Qu.:0.00000   3rd Qu.:0.0000  
##  Max.   :99.0   Max.   :4.000   Max.   :1.00000   Max.   :1.0000  
##                                                                   
##      SOATL             v22              v23           WNOCENT       
##  Min.   :0.0000   Min.   : 1.00    Min.   : 0.00   Min.   :0.00000  
##  1st Qu.:0.0000   1st Qu.:17.00    1st Qu.:20.00   1st Qu.:0.00000  
##  Median :0.0000   Median :29.00    Median :52.00   Median :0.00000  
##  Mean   :0.1652   Mean   :28.79    Mean   :38.56   Mean   :0.07651  
##  3rd Qu.:0.0000   3rd Qu.:41.00    3rd Qu.:52.00   3rd Qu.:0.00000  
##  Max.   :1.0000   Max.   :56.00    Max.   :52.00   Max.   :1.00000  
##                   NA's   :164409                                    
##     WSOCENT             v26               YOB        
##  Min.   :0.00000   Min.   :-1.0000   Min.   :  30.0  
##  1st Qu.:0.00000   1st Qu.: 0.0000   1st Qu.:  38.0  
##  Median :0.00000   Median : 0.0000   Median :  44.0  
##  Mean   :0.09549   Mean   : 0.1515   Mean   : 478.6  
##  3rd Qu.:0.00000   3rd Qu.: 0.0000   3rd Qu.:  49.0  
##  Max.   :1.00000   Max.   : 1.0000   Max.   :1929.0  
## 
```

```r
str(qob_new)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1063634 obs. of  27 variables:
##  $ AGE     : int  40 41 41 46 46 47 48 41 47 44 ...
##  $ AGEQ    : num  40.5 41 41.5 46.2 46 ...
##  $ v3      : int  1 1 1 1 1 1 1 2 1 2 ...
##  $ EDUC    : int  11 12 12 12 16 12 14 9 12 17 ...
##  $ ENOCENT : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ESOCENT : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ v7      : int  13 14 14 14 18 14 16 12 14 20 ...
##  $ v8      : num  8.96 8.99 9.31 9.11 10.31 ...
##  $ LWKLYWGE: num  5.02 5.06 5.38 5.18 6.38 ...
##  $ MARRIED : int  1 1 1 1 1 0 1 1 1 1 ...
##  $ MIDATL  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ MT      : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ NEWENG  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ v14     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ v15     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CENSUS  : int  70 70 70 70 70 70 70 70 70 70 ...
##  $ v17     : int  54 10 39 42 47 42 42 42 42 37 ...
##  $ QOB     : int  3 1 3 4 1 1 2 2 3 2 ...
##  $ RACE    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ SMSA    : int  1 0 0 0 0 1 1 0 0 0 ...
##  $ SOATL   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ v22     : int  10 10 10 10 10 10 10 10 10 10 ...
##  $ v23     : int  5 5 5 5 5 5 5 5 5 5 ...
##  $ WNOCENT : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ WSOCENT : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ v26     : int  0 0 0 0 1 1 1 0 1 1 ...
##  $ YOB     : int  1929 1929 1928 1923 1924 1923 1921 1928 1922 1925 ...
##  - attr(*, "datalabel")= chr ""
##  - attr(*, "time.stamp")= chr "31 May 2008 00:35"
##  - attr(*, "formats")= chr  "%8.0g" "%9.0g" "%8.0g" "%8.0g" ...
##  - attr(*, "types")= int  251 254 251 251 251 251 251 254 254 251 ...
##  - attr(*, "val.labels")= chr  "" "" "" "" ...
##  - attr(*, "var.labels")= chr  "" "" "" "" ...
##  - attr(*, "version")= int 12
```

```r
qob_new2 <- sample_n(qob_new, 10000)
summary(qob_new2)
```

```
##       AGE             AGEQ               v3             EDUC     
##  Min.   :30.00   Min.   :  40.25   Min.   :0.000   Min.   : 0.0  
##  1st Qu.:34.00   1st Qu.:1930.50   1st Qu.:2.000   1st Qu.:12.0  
##  Median :41.00   Median :1935.25   Median :2.000   Median :12.0  
##  Mean   :39.95   Mean   :1494.01   Mean   :1.879   Mean   :12.8  
##  3rd Qu.:45.00   3rd Qu.:1942.00   3rd Qu.:2.000   3rd Qu.:15.0  
##  Max.   :50.00   Max.   :1950.00   Max.   :3.000   Max.   :20.0  
##                                                                  
##     ENOCENT         ESOCENT             v7              v8        
##  Min.   :0.000   Min.   :0.0000   Min.   : 0.00   Min.   : 1.609  
##  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:14.00   1st Qu.: 9.211  
##  Median :0.000   Median :0.0000   Median :14.00   Median : 9.681  
##  Mean   :0.208   Mean   :0.0698   Mean   :14.98   Mean   : 9.535  
##  3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:18.00   3rd Qu.:10.031  
##  Max.   :1.000   Max.   :1.0000   Max.   :22.00   Max.   :11.225  
##                                                                   
##     LWKLYWGE         MARRIED           MIDATL             MT        
##  Min.   :-2.342   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.: 5.299   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median : 5.784   Median :1.0000   Median :0.0000   Median :0.0000  
##  Mean   : 5.673   Mean   :0.8389   Mean   :0.1604   Mean   :0.0438  
##  3rd Qu.: 6.127   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   : 9.852   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##                                                                     
##      NEWENG            v14              v15             CENSUS     
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :70.00  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:80.00  
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :80.00  
##  Mean   :0.0583   Mean   :0.0065   Mean   :0.1281   Mean   :77.65  
##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:80.00  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :80.00  
##                                                                    
##       v17            QOB             RACE             SMSA       
##  Min.   : 1.0   Min.   :1.000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:19.0   1st Qu.:2.000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :34.0   Median :3.000   Median :0.0000   Median :0.0000  
##  Mean   :30.7   Mean   :2.523   Mean   :0.0825   Mean   :0.2188  
##  3rd Qu.:42.0   3rd Qu.:4.000   3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :99.0   Max.   :4.000   Max.   :1.0000   Max.   :1.0000  
##                                                                  
##      SOATL            v22            v23           WNOCENT      
##  Min.   :0.000   Min.   : 1.0   Min.   : 0.00   Min.   :0.0000  
##  1st Qu.:0.000   1st Qu.:17.0   1st Qu.:19.75   1st Qu.:0.0000  
##  Median :0.000   Median :29.0   Median :52.00   Median :0.0000  
##  Mean   :0.161   Mean   :28.7   Mean   :38.45   Mean   :0.0766  
##  3rd Qu.:0.000   3rd Qu.:41.0   3rd Qu.:52.00   3rd Qu.:0.0000  
##  Max.   :1.000   Max.   :56.0   Max.   :52.00   Max.   :1.0000  
##                  NA's   :1590                                   
##     WSOCENT           v26               YOB        
##  Min.   :0.000   Min.   :-1.0000   Min.   :  30.0  
##  1st Qu.:0.000   1st Qu.: 0.0000   1st Qu.:  38.0  
##  Median :0.000   Median : 0.0000   Median :  44.0  
##  Mean   :0.094   Mean   : 0.1529   Mean   : 483.3  
##  3rd Qu.:0.000   3rd Qu.: 0.0000   3rd Qu.:  49.0  
##  Max.   :1.000   Max.   : 1.0000   Max.   :1929.0  
## 
```

```r
str(qob_new2)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	10000 obs. of  27 variables:
##  $ AGE     : int  43 33 33 42 45 30 42 43 46 41 ...
##  $ AGEQ    : num  43.5 1933.5 1933.8 1942.8 45.8 ...
##  $ v3      : int  1 2 3 3 1 2 2 2 2 2 ...
##  $ EDUC    : int  12 12 13 16 12 12 18 17 20 12 ...
##  $ ENOCENT : int  1 0 0 0 0 0 0 1 0 0 ...
##  $ ESOCENT : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ v7      : int  14 14 16 19 14 14 20 19 22 14 ...
##  $ v8      : num  9.91 9.89 9.55 10.09 9.11 ...
##  $ LWKLYWGE: num  5.97 5.94 5.6 6.13 5.18 ...
##  $ MARRIED : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ MIDATL  : int  0 0 0 1 0 1 0 0 0 1 ...
##  $ MT      : int  0 0 0 0 0 0 0 0 1 0 ...
##  $ NEWENG  : int  0 0 0 0 1 0 0 0 0 0 ...
##  $ v14     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ v15     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CENSUS  : int  70 80 80 80 70 80 80 80 80 80 ...
##  $ v17     : int  17 51 25 20 25 51 34 26 27 42 ...
##  $ QOB     : int  3 3 2 2 2 3 1 1 2 1 ...
##  $ RACE    : int  0 0 0 0 0 1 0 0 0 0 ...
##  $ SMSA    : int  1 0 0 0 1 0 0 0 0 0 ...
##  $ SOATL   : int  0 1 1 0 0 0 1 0 0 0 ...
##  $ v22     : int  17 24 12 36 NA 36 24 26 30 42 ...
##  $ v23     : int  5 52 52 52 5 20 52 41 52 52 ...
##  $ WNOCENT : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ WSOCENT : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ v26     : int  1 0 0 0 0 0 0 0 0 0 ...
##  $ YOB     : int  1926 46 46 37 1924 49 38 37 33 39 ...
##  - attr(*, "datalabel")= chr ""
##  - attr(*, "time.stamp")= chr "31 May 2008 00:35"
##  - attr(*, "formats")= chr  "%8.0g" "%9.0g" "%8.0g" "%8.0g" ...
##  - attr(*, "types")= int  251 254 251 251 251 251 251 254 254 251 ...
##  - attr(*, "val.labels")= chr  "" "" "" "" ...
##  - attr(*, "var.labels")= chr  "" "" "" "" ...
##  - attr(*, "version")= int 12
```


```r
dat <- list(
    W=standardize(qob_new2$LWKLYWGE) ,
    E=standardize(qob_new2$EDUC) ,
    Q=standardize(qob_new2$QOB) )
summary(dat)
```

```
##   Length Class  Mode   
## W 10000  -none- numeric
## E 10000  -none- numeric
## Q 10000  -none- numeric
```

```r
str(dat)
```

```
## List of 3
##  $ W: num [1:10000] 0.415 0.369 -0.106 0.636 -0.681 ...
##   ..- attr(*, "scaled:center")= num 5.67
##   ..- attr(*, "scaled:scale")= num 0.726
##  $ E: num [1:10000] -0.2468 -0.2468 0.0613 0.9857 -0.2468 ...
##   ..- attr(*, "scaled:center")= num 12.8
##   ..- attr(*, "scaled:scale")= num 3.25
##  $ Q: num [1:10000] 0.428 0.428 -0.469 -0.469 -0.469 ...
##   ..- attr(*, "scaled:center")= num 2.52
##   ..- attr(*, "scaled:scale")= num 1.11
```


```r
## R code 14.24
m14.4_qob <- ulam(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- aW + bEW*E,
        aW ~ dnorm( 0 , 0.2 ),
        bEW ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp( 1 )
    ) , data=dat , chains=4 , cores=4 , log_lik = TRUE)

precis( m14.4_qob )
```

```
##                mean          sd        5.5%      94.5%    n_eff      Rhat
## aW    -0.0001747455 0.009208036 -0.01459498 0.01500338 1786.246 1.0000515
## bEW    0.3693579126 0.009472848  0.35405919 0.38473741 1837.764 1.0009883
## sigma  0.9292246776 0.006693665  0.91901061 0.93998243 1915.759 0.9990892
```


```r
## R code 14.25
m14.5_qob <- ulam(
    alist(
        c(W,E) ~ multi_normal( c(muW,muE) , Rho , Sigma ),
        muW <- aW + bEW*E,
        muE <- aE + bQE*Q,
        c(aW,aE) ~ normal( 0 , 0.2 ),
        c(bEW,bQE) ~ normal( 0 , 0.5 ),
        Rho ~ lkj_corr( 2 ),
        Sigma ~ exponential( 1 )
    ), data=dat , chains=4 , cores=4 )

precis( m14.5_qob , depth=3 )
```

```
##                   mean           sd        5.5%      94.5%     n_eff     Rhat
## aE       -5.412850e-05 9.914912e-03 -0.01546060 0.01613586 1322.4900 1.002324
## aW       -6.557216e-05 9.387539e-03 -0.01513337 0.01516069 1316.4592 1.004163
## bQE       4.438868e-02 1.046021e-02  0.02725809 0.06185149 1424.3341 1.001382
## bEW       4.811464e-01 1.736913e-01  0.21670301 0.75197365  544.4486 1.001335
## Rho[1,1]  1.000000e+00 0.000000e+00  1.00000000 1.00000000       NaN      NaN
## Rho[1,2] -1.136972e-01 1.741125e-01 -0.38198505 0.16279078  559.9086 1.001204
## Rho[2,1] -1.136972e-01 1.741125e-01 -0.38198505 0.16279078  559.9086 1.001204
## Rho[2,2]  1.000000e+00 9.620423e-17  1.00000000 1.00000000 1923.1279 0.997998
## Sigma[1]  9.516058e-01 3.250105e-02  0.92377214 1.00545753  464.1535 1.004508
## Sigma[2]  9.991797e-01 6.921307e-03  0.98830597 1.01011924 1614.1167 1.000401
```



