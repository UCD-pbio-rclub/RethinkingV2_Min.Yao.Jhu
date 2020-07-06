---
title: "chapter14-7_phylogenetic_regression"
author: "Min-Yao"
date: "2020/7/6"
output: 
  html_document: 
    keep_md: yes
---

## You want to rest the hypothesis that the range size of carnivores is influenced by their body size (larger carnivores roam further).  You want to be sure that if you do see a relationship that is is not just due to shared evolutionary history.  That is you want to do a phylogenetic regression.

# 1) Load the data as follows


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
library(ade4)
```

```
## Warning: package 'ade4' was built under R version 4.0.2
```


```r
data("carni70")
```


```r
tre <- read.tree(text=carni70$tre)
d <- carni70$tab
```

# 2) Note that the species name given in "rownames(d)" and in "tre$tip.label" do not match.  Fix that (e.g. using str_replace() )

# 3) Should either range or size be transformed?

# 4) Does size predict range?  Evaluate this question without and with accounting for phylogenetic relationships.

# 5) Discuss your findings.






