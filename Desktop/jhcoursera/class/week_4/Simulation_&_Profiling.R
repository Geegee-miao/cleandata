## Week 4: Simulation & Profiling

## 'str' (structure function): Compactly display the internal structure of an R object
    ## A diagnostic function and an alternative to 'summary'
    ## It is especially well suited to compactly display the (abbreviated) contents of (possibly nested) large lists.
    ## Roughly one line per basic object
    ## Basic goal of 'str' function is to answer "What is in this object?"


> str(str)
function (object, ...)  

## What is in the argument of the 'lm()'
> str(lm)
function (formula, data, subset, weights, na.action, method = "qr", model = TRUE, 
          x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, 
          ...)  

## What is in the argument of the 'ls()'
> str(ls)
function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE, pattern, 
          sorted = TRUE) 
    

## Example

## 'summary()' gives what the range is and how it varies
> x <- rnorm(100, 2, 4)
> summary(x)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    -5.2990 -0.1598  2.9067  2.7448  5.7059 13.4050 

    
## str(x) tells you that it is a numeric vector, there are 100 elements and the first five numbers in the vector
    ## what the data looks like
> str(x)
    num [1:100] -1.41 1.15 7.92 7.14 2.92 ...
    
    
> str(f)
Factor w/ 40 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...

## Not as compact as 'str(f)'
> summary(f)
1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 
10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 
30 31 32 33 34 35 36 37 38 39 40 
10 10 10 10 10 10 10 10 10 10 10 

## Using 'str()' in data frame
> library(datasets)
> head(airquality)
Ozone Solar.R Wind Temp Month Day
1    41     190  7.4   67     5   1
2    36     118  8.0   72     5   2
3    12     149 12.6   74     5   3
4    18     313 11.5   62     5   4
5    NA      NA 14.3   56     5   5
6    28      NA 14.9   66     5   6

> str(airquality)
'data.frame':	153 obs. of  6 variables:
    $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
$ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
$ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
$ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
$ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
$ Day    : int  1 2 3 4 5 6 7 8 9 10 ...


> m <- matrix(rnorm(100), 10, 10)
> str(m)
num [1:10, 1:10] 0.156 -0.934 0.215 0.283 -0.893 ...
> m[, 1]
[1]  0.1555617 -0.9338970  0.2146500  0.2829444 -0.8925353  0.3993015  0.4035061
[8] -0.5117104 -0.2830959 -1.3863042



> s <- split(airquality, airquality$Month)
> str(s)
List of 5
$ 5:'data.frame':	31 obs. of  6 variables:
    ..$ Ozone  : int [1:31] 41 36 12 18 NA 28 23 19 8 NA ...
..$ Solar.R: int [1:31] 190 118 149 313 NA NA 299 99 19 194 ...
..$ Wind   : num [1:31] 7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
..$ Temp   : int [1:31] 67 72 74 62 56 66 65 59 61 69 ...
..$ Month  : int [1:31] 5 5 5 5 5 5 5 5 5 5 ...
..$ Day    : int [1:31] 1 2 3 4 5 6 7 8 9 10 ...
$ 6:'data.frame':	30 obs. of  6 variables:
    ..$ Ozone  : int [1:30] NA NA NA NA NA NA 29 NA 71 39 ...
..$ Solar.R: int [1:30] 286 287 242 186 220 264 127 273 291 323 ...
..$ Wind   : num [1:30] 8.6 9.7 16.1 9.2 8.6 14.3 9.7 6.9 13.8 11.5 ...
..$ Temp   : int [1:30] 78 74 67 84 85 79 82 87 90 87 ...
..$ Month  : int [1:30] 6 6 6 6 6 6 6 6 6 6 ...
..$ Day    : int [1:30] 1 2 3 4 5 6 7 8 9 10 ...
$ 7:'data.frame':	31 obs. of  6 variables:
    ..$ Ozone  : int [1:31] 135 49 32 NA 64 40 77 97 97 85 ...
..$ Solar.R: int [1:31] 269 248 236 101 175 314 276 267 272 175 ...
..$ Wind   : num [1:31] 4.1 9.2 9.2 10.9 4.6 10.9 5.1 6.3 5.7 7.4 ...
..$ Temp   : int [1:31] 84 85 81 84 83 83 88 92 92 89 ...
..$ Month  : int [1:31] 7 7 7 7 7 7 7 7 7 7 ...
..$ Day    : int [1:31] 1 2 3 4 5 6 7 8 9 10 ...
$ 8:'data.frame':	31 obs. of  6 variables:
    ..$ Ozone  : int [1:31] 39 9 16 78 35 66 122 89 110 NA ...
..$ Solar.R: int [1:31] 83 24 77 NA NA NA 255 229 207 222 ...
..$ Wind   : num [1:31] 6.9 13.8 7.4 6.9 7.4 4.6 4 10.3 8 8.6 ...
..$ Temp   : int [1:31] 81 81 82 86 85 87 89 90 90 92 ...
..$ Month  : int [1:31] 8 8 8 8 8 8 8 8 8 8 ...
..$ Day    : int [1:31] 1 2 3 4 5 6 7 8 9 10 ...



## SIMULATION

## Generating Random Numbers
    ## Functions for probability distributions in R
        ## 'rnorm': generate random Normal variables with a given mean and standard deviation
        ## 'dnorm': evaluate the Normal probability density (with a given mean/SD) at a point (or vector of points)
        ## 'pnorm': evaluate the cumulative distribution function for a Normal distribution
        ## 'rpois': generate random Poisson variables with a given rate

    ## Probability distribution functions usually have four functions associated with them. 
    ## The functions are prefixed with a
        ## 'd' for density
        ## 'r' for random number generation
        ## 'p' for cumulative distribution
        ## 'q' for quantile function


    ## Working with the Normal distributions requires using these four functions

    dnrom(x, mean = 0, sd = 1, log = FALSE)  ## evaluate the density. When evaluating the density function for a normal
                                                ## distribution, you want to use the log of that value.
    
    pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  ## lower.tail is the probability distribution that goes
                                                                ## to the left. If want to evaluate the upper.tail then     
                                                                ## lower.tail = FALSE
    
    qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
    
    rnorm(n, mean = 0, sd = 1) 
    
    ## All the functions required to specify the mean and the standard deviation because that's what specifies
        ## the actual probability distribution. If do not specify then the default values are a distribution,
        ## a standard normal distribution which has mean = 0 and sd = 1.
    
    ## If Phi is the cumulative distribution function for a standard Normal distribution then 'pnorm(q) = Phi(q)'
        ## and 'qnorm(p) = Phi(e-1)(p)'
    
    > x <- rnorm(10)
    > x
    [1]  1.15126075  0.83268084  3.46137281  0.23575571  0.23616834 -0.34253727
    [7] -1.40632575  0.08509148 -0.86664284 -1.07792544
    > 
    
    > x <- rnorm(10, 20, 2)
    > x
    [1] 16.98744 19.66643 16.69723 18.74888 21.63147 19.67251 21.18783 21.72969 19.50321
    [10] 17.87720
    > 
        > summary(x)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    16.70   18.10   19.58   19.37   20.81   21.73 
    
    
    ## Important to set the reandom number generator seed any time a simulate random numbers for any distribution 
        ## Setting the random number seed with 'set.seed' ensures reproducibility. So people can reproduce what you done.
            ## and go back to produce the same result.        
    
        ## The random numbers are not really random (pseudo random) but appear to be random. To generate the same set of random
            ## number, set the seed. 
    
> set.seed(1)
> rnorm(5)
[1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078  
    
> rnorm(5)
[1] -0.8204684  0.4874291  0.7383247  0.5757814 -0.3053884  ## seed not set
> 
> set.seed(1)
> rnorm(5)
[1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078



## Generating Poisson data
    ## Mean is equal to the rate
    
> rpois(10, 1)
[1] 0 0 1 1 2 1 1 4 1 2
> rpois(10,2)
[1] 4 1 2 0 1 1 0 1 4 1
> rpois(10, 20)
[1] 24 23 20 11 22 24 16 17 18 17
> 
> ppois(2, 2)  ## Cumulative distribution
[1] 0.6766764  ## Pr(x <= 2)
> ppois(4, 2)
[1] 0.947347  ## PR(x <= 4)
> ppois(6,2)
[1] 0.9954662  ## PR(x <= 6)
    

## GENERATING SIMULATION IN A LINEAR MODEL
    
## Generating Random Numbers from a Linear Model
    ## Support we want to simulate from the following 
    
> set.seed(20)
> x <- rnorm(100)
> e <- rnorm(100, 0, 2)
> y <- 0.5 + 2 * x + e
> summary(y)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    -6.4084 -1.5402  0.6789  0.6893  2.9303  6.5052 


## What if x is binary?
    
> set.seed(10)
> x <- rbinom(100, 1, 0.5)
> e <- rnorm(100, 0, 2)
> y <- 0.5 + 2 * x + e
> summary(y)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    -3.4936 -0.1409  1.5767  1.4322  2.8397  6.9410 
> plot(x,y)
    

## Simulate from a Poisson model

> set.seed(1)
> x <- rnorm(100)
> log.mu <- 0.5 + 0.3 * x
> y <- rpois(100, exp(log.mu))
> summary(y)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.00    1.00    1.00    1.55    2.00    6.00 
> plot(x, y)
    

    
    









