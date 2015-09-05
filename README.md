




# What is `binnr`?
`binnr` is a package that creates, manages, and applies simple binning
transformations. It makes scorecard modeling easy and fast. Using `binnr`,
a modeler can discretize continuous variables, expand & collapse bins,
and apply monotonicity constraints all from within an easy-to-use
interface.

`binnr` not only discretizes variables and provides functions for 
manipulating them, but also performs weight-of-evidence substitution
on a dataset, transforming all predictors to the same scale and 
making them continuous. This has a number of benefits:

1. Subsituting weight-of-evidence forces a linear relationship between
the predictor and the binary target - Ideal for logistic regression
2. Continuous features can be used in more training algorithms
3. Missing values are also substituted creating a data set of complete
cases

When paired with penalized regression techniques such as ridge or LASSO
regression, a model can be quickly created and fine-tuned to completion 
in a fraction of the time of traditional modeling techniques. All
of this with no loss (and often a gain) in predictive performance.

### Binning Algorithm

The binning algorithm used by `binnr` is completely writtin in `C` and 
is very fast. It uses a supervised disretization method based on 
information value to make recursive splits of the data. The algorithm
support monotonicity constraints, exception values, and missing values.

#### Monotonicity

`binnr` supports 4 types of monotonicity within the `C` implementation.
Each type of constratint is specified by a special integer value.

| **Value** | **Meaning** |
|---------|-----------|
| 0 | No montonicity |
| -1 | Decreasing y as x increases |
| 1 | Increasing y as x increases |
| 2 | Either increasing or decreasing y as x increases |

Of special note is the value of 2. The algorithm implements this by 
making the first split in *any* direction and then uses that 
direction for the rest of the splits. This often results in the best
monotonic relationship without specifying the direction apriori.

#### Exception Values

`binnr` also supports exception values for each variable. Exceptions
only apply to continuous variables. The algorithm does not collapse exception
values but *does* use them to calculate information value statistics.

#### Missing Values

Missing values are handled by excluding them entirely from the binning
step. They do not inform the binning process at all. Missing values
are substituted with zeros when performing weight-of-evidence
substitution.

## Modeling with `binnr` Overview

The basic workflow of building a scorecard with `binnr` is comprised of
a few basic steps:

1. Bin the dataset using the `bin` function
  * Use `summary` to see a high-level view of the binned data
  * Look for variables that should not be modeled
2. Perform the weight-of-evidence substition on the data
3. Fit a LASSO regression model
4. Use `adjust` on the final model variables to tweak them
5. Repeat steps 3-4 until satisfied

Each of these steps will be detailed further below with examples.

### Bin the data

A small dataset containig a variety of variable types is included with 
the `binnr` package. It consists of 891 passengers on the Titanic, their 
survival status, and several demographic and socioeconomic attributes.
This dataset will be used throught this help document.


```r
data(titanic)
head(titanic)
  Survived Pclass    Sex Age SibSp Parch    Fare Embarked
1        0      3   male  22     1     0  7.2500        S
2        1      1 female  38     1     0 71.2833        C
3        1      3 female  26     0     0  7.9250        S
4        1      1 female  35     1     0 53.1000        S
5        0      3   male  35     0     0  8.0500        S
6        0      3   male  NA     0     0  8.4583        Q
```

Binning the data is as simple as calling the `bin` function on a `data.frame`.
The `bin` function accepts several arguments that control the binning
algorithm:

| **Argument** | **Controls** | **Example**|
|--------------|--------------|------------|
| min.iv | Minimum IV increase to split data | `min.iv = .01` |
| min.cnt | Mininmum # Obs in bins after splitting | `min.cnt = 100` |
| max.bin | Maximum # Bins excluding exceptions and missing | `max.bin = 10` |
| mono | Monotonicity relationship between x and y | `mono = c(Fare=1, Pclass=2)` |
| exceptions | List of exception values for each x | `exceptions = list(ALL=-1)` |

The `mono` argument accepts a named *vector* of values. The special name `ALL` 
applies to all of the variables. Monotonicity is applied on where names match.
Similarly, `exceptions` accepts a named *list* of values. Because variables can
have multiple exception values, each entry can be a vector. Like, `mono`, the
reserved name, `ALL`, applies the exceptions to each variable.

#### Examples using `mono` and `exceptions`

| **Example** | **Explanation** |
|---|---|
| `mono = c(ALL=1, Fare=2)` | Bin Fare in any monotonic direction; bin the rest with positive montonicity
| `exceptions = list(ALL = -1, Age = c(-99, -100))` | Exclude -99 and -100 when binning Age, exclude -1 for the rest of the variables |
| `mono = c(ALL=2)` | Bin all variables monotonically in any direction |


```r
bins <- bin(titanic[,-1], titanic$Survived)
```

This returns a `bin.list` object which contains a `bin` object for every
column of the dataset. Each `bin` object contains all of the information 
necessary to perform manipulations, plot data, and perform weight of 
evidence substitutions. Printing a `bin.list` object prints a summary of 
what it contains.


```r
bins
binnr bin.list object
  |--   7 Bins
  |--   3 Discrete
  |--   4 Continuous
```

There are 7 bins contained within the `bin.list` object - 3 discreted and 4
continuous. The distinction between discrete and continuous bins will be
demonstrated when using the `adjust` function.

Because `bin.list` is a list underneath, individual bins can be accessed 
in the normal list indexing manner. Printing a single bin produces a WoE
table with detailed statistics about the binned and target variables


```r
bins$Pclass

IV: 0.50095 | Variable: Pclass
       #0  #1   N    %0    %1  P(1)    WoE      IV
 1. 1  80 136 216 0.146 0.398 0.630  1.004 0.25293
 2. 2  97  87 184 0.177 0.254 0.473  0.364 0.02832
 3. 3 372 119 491 0.678 0.348 0.242 -0.666 0.21970
Total 549 342 891 1.000 1.000 0.384  0.000 0.50095
```

Furthermore, a `bin.list` may also be subset just like a base R list:


```r
bins[1:4]
binnr bin.list object
  |--   4 Bins
  |--   2 Discrete
  |--   2 Continuous
```

Calling the summary function on a `bin.list` returns a `data.frame`
of high level information about each binned attribute:


```r
s <- summary(bins)
print(s)
      Name         IV # Bins Tot N # Valid # Exception # Missing Monotonicty
2      Sex 1.34168141      2   891     891           0         0           0
6     Fare 0.87823669     10   891     891           0         0           0
1   Pclass 0.50094974      3   891     891           0         0           0
3      Age 0.21059108     10   891     714           0       177           0
4    SibSp 0.18114137      4   891     891           0         0           0
7 Embarked 0.12237459      4   891     891           0         0           0
5    Parch 0.09745514      3   891     891           0         0           0
```

The summary is sorted by descending information value placing the 
most predictive attribuets at the top of the list. The summary
`data.frame` can be used to identify variables that should not be
modeled. For example, a discrete variable with 40 bins should be 
collapsed before using.

### Apply Weight-of-Evidence Substitutions

`binnr` provides a `predict` function that is used to perform the WoE 
substitution on a `data.frame`. The columns are matched by name and a
matrix of numeric values is returned.


```r
binned <- predict(bins, titanic)
```


```r
head(binned)
      Pclass        Sex        Age      SibSp      Parch       Fare    Embarked
1 -0.6664827 -0.9838327 -0.1027299  0.6170756 -0.1737481 -0.8484681 -0.20359896
2  1.0039160  1.5298770 -0.1148437  0.6170756 -0.1737481  0.7327989  0.68839908
3 -0.6664827  1.5298770 -0.1027299 -0.1660568 -0.1737481 -0.9238176 -0.20359896
4  1.0039160  1.5298770  0.5805232  0.6170756 -0.1737481  0.7327989 -0.20359896
5 -0.6664827 -0.9838327  0.5805232 -0.1660568 -0.1737481 -0.9238176 -0.20359896
6 -0.6664827 -0.9838327  0.0000000 -0.1660568 -0.1737481 -0.9238176  0.02433748
```

Creating a table of the WoE-substituted values with the original values
illustrates what `binnr` is doing behind the scenes:


```r
bins$Embarked

IV: 0.12237 | Variable: Embarked
       #0  #1   N    %0    %1  P(1)    WoE      IV
 1.     0   2   2 0.000 0.006 1.000  0.000 0.00000
 2. C  75  93 168 0.137 0.272 0.554  0.688 0.09315
 3. Q  47  30  77 0.086 0.088 0.390  0.024 0.00005
 4. S 427 217 644 0.778 0.635 0.337 -0.204 0.02917
Total 549 342 891 1.000 1.000 0.384  0.000 0.12237

table(titanic$Embarked, round(binned[,'Embarked'], 3))
   
    -0.204   0 0.024 0.688
         0   2     0     0
  C      0   0     0   168
  Q      0   0    77     0
  S    644   0     0     0
```

The raw values of the Embarked attribtue are mapped to the WoE value found
in the Embarked `bin` object.

### Logistic Regression

Once the variable transoformations have been applied, a logistic regression
model may be fit. We will be applying a new logistic regression algorithm called
`LASSO`. It fits the model and performs variable selection at the same time.
More about LASSO regression can be found [here](http://statweb.stanford.edu/~tibs/lasso.html).

LASSO regression requires that we specify a penalty argument to constrain the 
coefficients. We will be using cross-validation to determine this parameter
automatically. Furthermore, since our variables are already transformed the way
we like, we will also force the parameters to be greater than zero. This will
prevent any "flips" from occuring in our final model.

And here is the raw variable crossed with the transformed variable:



```r
fit <- cv.glmnet(binned, titanic$Survived, alpha=1, family="binomial",
                 nfolds = 5, lower.limits=0)
plot(fit)
```

![plot of chunk unnamed-chunk-12](plots/README-unnamed-chunk-12-1.png) 

The resulting plot shows the error on the y-axis and the penalty term on the
x-axis. The penalty term controls the size of the coefficients and how many of
them are not equal to zero. The first dashed line represents the size of the
penalty term that has the lowest cross-validation error. We can access this
value easily by using the "lambda.min" argument where appropriate. For example, 
to find the optimal coefficients:


```r
coef(fit, s="lambda.min")
8 x 1 sparse Matrix of class "dgCMatrix"
                     1
(Intercept) -0.4699424
Pclass       0.8618862
Sex          0.9899909
Age          1.1595609
SibSp        0.4288312
Parch        .        
Fare         0.2607622
Embarked     0.5080654
```


