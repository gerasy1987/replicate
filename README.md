
-   [Description of the replicate functionality:](#description-of-the-replicate-functionality)
    -   [`create_replication()`](#create_replication)
    -   [`summary()`](#summary)
    -   [Examples](#examples)
        -   [Use of `create_replication()`](#use-of-create_replication)
        -   [Use of `summary.replication()`](#use-of-summary.replication)
-   [TODO](#todo)

``` r
if (!require(pacman)) install.packages(pacman)
pacman::p_load(plyr, dplyr, broom, Hmisc, lfe, multiwayvcov, lmtest, wakefield, magrittr)
```

Description of the replicate functionality:
===========================================

`create_replication()`
----------------------

*The function takes main parts of replication object as an arguments and returns the replication class object. (see [create\_replication.R](https://github.com/gerasy1987/replicate/blob/master/create_replication.R) for code).*

The function takes 6 main arguments arguments:

-   `description_list`: List of miscellaneous descriptions of replication. *Example*:

``` r
description_list =
  list(
    study_name = "Fake Study",
    study_authors = c("Georgiy Syunyaev", "Someone Else"),
    study_affiliations = c("Columbia University",
                           "Some Other University"),
    rep_authors = c("Georgiy Syunyaev"),
    study_abstract =
      paste0("The aim of this study is to test the create_replication() functionality. ",
             "This is the first attempt at creatreplication class of objects in [R] ",
             "for systematic storage and access to study replication materials.")
  )
```

-   `packages = NULL`: Character vector of packages required for replication in `[R]`. Defaults to `NULL`.
-   `project_path = NULL`: Character string giving the path to the directory, where function and replication scripts are stored. Defaults to `NULL` (which is reasonable if RStudio project is used).
-   `data_list`: A named list of data-frames used for the replication. *Example*:

``` r
data_list = list(data_admin = data_admin, data_individual = data_individual)
```

-   `function_script_path`: Character string giving the name of `[R]` script which contains all functions required for the replication. The script should only include declarations of named functions using `<-` operator. See [replication\_functions.R](https://github.com/gerasy1987/replicate/blob/master/example/replication_functions.R) for example.
-   `replication_script_path`: Character string giving the name of `[R]` script which contains all calls for table replications. The script should consist of only single calls for replication of one table and the table objects should be created using `<-` operator. See [replication\_script.R](https://github.com/gerasy1987/replicate/blob/master/example/replication_script.R) for example.

There are also 2 additional arguments:

-   `quietly = FALSE`: Logical. Whether the creation of replication should go without any messages printed to `console`. Defaults to `FALSE`.
-   `checks = TRUE`: Logical. If `quietly = FALSE`, whether the checks for packages and consistency of replication should be performed. Defaults to `TRUE`.

`summary()`
-----------

*The function takes replication object and either returns miscellaneous description of the object, or if additional arguments are specified, then only summary of parts of object are returned. (see [summary.R](https://github.com/gerasy1987/replicate/blob/master/summary.R) for code).*

The function takes the following arguments:

-   `object`: Object of class `replication` created by `create_replication()`.
-   `table = NULL`: Character string specifying the table to be replicated. The table name should include number of the table as specified in `replication_script_path` file and the word "table".
-   `reported = TRUE`: Logical. Whether to show columns with specifications reported in the paper.
-   `registered = FALSE`: Logical. Whether to show columns with specifications registered in PAP.
-   `desc = FALSE`: *To be implemented...*
-   `data_desc = FALSE`: *To be implemented...*

Examples
--------

### Use of `create_replication()`

``` r
load(file = "example/replication_data.Rdata")

source("create_replication.R")
source("summary.R")

x <-
  create_replication(
    data_list =
      list(data_admin = data_admin,
           data_individual = data_individual),
    packages =
      c("plyr", "dplyr", "broom", "Hmisc",
        "lfe", "multiwayvcov", "lmtest",
        "wakefield", "magrittr"),
    project_path = "example/",
    function_script_path = "replication_functions.R",
    replication_script_path = "replication_script.R",
    description_list =
      list(study_name = "Fake Study",
           study_authors = c("Georgiy Syunyaev", "Someone Else"),
           study_affiliations = c("Columbia University",
                                  "Some Other University"),
           rep_authors = c("Georgiy Syunyaev"),
           study_abstract = 
             paste0("The aim of this study is to test the create_replication() functionality. ",
                    "This is the first attempt at creatreplication class of objects in [R] ",
                    "for systematic storage and access to study replication materials.")),
    quietly = TRUE,
    checks = FALSE
  )
```

### Use of `summary.replication()`

``` r
summary(x)
```

    ## Miscellany:
    ## This is a replication of the Fake Study. The original study is conducted by Georgiy Syunyaev from Columbia University and Someone Else from Some Other University. The replication is conducted by Georgiy Syunyaev.
    ## 
    ## Abstract:
    ## The aim of this study is to test the create_replication() functionality. This is the first attempt at creatreplication class of objects in [R] for systematic storage and access to study replication materials.
    ## 
    ## Technical:
    ## There are 2 datasets provided: data_admin (50 obs. of 11 variables), data_individual (1000 obs. of 12 variables). There are 7 custom functions provided: analyses, absorb, fround, mgsub, pfround, set_seed, wtd_mean. There are 2 table replications provided: table_1, table_2.

``` r
summary(x, table = "table_1", reported = TRUE, registered = FALSE)
```

    ## Results for table_1 
    ## 
    ## Reported :
    ## 
    ## column_1 
    ## 
    ##        term estimate std.error       printout p.value
    ## 1 intercept   85.013     1.037 85.013 [1.037]   0.000
    ## 2     treat   -1.080     0.922 -1.080 [0.922]   0.242
    ## 3      male   -0.298     0.924 -0.298 [0.924]   0.747
    ## 4    income    0.000     0.000  0.000 [0.000]   0.902
    ## 
    ## adj.r.squared = -0.003, n_obs = 997, HETEROGENOUS = NA, FE = ethnicity, CLUSTER = no, IPW = no 
    ## 
    ## column_2 
    ## 
    ##        term estimate std.error       printout p.value
    ## 1 intercept   84.937     0.672 84.937 [0.672]   0.000
    ## 2     treat   -1.076     0.921 -1.076 [0.921]   0.243
    ## 
    ## adj.r.squared = -0.001, n_obs = 997, HETEROGENOUS = NA, FE = ethnicity, CLUSTER = no, IPW = no

``` r
summary(x, table = "table_2", reported = TRUE, registered = TRUE)
```

    ## Results for table_2 
    ## 
    ## Reported :
    ## 
    ## column_1 
    ## 
    ##           term estimate std.error       printout p.value
    ## 1    intercept   -0.368     0.846 -0.368 [0.846]   1.334
    ## 2        treat    0.072     0.058  0.072 [0.058]   0.219
    ## 3          age   -0.004     0.009 -0.004 [0.009]   0.636
    ## 4 school_grade    0.012     0.010  0.012 [0.010]   0.231
    ## 
    ## adj.r.squared = -0.021, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 
    ## 
    ## column_2 
    ## 
    ##        term estimate std.error       printout p.value
    ## 1 intercept    0.711     0.682  0.711 [0.682]   0.303
    ## 2     treat    0.051     0.057  0.051 [0.057]   0.380
    ## 3    height   -0.001     0.004 -0.001 [0.004]   0.890
    ## 4    income    0.000     0.000  0.000 [0.000]   0.543
    ## 
    ## adj.r.squared = -0.048, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 
    ## 
    ## column_3 
    ## 
    ##           term estimate std.error       printout p.value
    ## 1    intercept   -0.215     1.106 -0.215 [1.106]   1.153
    ## 2        treat    0.068     0.060  0.068 [0.060]   0.262
    ## 3          age   -0.004     0.009 -0.004 [0.009]   0.690
    ## 4 school_grade    0.012     0.010  0.012 [0.010]   0.268
    ## 5       height    0.000     0.004  0.000 [0.004]   0.946
    ## 6       income    0.000     0.000  0.000 [0.000]   0.662
    ## 
    ## adj.r.squared = -0.063, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 
    ## 
    ## Registered :
    ## 
    ## column_1 
    ## 
    ##           term estimate std.error       printout p.value
    ## 1    intercept   -0.368     0.846 -0.368 [0.846]   1.334
    ## 2        treat    0.072     0.058  0.072 [0.058]   0.219
    ## 3          age   -0.004     0.009 -0.004 [0.009]   0.636
    ## 4 school_grade    0.012     0.010  0.012 [0.010]   0.231
    ## 
    ## adj.r.squared = -0.021, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 
    ## 
    ## column_1_rep 
    ## 
    ##           term estimate std.error       printout p.value
    ## 1    intercept    0.324     1.184  0.324 [1.184]   0.786
    ## 2        treat   -0.744     1.508 -0.744 [1.508]   0.624
    ## 3           iq   -0.006     0.009 -0.006 [0.009]   0.474
    ## 4          age   -0.003     0.009 -0.003 [0.009]   0.740
    ## 5 school_grade    0.011     0.010  0.011 [0.010]   0.300
    ## 6     treat:iq    0.008     0.015  0.008 [0.015]   0.594
    ## 
    ## adj.r.squared = -0.055, n_obs = 50, HETEROGENOUS = iq, FE = urban, CLUSTER = no, IPW = no 
    ## 
    ## column_2 
    ## 
    ##        term estimate std.error       printout p.value
    ## 1 intercept    0.711     0.682  0.711 [0.682]   0.303
    ## 2     treat    0.051     0.057  0.051 [0.057]   0.380
    ## 3    height   -0.001     0.004 -0.001 [0.004]   0.890
    ## 4    income    0.000     0.000  0.000 [0.000]   0.543
    ## 
    ## adj.r.squared = -0.048, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 
    ## 
    ## column_2_rep 
    ## 
    ##        term estimate std.error       printout p.value
    ## 1 intercept    1.694     1.120  1.694 [1.120]   0.138
    ## 2     treat   -1.053     1.495 -1.053 [1.495]   0.485
    ## 3        iq   -0.009     0.009 -0.009 [0.009]   0.304
    ## 4    height   -0.001     0.004 -0.001 [0.004]   0.815
    ## 5    income    0.000     0.000  0.000 [0.000]   0.487
    ## 6  treat:iq    0.011     0.015  0.011 [0.015]   0.468
    ## 
    ## adj.r.squared = -0.069, n_obs = 50, HETEROGENOUS = iq, FE = urban, CLUSTER = no, IPW = no 
    ## 
    ## column_3_rep 
    ## 
    ##           term estimate std.error       printout p.value
    ## 1    intercept    0.676     1.491  0.676 [1.491]   0.653
    ## 2        treat   -0.824     1.546 -0.824 [1.546]   0.597
    ## 3           iq   -0.007     0.009 -0.007 [0.009]   0.431
    ## 4          age   -0.002     0.009 -0.002 [0.009]   0.820
    ## 5 school_grade    0.010     0.011  0.010 [0.011]   0.362
    ## 6       height   -0.001     0.004 -0.001 [0.004]   0.878
    ## 7       income    0.000     0.000  0.000 [0.000]   0.592
    ## 8     treat:iq    0.009     0.015  0.009 [0.015]   0.571
    ## 
    ## adj.r.squared = -0.098, n_obs = 50, HETEROGENOUS = iq, FE = urban, CLUSTER = no, IPW = no

TODO
====

-   Discuss
-   Transform into package
-   Finish writing the `summary` method
-   Test on Benin study
-   Implement `output_table()` functionality
