
-   [Description of the replicate functionality:](#description-of-the-replicate-functionality)
    -   [`create_replication()`](#create_replication)
    -   [`summary()`](#summary)
    -   [Examples](#examples)
        -   [Use of `create_replication()`](#use-of-create_replication)
        -   [Use of `summary.replication()`](#use-of-summary.replication)

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
summary(x, table = "table_1", reported = TRUE, registered = FALSE)
summary(x, table = "table_2", reported = TRUE, registered = TRUE)
```
