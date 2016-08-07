
-   [TODO](#todo)
-   [Description of the replicate functionality:](#description-of-the-replicate-functionality)
    -   [`create_replication()`](#create_replication)
    -   [`summary()`](#summary)
    -   [Examples](#examples)
        -   [Use of `create_replication()`](#use-of-create_replication)
        -   [Use of `summary.replication()`](#use-of-summary.replication)
            -   [Genearal summary](#genearal-summary)
            -   [Table summary](#table-summary)
            -   [Replication script](#replication-script)

TODO
====

-   \[ \] Discuss
-   \[x\] ~~Transform into package~~
-   Finish writing the `summary` method
    -   \[x\] ~~Add replication script functionality~~
    -   \[ \] Add description functionality for data, etc. (?)
-   \[ \] Implement `output_table()` functionality
-   \[ \] Test on Benin study (?)

Description of the replicate functionality:
===========================================

`create_replication()`
----------------------

*The function takes main parts of replication object as an arguments and returns the replication class object. (see [create\_replication.R](https://github.com/gerasy1987/replicate/blob/master/create_replication.R) for code).*

The function takes 6 main arguments arguments (defaults are given in parentheses if specified):

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

-   `packages` (`= NULL`): Character vector of packages required for replication in `[R]`.
-   `project_path` (`= NULL`): Character string giving the path to the directory, where function and replication scripts are stored. Defaults to `NULL`, which is reasonable if RStudio project is used.
-   `data_list`: A named list of data-frames used for the replication. *Example*:

``` r
data_list = list(data_admin = data_admin, data_individual = data_individual)
```

-   `function_script_path`: Character string giving the name of `[R]` script which contains all functions required for the replication. The script should only include declarations of named functions using `<-` operator. See [replication\_functions.R](https://github.com/gerasy1987/replicate/blob/master/example/replication_functions.R) for example.
-   `replication_script_path`: Character string giving the name of `[R]` script which contains all calls for table replications. The script should consist of only single calls for replication of one table and the table objects should be created using `<-` operator. See [replication\_script.R](https://github.com/gerasy1987/replicate/blob/master/example/replication_script.R) for example.

There are also 2 additional arguments:

-   `quietly` (`= FALSE`): Logical. Whether the creation of replication should go without any messages printed to `console`.
-   `checks` (`= TRUE`): Logical. If `quietly = FALSE`, whether the checks for packages and consistency of replication should be performed.

`summary()`
-----------

*The function takes replication object and either returns miscellaneous description of the object, or if additional arguments are specified, then only summary of parts of object are returned. (see [summary.R](https://github.com/gerasy1987/replicate/blob/master/summary.R) for code).*

The function takes the following arguments:

-   `object`: Object of class `replication` created by `create_replication()`.
-   `table` (`= NULL`): Character string specifying the table to be replicated. The table name should include number of the table as specified in `replication_script_path` file and the word "table".
-   `reported` (`= FALSE`): Logical. Whether to show columns with specifications reported in the paper.
-   `registered` (`= FALSE`): Logical. Whether to show columns with specifications registered in PAP.
-   `script` (`= FALSE`): Logical. Whether to print the script to replicate the results of the study. If `table = NULL`, then returns preamble which includes all the functions and packages required for replication. If `table != NULL`, then returns preamble and the code for replication of the specified table.
-   `desc` (`= FALSE`): *To be implemented...*

Examples
--------

``` r
devtools::install_github("gerasy1987/replicate", quiet = TRUE)

library(replicate)

load(file = "example/replication_data.Rdata")
```

### Use of `create_replication()`

``` r
(
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
    checks = TRUE
  )
)
```

    ## Miscellany:
    ## This is a replication of the Fake Study. The original study is conducted by Georgiy Syunyaev from Columbia University and Someone Else from Some Other University. The replication is conducted by Georgiy Syunyaev.
    ## 
    ## Abstract:
    ## The aim of this study is to test the create_replication() functionality. This is the first attempt at creatreplication class of objects in [R] for systematic storage and access to study replication materials.
    ## 
    ## Technical:
    ## There are 2 datasets provided: data_admin (50 obs. of 11 variables), data_individual (1000 obs. of 12 variables). There are 7 custom functions provided: analyses, absorb, fround, mgsub, pfround, set_seed, wtd_mean. There are 2 table replications provided: table_1, table_2. There are 9 [R] packages required for the replication: plyr, dplyr, broom, Hmisc, lfe, multiwayvcov, lmtest, wakefield, magrittr. 
    ## 
    ## [[1]]
    ## [1] "plyr"         "dplyr"        "broom"        "Hmisc"       
    ## [5] "lfe"          "multiwayvcov" "lmtest"       "wakefield"   
    ## [9] "magrittr"    
    ## 
    ## [[2]]
    ## [[2]]$data_admin
    ## # A tibble: 50 x 11
    ##    village_id   age school_grade   income        iq   height treat
    ## *       <int> <dbl>        <dbl>    <dbl>     <dbl>    <dbl> <dbl>
    ## 1           1 39.95       84.600 44699.83 101.58325 187.5443     0
    ## 2           2 38.65       85.365 29935.60  97.68520 186.0175     0
    ## 3           3 35.55       84.065 34020.71  95.80900 178.7253     0
    ## 4           4 44.20       82.430 36115.11 106.63790 169.8621     0
    ## 5           5 43.70       83.355 33057.74 105.18525 166.9320     0
    ## 6           6 43.90       85.885 57083.52  92.49965 172.3317     0
    ## 7           7 38.50       77.200 44961.11 108.90850 188.9587     0
    ## 8           8 42.15       76.865 35599.71  99.64625 166.1701     1
    ## 9           9 40.85       86.345 47641.29  96.48255 176.1206     1
    ## 10         10 42.05       84.830 42481.76 101.54255 177.4276     0
    ## # ... with 40 more rows, and 4 more variables: turnout <dbl>, urban <int>,
    ## #   rural <int>, population <int>
    ## 
    ## [[2]]$data_individual
    ## # A tibble: 1,000 x 12
    ##     vote   age ethnicity  male female school_grade   income      iq
    ##    <int> <int>    <fctr> <int>  <int>        <dbl>    <dbl>   <dbl>
    ## 1      0    63     black     0      1         66.8  44418.0  93.631
    ## 2      1    57     white     1      0         91.3  49502.5 115.324
    ## 3      0    22     black     1      0        100.0  27007.4  95.583
    ## 4      0    51  hispanic     1      0         75.5 105206.0  78.870
    ## 5      0    59     white     0      1        100.0  33244.2  97.390
    ## 6      1    41     white     0      1         87.7   1452.1  44.597
    ## 7      0    45     black     0      1        100.0  41811.7 102.020
    ## 8      1    39     black     1      0         84.9  26964.4  79.785
    ## 9      0    37     white     0      1         98.3  22710.5 110.571
    ## 10     0    62     white     0      1         82.8   9470.8 104.752
    ## # ... with 990 more rows, and 4 more variables: height <dbl>, treat <int>,
    ## #   village_id <int>, ind_id <int>
    ## 
    ## 
    ## [[3]]
    ## [[3]]$analyses
    ## [1] "analyses <- function(DV, treat, covs = NULL, heterogenous = NULL, subset = NULL, FE = NULL, cluster = NULL, IPW = NULL, data, model = \"lm\", status = c(TRUE, TRUE, TRUE)) {\n    suppressMessages(stopifnot(require(plyr)))\n    suppressMessages(stopifnot(require(dplyr)))\n    suppressMessages(stopifnot(require(broom)))\n    suppressMessages(stopifnot(require(Hmisc)))\n    suppressMessages(stopifnot(require(lfe)))\n    suppressMessages(stopifnot(require(multiwayvcov)))\n    suppressMessages(stopifnot(require(lmtest)))\n    if (!is.null(FE) & model != \"lm\") \n        stop(\"Function does not support FE for other than OLS models\")\n    frame_formula <- stats::as.formula(paste(DV, \"~\", paste(c(treat, covs, FE, cluster, IPW, heterogenous), collapse = \" + \")))\n    if (is.null(heterogenous)) {\n        main_formula <- paste(c(treat, covs), collapse = \" + \")\n    }\n    else {\n        main_formula <- paste(c(treat, paste0(treat, \":\", heterogenous), heterogenous, covs), collapse = \" + \")\n    }\n    main_formula <- paste(DV, \"~\", main_formula)\n    FE_formula <- ifelse(is.null(FE), 0, paste(FE, collapse = \"+\"))\n    cluster_formula <- ifelse(is.null(cluster), 0, paste(cluster, collapse = \"+\"))\n    fit_formula <- stats::as.formula(paste(main_formula, \"|\", FE_formula, \"|\", 0, \"|\", cluster_formula))\n    frame_df <- dplyr::filter_(.data = data, .dots = subset)\n    frame_df <- dplyr::filter_(.data = frame_df, .dots = paste(paste0(\"!is.na(\", c(treat, DV, FE, cluster, IPW, heterogenous), \")\"), collapse = \" & \"))\n    frame_df <- stats::model.frame(frame_formula, data = frame_df)\n    if (length(FE) > 1) \n        frame_df[, FE] <- (plyr::colwise(as.factor))(frame_df[, FE])\n    if (length(FE) == 1) \n        frame_df[, FE] <- as.factor(frame_df[, FE])\n    if (model == \"lm\") {\n        if (is.null(IPW)) {\n            fit <- lfe::felm(formula = fit_formula, data = frame_df)\n        }\n        else {\n            fit <- lfe::felm(formula = fit_formula, data = frame_df, weights = unlist(frame_df[, IPW]))\n        }\n    }\n    else if (model == \"logit\") {\n        if (is.null(IPW)) {\n            fit <- suppressWarnings(stats::glm(formula = stats::as.formula(main_formula), data = frame_df, family = binomial(link = \"logit\")))\n        }\n        else {\n            fit <- suppressWarnings(stats::glm(formula = stats::as.formula(main_formula), data = frame_df, weights = unlist(frame_df[, IPW]), family = binomial(link = \"logit\")))\n        }\n        if (!is.null(cluster)) {\n            fit <- lmtest::coeftest(x = fit, vcov = multiwayvcov::cluster.vcov(model = fit, cluster = frame_df[, cluster]))\n        }\n    }\n    col_names <- c(\"term\", \"estimate\", \"std.error\", \"p.value\")\n    if (!is.null(FE)) {\n        icpt <- unname(plyr::name_rows(lfe::getfe(fit, ef = function(gamma, addnames) absorb(gamma = gamma, addnames = addnames, .FE = frame_df[, FE]), se = T, bN = 1000, cluster = TRUE)))\n        icpt <- cbind(icpt[c(5, 1, 4)], pval = 2 * stats::pt(unlist(icpt[1])/unlist(icpt[4]), df = suppressWarnings(broom::glance(fit)[, \"df\"]), lower.tail = FALSE))\n        colnames(icpt) <- col_names\n        estout <- rbind(icpt, suppressWarnings(broom::tidy(fit)[, col_names]))\n    }\n    else {\n        estout <- broom::tidy(fit)[, col_names]\n        estout[1, 1] <- \"intercept\"\n    }\n    out <- dplyr::mutate(estout, printout = paste0(fround(estimate, digits = 3), \" [\", fround(std.error, digits = 3), \"]\"), estimate = round(estimate, digits = 3), std.error = round(std.error, digits = 3), p.value = round(p.value, digits = 3))\n    out <- dplyr::select(.data = out, term, estimate, std.error, printout, p.value)\n    list(estimates = out, stat = c(adj.r.squared = ifelse(model == \"lm\", fround(broom::glance(fit)$adj.r.squared, digits = 3), NA), n_obs = fround(nrow(frame_df), digits = 0)), model_spec = c(HETEROGENOUS = ifelse(!is.null(heterogenous), paste(heterogenous, collapse = \" \"), NA), FE = ifelse(!is.null(FE), paste(FE, collapse = \" \"), \"no\"), CLUSTER = ifelse(!is.null(cluster), paste(cluster, collapse = \" \"), \"no\"), IPW = ifelse(!is.null(IPW), paste(IPW, collapse = \" \"), \"no\")), model_status = c(R = status[1], \n        S = status[2], P = status[3]))\n}"
    ## 
    ## [[3]]$absorb
    ## [1] "absorb <- function(gamma, addnames, .FE) {\n    ws <- table(.FE, useNA = \"no\")\n    icpt <- wtd_mean(gamma, weights = ws)\n    result <- c(icpt)\n    if (addnames) {\n        names(result) <- \"intercept\"\n        attr(result, \"extra\") <- list(fe = factor(\"icpt\"), obs = factor(length(.FE)))\n    }\n    result\n}"
    ## 
    ## [[3]]$fround
    ## [1] "fround <- function(x, digits) {\n    format(round(x, digits), nsmall = digits)\n}"
    ## 
    ## [[3]]$mgsub
    ## [1] "mgsub <- function(pattern, replacement, x, ...) {\n    if (length(pattern) != length(replacement)) {\n        stop(\"pattern and replacement do not have the same length.\")\n    }\n    result <- x\n    for (i in 1:length(pattern)) {\n        result <- gsub(pattern[i], replacement[i], result, ...)\n    }\n    result\n}"
    ## 
    ## [[3]]$pfround
    ## [1] "pfround <- function(x, digits) {\n    print(fround(x, digits), quote = FALSE)\n}"
    ## 
    ## [[3]]$set_seed
    ## [1] "set_seed <- function(.seed = 12345, .parallel = FALSE) {\n    suppressMessages(stopifnot(require(mosaic)))\n    if (.parallel) \n        mosaic::set.rseed(seed = .seed)\n    else set.seed(seed = .seed)\n}"
    ## 
    ## [[3]]$wtd_mean
    ## [1] "wtd_mean <- function(x, weights = NULL, normwt = \"ignored\", na.rm = TRUE) {\n    if (!length(weights)) \n        return(mean(x, na.rm = na.rm))\n    if (na.rm) {\n        s <- !is.na(x + weights)\n        x <- x[s]\n        weights <- weights[s]\n    }\n    return(sum(weights * x)/sum(weights))\n}"
    ## 
    ## 
    ## [[4]]
    ## [[4]]$table_1
    ## [1] "mapply(FUN = analyses, MoreArgs = list(DV = \"school_grade\", treat = \"treat\", FE = \"ethnicity\", data = data_individual), covs = list(column_1 = c(\"male\", \"income\"), column_1_rep = c(\"male\", \"income\"), column_2 = NULL, column_2_rep = NULL), heterogenous = list(NULL, \"iq\", NULL, \"iq\"), subset = list(\"iq >= 50\", NULL, \"iq >= 50\", NULL), status = list(c(F, T, T), c(T, T, F), c(F, T, T), c(T, F, F)), USE.NAMES = TRUE)"
    ## 
    ## [[4]]$table_2
    ## [1] "mapply(FUN = analyses, MoreArgs = list(DV = \"turnout\", treat = \"treat\", FE = \"urban\", data = data_admin), covs = list(column_1 = c(\"age\", \"school_grade\"), column_1_rep = c(\"age\", \"school_grade\"), column_2 = c(\"height\", \"income\"), column_3 = c(\"age\", \"school_grade\", \"height\", \"income\"), column_3_rep = c(\"age\", \"school_grade\", \"height\", \"income\")), heterogenous = list(NULL, \"iq\", NULL, NULL, \"iq\"), subset = list(\"iq >= 50\", NULL, \"iq >= 50\", \"iq >= 50\", NULL), status = list(c(F, T, T), c(T, T, F), \n    c(T, T, T), c(F, T, T), c(T, F, F)), USE.NAMES = TRUE)"

### Use of `summary.replication()`

#### Genearal summary

``` r
summary(x)
```

    Miscellany:
    This is a replication of the Fake Study. The original study is conducted by Georgiy Syunyaev from Columbia University and Someone Else from Some Other University. The replication is conducted by Georgiy Syunyaev.

    Abstract:
    The aim of this study is to test the create_replication() functionality. This is the first attempt at creatreplication class of objects in [R] for systematic storage and access to study replication materials.
     
    Technical:
    There are 2 datasets provided: data_admin (50 obs. of 11 variables), data_individual (1000 obs. of 12 variables). There are 7 custom functions provided: analyses, absorb, fround, mgsub, pfround, set_seed, wtd_mean. There are 2 table replications provided: table_1, table_2. There are 9 [R] packages required for the replication: plyr, dplyr, broom, Hmisc, lfe, multiwayvcov, lmtest, wakefield, magrittr.

#### Table summary

``` r
summary(x, table = "table_1", reported = TRUE, registered = FALSE)
```

    Results for table_1 

    Reported :

    column_1 

           term estimate std.error       printout p.value
    1 intercept   85.013     1.023 85.013 [1.023]   0.000
    2     treat   -1.080     0.922 -1.080 [0.922]   0.242
    3      male   -0.298     0.924 -0.298 [0.924]   0.747
    4    income    0.000     0.000  0.000 [0.000]   0.902

    adj.r.squared = -0.003, n_obs = 997, HETEROGENOUS = NA, FE = ethnicity, CLUSTER = no, IPW = no 

    column_2 

           term estimate std.error       printout p.value
    1 intercept   84.937     0.650 84.937 [0.650]   0.000
    2     treat   -1.076     0.921 -1.076 [0.921]   0.243

    adj.r.squared = -0.001, n_obs = 997, HETEROGENOUS = NA, FE = ethnicity, CLUSTER = no, IPW = no 

``` r
summary(x, table = "table_2", reported = TRUE, registered = TRUE)
```

    Results for table_2 

    Reported :

    column_1 

              term estimate std.error       printout p.value
    1    intercept   -0.368     0.828 -0.368 [0.828]   1.341
    2        treat    0.072     0.058  0.072 [0.058]   0.219
    3          age   -0.004     0.009 -0.004 [0.009]   0.636
    4 school_grade    0.012     0.010  0.012 [0.010]   0.231

    adj.r.squared = -0.021, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 

    column_2 

           term estimate std.error       printout p.value
    1 intercept    0.711     0.653  0.711 [0.653]   0.282
    2     treat    0.051     0.057  0.051 [0.057]   0.380
    3    height   -0.001     0.004 -0.001 [0.004]   0.890
    4    income    0.000     0.000  0.000 [0.000]   0.543

    adj.r.squared = -0.048, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 

    column_3 

              term estimate std.error       printout p.value
    1    intercept   -0.215     1.100 -0.215 [1.100]   1.154
    2        treat    0.068     0.060  0.068 [0.060]   0.262
    3          age   -0.004     0.009 -0.004 [0.009]   0.690
    4 school_grade    0.012     0.010  0.012 [0.010]   0.268
    5       height    0.000     0.004  0.000 [0.004]   0.946
    6       income    0.000     0.000  0.000 [0.000]   0.662

    adj.r.squared = -0.063, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 

    Registered :

    column_1_rep 

              term estimate std.error       printout p.value
    1    intercept    0.324     1.258  0.324 [1.258]   0.798
    2        treat   -0.744     1.508 -0.744 [1.508]   0.624
    3           iq   -0.006     0.009 -0.006 [0.009]   0.474
    4          age   -0.003     0.009 -0.003 [0.009]   0.740
    5 school_grade    0.011     0.010  0.011 [0.010]   0.300
    6     treat:iq    0.008     0.015  0.008 [0.015]   0.594

    adj.r.squared = -0.055, n_obs = 50, HETEROGENOUS = iq, FE = urban, CLUSTER = no, IPW = no 

    column_2 

           term estimate std.error       printout p.value
    1 intercept    0.711     0.653  0.711 [0.653]   0.282
    2     treat    0.051     0.057  0.051 [0.057]   0.380
    3    height   -0.001     0.004 -0.001 [0.004]   0.890
    4    income    0.000     0.000  0.000 [0.000]   0.543

    adj.r.squared = -0.048, n_obs = 50, HETEROGENOUS = NA, FE = urban, CLUSTER = no, IPW = no 

    column_3_rep 

              term estimate std.error       printout p.value
    1    intercept    0.676     1.559  0.676 [1.559]   0.667
    2        treat   -0.824     1.546 -0.824 [1.546]   0.597
    3           iq   -0.007     0.009 -0.007 [0.009]   0.431
    4          age   -0.002     0.009 -0.002 [0.009]   0.820
    5 school_grade    0.010     0.011  0.010 [0.011]   0.362
    6       height   -0.001     0.004 -0.001 [0.004]   0.878
    7       income    0.000     0.000  0.000 [0.000]   0.592
    8     treat:iq    0.009     0.015  0.009 [0.015]   0.571

    adj.r.squared = -0.098, n_obs = 50, HETEROGENOUS = iq, FE = urban, CLUSTER = no, IPW = no 

#### Replication script

``` r
summary(x, script = TRUE)
```

    ############
    ## This is preamble code.
    ## Run it before the replication of your first table in the study.
    ############

    ipak <- function (pkg, quietly = FALSE) 
    {
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg)) 
            install.packages(new.pkg, dependencies = TRUE)
        loaded_packages <- sapply(pkg, require, character.only = TRUE)
        if (any(!loaded_packages)) 
            stop(paste0("The following packages required for replication failed to load: ", 
                paste0(names(pkg)[!pkg], collapse = ", "), ". This can cause failure to replicate the study."))
        if (all(loaded_packages) & !quietly) 
            cat(paste0("Succesfully installed and/or loaded all packages required for replication: ", 
                paste0(pkg, collapse = ", "), ".\n\n"))
    }

    ipak(c("plyr", "dplyr", "broom", "Hmisc", "lfe", "multiwayvcov", "lmtest", "wakefield", "magrittr"))

    analyses <- function(DV, treat, covs = NULL, heterogenous = NULL, subset = NULL, FE = NULL, cluster = NULL, IPW = NULL, data, model = "lm", status = c(TRUE, TRUE, TRUE)) {
        suppressMessages(stopifnot(require(plyr)))
        suppressMessages(stopifnot(require(dplyr)))
        suppressMessages(stopifnot(require(broom)))
        suppressMessages(stopifnot(require(Hmisc)))
        suppressMessages(stopifnot(require(lfe)))
        suppressMessages(stopifnot(require(multiwayvcov)))
        suppressMessages(stopifnot(require(lmtest)))
        if (!is.null(FE) & model != "lm") 
            stop("Function does not support FE for other than OLS models")
        frame_formula <- stats::as.formula(paste(DV, "~", paste(c(treat, covs, FE, cluster, IPW, heterogenous), collapse = " + ")))
        if (is.null(heterogenous)) {
            main_formula <- paste(c(treat, covs), collapse = " + ")
        }
        else {
            main_formula <- paste(c(treat, paste0(treat, ":", heterogenous), heterogenous, covs), collapse = " + ")
        }
        main_formula <- paste(DV, "~", main_formula)
        FE_formula <- ifelse(is.null(FE), 0, paste(FE, collapse = "+"))
        cluster_formula <- ifelse(is.null(cluster), 0, paste(cluster, collapse = "+"))
        fit_formula <- stats::as.formula(paste(main_formula, "|", FE_formula, "|", 0, "|", cluster_formula))
        frame_df <- dplyr::filter_(.data = data, .dots = subset)
        frame_df <- dplyr::filter_(.data = frame_df, .dots = paste(paste0("!is.na(", c(treat, DV, FE, cluster, IPW, heterogenous), ")"), collapse = " & "))
        frame_df <- stats::model.frame(frame_formula, data = frame_df)
        if (length(FE) > 1) 
            frame_df[, FE] <- (plyr::colwise(as.factor))(frame_df[, FE])
        if (length(FE) == 1) 
            frame_df[, FE] <- as.factor(frame_df[, FE])
        if (model == "lm") {
            if (is.null(IPW)) {
                fit <- lfe::felm(formula = fit_formula, data = frame_df)
            }
            else {
                fit <- lfe::felm(formula = fit_formula, data = frame_df, weights = unlist(frame_df[, IPW]))
            }
        }
        else if (model == "logit") {
            if (is.null(IPW)) {
                fit <- suppressWarnings(stats::glm(formula = stats::as.formula(main_formula), data = frame_df, family = binomial(link = "logit")))
            }
            else {
                fit <- suppressWarnings(stats::glm(formula = stats::as.formula(main_formula), data = frame_df, weights = unlist(frame_df[, IPW]), family = binomial(link = "logit")))
            }
            if (!is.null(cluster)) {
                fit <- lmtest::coeftest(x = fit, vcov = multiwayvcov::cluster.vcov(model = fit, cluster = frame_df[, cluster]))
            }
        }
        col_names <- c("term", "estimate", "std.error", "p.value")
        if (!is.null(FE)) {
            icpt <- unname(plyr::name_rows(lfe::getfe(fit, ef = function(gamma, addnames) absorb(gamma = gamma, addnames = addnames, .FE = frame_df[, FE]), se = T, bN = 1000, cluster = TRUE)))
            icpt <- cbind(icpt[c(5, 1, 4)], pval = 2 * stats::pt(unlist(icpt[1])/unlist(icpt[4]), df = suppressWarnings(broom::glance(fit)[, "df"]), lower.tail = FALSE))
            colnames(icpt) <- col_names
            estout <- rbind(icpt, suppressWarnings(broom::tidy(fit)[, col_names]))
        }
        else {
            estout <- broom::tidy(fit)[, col_names]
            estout[1, 1] <- "intercept"
        }
        out <- dplyr::mutate(estout, printout = paste0(fround(estimate, digits = 3), " [", fround(std.error, digits = 3), "]"), estimate = round(estimate, digits = 3), std.error = round(std.error, digits = 3), p.value = round(p.value, digits = 3))
        out <- dplyr::select(.data = out, term, estimate, std.error, printout, p.value)
        list(estimates = out, stat = c(adj.r.squared = ifelse(model == "lm", fround(broom::glance(fit)$adj.r.squared, digits = 3), NA), n_obs = fround(nrow(frame_df), digits = 0)), model_spec = c(HETEROGENOUS = ifelse(!is.null(heterogenous), paste(heterogenous, collapse = " "), NA), FE = ifelse(!is.null(FE), paste(FE, collapse = " "), "no"), CLUSTER = ifelse(!is.null(cluster), paste(cluster, collapse = " "), "no"), IPW = ifelse(!is.null(IPW), paste(IPW, collapse = " "), "no")), model_status = c(R = status[1], 
            S = status[2], P = status[3]))
    }

    absorb <- function(gamma, addnames, .FE) {
        ws <- table(.FE, useNA = "no")
        icpt <- wtd_mean(gamma, weights = ws)
        result <- c(icpt)
        if (addnames) {
            names(result) <- "intercept"
            attr(result, "extra") <- list(fe = factor("icpt"), obs = factor(length(.FE)))
        }
        result
    }

    fround <- function(x, digits) {
        format(round(x, digits), nsmall = digits)
    }

    mgsub <- function(pattern, replacement, x, ...) {
        if (length(pattern) != length(replacement)) {
            stop("pattern and replacement do not have the same length.")
        }
        result <- x
        for (i in 1:length(pattern)) {
            result <- gsub(pattern[i], replacement[i], result, ...)
        }
        result
    }

    pfround <- function(x, digits) {
        print(fround(x, digits), quote = FALSE)
    }

    set_seed <- function(.seed = 12345, .parallel = FALSE) {
        suppressMessages(stopifnot(require(mosaic)))
        if (.parallel) 
            mosaic::set.rseed(seed = .seed)
        else set.seed(seed = .seed)
    }

    wtd_mean <- function(x, weights = NULL, normwt = "ignored", na.rm = TRUE) {
        if (!length(weights)) 
            return(mean(x, na.rm = na.rm))
        if (na.rm) {
            s <- !is.na(x + weights)
            x <- x[s]
            weights <- weights[s]
        }
        return(sum(weights * x)/sum(weights))
    }

``` r
summary(x, table = "table_1", script = TRUE)
```

    ############
    ## This is preamble code.
    ## Run it before the replication of your first table in the study.
    ############

    ipak <- function (pkg, quietly = FALSE) 
    {
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg)) 
            install.packages(new.pkg, dependencies = TRUE)
        loaded_packages <- sapply(pkg, require, character.only = TRUE)
        if (any(!loaded_packages)) 
            stop(paste0("The following packages required for replication failed to load: ", 
                paste0(names(pkg)[!pkg], collapse = ", "), ". This can cause failure to replicate the study."))
        if (all(loaded_packages) & !quietly) 
            cat(paste0("Succesfully installed and/or loaded all packages required for replication: ", 
                paste0(pkg, collapse = ", "), ".\n\n"))
    }

    ipak(c("plyr", "dplyr", "broom", "Hmisc", "lfe", "multiwayvcov", "lmtest", "wakefield", "magrittr"))

    analyses <- function(DV, treat, covs = NULL, heterogenous = NULL, subset = NULL, FE = NULL, cluster = NULL, IPW = NULL, data, model = "lm", status = c(TRUE, TRUE, TRUE)) {
        suppressMessages(stopifnot(require(plyr)))
        suppressMessages(stopifnot(require(dplyr)))
        suppressMessages(stopifnot(require(broom)))
        suppressMessages(stopifnot(require(Hmisc)))
        suppressMessages(stopifnot(require(lfe)))
        suppressMessages(stopifnot(require(multiwayvcov)))
        suppressMessages(stopifnot(require(lmtest)))
        if (!is.null(FE) & model != "lm") 
            stop("Function does not support FE for other than OLS models")
        frame_formula <- stats::as.formula(paste(DV, "~", paste(c(treat, covs, FE, cluster, IPW, heterogenous), collapse = " + ")))
        if (is.null(heterogenous)) {
            main_formula <- paste(c(treat, covs), collapse = " + ")
        }
        else {
            main_formula <- paste(c(treat, paste0(treat, ":", heterogenous), heterogenous, covs), collapse = " + ")
        }
        main_formula <- paste(DV, "~", main_formula)
        FE_formula <- ifelse(is.null(FE), 0, paste(FE, collapse = "+"))
        cluster_formula <- ifelse(is.null(cluster), 0, paste(cluster, collapse = "+"))
        fit_formula <- stats::as.formula(paste(main_formula, "|", FE_formula, "|", 0, "|", cluster_formula))
        frame_df <- dplyr::filter_(.data = data, .dots = subset)
        frame_df <- dplyr::filter_(.data = frame_df, .dots = paste(paste0("!is.na(", c(treat, DV, FE, cluster, IPW, heterogenous), ")"), collapse = " & "))
        frame_df <- stats::model.frame(frame_formula, data = frame_df)
        if (length(FE) > 1) 
            frame_df[, FE] <- (plyr::colwise(as.factor))(frame_df[, FE])
        if (length(FE) == 1) 
            frame_df[, FE] <- as.factor(frame_df[, FE])
        if (model == "lm") {
            if (is.null(IPW)) {
                fit <- lfe::felm(formula = fit_formula, data = frame_df)
            }
            else {
                fit <- lfe::felm(formula = fit_formula, data = frame_df, weights = unlist(frame_df[, IPW]))
            }
        }
        else if (model == "logit") {
            if (is.null(IPW)) {
                fit <- suppressWarnings(stats::glm(formula = stats::as.formula(main_formula), data = frame_df, family = binomial(link = "logit")))
            }
            else {
                fit <- suppressWarnings(stats::glm(formula = stats::as.formula(main_formula), data = frame_df, weights = unlist(frame_df[, IPW]), family = binomial(link = "logit")))
            }
            if (!is.null(cluster)) {
                fit <- lmtest::coeftest(x = fit, vcov = multiwayvcov::cluster.vcov(model = fit, cluster = frame_df[, cluster]))
            }
        }
        col_names <- c("term", "estimate", "std.error", "p.value")
        if (!is.null(FE)) {
            icpt <- unname(plyr::name_rows(lfe::getfe(fit, ef = function(gamma, addnames) absorb(gamma = gamma, addnames = addnames, .FE = frame_df[, FE]), se = T, bN = 1000, cluster = TRUE)))
            icpt <- cbind(icpt[c(5, 1, 4)], pval = 2 * stats::pt(unlist(icpt[1])/unlist(icpt[4]), df = suppressWarnings(broom::glance(fit)[, "df"]), lower.tail = FALSE))
            colnames(icpt) <- col_names
            estout <- rbind(icpt, suppressWarnings(broom::tidy(fit)[, col_names]))
        }
        else {
            estout <- broom::tidy(fit)[, col_names]
            estout[1, 1] <- "intercept"
        }
        out <- dplyr::mutate(estout, printout = paste0(fround(estimate, digits = 3), " [", fround(std.error, digits = 3), "]"), estimate = round(estimate, digits = 3), std.error = round(std.error, digits = 3), p.value = round(p.value, digits = 3))
        out <- dplyr::select(.data = out, term, estimate, std.error, printout, p.value)
        list(estimates = out, stat = c(adj.r.squared = ifelse(model == "lm", fround(broom::glance(fit)$adj.r.squared, digits = 3), NA), n_obs = fround(nrow(frame_df), digits = 0)), model_spec = c(HETEROGENOUS = ifelse(!is.null(heterogenous), paste(heterogenous, collapse = " "), NA), FE = ifelse(!is.null(FE), paste(FE, collapse = " "), "no"), CLUSTER = ifelse(!is.null(cluster), paste(cluster, collapse = " "), "no"), IPW = ifelse(!is.null(IPW), paste(IPW, collapse = " "), "no")), model_status = c(R = status[1], 
            S = status[2], P = status[3]))
    }

    absorb <- function(gamma, addnames, .FE) {
        ws <- table(.FE, useNA = "no")
        icpt <- wtd_mean(gamma, weights = ws)
        result <- c(icpt)
        if (addnames) {
            names(result) <- "intercept"
            attr(result, "extra") <- list(fe = factor("icpt"), obs = factor(length(.FE)))
        }
        result
    }

    fround <- function(x, digits) {
        format(round(x, digits), nsmall = digits)
    }

    mgsub <- function(pattern, replacement, x, ...) {
        if (length(pattern) != length(replacement)) {
            stop("pattern and replacement do not have the same length.")
        }
        result <- x
        for (i in 1:length(pattern)) {
            result <- gsub(pattern[i], replacement[i], result, ...)
        }
        result
    }

    pfround <- function(x, digits) {
        print(fround(x, digits), quote = FALSE)
    }

    set_seed <- function(.seed = 12345, .parallel = FALSE) {
        suppressMessages(stopifnot(require(mosaic)))
        if (.parallel) 
            mosaic::set.rseed(seed = .seed)
        else set.seed(seed = .seed)
    }

    wtd_mean <- function(x, weights = NULL, normwt = "ignored", na.rm = TRUE) {
        if (!length(weights)) 
            return(mean(x, na.rm = na.rm))
        if (na.rm) {
            s <- !is.na(x + weights)
            x <- x[s]
            weights <- weights[s]
        }
        return(sum(weights * x)/sum(weights))
    }

    ############
    ## Below is the table replication code
    ############

    table_1 <- mapply(FUN = analyses, MoreArgs = list(DV = "school_grade", treat = "treat", FE = "ethnicity", data = data_individual), covs = list(column_1 = c("male", "income"), column_1_rep = c("male", "income"), column_2 = NULL, column_2_rep = NULL), heterogenous = list(NULL, "iq", NULL, "iq"), subset = list("iq >= 50", NULL, "iq >= 50", NULL), status = list(c(F, T, T), c(T, T, F), c(F, T, T), c(T, F, F)), USE.NAMES = TRUE)

    table_1
