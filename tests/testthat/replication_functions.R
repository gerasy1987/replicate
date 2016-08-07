analyses <- function(DV,
                     treat,
                     covs = NULL,
                     heterogenous = NULL,
                     subset = NULL,
                     FE = NULL,
                     cluster = NULL,
                     IPW = NULL,
                     data,
                     model = "lm",
                     status = c(TRUE, TRUE, TRUE)) {

  # required packages
  suppressMessages(stopifnot(require(plyr)))
  suppressMessages(stopifnot(require(dplyr)))
  suppressMessages(stopifnot(require(broom)))
  suppressMessages(stopifnot(require(Hmisc)))
  suppressMessages(stopifnot(require(lfe)))
  suppressMessages(stopifnot(require(multiwayvcov)))
  suppressMessages(stopifnot(require(lmtest)))

  if (!is.null(FE) & model != "lm")
    stop("Function does not support FE for other than OLS models")

  # generate the formula to use in model.frame to produce nice data frame
  frame_formula <-
    stats::as.formula(
      paste(DV, "~",
            paste(c(treat, covs, FE, cluster, IPW, heterogenous), collapse = " + ")
      )
    )

  if (is.null(heterogenous)) {
    main_formula <- paste(c(treat, covs), collapse = " + ")
  } else {
    main_formula <-
      paste(c(treat,
              paste0(treat, ":", heterogenous), heterogenous, covs),
            collapse = " + ")
  }

  main_formula <- paste(DV, "~", main_formula)

  FE_formula <- ifelse(is.null(FE), 0, paste(FE, collapse = "+"))
  cluster_formula <- ifelse(is.null(cluster), 0, paste(cluster, collapse = "+"))

  fit_formula <-
    stats::as.formula(
      paste(main_formula,"|", FE_formula, "|", 0, "|", cluster_formula)
    )

  # generate formula for estimation taking into account possible heterogenous effects


  # modify initial dataset to the dataset only with needed information
  frame_df <- dplyr::filter_(.data = data, .dots = subset)
  frame_df <-
    dplyr::filter_(.data = frame_df,
                   .dots =
                     paste(paste0("!is.na(",c(treat, DV, FE, cluster, IPW, heterogenous),")"),
                           collapse = " & "
                     ) )
  frame_df <- stats::model.frame(frame_formula, data = frame_df)


  # transform fixed effects to be factors
  if (length(FE) > 1)
    frame_df[, FE] <- plyr::colwise(as.factor)(frame_df[, FE])

  if (length(FE) == 1)
    frame_df[, FE] <- as.factor(frame_df[, FE])

  if (model == "lm"){

    if (is.null(IPW)){
      fit <- lfe::felm(formula = fit_formula,
                       data = frame_df)
    } else {
      fit <- lfe::felm(formula = fit_formula,
                       data = frame_df,
                       weights = unlist(frame_df[,IPW]))
    }
  } else if (model == "logit") {
    if (is.null(IPW)){
      fit <-
        suppressWarnings(
          stats::glm(formula = stats::as.formula(main_formula),
                     data = frame_df,
                     family = binomial(link="logit"))
        )
    } else {
      fit <-
        suppressWarnings(
          stats::glm(formula = stats::as.formula(main_formula),
                     data = frame_df,
                     weights = unlist(frame_df[,IPW]),
                     family = binomial(link="logit"))
        )
    }

    if (!is.null(cluster)) {
      fit <- lmtest::coeftest(x = fit,
                              vcov = multiwayvcov::cluster.vcov(model = fit,
                                                                cluster = frame_df[,cluster]))
    }
  }

  col_names <- c("term", "estimate", "std.error","p.value")

  if (!is.null(FE)){

    icpt <-
      unname(
        plyr::name_rows(
          lfe::getfe(fit, ef = function(gamma,addnames) absorb(gamma = gamma,
                                                               addnames = addnames,
                                                               .FE = frame_df[, FE]),
                     se = T, bN = 1000, cluster = TRUE)
        )
      )
    icpt <- cbind(icpt[c(5,1,4)],
                  "pval" = 2*stats::pt(unlist(icpt[1])/unlist(icpt[4]),
                                       df = suppressWarnings(broom::glance(fit)[,"df"]),
                                       lower.tail = FALSE)
    )
    colnames(icpt) <- col_names
    estout <- rbind(icpt, suppressWarnings(broom::tidy(fit)[,col_names]))
  } else {
    estout <- broom::tidy(fit)[,col_names]
    estout[1,1] <- "intercept"
  }

  # subset only the estimated coefficients related to treatment(s) and intercepts

  # generate a nice text output
  out <-
    dplyr::mutate(estout,
                  printout = paste0(fround(estimate, digits = 3),
                                    " [", fround(std.error, digits = 3), "]"),
                  estimate = round(estimate, digits = 3),
                  std.error = round(std.error, digits = 3),
                  p.value = round(p.value, digits = 3))
  out <- dplyr::select(.data = out,
                       term, estimate, std.error, printout,p.value)

  # return list with adjusted r.sq, estimates and number of observations
  list(estimates = out,
       stat = c(
         adj.r.squared = ifelse(model == "lm", fround(broom::glance(fit)$adj.r.squared,digits = 3), NA),
         n_obs = fround(nrow(frame_df), digits = 0)
       ),
       model_spec = c(HETEROGENOUS = ifelse(!is.null(heterogenous), paste(heterogenous, collapse = " "), NA),
                      FE = ifelse(!is.null(FE), paste(FE, collapse = " "), "no"),
                      CLUSTER = ifelse(!is.null(cluster), paste(cluster, collapse = " "), "no"),
                      IPW = ifelse(!is.null(IPW), paste(IPW, collapse = " "), "no")),
       model_status = c(R = status[1],
                        S = status[2],
                        P = status[3])
  )
}



absorb <- function(gamma, addnames, .FE){
  ws <- table(.FE, useNA = 'no')
  icpt <- wtd_mean(gamma, weights = ws)  # first level of f1
  result <- c(icpt)
  if(addnames) {
    names(result) <- "intercept"
    attr(result, "extra") <- list(fe = factor("icpt"),
                                  obs = factor(length(.FE)))
  }
  result
}



fround <- function (x, digits) {
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

pfround <- function (x, digits) {
  print(fround(x, digits), quote = FALSE)
}

set_seed <- function(.seed = 12345, .parallel = FALSE) {
  # required packages
  suppressMessages(stopifnot(require(mosaic)))

  if (.parallel) mosaic::set.rseed(seed = .seed)
  else set.seed(seed = .seed)
}

wtd_mean <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE) {
  if (!length(weights))
    return(mean(x, na.rm = na.rm))
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  return(sum(weights * x)/sum(weights))
}
