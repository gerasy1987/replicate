rm(list = ls())
library(testthat)
library(replicate)

context("create replication object")

test_that("replication object creation works", {

  # NEED TO REPLACE THIS
  data_admin <- data_individual <- datasets::mtcars

  x <-
    create_replication(
      data_list =
        list(data_admin = data_admin,
             data_individual = data_individual),
      packages =
        c("plyr", "dplyr", "broom", "Hmisc",
          "lfe", "multiwayvcov", "lmtest",
          "wakefield", "magrittr"),
      project_path = NULL,
      function_script_path = "replication_functions.R",
      replication_script_path = "replication_script.R",
      description_list =
        list(study_name = "Fake Study",
             study_authors = c("Georgiy Syunyaev", "Someone Else"),
             study_affiliations = c("Columbia University",
                                    "Some Other University"),
             rep_authors = c("Georgiy Syunyaev"),
             study_abstract = "The aim of this study is to test the create_replication() functionality. This is the first attempt at creating replication class of objects in [R] for systematic storage and access to study replication materials."),
      quietly = TRUE,
      checks = FALSE
    )


  expect_true(
    all("summary.replication" %in% list(class(summary(x, script = TRUE)),class(summary(x))))
  )

  # Nothing to report, nothing returned
  expect_equal(class(summary(x, table = "table_1")), "NULL")

  # Throws error since table_1 code does not work on mtcars dataset
  expect_error(summary(x, table = "table_1", reported = TRUE))

})
