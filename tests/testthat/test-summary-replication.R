context("Summary method for replication")

data_admin <- read.csv("data_admin.csv")
data_individual <- read.csv("data_individual.csv")

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

test_that("misc summary works", {
  expect_true(
    all("summary.replication" %in% list(class(summary(x, script = TRUE)),class(summary(x))))
  )
})

test_that("nothing returned with no reported/registered", {
  expect_equal(class(summary(x, table = "table_1")), "NULL")
})

test_that("replication table is not produced, since the required packages are not there", {
  # Test table replication reported
  expect_error(summary(x, table = "table_1", reported = TRUE))
  # Test table replication reported
  expect_error(summary(x, table = "table_1", registered = TRUE))
  # Test name in a seinsible format
  expect_error(summary(x, table = "Table 2", reported = TRUE))
})


test_that("replication script is produced", {
  # Test script for table replication
  expect_equal(class(summary(x, script = TRUE)),
               c("summary.replication","replication.script"))
  # Test script for table replication
  expect_equal(class(summary(x, table = "table_1", script = TRUE)),
               c("summary.replication","replication.script"))
})
