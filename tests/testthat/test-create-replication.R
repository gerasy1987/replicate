context("Create replication object")

data_admin <- read.csv("data_admin.csv")
data_individual <- read.csv("data_individual.csv")


test_that("there is no error with no checks", {
  expect_true("replication" %in%
                class(
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
                  ))
  )
})


test_that("there is an error with checks, since the required packages are not there", {
  # Checks of replication object should pass
  expect_error(create_replication(
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
                    checks = TRUE
                  ))
})
