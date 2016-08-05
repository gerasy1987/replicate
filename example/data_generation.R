rm(list = ls())

if (!require(pacman)) install.packages(pacman)
pacman::p_load(plyr, dplyr, broom, Hmisc, lfe, multiwayvcov, lmtest,
               wakefield, magrittr)

data_individual <-
  r_data_frame(
    n = 1000,
    vote = rbinom(size = 0:1, prob = c(.4,.6)),
    age(x = 18:65, name = "age"),
    race(x = c("white", "hispanic", "black", "asian"),
         prob = c(0.5, 0.20, 0.20, 0.1), name = "ethnicity"),
    r_dummy(sex, x = c("male", "female"), name = "gender"),
    grade(sd = 18, name = "school_grade"),
    income(digits = 1, name = "income"),
    iq(mean = 100, sd = 20, name = "iq", digits = 3),
    height_cm(mean = 175, sd = 30, name = "height", digits = 3)
  ) %>%
  r_na(cols = -c(1:9), prob=.1)

data_individual %<>%
  mutate(treat = rep(sample(rep(0:1, each = 25), 50, replace = FALSE), each = 20),
         village_id = rep(1:50, each = 20),
         ind_id = 1:1000)

data_admin <-
  data_individual %>%
  group_by(village_id) %>%
  summarise_at(.cols = vars(age, school_grade, income, iq, height, treat),
               .funs = funs(mean(., na.rm = TRUE)))

data_admin <-
  r_data_frame(
    n = 50,
    turnout = rbeta(shape1 = 2, shape2 = 2),
    r_dummy(area, name = "area", x = c("urban", "rural")),
    population = rpois(lambda = 1000)
  ) %>%
  cbind(data_admin, .) %>%
  as.tbl

save(data_individual, data_admin, file = "replication_data.Rdata")
