table_1 <-
  mapply(FUN = analyses,
         MoreArgs = list(DV = "school_grade",
                         treat = "treat",
                         FE = "ethnicity",
                         data = data_individual),
         covs = list(column_1 = c("male", "income"),
                     column_1_rep = c("male", "income"),
                     column_2 = NULL,
                     column_2_rep = NULL),
         heterogenous = list(NULL,"iq", NULL, "iq"),
         subset = list("iq >= 50", NULL, "iq >= 50", NULL),
         status = list(c(F,T,T), c(T,T,F), c(F,T,T), c(T,F,F)  ),
         USE.NAMES = TRUE)

table_2 <-
  mapply(FUN = analyses,
         MoreArgs = list(DV = "turnout",
                         treat = "treat",
                         FE = "urban",
                         data = data_admin),
         covs = list(column_1 = c("age", "school_grade"),
                     column_1_rep = c("age", "school_grade"),
                     column_2 = c("height", "income"),
                     column_3 = c("age", "school_grade", "height", "income"),
                     column_3_rep = c("age", "school_grade", "height", "income")
                     ),
         heterogenous = list(NULL,"iq", NULL, NULL, "iq"),
         subset = list("iq >= 50", NULL, "iq >= 50", "iq >= 50", NULL),
         status = list(c(F,T,T), c(T,T,F), c(T,T,T), c(F,T,T), c(T,F,F) ),
         USE.NAMES = TRUE)
