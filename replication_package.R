
# Sketch of replication package

description <- "This archive replicates dummy study. There are 2 data sets and 2 tables."
result_to_table_mapping <- matrix(c(1,1,0,0,0,1),3)
rownames(result_to_table_mapping) <- paste("result", 1:3)
colnames(result_to_table_mapping) <- paste("table", 1:2)
meta <- list()
meta[[1]] <- description
meta[[2]] <- result_to_table_mapping

data1 <- data.frame(X1 = runif(10), Y1 = runif(10))
data2 <- data.frame(X2 = runif(10), Y2 = runif(10))
data <- list()
data[[1]] <- data1
data[[2]] <- data2
names(data) <- c("data1", "data2")

tables <- list()
table_1 <- function() {coef(summary(lm(Y1~X1, data = data1)))[1:2,]}
table_2 <- function() {coef(summary(lm(Y2~X2, data = data2)))[1:2,]}
tables[[1]] <- table_1
tables[[2]] <- table_2
names(tables) <- c("table_1", "table_2")

tables_reported <- list()
tables_reported[[1]] <- table_1()
tables_reported[[2]] <- table_2()
names(tables_reported) <- c("table_1", "table_2")

dummystudy <- list()
dummystudy[[1]] <- meta
dummystudy[[2]] <- data
dummystudy[[3]] <- tables
dummystudy[[4]] <- tables_reported

names(dummystudy) <- c("meta", "data", "tables", "tables_reported")

dummystudy


table_replicate <- function(
  table_name,
  study,
  include_reported = FALSE,
  return_code = FALSE,
  replicate = FALSE,
  check = FALSE,
  warn.conflicts=FALSE) {

  if(include_reported) {
    eval(parse(text = paste("reported <- ",study,"$tables_reported$",table_name, sep="")))
    print(paste("The archived results for", table_name, " from ", study,  "are"))
    print(reported)
    }

  if(return_code) {print("The replication code is:"); print(
      eval(parse(text = paste(study,"$tables$",table_name, sep=""))))
    }


  if(replicate) {
    eval(parse(text = paste("attach(", study, "$data, warn.conflicts= warn.conflicts)", sep="")))
    print(paste("This is the replication of ", table_name, " from ", study, ":", sep = ""))
    print(eval(parse(text = paste(study,"$tables$",table_name,"()", sep=""))))
    eval(parse(text = paste("detach(", study, "$data)", sep="")))
  }

  if(check) {
    eval(parse(text = paste("attach(", study, "$data, warn.conflicts= warn.conflicts)", sep="")))
    print(paste("Comparison of reported and relicated results for ", table_name, " from ", study, ":", sep = ""))
    eval(parse(text = paste("compare <-", study,"$tables$",table_name,"() == ", study, "$tables_reported$", table_name, sep="")))
    eval(parse(text = paste("detach(", study, "$data)", sep="")))
    print(compare)
  }

    }

describe_data <- function(study, summaries = FALSE, hmiscdescribe = FALSE){
  eval(parse(text = paste("data <-", study, "$data", sep = "")))
  print(paste("The archive contains ", length(data), " datasets named ", paste(names(data), collapse = ", "), sep = ""))
  if(summaries) {print(lapply(data, summary))}
  if(hmiscdescribe) {require(Hmisc); lapply(data, describe)}
}


# Examples of replications of tables from a given study archive "dummystudy"
table_replicate(table_name = "table_1", study = "dummystudy", replicate = TRUE)

table_replicate(table_name = "table_1", study = "dummystudy", return_code = TRUE)

table_replicate(table_name = "table_1", study = "dummystudy", include_reported = TRUE)

table_replicate(table_name = "table_1", study = "dummystudy", check = TRUE)

table_replicate(table_name = "table_2", study = "dummystudy", replicate = TRUE, include_reported = TRUE)


# Example of the quick creation of a new replication archive and the replication of a table from that archive
otherstudy <- dummystudy
otherstudy$data$data1 <- data.frame(A = runif(100), B = runif(100))

table_replicate(table_name = "table_1", study = "otherstudy", replicate = TRUE)


# Other things
describe_data(study = "otherstudy", summaries = FALSE)

describe_data(study = "otherstudy", summaries = TRUE)

describe_data(study = "otherstudy", hmiscdescribe = TRUE)

