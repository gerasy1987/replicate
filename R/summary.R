#' Summary method for replication class
#'
#' FUNCTION DESCRIPTION
#'
#' @param object Object of class replication created by \code{create_replication()}.
#' @param table Character string specifying the table to be replicated. The table name should include number of the table as specified in \code{replication_script_path} file and the word "table".
#' @param published Logical. Whether to show columns with specifications published in the paper.
#' @param registered Logical. Whether to show columns with specifications registered in PAP.
#' @param script Logical. Whether to print the script to replicate the results of the study. If \code{table = NULL}, then returns preamble which includes all the functions and packages required for replication. If \code{table != NULL}, then returns preamble and the code for replication of the specified table.
#' @param desc To be implemented...
#' @param ... To be implemented...
#'
#' @return RETURN DESCRIPTION
#' @examples
#' # ADD EXAMPLES HERE
#'
#' @export

summary.replication <- function(object,
                                table = NULL,
                                published = TRUE,
                                registered = FALSE,
                                script = FALSE,
                                desc = NULL,
                                ...) {
  if (is.null(table) & !script) {
    out <- attr(x = object, which = "misc")

    structure(out, class = c("summary.replication"))
  } else if (!is.null(table) & !script & any(published, registered)) {
    table_name <- paste0(unlist(strsplit(x = tolower(table), split = " ")), collapse = "_")

    attach(environment(object))
    out <- eval(parse(text = object$tables[[table_name]]))
    rep <- reg <- NULL
    if (published) rep <- which(do.call(cbind, out["model_status",])["P",])
    if (registered) reg <- which(do.call(cbind, out["model_status",])["R",])

    out <- list("Published" = out[,rep],
                "Registered" = out[,reg])
    attr(out, which = "name") <- table
    detach(environment(object))

    structure(out, class = c("summary.replication", "replication.table"))
  } else if (script) {
    script_preamble <-
      paste("############\n## This is preamble code.\n## Run it before the replication of your first table in the study.\n############",
            paste0("ipak <- ", paste0(deparse(ipak), collapse = "\n")),
            paste0("ipak(", paste0(deparse(object$packages), collapse = ""), ")"),
            paste(do.call(c, object$functions), collapse = "\n\n"),
            do.call("paste",
                    list(
                      lapply(names(object$data),
                             FUN = function(x) paste0(x, " <- ", deparse(substitute(object)), "$data$", x)),
                      collapse = "\n\n")),
            sep = "\n\n")
    if (is.null(table)) {
      out <- c(preamble = script_preamble)
    } else {
      table_name <- paste0(unlist(strsplit(x = tolower(table), split = " ")), collapse = "_")
      out <- c(preamble = script_preamble,
               table = paste0("############\n## Below is the table replication code\n############\n\n",
                              table_name, " <- ", object$tables[[table_name]],
                              "\n\n", table_name))
    }
    structure(out, class = c("summary.replication", "replication.script"))
  }
}

#' @export
print.summary.replication <- function(x, ...) {
  if ("replication.table" %in% class(x)) {
    class(x) <- "list"
    cat("Results for", attr(x, which = "name"), "\n\n")
    for (i in 1:length(x)) {
      if (length(x[[i]]) != 0) {
        cat(names(x)[i], ":\n\n")
        for (j in (1:dim(x[[i]])[2])){
          cat(colnames(x[[i]])[j],"\n\n")
          print(x[[i]][["estimates",j]])
          cat("\n")
          stats_specs <- c(x[[i]][["stat",j]],x[[i]][["model_spec",j]])
          cat(paste(paste0(names(stats_specs), " = ", stats_specs), collapse = ", "), "\n\n")
        }
      }
    }
    invisible(x)
  } else if ("replication.script" %in% class(x)) {
    class(x) <- "character"
    cat(x, sep = "\n\n")
    invisible(x)
  } else {
    class(x) <- "list"
    cat(unlist(x))
    invisible(x)
  }
}


