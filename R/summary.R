#' Summary method for replication class
#'
#' FUNCTION DESCRIPTION
#'
#' @param object DESCRIPTION.
#' @param table DESCRIPTION.
#' @param reported DESCRIPTION.
#' @param registered DESCRIPTION.
#' @param script DESCRIPTION.
#' @param desc DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @return RETURN DESCRIPTION
#' @examples
#' # ADD EXAMPLES HERE
#'
#' @export

summary.replication <- function(object,
                                table = NULL,
                                reported = FALSE,
                                registered = FALSE,
                                script = FALSE,
                                desc = NULL,
                                ...) {
  if (is.null(table) & !script) {
    out <- attr(x = object, which = "misc")

    structure(out, class = c("summary.replication"))
  } else if (!is.null(table) & !script & any(reported, registered)) {
    table_name <- paste0(unlist(strsplit(x = tolower(table), split = " ")), collapse = "_")

    attach(environment(object))
    out <- eval(parse(text = object$tables[[table_name]]))
    rep <- reg <- NULL
    if (reported) rep <- which(do.call(cbind, out["model_status",])["P",])
    if (registered) reg <- which(do.call(cbind, out["model_status",])["R",])

    out <- list("Reported" = out[,rep],
                "Registered" = out[,reg])
    attr(out, which = "name") <- table
    detach(environment(object))

    structure(out, class = c("summary.replication", "replication.table"))
  } else if (script & !any(reported, registered)) {
    script_preamble <-
      paste("############\n## This is preamble code.\n## Run it before the replication of your first table in the study.\n############",
            paste0("ipak <- ", paste0(deparse(ipak), collapse = "\n")),
            paste0("ipak(", paste0(deparse(object$packages), collapse = ""), ")"),
            paste(do.call(c, object$functions), collapse = "\n\n"),
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


