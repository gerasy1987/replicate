#' @export
print.replication <- function(object, ...){
  cat(paste0(unlist(attr(x = object, which = "misc")), collapse = ""), "\n\n")
  x <- object
  class(x) <- "list"
  attributes(x) <- NULL
  print(x)
  invisible(object)
}

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
print.summary.replication <- function(object, ...) {
  if ("replication.table" %in% class(object)) {
    class(object) <- "list"
    cat("Results for", attr(object, which = "name"), "\n\n")
    for (i in 1:length(object)) {
      if (length(object[[i]]) != 0) {
        cat(names(object)[i], ":\n\n")
        for (j in (1:dim(object[[i]])[2])){
          cat(colnames(object[[i]])[j],"\n\n")
          print(object[[i]][["estimates",j]])
          cat("\n")
          stats_specs <- c(object[[i]][["stat",j]],object[[i]][["model_spec",j]])
          cat(paste(paste0(names(stats_specs), " = ", stats_specs), collapse = ", "), "\n\n")
        }
      }
    }
    invisible(object)
  } else if ("replication.script" %in% class(object)) {
    class(object) <- "character"
    cat(object, sep = "\n\n")
    invisible(object)
  } else {
    class(object) <- "list"
    cat(unlist(object))
    invisible(object)
  }
}


