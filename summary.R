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
                                reported = TRUE,
                                registered = FALSE,
                                desc = FALSE,
                                data_desc = FALSE,
                                ...) {
  if (is.null(table)) {
    out <- attr(x = object, which = "misc")

    structure(out, class = c("summary.replication"))

  } else {
    table_name <- paste0(unlist(strsplit(x = tolower(table), split = " ")), collapse = "_")

    attach(environment(object), warn.conflicts = FALSE)
    out <- eval(parse(text = object$tables[[table_name]]))
    rep <- reg <- NULL
    if (reported) rep <- which(do.call(cbind, out["model_status",])["P",])
    if (registered) reg <- which(do.call(cbind, out["model_status",])["R",])

    out <- list("Reported" = out[,rep],
                "Registered" = out[,reg])
    attr(out, which = "name") <- table
    detach(environment(object))

    structure(out, class = c("summary.replication", "replication_table"))
  }
}

#' @export
print.summary.replication <- function(object, ...) {
  if (!("replication_table" %in% class(object))) {
    class(object) <- "list"
    cat(paste0(unlist(object), collapse = ""))
    invisible(object)
  } else {
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
  }
}
