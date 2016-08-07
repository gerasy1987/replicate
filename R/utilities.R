#' FUNCTION TITLE
#'
#' FUNCTION DESCRIPTION
#'
#' @param pkg DESCRIPTION.
#' @param quietly DESCRIPTION.
#'
#' @return RETURN DESCRIPTION
#' @examples
#' # ADD EXAMPLES HERE
#'
#' @importFrom utils install.packages installed.packages
#'
#' @export

ipak <- function(pkg, quietly = FALSE) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg,
                     dependencies = TRUE)
  loaded_packages <- sapply(pkg,
                            require,
                            character.only = TRUE,
                            warn.conflicts = !quietly)
  if (any(!loaded_packages))
    stop(paste0("The following packages required for replication failed to load: ",
                paste0(names(pkg)[!loaded_packages], collapse = ", "),
                ". This can cause failure to replicate the study.") )
  if (all(loaded_packages) & !quietly)
    cat(paste0("Succesfully installed and/or loaded all packages required for replication: ",
               paste0(pkg, collapse = ", "), ".\n\n"))
}
