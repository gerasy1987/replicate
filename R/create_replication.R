#' Create replication object
#'
#' FUNCTION DESCRIPTION
#'
#' @param description_list DESCRIPTION.
#' @param packages DESCRIPTION.
#' @param project_path DESCRIPTION.
#' @param data_list DESCRIPTION.
#' @param function_script_path DESCRIPTION.
#' @param replication_script_path DESCRIPTION.
#' @param quietly DESCRIPTION.
#' @param checks DESCRIPTION.
#'
#' @return RETURN DESCRIPTION
#' @examples
#' # ADD EXAMPLES HERE
#'
#' @importFrom dplyr tbl as.tbl
#' @importFrom readr read_file
#' @importFrom magrittr %>%
#'
#' @export

create_replication <- function(description_list,
                               packages = NULL,
                               project_path = NULL,
                               data_list,
                               function_script_path,
                               replication_script_path,
                               quietly = FALSE,
                               checks = TRUE) {

  # required packages
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("magrittr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)

  # checks
  if (!(class(packages) %in% c("NULL", "character")))
    stop("Packages required for replication should be specified as character vector of names.")
  if (!all(sapply(data_list,
                  function(x) is.data.frame(x) | is.matrix(x))))
    stop("All element of data_list should be of either data.frame or matrix class.")
  if (ifelse(!is.null(project_path), !dir.exists(project_path), FALSE))
    stop("Project path does not exist on this machine. ",
         "Please check the project_path option of create_replication call.")
  if (!all(file.exists(paste0(project_path, c(function_script_path,
                                              replication_script_path)))))
    stop("Either function or replication calls script name provided incorrectly, ",
         "or do not exist in specified project directory path.")

  # empty lists
  replication <- functions_list <- table_list <- environment_list <- list()

  # check packages
  if (!is.null(packages)) {
    if (!quietly){
      cat("Do you want to check and load packages required for replication? (Yes/No):")
      check_pkg <- readLines(n = 1)
    } else {
      check_pkg <- ifelse(checks, "y", "n")
    }
    if (ifelse(is.character(check_pkg),
               !(tolower(check_pkg) %in% c("n","no")),
               FALSE) ) {
      ipak(packages, quietly = quietly)
    } else if (!quietly) {
      warning("You chose not to check that all packages required for replication are installed. ",
              "The replication of some of the results might not be ",
              "possible without required packages")
    }
  }

  replication$packages <- packages

  # read data
  if (!is.list(class(data_list))) data_list <- as.list(data_list)
  if (is.null(names(data_list))) stop("Please provide names for each object of data_list argument.")
  for (i in 1:length(data_list)) {
    if (all(sapply(data_list, class) %in% c("data.frame","tbl"))){
      environment_list[[names(data_list)[i]]] <- dplyr::as.tbl( data_list[[i]] )
    } else if (all(sapply(data_list, class) %in% c("character"))) {
      environment_list[[names(data_list)[i]]] <-
        dplyr::as.tbl( readr::read_file(paste0(project_path,data_list[[i]])) ) # need to test this
    }
  }

  replication$data <- data_list

  # parse and create functions list
  parsed_functions <- parse(file = paste0(project_path, function_script_path))

  for (i in 1:length(parsed_functions)) {
    character_call <- as.character(parsed_functions[[i]])
    if (character_call[1] == "<-") {
      functions_list[[character_call[2]]] <- paste(character_call[c(2,1,3)], collapse = " ")
      environment_list[[character_call[2]]] <- eval(parse(text = character_call[3]))
    } else if (as.character(replication_functions[[i]])[1] != "<-") {
      stop(paste("Either the function script has calls which do not create functions or",
                 "some of the custom functions for replication are not named using '<-' operator.",
                 "Please check the following file for one of those errors:",
                 paste0(project_path, function_script_path), "."))
    }
  }

  replication$functions <- functions_list

  parsed_replication_calls <- parse(file = paste0(project_path, replication_script_path))

  for (i in 1:length(parsed_replication_calls)) {
    character_call <- as.character(parsed_replication_calls[[i]])
    if (character_call[1] == "<-") {
      table_list[[character_call[2]]] <- paste(character_call[3], collapse = " ")
    } else if (as.character(replication_functions[[i]])[1] != "<-") {
      stop(paste("Some of the calls in replication script are not using '<-' operator.",
                 "Please check the following file for one of those errors:",
                 paste0(project_path, replication_script_path), "."))
    }
  }

  replication$tables <- table_list

  for (i in 1:length(description_list))
    attr(replication, which = names(description_list)[i]) <- description_list[[i]]

  study_misc <-
    "Miscellany:\n" %>%
    add_study_description(pattern = "^(?=.*nam)(?=.*stud).*$",
                          description_text = "This is a replication of the ",
                          list = description_list) %>%
    add_study_description(pattern = c("^(?=.*auth)(?=.*stud).*$",
                                      "^(?=.*affil)(?=.*stud).*$"),
                          description_text = "The original study is conducted by ",
                          list = description_list,
                          merge_by = "from") %>%
    add_study_description(pattern = c("^(?=.*auth)(?=.*rep).*$",
                                      "^(?=.*affil)(?=.*rep).*$"),
                          description_text = "The replication is conducted by ",
                          ends_with = ".\n",
                          list = description_list,
                          merge_by = "from") %>%
    add_study_description(pattern = "^(?=.*abstract)(?=.*stud).*$",
                          description_text = "\nAbstract:\n",
                          collapse_pattern = ". ",
                          ends_with = "\n",
                          list = description_list)

  technical_misc <-
    "\nTechnical:\n" %>%
    add_tech_description(type_of_object = "dataset",
                         list = data_list,
                         add_stat = function(x) paste(dim(x)[1], "obs. of",
                                                      dim(x)[2], "variables")) %>%
    add_tech_description(type_of_object = "custom function",
                         list = functions_list) %>%
    add_tech_description(type_of_object = "table replication",
                         list = table_list) %>%
    add_tech_description(type_of_object = "[R] package",
                         ends_with = ".",
                         middle_part = " required for the replication: ",
                         list = as.list(packages) %>% `names<-`(packages))

  attr(replication, which = "misc") <- list(study = study_misc,
                                            tech = technical_misc)
  if (!quietly){
    cat("Do you want to check that replication works? (Yes/No):")
    check_rep <- readLines(n = 1)
  } else {
    check_rep <- ifelse(checks, "y", "n")
  }
  if (ifelse(is.character(check_rep),
             !(tolower(check_rep) %in% c("n","no")),
             FALSE)) {
    attach(list2env(environment_list), warn.conflicts = !quietly)

    for (i in 1:length(table_list)) {
      try(expr = eval(parse(text = table_list[[i]])), silent = TRUE)
      if ( class(try(expr = eval(parse(text = table_list[[i]])),
                     silent = TRUE)) == "try-error" & !quietly) {
        warning(paste0("The check of replication of ", names(table_list)[i], " failed."))
      } else if (!quietly) {
        cat(paste0("Succesfully replicated ", names(table_list)[i], ".\n"))
      }
    }
    Sys.sleep(2)

    detach(list2env(environment_list))
  }

  environment(replication) <- list2env(x = environment_list)

  structure(replication, class = c("replication","list"))
}

#' @export
print.replication <- function(x, ...){
  cat(paste0(unlist(attr(x = x, which = "misc")), collapse = ""), "\n\n")
  object <- x
  class(object) <- "list"
  attributes(object) <- NULL
  print(object)
  invisible(x)
}

add_study_description <- function(starting_description,
                                  pattern,
                                  description_text,
                                  collapse_pattern = " and ",
                                  ends_with = ". ",
                                  list,
                                  merge_by = NULL) {
  if (is.null(merge_by)){

    grep(pattern, names(list), ignore.case = TRUE, perl = TRUE) %>%
    {
      ifelse(length(.) != 0,
             yes = paste0(starting_description, description_text,
                          paste(list[[.]], collapse = collapse_pattern), ends_with),
             no = paste0(starting_description, ends_with) )
    }

  } else if (!is.null(merge_by) & (length(pattern) > 1) &
             (length(merge_by) = (length(pattern) - 1))) {
    pattern_match <- c()
    for (i in 1:length(pattern))
      pattern_match <- c(pattern_match, grep(pattern[i], names(list),
                                             ignore.case = TRUE, perl = TRUE))

    j <- 1
    out <- list[[pattern_match[j]]]
    while (j < length(pattern_match)) {
      out <- paste(out, merge_by, list[[pattern_match[j+1]]])
      j <-  j + 1
    }

    ifelse(length(pattern_match) != 0,
           yes = paste0(starting_description, description_text,
                        paste(out, collapse = collapse_pattern), ends_with),
           no = paste0(starting_description, ends_with) )
  } else {
    stop("There should be multiple regex patterns specified in character vector if merge_by is not NULL. Also the length of merge_by vector should be length of pattern vector minus 1.")
  }
}

add_tech_description <- function(starting_description,
                                 type_of_object,
                                 middle_part = " provided: ",
                                 collapse_pattern = ", ",
                                 ends_with = ". ",
                                 list,
                                 add_stat = NULL) {
  part1 <- paste0(starting_description,
                  ifelse(length(list) == 1, "There is ", "There are "),
                  length(list), " ", type_of_object,
                  ifelse(length(list) == 1, "", "s"), middle_part)
  if (!is.null(add_stat) & class(add_stat) == "function"){
    part2 <-
      paste0(paste(paste0(names(list), " (",
                          sapply(list, FUN = add_stat), ")"),
                   collapse = collapse_pattern),
             ends_with)
  } else if (is.null(add_stat)) {
    part2 <- paste0(paste(names(list), collapse = ", "),
                    ends_with)
  }
  paste0(part1, part2)
}
