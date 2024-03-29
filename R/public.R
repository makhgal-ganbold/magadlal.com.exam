#' Begin Exam
#'
#' It begins an online exam session on \href{https://www.magadlal.com/}{www.magadlal.com}.
#'
#' @param sisi_id string, the SISI ID of the student
#' @param password string, the exam password that is assigned to the student by \href{https://www.magadlal.com/}{www.magadlal.com}
#' @param exam_id integer, exam ID on \href{https://www.magadlal.com/}{www.magadlal.com}
#'
#' @return It returns a server response message or an error message.
#'
#' @seealso \code{\link{get_problem}}, \code{\link{check_solution}}
#'
#' @export

begin_exam <- function (sisi_id, password, exam_id) {
  tryCatch(expr = {
    response <- .interface(list(operation = "begin_exam", sisi_id = sisi_id, password = password, exam_id = exam_id))
    response$message
  }, error = function(e) {
    e$message
  })
}

#' Get Problem
#'
#' Get a new or current problem is assigned to a student by the server.
#'
#' @param sisi_id string, a SISI ID of a student
#' @param exam_id integer, exam ID on \href{https://www.magadlal.com/}{www.magadlal.com}
#' @param prevent_new_problem If \code{TRUE}, it prevents from getting a new problem accidentally and attempts to get the problem which is assigned recently.
#'
#' @return A list that contains a new or current problem is assigned to a student by the server. If an error is detected, it returns the error message.
#'
#' @seealso \code{\link{begin_exam}}, \code{\link{check_solution}}
#'
#' @export

get_problem <- function (sisi_id, exam_id, prevent_new_problem = FALSE) {
  tryCatch(expr = {
    if (isTRUE(prevent_new_problem)) {
      prevent_new_problem <- "true"
    } else if (isFALSE(prevent_new_problem)) {
      prevent_new_problem <- "false"
    } else {
      stop("You have supplied an invalid value for the argument prevent_new_problem.")
    }
    response <- .interface(list(operation = "get_problem", sisi_id = sisi_id, exam_id = exam_id, prevent_new_problem = prevent_new_problem))
    if (is.null(response$problem)) {
      response <- response$message
    }
    response
  }, error = function(e) {
    e$message
  })
}

#' Check Problem Solution
#'
#' This function checks a solution and submits it to the server when it is correct.
#'
#' @param sisi_id string, the SISI ID of the student
#' @param exam_id integer, exam ID on \href{https://www.magadlal.com/}{www.magadlal.com}
#' @param expr an expression, an R code which is a solution
#'
#' @return Typically, a server response message or a string describing the differences between a student's answer and the correct answer of the assigned problem from the server. If an error is detected, it returns the error message.
#'
#' @seealso \code{\link{begin_exam}}, \code{\link{get_problem}}
#'
#' @export

check_solution <- function (sisi_id, exam_id, expr) {
  (function () {
    .globenv <- globalenv()
    .vars <- ls(all.names = FALSE, envir = .globenv)
    .cache <- list()
    for (var in .vars) {
      .cache[[var]] <- .globenv[[var]]
    }
    rm(list = ls(all.names = TRUE, envir = .globenv), envir = .globenv)
    .cache
  })() -> .varcache
  on.exit(expr = {
    (function (.cache) {
      .globenv <- globalenv()
      .vars <- names(.cache)
      for (var in .vars) {
        .cache[[var]] -> .globenv[[var]]
      }
    })(.varcache)
  })
  (function () {
    attached.packages <- .loaded.packages()
    if (length(attached.packages) > 0) {
      for (attached.package in paste0("package:", attached.packages)) {
        detach(attached.package, character.only = TRUE)
      }
    }
    attached.packages
  })() -> .attached.packages
  on.exit(expr = {
    (function (attached.packages) {
      if (length(attached.packages) > 0) {
        for (attached.package in attached.packages) {
          library(attached.package, character.only = TRUE)
        }
      }
    })(.attached.packages)
  }, add = TRUE)
  result <- tryCatch(expr = {
    result <- eval(expr = expr, envir = new.env())
    response <- .interface(list(operation = "check_solution", sisi_id = sisi_id, exam_id = exam_id))
    if (response$message != "") {
      stop(response$message)
    }
    restricted_functions <- response$restricted_functions
    if (restricted_functions != "") {
      restricted_functions <- strsplit(x = restricted_functions, split = ",")[[1]]
      for (restricted_function in restricted_functions) {
        if (grepl(pattern = paste0("[^a-zA-Z0-9._]*",restricted_function,"[ ]*\\("), x = paste0(substitute(expr), collapse = " "))) {
          stop("You have used a restricted function.")
        }
      }
    }
    if (response$dependencies != "") {
      response$dependencies <- strsplit(x = response$dependencies, split = ",")[[1]]
      response$dependencies <- setdiff(response$dependencies, .packages(all.available = TRUE))
      if (length(response$dependencies) > 0) {
        utils::install.packages(pkgs = response$dependencies, dependencies = TRUE)
      }
    }
    if (response$test_mode == 'test') {
      result <- try(expr = {
        eval(expr = str2expression(paste(expr, response$code, sep = "; ")), envir = new.env())
      }, silent = TRUE)
      if (inherits(result, "try-error")) {
        stop("An error has occurred. Please check if your solution meets the requirements of the problem.")
      }
      if (isFALSE(result)) {
        stop("Your solution couldn't pass the test. If the result of your code is random, please try again.")
      }
    } else { # 'match'
      true_answer <- try(expr = {
        eval(expr = parse(text = response$code), envir = new.env())
      }, silent = TRUE)
      if (inherits(true_answer, "try-error")) {
        stop("Could not check your solution.")
      }
      result <- all.equal(target = true_answer, current = result, check.attributes = FALSE)
    }
    result
  }, error = function(e) {
    e$message
  })
  if (isTRUE(result)) {
    return(tryCatch(expr = {
      response <- .interface(list(operation = "submit_solution", sisi_id = sisi_id, exam_id = exam_id, code = paste0(deparse(substitute(expr)), collapse = "\n")))
      response$message
    }, error = function(e) {
      e$message
    }))
  }
  result
}
