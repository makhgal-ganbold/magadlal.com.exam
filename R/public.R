#' Begin Exam
#'
#' It begins an online exam session on \href{https://www.magadlal.com/}{www.magadlal.com}.
#'
#' @param sisi_id string, the SISI ID of the student
#' @param password string, the exam password which is assigned to the student from \href{https://www.magadlal.com/}{www.magadlal.com}
#' @param exam_id integer, exam ID on \href{https://www.magadlal.com/}{www.magadlal.com}
#'
#' @return \code{NULL}. It prints a response message from the server.
#'
#' @seealso \code{\link{problem}}, \code{\link{check}}
#'
#' @export

begin <- function (sisi_id, password, exam_id) {
  response <- send_request(list(operation = "begin", sisi_id = sisi_id, password = password, exam_id = exam_id))
  message(response$message)
}

#' Get Problem
#'
#' Get a new or current problem which is assigned to a student from the server.
#'
#' @param sisi_id string, the SISI ID of the student
#' @param exam_id integer, exam ID on \href{https://www.magadlal.com/}{www.magadlal.com}
#' @param prevent_new_problem If \code{TRUE}, it prevents from getting a new problem accidentally and attempts to get the problem which is currently assigned.
#'
#' @return A list which is contains a new or current problem is assigned to a student from the server.
#'
#' @seealso \code{\link{begin}}, \code{\link{check}}
#'
#' @export

problem <- function (sisi_id, exam_id, prevent_new_problem = FALSE) {
  if (isTRUE(prevent_new_problem)) {
    prevent_new_problem <- "true"
  } else if (isFALSE(prevent_new_problem)) {
    prevent_new_problem <- "false"
  } else {
    stop("Invalid value supplied for the argument prevent_new_problem")
  }
  response <- send_request(list(operation = "problem", sisi_id = sisi_id, exam_id = exam_id, prevent_new_problem = prevent_new_problem))
  message(response$message)
  if (!is.null(response$problem)) {
    return(list(problem = response$problem, expire_at = response$expire_at))
  }
}

#' Check Problem Solution
#'
#' This function checks a solution and submits it to the server when the solution is correct.
#'
#' @param sisi_id string, the SISI ID of the student
#' @param exam_id integer, exam ID on \href{https://www.magadlal.com/}{www.magadlal.com}
#' @param expr an expression, an R code which is a solution
#'
#' @return Typically, either \code{TRUE} or a string describing the differences between students answer and the correct answer of the assigned problem from the server. When an error occurs, \code{NULL} is returned.
#'
#' @seealso \code{\link{begin}}, \code{\link{problem}}
#'
#' @export

check <- function (sisi_id, exam_id, expr) {
  response <- send_request(list(operation = "check", sisi_id = sisi_id, exam_id = exam_id))
  if (response$message != "") {
    message(response$message)
    return(NULL)
  }
  if (response$dependencies != "") {
    response$dependencies <- strsplit(x = response$dependencies, split = ",")[[1]]
    response$dependencies <- setdiff(response$dependencies, .packages(all.available = TRUE))
    if (length(response$dependencies) > 0) {
      utils::install.packages(pkgs = response$dependencies, dependencies = TRUE)
    }
  }
  true_answer <- try(expr = {
    eval(expr = parse(text = response$code), envir = new.env())
  }, silent = TRUE)
  if (inherits(true_answer, "try-error")) {
    message("Could not check your solution. Maybe required packages are not installed.")
    return(NULL)
  }
  result <- all.equal(target = expr, current = true_answer, check.attributes = FALSE)
  if (isTRUE(result)) {
    response <- send_request(list(operation = "submit", sisi_id = sisi_id, exam_id = exam_id, code = paste0(deparse(substitute(expr)), collapse = "\n")))
    message(response$message)
  }
  result
}
