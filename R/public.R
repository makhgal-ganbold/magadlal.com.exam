#' Begin Exam
#'
#' It begins a new online exam session on \href{https://www.magadlal.com/}{www.magadlal.com}.
#'
#' @param sisi_id string, the SISi ID of the student
#' @param password string, the exam password which is assigned to the student from \href{https://www.magadlal.com/}{www.magadlal.com}
#' @param exam_id string, exam ID on \href{https://www.magadlal.com/}{www.magadlal.com}
#'
#' @return \code{NULL}. It prints a response message from the server.
#'
#' @export

begin_exam <- function (sisi_id, password, exam_id) {
  response <- send_request(list(operation = "begin_exam", sisi_id = sisi_id, password = password, exam_id = exam_id))
  message(response$message)
}

#' Get Problem
#'
#' Get a new or current problem which is assigned to a student from the server.
#'
#' @param prevent_new_problem If \code{TRUE}, it prevents from getting a new problem accidentally and attempts to get the problem which is currently assigned.
#'
#' @return A list which is contains a new or current problem is assigned to a student from the server.
#'
#' @export

get_problem <- function (prevent_new_problem = FALSE) {
  if (isTRUE(prevent_new_problem)) {
    prevent_new_problem <- "true"
  } else if (isFALSE(prevent_new_problem)) {
    prevent_new_problem <- "false"
  } else {
    stop("Invalid argument supplied")
  }
  response <- send_request(list(operation = "get_problem", prevent_new_problem = prevent_new_problem))
  if (!is.null(response$problem)) {
    return(list(problem = response$problem, expire_at = response$expire_at))
  }
  message(response$message)
}

#' Check Answer
#'
#' It checks an answer.
#'
#' @param answer an object or expression, an answer from a student
#'
#' @return Either \code{TRUE} or a string describing the differences between students answer and the correct answer of the assigned problem from the server.
#'
#' @export

check_answer <- function (answer) {
  response <- send_request(list(operation = "check_answer"))
  true_answer <- try(expr = {
    eval(expr = parse(text = response$code), envir = new.env())
  }, silent = TRUE)
  if (inherits(true_answer, "try-error")) {
    stop("Could not prepare the true answer. Please check whether required packages are installed.")
  }
  result <- all.equal(target = answer, current = true_answer, check.attributes = FALSE)
  if (isTRUE(result)) {
    response <- send_request(list(operation = "save_correct_answer"))
  }
  result
}

#' Remaining Time
#'
#' Check remaining time from the server for the current problem.
#'
#' @return \code{NULL}. It prints remaining time for the current problem.
#'
#' @export

remaining_time <- function () {
  response <- send_request(list(operation = "check_remaining_time"))
  message(response$message)
}

#' Skip current problem
#'
#' It skips a problem which is currently assigned to a student from the server.
#'
#' @return \code{NULL}. It prints a response message from the server.
#'
#' @export

skip_problem <- function () {
  response <- send_request(list(operation = "skip_current_problem"))
  message(response$message)
}

#' Check Exam is Completed
#'
#' I checks whether an exam is completed or not.
#'
#' @return \code{NULL}. It prints a response message from the server.
#'
#' @export

is.completed <- function () {
  response <- send_request(list(operation = "check_exam_complete"))
  message(response$message)
}
