#' Check and Submit a Solution
#'
#' This function checks a solution and submits it to the server when it is correct.
#'
#' @param uid integer, the unique identification number that is assigned to the student via the exam page of the \href{https://www.magadlal.com/}{www.magadlal.com}
#' @param expr an expression, an R code which is a solution
#'
#' @return Typically, a server response message or an error message, if a student's answer has an error.
#'
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off
#'
#' @export

submit_solution <- function (uid, expr) {
  expr_str <- paste0(deparse(substitute(expr)), collapse = "\n")
  if (nchar(expr_str) > 4096) {
    stop("The written code is too long.")
  }
  response <- .interface(list(operation = "verify", uid = uid))
  if (response$message != "VERIFIED") {
    stop(response$message)
  }
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
  tryCatch(expr = {
    grDevices::pdf(NULL)
    evalq(expr = expr, envir = new.env()) |>
      jsonlite::as_gzjson_b64() ->
      b64_gz_json
    if (nchar(b64_gz_json) > 4096) {
      stop("The output is too long.")
    }
    grDevices::dev.off()
    .interface(list(
      operation = "submit",
      uid = uid,
      solution = expr_str,
      b64_gz_json = b64_gz_json
    )) ->
      response
    response$message
  }, error = function(e) {
    e$message
  })
}
