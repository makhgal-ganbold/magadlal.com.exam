#' Returns Mac Address
#'
#' @return vector, mac address
#'
#' @importFrom stringr str_extract
#'
#' @noRd

.identification <- function () {
  mac_addr <- try(expr = {
    if (Sys.info()['sysname'] == "Windows") {
      command <- "getmac"
    } else { # Linux or Mac
      command <- "ifconfig -a | grep ether"
    }
    addr_info <- system(command = command, intern = TRUE)
    mac_addr <- stringr::str_extract(string = addr_info, pattern = "[0-9a-fA-F]{2}[:-]?[0-9a-fA-F]{2}[:-]?[0-9a-fA-F]{2}[:-]?[0-9a-fA-F]{2}[:-]?[0-9a-fA-F]{2}[:-]?[0-9a-fA-F]{2}")
    mac_addr <- mac_addr[!is.na(mac_addr)]
    n.mac_addr <- length(mac_addr)
    if (n.mac_addr == 0) {
      stop()
    } else if (n.mac_addr > 1) {
      mac_addr <- paste0(mac_addr, collapse = ",")
    }
    mac_addr
  }, silent = TRUE)
  if (inherits(mac_addr, "try-error")) {
    stop("It could not determine your computer's Mac Address.")
  }
  mac_addr
}

#' Send Request and Receive Response
#'
#' @param query list, query strings
#'
#' @return list, server response that is formatted as JSON object
#'
#' @importFrom httr POST
#' @importFrom jsonlite parse_json
#'
#' @noRd

.interface <- function (query = list()) {

  # attach Mac Address

  query$mac <- .identification()

  # send request and receive response

  response <- httr::POST(
    url = "https://www.magadlal.com/exam/api",
#    url = "http://localhost/www/magadlal.com/exam/api",
    body = query,
    encode = "form"
  )

  # check HTTP Status Code

  if (response$status_code != 200) {
    stop(paste0("HTTP status code: ", response$status_code))
  }

  # convert response to list

  response <- try(expr = {
    jsonlite::parse_json(rawToChar(response$content))
  }, silent = TRUE)
  if (inherits(response, "try-error")) {
    stop("An unknown error occured on the server.")
  }

  # response

  response

}

#' Loaded Packages
#'
#' @return character vector, loaded packages
#'
#' @noRd

.loaded.packages <- function () {
  pckgs <- search()
  pckgs <- pckgs[grep(pattern = "package:", x = pckgs)]
  pckgs <- sub(pattern = "package:", replacement = "", x = pckgs)
  pckgs[!{pckgs %in% c("MASS", "magadlalcomexam", "stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")}]
}
