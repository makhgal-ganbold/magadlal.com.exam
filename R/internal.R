#' Returns Mac Address
#'
#' @return vector, mac address
#'
#' @importFrom stringr str_extract
#'
#' @noRd

get_mac_address <- function () {
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
    stop("Could not determine your Mac Address.")
  }
  mac_addr
}

#' Send Request and Receive Response
#'
#' @param query list, query strings
#'
#' @return list, server response. Must contains an entry named as "status".
#'
#' @importFrom httr POST
#' @importFrom jsonlite parse_json
#'
#' @noRd

send_request <- function (query = list()) {

  # attach Mac Address

  query$mac <- get_mac_address()

  # send request and receive response

  response <- httr::POST(
    url = "https://www.magadlal.com/exam/api",
    body = query,
    encode = "json"
  )

  # check HTTP Status Code

  if (response$status_code != 200) {
    stop(paste0("HTTP status code: ", response$status_code))
  }

  # convert response to list

  response <- jsonlite::parse_json(rawToChar(response$content))

  # confirm whether request and response is valid

  if (response$status != "success") {
    stop(response$status)
  }

  # response

  response

}
