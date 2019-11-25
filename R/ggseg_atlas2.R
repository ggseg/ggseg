#' #' @export
#' ggseg_area <- function(.long , .lat, .id ) {
#'   ggseg_area(tibble(.long = .long, .lat = .lat, .id = .id))
#' }
#'
#' #' @export
#' as_ggseg_area <- function(x) {
#'   structure(x, class = "ggseg_area")
#' }
#'
#' #' @export
#' c.ggseg_area <- function(x, ...) {
#'   as_ggseg_area(NextMethod())
#' }
#'
#' #' @export
#' `[.ggseg_area` <- function(x, i) {
#'   as_ggseg_area(NextMethod())
#' }
#'
#' #' @export
#' format.ggseg_area <- function(x, ...) {
#'   ret <- sprintf("< ggseg area [%sx%s] >", length(x$.id), length(x))
#'   format(ret, justify = "right")
#' }
#'
#' #' @export
#' print.ggseg_area <- function(x, ...) {
#'   cat(format(x), sep = "\n")
#'   invisible(x)
#' }
#'
#' # latlon(32.7102978, -117.1704058)
