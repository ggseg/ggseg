#' #' @export
#' ggseg_region <- function(.long , .lat, .id ) {
#'   ggseg_region(tibble(.long = .long, .lat = .lat, .id = .id))
#' }
#'
#' #' @export
#' as_ggseg_region <- function(x) {
#'   structure(x, class = "ggseg_region")
#' }
#'
#' #' @export
#' c.ggseg_region <- function(x, ...) {
#'   as_ggseg_region(NextMethod())
#' }
#'
#' #' @export
#' `[.ggseg_region` <- function(x, i) {
#'   as_ggseg_region(NextMethod())
#' }
#'
#' #' @export
#' format.ggseg_region <- function(x, ...) {
#'   ret <- sprintf("< ggseg region [%sx%s] >", length(x$.id), length(x))
#'   format(ret, justify = "right")
#' }
#'
#' #' @export
#' print.ggseg_region <- function(x, ...) {
#'   cat(format(x), sep = "\n")
#'   invisible(x)
#' }
#'
#' # latlon(32.7102978, -117.1704058)
