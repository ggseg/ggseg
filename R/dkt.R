#' Desikan-Killiany Cortical Atlas
#'
#' Coordinate data for the Desikan-Killany Cortical atlas,
#' with 40 regions in on the cortical surface of the brain.
#'
#' @docType data
#' @name dkt
#' @usage data(dkt)
#'
#' @format A data.frame with columns for area, hemisphere (left, right) and view (lateral, medial)
#'
#' @keywords datasets
#'
#' @references Fischl et al. (2004) Cerebral Cortex 14:11-22
#' (\href{https://academic.oup.com/cercor/article/14/1/11/433466}{PubMed})
#'
#' \describe{
#'   \item{lat}{coordinates for the x-axis}
#'   \item{long}{coordinates for the y-axis}
#'   \item{area}{acronym of region}
#'   \item{name}{full name of region}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#' }
#' @examples
#' data(dkt)
"dkt"
