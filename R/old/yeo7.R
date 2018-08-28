#' Yeo 7 Resting-state Cortical Parcellations
#'
#' Coordinate data for the resting-state networks of
#' the Yeo 2011 7 networks.
#'
#' @docType data
#' @name yeo7
#' @usage data(yeo7)
#'
#' @format A data.frame with columns for area, hemisphere (left, right) and view (lateral, medial)
#'
#' @keywords datasets
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
#'
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of network}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{network}{network number (1:7)}
#'   \item{label}{unique name to each node}
#' }
#'
#' @examples
#' data(yeo7)
"yeo7"
