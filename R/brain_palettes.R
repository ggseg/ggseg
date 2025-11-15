#' Generate palettes from the ggseg atlases
#'
#' \code{brain_pal} return HEX colours for the different ggseg atlases.
#'
#' @param name String name of atlas
#' @param n Number of colours to return (or "all" [default])
#' @param package package to get brain_pals data from (ggseg or ggsegExtra)
#' @param direction Direction of  HEX, -1 reverses order
#' Necessary if applying palette to other data than the brain atlas it comes from.
#' @param unname return unnamed vector (default = FALSE)
#' @return vector of colours
#' @export
#' @examples
#' brain_pal("dk")
#' brain_pal("aseg")
brain_pal <- function(
  name,
  n = "all",
  direction = 1,
  unname = FALSE,
  package = "ggseg"
) {
  brain_pals <- get_pals(package)

  info <- brain_pals_info(package = package)

  if (!(name %in% info$atlas)) {
    stop(paste(name, "is not a valid palette name for brain_pal\n"))
  }

  if (length(n) > 1) {
    n = n
  } else if (n == "all") {
    n = seq(1, info[info$atlas %in% name, "maxcol"])
  } else if (n < 3) {
    warning(
      "minimal value for n is 3, returning requested palette with 3 different levels\n"
    )
    n = seq(1:3)
  } else if (n > info[info$atlas %in% name, "maxcol"]) {
    warning(paste(
      "n too large, allowed maximum for palette",
      name,
      "is",
      info[info$atlas %in% name, "maxcol"],
      "\nReturning the palette you asked for with that many colors\n"
    ))
    n = seq(1, info[info$atlas %in% name, "maxcol"])
  } else {
    n = seq(1, n)
  }

  pal = brain_pals[[name]]

  if (direction == -1) {
    pal <- rev(pal)
  }

  if (unname) {
    pal = unname(pal)
  }

  pal[n]
}

#' Get info on brain palettes
#'
#' @inheritParams brain_pal
#' @return data.frame with palette information
#' @export
#'
#' @examples
#' brain_pals_info()
brain_pals_info <- function(package = "ggseg") {
  brain_pals <- get_pals(package)

  data.frame(
    atlas = names(unlist(lapply(brain_pals, length))),
    maxcol = unname(unlist(lapply(brain_pals, length))),
    category = "qual",
    colorblind = FALSE,
    stringsAsFactors = FALSE
  )
}


#' Get colours from brain palettes
#'
#' Needed to display the colours, and inspect
#' the number of colours of each palette
#'
#' @inheritParams brain_pal
#' @keywords internal
#' @noRd
get_colours <- function(name, n, unname, package) {
  data.frame(
    atlas = name,
    colour = brain_pal(name = name, n = n, unname = unname, package = package),
    stringsAsFactors = FALSE
  )
}

#' Get brain palettes
#'
#' Brain palettes can be found in either ggseg
#' or ggsegExtra. This functions is a helper
#' to choose which package to get the palette
#' from.
#'
#' @inheritParams brain_pal
#' @keywords internal
#' @noRd
get_pals <- function(package = "ggseg") {
  eval(parse(text = paste(package, "brain_pals", sep = ":::")))
}

## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("info", "brain_pals"))
}
