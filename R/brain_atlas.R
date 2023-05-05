#' @importFrom ggplot2 aes ggplot labs
#' @importFrom stats setNames
#' @export
plot.brain_atlas <- function(x,  ...){

  if(!"geometry" %in% names(x$data)){
    cli::cli_abort(
      "This is not a correctly formatted brain atlas.
       It is missing geometry data, and cannot be plotted.")
  }

  p <- ggplot() +
    geom_brain(atlas = x,
               ...) +
    labs(title = paste(x$atlas, x$type, "atlas"))

  if("colour" %in% names(x$data)){
    p <- p + scale_fill_manual(
      values = setNames(x[["data"]][["colour"]],
                        x[["data"]][["region"]])
      )
  }
  p
}

# Method here to avoid sf dependency on ggseg.formats
#' @export
as.data.frame.brain_atlas <- function(x, ...){
  cbind.data.frame(
    data.frame(
      atlas = rep(x$atlas, nrow(x$data)),
      type = rep(x$type, nrow(x$data)),
      stringsAsFactors = FALSE
    ),
    x$data
  )
}

## quiets concerns of R CMD checks
utils::globalVariables(c("region", "lab"))
