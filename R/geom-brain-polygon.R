# sf-optional brain renderer over flat polygon data ----
#
# Renders a brain atlas using ggplot2::geom_polygon over the polygon
# representation from ggseg.formats::atlas_polygons() (see ggseg.formats). No
# sf objects, no GDAL/GEOS/PROJ system libraries needed — enables wasm and
# air-gapped builds. First piece of Epic ggsegverse/ggseg#128.
#
# Scope of this iteration: simple per-view stacking from the polygons'
# pre-positioned coordinates. position_brain_sf() is not yet wired up for
# the polygon path; users wanting custom layouts should keep using
# geom_brain_sf() for now.

#' Plot brain atlas regions from the polygon representation (sf-optional)
#'
#' Renders a `ggseg_atlas` via `ggplot2::geom_polygon` using the polygon
#' representation from `ggseg.formats::atlas_polygons()`. Works on any atlas;
#' a polygon-backed atlas (e.g. from `ggseg.formats::as_polygon_atlas()`)
#' renders without requiring the `sf` package or its GDAL/GEOS/PROJ system
#' libraries.
#'
#' Hole geometry round-trips correctly via the `subgroup` aesthetic
#' (`grid::pathGrob` even-odd fill).
#'
#' @param mapping Set of aesthetic mappings created by `ggplot2::aes()`.
#' @param data A data.frame containing variables to map. If `NULL`, the
#'   atlas is plotted without user data.
#' @param atlas A `ggseg_atlas` object with 2D geometry (sf or polygons).
#' @param hemi Character vector of hemispheres to include.
#' @param view Character vector of views to include.
#' @param position Position adjustment. Defaults to [position_brain_polygon()],
#'   which lays views out horizontally without sf. Pass `"identity"` to use
#'   the polygons' raw coordinates. Per-view zoom is controlled here via
#'   [position_brain_polygon()]'s `zoom` argument.
#' @param context Logical. When `TRUE` (default), context regions (atlas rows
#'   with no `region` label, drawn grey) are kept. When `FALSE`, they are
#'   dropped so only true atlas regions are plotted.
#' @param show.legend Logical. Should this layer be included in legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics.
#' @param ... Additional arguments passed to `ggplot2::geom_polygon()`.
#'
#' @return A list of ggplot2 layer and coord objects.
#' @keywords internal
#' @noRd
#' @importFrom ggplot2 aes geom_polygon scale_fill_manual
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' poly <- ggseg.formats::as_polygon_atlas(dk())
#' ggplot() + geom_brain_polygon(atlas = poly)
#' }
geom_brain_polygon <- function(
  mapping = aes(),
  data = NULL,
  atlas,
  hemi = NULL,
  view = NULL,
  position = position_brain_polygon(),
  context = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  base_mapping <- aes(
    x = .data$x,
    y = .data$y,
    group = .data$.feature_id,
    subgroup = .data$subgroup
  )
  user_mapping <- utils::modifyList(base_mapping, as.list(mapping))
  class(user_mapping) <- "uneval"

  dots <- list(...)
  if (!"colour" %in% names(dots) && !"color" %in% names(dots)) {
    dots$colour <- "grey35"
  }
  if (!"linewidth" %in% names(dots) && !"size" %in% names(dots)) {
    dots$linewidth <- 0.2
  }

  layer <- layer_brain(
    mapping = user_mapping,
    data = data,
    atlas = atlas,
    hemi = hemi,
    view = view,
    position = position,
    context = context,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = dots
  )

  result <- list(layer, coord_brain())

  if (!is.null(atlas$palette) && !"fill" %in% names(mapping)) {
    result <- c(
      result,
      list(scale_fill_manual(values = atlas$palette, na.value = "grey"))
    )
  }

  result
}


# ggplot2 does not export `Layer`, so we reach into its namespace once
# to get the parent ggproto. Using `getFromNamespace()` rather than `:::`
# keeps `R CMD check` clean. Defined here (the canonical polygon path)
# because this file is sourced before layer-brain.R, whose `LayerBrainSf`
# also subclasses it.
ggplot2_Layer <- function() {
  utils::getFromNamespace("Layer", "ggplot2")
}

#' Build a deferred brain-polygon layer
#'
#' Wraps [ggplot2::layer()] with the [LayerBrain] class and stashes the
#' atlas and layout config on the layer object. The atlas is flattened and any
#' user data joined at plot-build time (in `setup_layer()`), so the layer can
#' see data and aesthetics inherited from the top-level `ggplot()` call.
#'
#' @keywords internal
#' @noRd
#' @importFrom ggplot2 GeomPolygon layer
layer_brain <- function(
  mapping,
  data,
  atlas,
  hemi,
  view,
  position,
  context,
  show.legend,
  inherit.aes,
  params
) {
  brain_layer <- layer(
    geom = GeomPolygon,
    stat = "identity",
    data = data,
    mapping = mapping,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params,
    layer_class = LayerBrain
  )
  brain_layer$brain_atlas <- atlas
  brain_layer$brain_hemi <- hemi
  brain_layer$brain_view <- view
  brain_layer$brain_position <- position
  brain_layer$brain_context <- context
  brain_layer
}


#' Custom ggplot2 Layer for the (default) sf-optional polygon path
#'
#' The canonical brain layer. Its `setup_layer()` runs at plot-build time, so it
#' inherits the top-level `data` and aesthetic mapping: it flattens the atlas,
#' joins inherited (or geom-level) data onto it, and only falls back to
#' colouring by `label` when neither the layer nor the plot maps `fill`. Fixes
#' ggsegverse/ggseg#158, where top-level `aes()`/`data` were dropped by the
#' eager (construction-time) polygon build. The deprecated sf renderer has a
#' parallel [LayerBrainSf].
#'
#' @keywords internal
#' @noRd
#' @importFrom ggplot2 ggproto ggproto_parent
LayerBrain <- ggproto(
  "LayerBrain",
  ggplot2_Layer(),
  setup_layer = function(self, data, plot) {
    dt <- ggproto_parent(ggplot2_Layer(), self)$setup_layer(data, plot)

    atlas <- self$brain_atlas
    if (is.null(atlas)) {
      cli::cli_abort(
        "No atlas supplied, please provide a brain atlas to the geom."
      )
    }

    has_data <- !inherits(dt, "waiver")

    if (has_data && !dplyr::is.grouped_df(dt)) {
      dt <- group_by_facet_vars(dt, plot, atlas)
    }

    zoom_spec <- if (is_polygon_position(self$brain_position)) {
      self$brain_position$zoom
    } else {
      NULL
    }
    focus <- resolve_zoom_focus(zoom_spec, if (has_data) dt else NULL, atlas)

    flat <- prepare_polygon_atlas(
      atlas,
      hemi = self$brain_hemi,
      view = self$brain_view,
      position = self$brain_position,
      context = self$brain_context,
      focus = focus
    )

    if (has_data) {
      flat <- brain_join_polygon(dt, flat)
    }

    if (is.null(self$computed_mapping$fill)) {
      self$computed_mapping$fill <- quote(.data$label)
    }

    flat
  }
)


#' Auto-group user data by facet variables for atlas replication
#'
#' Parity with [LayerBrainSf]: when data is faceted but not explicitly grouped,
#' group it by the facet variables so [brain_join_polygon()] replicates the full
#' atlas once per panel. Facet variables that are atlas identity columns (e.g.
#' `hemi`, `view`) are excluded -- faceting on those subsets the atlas rather
#' than replicating it.
#'
#' @keywords internal
#' @noRd
group_by_facet_vars <- function(data, plot, atlas) {
  facet_vars <- plot$facet$vars()
  group_vars <- intersect(facet_vars, names(data))
  atlas_cols <- unique(c(
    names(atlas$core),
    "view",
    "type",
    "atlas",
    "hemi"
  ))
  group_vars <- setdiff(group_vars, atlas_cols)
  if (length(group_vars) > 0) {
    data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(group_vars)))
  }
  data
}


#' Flatten a polygon atlas into a row-per-point data.frame for geom_polygon
#'
#' Unnests `ggseg.formats::atlas_polygons(atlas)`, joins with `atlas$core`,
#' applies optional
#' hemi/view filters, and adds the `.feature_id` helper column that the
#' polygon renderer maps to `group` so each (label, view, group) polygon
#' piece renders as one ring set.
#'
#' @keywords internal
#' @noRd
prepare_polygon_atlas <- function(
  atlas,
  hemi = NULL,
  view = NULL,
  position = NULL,
  context = TRUE,
  focus = NULL
) {
  if (is.null(ggseg.formats::atlas_geom(atlas))) {
    cli::cli_abort(c(
      "{.arg atlas} has no 2D geometry.",
      "i" = "Provide an atlas with {.field geom} (sf or polygons) in its data."
    ))
  }

  flat <- tidyr::unnest(
    ggseg.formats::atlas_polygons(atlas),
    cols = "geometry"
  )
  # Namespace the polygon-ring grouping as `.group` before joining `core`:
  # some atlases (e.g. tracula) carry their own `group` column in `core`,
  # which would otherwise collide and suffix this one away.
  names(flat)[names(flat) == "group"] <- ".group"
  flat <- dplyr::left_join(
    flat,
    atlas$core,
    by = "label",
    relationship = "many-to-many"
  )

  if (!is.null(view)) {
    avail <- unique(flat$view)
    invalid <- setdiff(view, avail)
    if (length(invalid)) {
      cli::cli_abort(
        "Invalid view(s): {.val {invalid}}. Available: {.val {avail}}"
      )
    }
    flat <- flat[flat$view %in% view, , drop = FALSE]
  }

  if (!is.null(hemi)) {
    avail <- unique(flat$hemi)
    invalid <- setdiff(hemi, avail)
    if (length(invalid)) {
      cli::cli_abort(
        "Invalid hemisphere(s): {.val {invalid}}. Available: {.val {avail}}"
      )
    }
    flat <- flat[flat$hemi %in% hemi, , drop = FALSE]
  }

  if (!context && "region" %in% names(flat)) {
    flat <- flat[!is.na(flat$region), , drop = FALSE]
  }

  flat$atlas <- atlas$atlas
  flat$type <- atlas$type

  if (atlas$type == "cortical") {
    if (!"hemi" %in% names(flat)) {
      flat$hemi <- NA_character_
    }
    missing_hemi <- is.na(flat$hemi)
    flat$hemi[missing_hemi] <- dplyr::case_when(
      grepl("^lh[_.]", flat$label[missing_hemi]) ~ "left",
      grepl("^rh[_.]", flat$label[missing_hemi]) ~ "right",
      .default = NA_character_
    )
  }

  flat$.feature_id <- as.integer(factor(
    paste(flat$label, flat$view, flat$.group, sep = "@@")
  ))

  if (is_polygon_position(position)) {
    flat <- frame_2_position_flat(
      flat,
      position$position,
      nrow = position$nrow,
      ncol = position$ncol,
      views = position$views,
      focus = focus,
      zoom_pad = if (is.null(position$zoom_pad)) 0.05 else position$zoom_pad
    )
  }

  flat
}


#' Polygon-path version of `brain_join()` — joins user data onto flat rows
#'
#' Matches on `region` or `label` (and `hemi` if both data and atlas carry
#' it). Polygon rows without a matching data row keep `NA` for the joined
#' columns; the renderer paints them grey via `na.value` on the fill scale.
#'
#' Grouped data (via [dplyr::group_by()]) is replicated like [brain_join()]:
#' the full atlas is repeated once per group, with the grouping columns set on
#' every row — including unmatched context regions — so [ggplot2::facet_wrap()]
#' shows the complete atlas in each panel.
#'
#' @keywords internal
#' @noRd
brain_join_polygon <- function(data, flat) {
  by <- intersect(
    c("region", "label", "hemi"),
    intersect(names(data), names(flat))
  )
  if (!length(by)) {
    cli::cli_abort(c(
      "{.arg data} has no columns in common with the atlas.",
      "i" = paste0(
        "Need {.field region} or {.field label} ",
        "(and optionally {.field hemi})."
      )
    ))
  }

  if (!dplyr::is.grouped_df(data)) {
    return(dplyr::left_join(flat, data, by = by, suffix = c("", ".user")))
  }

  group_cols <- dplyr::group_vars(data)
  nested <- tidyr::nest(data)
  pieces <- lapply(seq_len(nrow(nested)), function(i) {
    piece <- dplyr::left_join(
      flat,
      nested$data[[i]],
      by = by,
      suffix = c("", ".user")
    )
    for (g in group_cols) {
      piece[[g]] <- nested[[g]][[i]]
    }
    piece
  })
  dplyr::bind_rows(pieces)
}
