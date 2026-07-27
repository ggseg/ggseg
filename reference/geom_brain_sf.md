# Deprecated sf brain geom

**\[deprecated\]**

The sf rendering path is deprecated. `geom_brain_sf()` renders an atlas
via
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
and [coord_sf()](https://ggplot2.tidyverse.org/reference/ggsf.html). For
new code, use
[`geom_brain()`](https://ggsegverse.github.io/ggseg/reference/ggbrain.md)
(the polygon default), or convert the atlas with `as_sf_atlas()` and use
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
directly for the full sf toolkit (labels, other sf layers).

## Usage

``` r
geom_brain_sf(
  mapping = aes(),
  data = NULL,
  atlas,
  hemi = NULL,
  view = NULL,
  position = NULL,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- data:

  A data.frame containing variables to map. If `NULL`, the atlas is
  plotted without user data. Add a facet with
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  to draw one brain per group.

- atlas:

  A `ggseg_atlas` object (e.g.
  [`dk()`](https://ggsegverse.github.io/ggseg.formats/reference/dk.html),
  [`aseg()`](https://ggsegverse.github.io/ggseg.formats/reference/aseg.html),
  [`tracula()`](https://ggsegverse.github.io/ggseg.formats/reference/tracula.html)).

- hemi:

  Character vector of hemispheres to include (e.g. `"left"`, `"right"`).
  Defaults to all hemispheres in the atlas.

- view:

  Character vector of views to include, as recorded in the atlas data.
  For cortical atlases: `"lateral"`, `"medial"`. For subcortical/tract
  atlases: slice identifiers like `"axial_3"`. Defaults to all views.

- position:

  Position adjustment, either as a string or the result of a call to
  [`position_brain()`](https://ggsegverse.github.io/ggseg/reference/position_brain.md).

- show.legend:

  Logical. Should this layer be included in the legends?

- inherit.aes:

  Logical. If `FALSE`, overrides the default aesthetics rather than
  combining with them.

- ...:

  Additional arguments passed to
  [`ggplot2::geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.html).

## Value

A list of ggplot2 layer and coord objects.

## Examples

``` r
if (FALSE) { # \dontrun{
# Deprecated: prefer geom_brain(). Shown for reference only.
library(ggplot2)
ggplot() +
  geom_brain_sf(atlas = dk())
} # }
```
