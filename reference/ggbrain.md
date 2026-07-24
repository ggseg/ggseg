# Plot brain atlas regions

Colour brain regions by your own values. Give \`geom_brain()\` an atlas
like \`dk()\` and a data frame, and it matches your values to the right
regions and lays out the brain views for you. No data? It just draws the
atlas.

## Usage

``` r
geom_brain(
  mapping = aes(),
  data = NULL,
  atlas,
  hemi = NULL,
  view = NULL,
  position = position_brain(),
  context = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by \[ggplot2::aes()\].

- data:

  A data.frame containing variables to map. If \`NULL\`, the atlas is
  plotted without user data. Group it with \[dplyr::group_by()\] to
  facet.

- atlas:

  A \`ggseg_atlas\` object (e.g. \`dk()\`, \`aseg()\`, \`tracula()\`).

- hemi:

  Character vector of hemispheres to include (e.g. \`"left"\`,
  \`"right"\`). Defaults to all hemispheres in the atlas.

- view:

  Character vector of views to include, as recorded in the atlas data.
  For cortical atlases: \`"lateral"\`, \`"medial"\`. For
  subcortical/tract atlases: slice identifiers like \`"axial_3"\`.
  Defaults to all views.

- position:

  Position adjustment, either as a string or the result of a call to
  \[position_brain()\].

- context:

  Keep the rest of the brain as a soft grey backdrop (\`TRUE\`, the
  default), or show only the regions you're plotting (\`FALSE\`).

- show.legend:

  Logical. Should this layer be included in the legends?

- inherit.aes:

  Logical. If \`FALSE\`, overrides the default aesthetics rather than
  combining with them.

- ...:

  Additional arguments passed to \[ggplot2::geom_polygon()\].

## Value

A list of ggplot2 layer and coord objects.

## Details

Regions are drawn in the order they appear in your \`data\`, so when
outlines overlap (e.g. mapping \`colour\` to a threshold with a wide
\`linewidth\`) the later rows draw on top. Reorder your data with
\[dplyr::arrange()\] to control the layering; regions you supply no
value for stay underneath in atlas order.

## GeomBrain ggproto

\`GeomBrain\` is the \[ggplot2::Geom\] ggproto that renders brain atlas
polygons. It subclasses \[ggplot2::GeomPolygon\] and only supplies the
brain default outline \`colour\` (grey35) and \`linewidth\` (0.2)
through \`default_aes\`, so they apply when the user has not mapped or
set those aesthetics but yield to a mapping when present
(ggsegverse/ggseg#160). It is used internally by \[geom_brain()\] and
should not typically be called directly.

## Examples

``` r
library(ggplot2)

ggplot() +
  geom_brain(atlas = dk())
```
