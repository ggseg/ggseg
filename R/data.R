## Polygon data ----
#' Desikan-Killiany Cortical Atlas
#'
#' Coordinate data for the Desikan-Killany Cortical atlas,
#' with 40 regions in on the cortical surface of the brain.
#'
#' @docType data
#' @name dkt
#' @usage data(dkt)
#' @keywords datasets
#' @family ggseg_atlases
#'
#' @references Fischl et al. (2004) Cerebral Cortex 14:11-22
#' (\href{https://academic.oup.com/cercor/article/14/1/11/433466}{PubMed})
#'
#' @format A data.frame with 10913 observations and 11 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{acronym}{abbreviated name of region}
#'   \item{lobe}{lobe location}
#'   \item{label}{label from recon-all segmentation}
#'   \item{atlas}{name of the atlas}
#' }
#' @examples
#' data(dkt)
"dkt"


#' Freesurfer automatic subcortical segmentation of a brain volume
#'
#' Coordinate data for the subcortical parcellations implemented
#' in Freesurfer.
#'
#' @docType data
#' @name aseg
#' @usage data(aseg)
#' @family ggseg_atlases
#'
#' @keywords datasets
#'
#' @references Fischl et al., (2002). Neuron, 33:341-355
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/11832223}{PubMed})
#'
#' @format A data.frame with 2702 observations and 9 variables
#' \describe{
#'   \item{lat}{coordinates for the x-axis}
#'   \item{long}{coordinates for the y-axis}
#'   \item{area}{acronym of network}
#'   \item{name}{full name of network}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral,axial)}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(aseg)
"aseg"


################################



## Mesh data ----
#' Desikan-Killiany Cortical Atlas - tri-surf mesh
#' Mesh data for the Desikan-Killany Cortical atlas,
#' with 40 regions in on the cortical surface of the brain.
#'
#' A nested tibble for all available surfaces and hemispheres
#'
#' @docType data
#' @name dkt3d
#' @usage data(dkt_3d)
#' @keywords datasets
#' @family ggseg3d_atlases
#'
#' @references Fischl et al. (2004) Cerebral Cortex 14:11-22
#' (\href{https://academic.oup.com/cercor/article/14/1/11/433466}{PubMed})
#'
#' @format A tibble with 4 observations and a nested data.frame
#' \describe{
#'   \item{surf}{type of surface (`inflated` or `white`)}
#'   \item{hemi}{hemisphere (`left`` or `right`)}
#'   \item{data}{data.frame of necessary variables for plotting
#'   }
#'
#'   \item{atlas}{String. atlas name}
#'   \item{roi}{numbered region from surface}
#'   \item{annot}{concatenated region name}
#'   \item{label}{label `hemi_annot` of the region}
#'   \item{mesh}{list of meshes in two lists: vb and it}
#'   \item{acronym}{abbreviated name of annot}
#'   \item{lobe}{lobe localization}
#'   \item{area}{name of area in full}
#'   \item{colour}{HEX colour of region}
#' }
#' @examples
#' data(dkt_3d)
"dkt_3d"


#' Freesurfer automatic subcortical segmentation of a brain volume
#'
#' Coordinate data for the subcortical parcellations implemented
#' in Freesurfer.
#'
#' @docType data
#' @name aseg_3d
#' @usage data(aseg_3d)
#' @family ggseg3d_atlases
#'
#' @keywords datasets
#'
#' @references Fischl et al., (2002). Neuron, 33:341-355
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/11832223}{PubMed})
#'
#' @format A tibble with 4 observations and a nested data.frame
#' \describe{
#'   \item{surf}{type of surface (`inflated` or `white`)}
#'   \item{hemi}{hemisphere (`left`` or `right`)}
#'   \item{data}{data.frame of necessary variables for plotting
#'   }
#'
#'   \item{atlas}{String. atlas name}
#'   \item{roi}{numbered region from surface}
#'   \item{annot}{concatenated region name}
#'   \item{label}{label `hemi_annot` of the region}
#'   \item{mesh}{list of meshes in two lists: vb and it}
#'   \item{area}{name of area in full}
#'   \item{colour}{HEX colour of region}
#' }
#'
#' @examples
#' data(aseg_3d)
"aseg_3d"


################################




## Other utilities ----
#' ggseg palettes
#'
#' @docType data
#' @name brain.pals
#' @usage data(brain.pals)
#'
#' @format A list of palettes for each atlas in ggseg.
#'
#' @keywords palettes
"brain.pals"

#' Information on ggseg atlas palettes
#'
#' @docType data
#' @name brain.pal.info
#' @usage data(brain.pal.info)
#'
#' @format A data.frame summarising the ggseg palettes.
#'
#' @keywords palettes
"brain.pal.info"

#' Information on ggseg atlases
#'
#' @docType data
#' @name atlas.info
#' @usage data(atlas.info)
#'
#' @format A tibble summarising the ggseg data.
#'
#' @keywords data summary
"atlas.info"

