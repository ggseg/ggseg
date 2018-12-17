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
#'
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



#' Yeo 7 Resting-state Cortical Parcellations
#'
#' Coordinate data for the resting-state networks of
#' the Yeo 2011 7 networks.
#'
#' @docType data
#' @name yeo7
#' @usage data(yeo7)
#'
#'
#' @keywords datasets
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
#'
#' @format A data.frame with 8203 observations and 10 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of network}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{network}{network number (1:7)}
#'   \item{label}{unique name to each node}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(yeo7)
"yeo7"



#' Yeo 17 Resting-state Cortical Parcellations
#'
#' Coordinate data for the resting-state networks of
#' the Yeo 2011 17 networks.
#'
#' @docType data
#' @name yeo17
#' @usage data(yeo17)
#'
#'
#' @keywords datasets
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
#'
#' @format A data.frame with 8203 observations and 10 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of network}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{network}{network number (1:17)}
#'   \item{label}{unique name to each node}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(yeo17)
"yeo17"



#' Parcellation from the Human Connectome Project
#'
#' @docType data
#' @name glasser
#' @keywords datasets
#'
#' @references Glasser et al. (2016) Nature, volume 536, pages 171–178
#' (\href{https://www.nature.com/articles/nature18933}{PubMed})
#'
#' @format A data.frame with 2483 observations and 8 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{dummy name of the hemisphere}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{label}{label name from Freesurfer}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(glasser)
"glasser"



#' Parcellation from a midsagittal slice
#'
#' @docType data
#' @name midsagittal
#' @keywords datasets
#'
#'
#' @format A data.frame with 2483 observations and 8 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{dummy name of the hemisphere}
#'   \item{side}{which side to view (sagittal)}
#'   \item{label}{label name from Freesurfer}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(midsagittal)
"midsagittal"



#' Parcellation from JHU
#'
#' @docType data
#' @name jhu
#' @keywords datasets
#'
#' @references Hua et al., Tract probability maps in stereotaxic spaces:
#' analysis of white matter anatomy and tract-specific quantification. NeuroImage, 39(1):336-347 (2008)
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2724595/}{PubMed})
#'
#' @format A data.frame with 11341 observations and 11 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{dummy name of the hemisphere}
#'   \item{side}{which side to view (sagittal)}
#'   \item{label}{label name from Freesurfer}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(jhu)
"jhu"


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



#' Yeo 7 Resting-state Cortical Parcellations
#'
#' Mesh data for the resting-state networks of
#' the Yeo 2011 7 networks.
#'
#' @docType data
#' @name yeo7_3d
#' @usage data(yeo7_3d)
#'
#' @keywords datasets
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
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
#' @examples
#' data(yeo7_3d)
"yeo7_3d"


#' Yeo 17 Resting-state Cortical Parcellations
#'
#' Mesh data for the resting-state networks of
#' the Yeo 2011 17 networks.
#'
#' @docType data
#' @name yeo17_3d
#' @usage data(yeo17_3d)
#'
#' @keywords datasets
#'
#' @references Yeo et al. (2011) J. Neurophysiology 16(3):1125-1165
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/21653723}{PubMed})
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
#' @examples
#' data(yeo7_3d)
"yeo17_3d"



#' Freesurfer automatic subcortical segmentation of a brain volume
#'
#' Coordinate data for the subcortical parcellations implemented
#' in Freesurfer.
#'
#' @docType data
#' @name aseg_3d
#' @usage data(aseg_3d)
#'
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



#' Parcellation from the Human Connectome Project
#'
#' @docType data
#' @name glasser_3d
#' @keywords datasets
#'
#' @references Glasser et al. (2016) Nature, volume 536, pages 171–178
#' (\href{https://www.nature.com/articles/nature18933}{PubMed})
#'
#' @format A data.frame with 2483 observations and 8 variables
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{dummy name of the hemisphere}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{label}{label name from Freesurfer}
#'   \item{atlas}{name of the atlas}
#' }
#'
#' @examples
#' data(glasser_3d)
"glasser_3d"



#' Schaefer 7 Resting-state Cortical Parcellations
#'
#' Mesh data for the 7 resting-state networks of
#' the Schaefer et al. (2018) networks.
#'
#' @docType data
#' @name schaefer7_3d
#' @usage data(schaefer7_3d)
#'
#' @keywords datasets
#'
#'
#' @references Schaefer et al. (2018) Cereb Cortex. 2018 Sep 1;28(9):3095-3114. doi: 10.1093/cercor/bhx179
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/28981612}{PubMed})
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
#' @examples
#' data(schaefer7_3d)
"schaefer7_3d"



#' Schaefer 17 Resting-state Cortical Parcellations
#'
#' Mesh data for the 17 resting-state networks of
#' the Schaefer et al. (2018) networks.
#'
#' @docType data
#' @name schaefer17_3d
#' @usage data(schaefer17_3d)
#'
#' @keywords datasets
#'
#' @references Schaefer et al. (2018) Cereb Cortex. 2018 Sep 1;28(9):3095-3114. doi: 10.1093/cercor/bhx179
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/28981612}{PubMed})
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
#' @examples
#' data(schaefer17_3d)
"schaefer17_3d"



#' Desterieux cortical parcellations
#'
#' Mesh data for Desterieux cortical parcellations, also known as the
#' aparc 2009 parcellations of Freesurfer
#'
#' @docType data
#' @name desterieux_3d
#' @usage data(desterieux_3d)
#'
#' @keywords datasets
#'
#' @references Desterieux, Fischl, Dale,& Halgren (2010) Neuroimage. 53(1): 1–15. doi: 0.1016/j.neuroimage.2010.06.010
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2937159/}{PubMed})
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
#' @examples
#' data(desterieux_3d)
"desterieux_3d"



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
