#' Desikan-Killiany Cortical Atlas
#'
#' Coordinate data for the Desikan-Killany Cortical atlas,
#' with 40 regions in on the cortical surface of the brain.
#'
#' @docType data
#' @name dkt
#' @usage data(dkt)
#'
#' @format A data.frame with 10913 observations and 11 variables
#' @keywords datasets
#'
#' @references Fischl et al. (2004) Cerebral Cortex 14:11-22
#' (\href{https://academic.oup.com/cercor/article/14/1/11/433466}{PubMed})
#'
#' \describe{
#'   \item{long}{coordinates for the x-axis}
#'   \item{lat}{coordinates for the y-axis}
#'   \item{area}{name of region}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral)}
#'   \item{acronym}{abbreviated name of region}
#'   \item{lobe}{lobe location}
#'   \item{label}{label from recon-all segmentation}
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
#' @format A data.frame with 2702 observations and 9 variables
#'
#' @keywords datasets
#'
#' @references Fischl et al., (2002). Neuron, 33:341-355
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/11832223}{PubMed})
#'
#' \describe{
#'   \item{lat}{coordinates for the x-axis}
#'   \item{long}{coordinates for the y-axis}
#'   \item{area}{acronym of network}
#'   \item{name}{full name of network}
#'   \item{hemi}{name of the hemisphere (left, right)}
#'   \item{side}{which side to view (medial, lateral,axial)}
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
#' @format A data.frame with 8203 observations and 10 variables
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


#' ggbrain palettes
#'
#' @docType data
#' @name brain.pals
#' @usage data(brain.pals)
#'
#' @format A list of palettes for each atlas in ggbrain
#'
#' @keywords palettes
brain.pals = list(
  dkt  = c(`banks superior temporal`	= "#196428ff",
           `caudal anterior cingulate`	= "#7D64A0ff",
           `caudal middle frontal`	= "#641900ff",
           cuneus	= "#DC1464ff",
           entorhinal	= "#DC140Aff",
           fusiform	= "#B4DC8Cff",
           `inferior parietal`	= "#DC3CDCff",
           `inferior temporal`	= "#B42878ff",
           `isthmus cingulate`	= "#8C148Cff",
           lateraloccipital	= "#141E8Cff",
           `lateralorbito frontal`	= "#234B32ff",
           lingual	= "#E18C8Cff",
           `medial orbito frontal`	= "#C8234Bff",
           `middle temporal`	= "#A06432ff",
           parahippocampal	= "#14DC3Cff",
           `para central`	= "#3CDC3Cff",
           `pars opercularis`	= "#DCB48Cff",
           `pars orbitalis`	= "#146432ff",
           `pars triangularis`	= "#DC3C14ff",
           pericalcarine	= "#78643Cff",
           `post central`	= "#DC1414ff",
           `posterior cingulate`	= "#DCB4DCff",
           `pre central`	= "#3C14DCff",
           precuneus	= "#A08CB4ff",
           `rostral anterior cingulate`	= "#50148Cff",
           `rostral middle frontal`	= "#4B327Dff",
           `superior frontal`	= "#14DCA0ff",
           `superior parietal`	= "#14B48Cff",
           `superior temporal`	= "#8CDCDCff",
           supramarginal	= "#50A014ff",
           `frontal pole`	= "#640064ff",
           `temporal pole`	= "#464646ff",
           `transverse temporal`	= "#9696C8ff",
           insula	= "#FFC020ff"),
  yeo7 = c(visual = "#a153a2ff",
           somatomotor = "#6fabd2ff",
           `dorsal attention`= "#2c8b4bff",
           `ventral attention` = "#b77fb4ff",
           limbic = "#e7edcaff",
           frontoparietal = "#edaf5eff",
           default = "#e27283ff"),
  yeo17 = c("1" = "#762284ff",
            "2" = "#fc2a1cff",
            "3" = "#4984b2ff",
            "4" = "#36cca5ff",
            "5" = "#4d9a42ff",
            "6" = "#0d7418ff",
            "7" = "#c145f6ff",
            "8" = "#fd9bd4ff",
            "9" = "#ddf7a9ff",
            "10" = "#798639ff",
            "11" = "#778dafff",
            "12" = "#e39333ff",
            "13" = "#86334aff",
            "14" = "#143efaff",
            "15" = "#041b80ff",
            "16" = "#fff937ff",
            "17" = "#cb4051ff"),
  aseg =  c(Amygdala	= "#67FFFFff",
            Caudate	= "#7ABADCff",
            Hippocampus	= "#DCD814ff",
            `Lateral Ventricle`	= "#781286ff",
            Pallidum	= "#0D30FFff",
            Putamen	= "#EC0DB0ff",
            `Thalamus Proper`	= "#00760Eff",
            VentralDC	= "#A52A2Aff")
)
"brain.pals"

#' Information on ggbrain atlas palettes
#'
#' @docType data
#' @name brain.pal.info
#' @usage data(brain.pal.info)
#'
#' @format A list of palettes for each atlas in ggbrain
#'
#' @keywords palettes
brain.pal.info <- data.frame(atlas=names(unlist(lapply(brain.pals,length))),
                             maxcol=unname(unlist(lapply(brain.pals,length))),
                             category="qual",
                             colorblind=FALSE)
"brain.pal.info"
