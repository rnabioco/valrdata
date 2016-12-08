#' Segway encyclopedia of human regulatory elements
#'
#' A dataset containing segway annotations for 164 human cell types
#'
#' @format A data frame with 164,785 rows and 169 columns
#' \describe{
#'   \item{chrom}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{sum_score}{sum of functionality score}
#'   \item{mean_score}{average base-wise functionality score}
#'   \item{...}{segway label of each cell type in the interval}
#' }
#' @references
#' Libbrecht MW, Rodriguez O, Hoffman MM, Bilmes JA, Noble WS. 2016. A unified encyclopedia of human functional elements through fully automated annotation of 164 human cell types. Biorxiv preprint: http://dx.doi.org/10.1101/086025.
#' @seealso \url "http://noble.gs.washington.edu/proj/encyclopedia/"
#' @source \url"http://noble.gs.washington.edu/proj/encyclopedia/segway_encyclopedia.bed.gz"
"segway_data"


#' DnaseI hypersensitivity data
#'
#' A dataset containing DNaseI hypersensitivity data from 20 human cell types
#'
#' @format A list of 20 dataframes with about 150,000-200,000 rows and 4 columns each:
#' \describe{
#'   \item{chrom}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{name}{DnaseI hypersensitivity score}
#' }
#' @references
#' Maurano et al. Systematic Localization of Common Disease-Associated Variation in Regulatory DNA. Science. 2012. Vol. 337 no. 6099 pp. 1190-1195.
#' @seealso \url "http://www.sciencemag.org/content/337/6099/1190.short"
#' @source \url"https://s3.amazonaws.com/bedtools-tutorials/web/maurano.dnaseI.tgz"
"dnase_data"


#' Human repetitive elements
#'
#' A dataset containing annotations for repeat classes in the hg38 human genome, as
#' annotated by RepeatMasker
#'
#' @format A data frame with 5,520,017 rows and 8 variables:
#' \describe{
#'   \item{chrom}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{name}{repeat name}
#'   \item{score}{Smith-Waterman alignment score, i.e. swScore}
#'   \item{strand}{strand}
#'   \item{repClass}{repeat class}
#'   \item{repFamily}{repeat family}
#' }
#' @source \url"http://genome.ucsc.edu/cgi-bin/hgTables"
"repeat_data"



