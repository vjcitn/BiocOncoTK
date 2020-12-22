#' use curatedTCGAData request to acquire plate codes for samples
#' @importFrom curatedTCGAData curatedTCGAData
#' @param tumcode a TCGA tumor code, usually 3 or for characters
#' @param assay a curatedTCGAData assay code, run curatedTCGAData() to
#' see a message with available options
#' @param samptypes a character vector with codes as
#' defined at \url{https://gdc.cancer.gov/resources-tcga-users/tcga-code-tables/sample-type-codes}
#' @return a data.frame with a row for each TCGA contribution for the selected
#' tumor type and assay type
#' @examples
#' if (interactive()) {
#'  plts_blca_rnaseq = get_plates()
#'  dim(plts_blca_rnaseq)
#'  head(plts_blca_rnaseq)
#' }
#' @export
get_plates = function(tumcode="BLCA", assay="RNASeq2GeneNorm",
  samptypes=c("01", "02", "03", "04", "06", "09", "40")) {
 mae = curatedTCGAData::curatedTCGAData(
    tumcode, assay, version = "1.1.38", dry.run=FALSE
 )
 el = MultiAssayExperiment::experiments(mae)
 stopifnot(length(el)==1)
 curse = MultiAssayExperiment::experiments(mae)[[1]]
 curse_barcodes = colnames(curse)
 samptypes = substr(curse_barcodes,14,15)
 keep = which(samptypes %in% c("01", "02", "03", "04", "05", "06", "07",
       "08", "09", "40"))
 keep = which(samptypes %in% c("01", "02", "03", "04", "06", "09", "40"))
 if (length(keep)==0) stop("no tumor data available using barcode segment 14:15") 
 curse = curse[,keep]  # just tumor samples
 plate = substr(colnames(curse),22,25)
 patid = substr(colnames(curse),1,12)
 data.frame(patient_barcode=patid, tumcode=tumcode, plate=plate, sample_barcode=colnames(curse), stringsAsFactors=FALSE)
}

