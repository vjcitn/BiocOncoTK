#' CCLE_DRUG_BROAD: serialization of legacy CCLE 'Drug data' from Broad Institute
#' @importFrom utils data
#' @importFrom S4Vectors metadata
#' @docType data
#' @format S4Vectors DataFrame instance
#' @source \url{"https://data.broadinstitute.org/ccle_legacy_data/pharmacological_profiling/CCLE_NP24.2009_Drug_data_2015.02.24.csv"}
#' @examples
#' data(CCLE_DRUG_BROAD)
#' requireNamespace("S4Vectors")
#' S4Vectors::metadata(CCLE_DRUG_BROAD) # imported using read.csv, stringsAsFactors=FALSE, coerced to DataFrame
#' head(CCLE_DRUG_BROAD)
"CCLE_DRUG_BROAD"

