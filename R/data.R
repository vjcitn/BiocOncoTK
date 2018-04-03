#' CCLE_DRUG_BROAD: serialization of legacy CCLE 'Drug data' from Broad Institute
#' @importFrom utils data
#' @importFrom dplyr tbl tbl_df select filter summarise n group_by src_tbls tbl_vars
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
#' cell_70138: a table with cell-line information from LINCS
#' @docType data
#' @format data.frame
#' @source GEO GSE70138 GSE70138_Broad_LINCS_cell_info_2017-04-28.txt.gz
#' @examples
#' data(cell_70138)
"cell_70138"
#' pert_70138: a table with perturbagen information from LINCS
#' @docType data
#' @format data.frame
#' @source GEO GSE70138 GSE70138_Broad_LINCS_pert_info.txt.gz
#' @examples
#' data(pert_70138)
"pert_70138"
