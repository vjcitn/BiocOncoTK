#' CCLE_DRUG_BROAD: serialization of legacy CCLE 'Drug data' from Broad Institute
#' @importFrom utils data
#' @importFrom dplyr tbl tbl_df select filter summarise n group_by src_tbls tbl_vars select_ group_by_
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

#' pancan.clin.varnames: a data.frame with a list of variable names for clinical patient data
#' @docType data
#' @format data.frame
#' @source pancancer-atlas in BigQuery
#' @examples
#' BiocOncoTK::pancan.clin.varnames[1:5,]
"pancan.clin.varnames"

#' table names in Annotated pancancer data release
#' @docType data
#' @format character vector
#' @source pancancer-atlas in BigQuery
#' @examples
#' BiocOncoTK::annotTabs
"annotTabs"

#' helper for interpreting ICD-10 codes
#' @docType data
#' @format data.frame
#' @source ICD-10
#' @examples
#' BiocOncoTK::icd10_c
"icd10_c"

#' helper for interpreting pancan-atlas sample type codes
#' @docType data
#' @format data.frame
#' @source ISB BigQuery pancan-atlas project
#' @note The sample type codes are not straightforward to interpret.
#' Primary solid tumor is denoted "TP", and metastatic samples are 
#' denoted "TM".  This data frame pairs code and natural language terms.
#' @examples
#' BiocOncoTK::pancan_sampTypeMap
"pancan_sampTypeMap"

#' a virtual MultiAssayExperiment for pancancer-atlas BRCA data
#' @md
#' @docType data
#' @format MultiAssayExperiment instance with DelayedArray (BQ3_Array) assay data
#' @source ISB BigQuery pancan-atlas project
#' @note Constructed as
#' ```
#' library(BiocOncoTK)
#' pcbq = pancan_BQ()
#' library(restfulSE)
#' BRCA_mir = pancan_SE(pcbq)
#' BRCA_mrna = pancan_SE(pcbq,
#'    assayDataTableName = pancan_longname("rnaseq"),
#'    assayFeatureName = "Entrez",
#'    assayValueFieldName = "normalized_count")
#' BRCA_rppa = pancan_SE(pcbq,
#'    assayDataTableName = pancan_longname("RPPA"),
#'    assayFeatureName = "Protein",
#'    assayValueFieldName = "Value")
#' BRCA_meth = pancan_SE(pcbq,
#'    assayDataTableName = pancan_longname("27k")[2],
#'    assayFeatureName = "ID",
#'    assayValueFieldName = "Beta")
#' library(MultiAssayExperiment)
#' library(dplyr)
#' library(magrittr)
#' clinBRCA = pcbq %>% tbl(pancan_longname("clinical")) %>%
#'   filter(acronym=="BRCA") %>% as.data.frame() 
#' rownames(clinBRCA) = clinBRCA[,2]
#' clinDF = DataFrame(clinBRCA)
#' library(MultiAssayExperiment)
#' brcaMAE = MultiAssayExperiment(
#'   ExperimentList(rnaseq=BRCA_mrna, meth=BRCA_meth, rppa=BRCA_rppa,
#'     mirna=BRCA_mir),colData=clinDF)
#' upsetSamples(brcaMAE) # to view display
#' ```
#' @examples
#' if (requireNamespace("MultiAssayExperiment"))
#' BiocOncoTK::brcaMAE
"brcaMAE"

#' Data in count_lstpm format from Darmanis 2017 (PMC 5810554) single cell RNA-seq in GBM
#' @docType data
#' @format SummarizedExperiment with HDF Object store back end
#' @source http://imlspenticton.uzh.ch/robinson_lab/conquer/data-mae/GSE84465.rds
#' @note https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5810554/ is the main source article.
#' @examples
#' BiocOncoTK::darmGBMcls
"darmGBMcls"
