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

#' microsatellite instability data in TCGA, collected from curatedTCGAData
#' @docType data
#' @format DataFrame
#' @source firehose via curatedTCGAData; see metadata(BiocOncoTK::fireMSI)
#' @examples
#' str(S4Vectors::metadata(BiocOncoTK::fireMSI))
"fireMSI"

#' microsatellite instability data in TCGA, collected from Ding et al. Cell 173(2) 2018.
#' @docType data
#' @format DataFrame
#' @source \url{https://www.cell.com/cms/10.1016/j.cell.2018.03.033/attachment/0ac495ba-3578-41cf-8fb1-94487f554bb5/mmc5.xlsx} retrieved 9/17/2018.
#' @examples
#' str(BiocOncoTK::dingMSI)
"dingMSI"

#' filtered MSI data for demonstrating exploratory app
#' @docType data
#' @format DataFrame
#' @source MSI values from dingMSI, expression from curatedTCGAData for three genes, two tumors
#' @examples
#' head(BiocOncoTK::small_msi)
"small_msi"


#' representation of 3'UTR MSI events in TCGA from Cortes-Ciriano et al. 2017
#' @docType data
#' @import SummarizedExperiment
#' @import GenomicRanges
#' @format SummarizedExperiment
#' @source \url{https://www.nature.com/articles/ncomms15180#Sec22}
#' @note Supplementary data 6 from publication noted in Source.  See
#' metadata() component of this SummarizedExperiment for more details.
#' @examples
#' molpo_3utr
"molpo_3utr"
#' representation of MSI events in coding regions TCGA from Cortes-Ciriano et al. 2017
#' @docType data
#' @format SummarizedExperiment
#' @source \url{https://www.nature.com/articles/ncomms15180#Sec22}
#' @note Supplementary data 5 from publication noted in Source. See
#' metadata() component of this SummarizedExperiment for more details.
#' @examples
#' molpo_CDS
"molpo_CDS"
#' representation of 5'UTR MSI events in TCGA from Cortes-Ciriano et al. 2017
#' @docType data
#' @format SummarizedExperiment
#' @source \url{https://www.nature.com/articles/ncomms15180#Sec22}
#' @note Supplementary data 7 from publication noted in Source. See
#' metadata() component of this SummarizedExperiment for more details.
#' @examples
#' molpo_5utr
"molpo_5utr"
#' MSIsensor microsatellite instability scores for TCGA, collected from Ding et al. Cell 173(2) 2018.
#' @docType data
#' @format DataFrame
#' @source \url{https://www.cell.com/cms/10.1016/j.cell.2018.03.033/attachment/0ac495ba-3578-41cf-8fb1-94487f554bb5/mmc5.xlsx} retrieved 9/17/2018.
#' @examples
#' str(BiocOncoTK::dingMSI)
"MSIsensor.10k"

#' representation of events detected in 708 WGS experiments TCGA from Cortes-Ciriano et al. 2017
#' @docType data
#' @format SummarizedExperiment
#' @source \url{https://www.nature.com/articles/ncomms15180#Sec22}
#' @note Supplementary data 10 from publication noted in Source. See
#' metadata() component of this SummarizedExperiment for more details.
#' @examples
#' molpo_WGS
"molpo_WGS"

#' data.frame mapping from TCGA patient_barcode to TCGA tumor code
#' @docType data
#' @format data.frame
#' @source \url{https://portal.gdc.cancer.gov/exploration?uploadCaseTab=matched}
#' @note Used IDs recorded in MSISensor.10k; one is unmatched at TCGA portal
#' metadata() component of this SummarizedExperiment for more details.
#' @examples
#' head(patient_to_tumor_code)
"patient_to_tumor_code"

#' list of 151 genes annotated as DNA repair pathway members
#' @docType data
#' @format named list
#' @source \url{https://academic.oup.com/jnci/article/104/9/670/872781#supplementary-data}
#' @note The zipped PDF was read using pdftools::pdf_text and then
#' manually organized.  All gene symbols present in curatedTCGAData
#' RNASeq2GeneNorm rownames.  The list elements are
#' ATM, BER, FA.HR, MMR, NER, NHEJ, OTHER, TLS, RECQ, and XLR.
#' These denote, respectively, ataxia-telangiectasia-mutated,
#' base excision repair, Fanconi anemia/homologous recombination,
#' mismatch repair, nucleotide excision repair,
#' non-homologous end joining, other, translesion synthesis,
#' recQ helicase pathway, and cross-link repair.
"kang_DNArepair"

#' a table of 'significant' MSIsensor-score/expression relationships in TCGA
#' @docType data
#' @format data.frame
#' @note provided to demonstrate bipartite graph construction
#' @examples
#' head(k23sig)
"k23sig"

#' a manually constructed table mapping TCGA acronyms to NCIT thesaurus tags
#' @format data.frame
#' @note Constructed using ontoProc::getOncotreeOnto() result.  See the vignette
#' on Mapping TCGA tumor codes to NCIT for elaborating the mapping to
#' aggregate tumors into NCIT organ systems.
"map_tcga_ncit"
