# helpers not exported at present
featIDMap = function() {
 c(RNASeqv2="Entrez",
   RPPA_clean="Protein",
   meth27k="ID",
   meth450k="ID",
   methMerged="ID",
   miRNA="ID")
}
featValMap=function() {
 c(RNASeqv2="normalized_count",
   RPPA_clean="Value",
   meth27k="Beta",
   meth450k="Beta",
   methMerged="Beta",
   miRNA="miRNAexpr")
}
#' helper for SummarizedExperiment construction from pancan
#' @param bq instance of BigQueryConnection for pancancer-atlas.Annotated Dataset
#' @param acronym character(1) 'cohort' label, e.g., 'BLCA'
#' @param assay character(1) element from names(BiocOncoTK::annotTabs), e.g., 'meth450k'
#' @param sampType character(1) element from 
#' BiocOncoTK::pancan_sampTypeMap$"SampleTypeLetterCode", 
#' e.g., 'TP' for Primary solid Tumor samples,
#' or 'TB' for peripheral blood sample from primary blood derived cancer
#' @param subjectIDName character(1) field name for subject identifier
#' @note Note that pancancer-atlas is distinguished from TCGA by the presence of more
#' sample types.  The default type is 'TP' for primary solid tumor.
#' Codes and their interpretations are available in BiocOncoTK::pancan_sampTypeMap.
#' @return SummarizedExperiment
#' @examples
#' if (interactive() && Biobase::testBioCConnection()) {
#'    billco = Sys.getenv("CGC_BILLING")
#'    if (nchar(billco)>0) {
#'      bq = pancan_BQ()
#'      methSE_BLCA = try(buildPancanSE(bq))
#'      methSE_BLCA
#'    }
#' }
#' @export
buildPancanSE = function(bq, acronym = 'BLCA',
  assay = 'meth450k', sampType = 'TP', subjectIDName = "ParticipantBarcode") {
 if (!requireNamespace("restfulSE")) stop("install restfulSE to use this function")
 stopifnot (assay %in% names(BiocOncoTK::annotTabs) )
 restfulSE::pancan_SE( bq, colDFilterValue = acronym,
     assayDataTableName = BiocOncoTK::annotTabs[ assay ],
     assayFeatureName = featIDMap()[ assay ], assaySampleTypeCode = sampType,
     subjectIDName = subjectIDName, 
     tumorFieldName = "Study", tumorFieldValue = acronym,
      assayValueFieldName = featValMap()[ assay ] )
}
