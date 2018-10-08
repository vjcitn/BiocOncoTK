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
#' @param seTransform a function that accepts a SummarizedExperiment and returns a SummarizedExperiment; useful for feature name remapping, defaults to force (does nothing)
#' @param bindMethRowranges logical(1) if true and assay is meth27k
#' or meth450k, annotation from FDb.InfiniumMethylation.hg19
#' and EnsDb.Hsapiens.v75 is obtained for available features
#' and bound into the rowRanges component of returned object
#' @note Note that pancancer-atlas is distinguished from TCGA by the presence of more
#' sample types.  The default type is 'TP' for primary solid tumor.
#' Codes and their interpretations are available in 
#' BiocOncoTK::pancan_sampTypeMap.
#' @return SummarizedExperiment, with metadata on acronym, assay,
#' and sampleType propagated; if the assay is a methylation
#' assay and bindMethRowranges is TRUE, a RangedSummarizedExperiment
#' is returned.
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
  assay = 'meth450k', sampType = 'TP', 
  subjectIDName = "ParticipantBarcode", seTransform=force,
  bindMethRowranges = TRUE) {
 if (!requireNamespace("restfulSE")) stop("install restfulSE to use this function")
 stopifnot (assay %in% names(BiocOncoTK::annotTabs) )
 stopifnot (is(bq, "BigQueryConnection"))
 ans = restfulSE::pancan_SE( bq, colDFilterValue = acronym,
     assayDataTableName = BiocOncoTK::annotTabs[ assay ],
     assayFeatureName = featIDMap()[ assay ], assaySampleTypeCode = sampType,
     subjectIDName = subjectIDName, 
     tumorFieldName = "Study", tumorFieldValue = acronym,
      assayValueFieldName = featValMap()[ assay ] )
 if (is.list(metadata(ans))) metadata(ans) = 
      c(metadata(ans), acronym=acronym,
         assay = assay, sampType=sampType)
 if ((assay %in% c("meth27k", "meth450k")) & bindMethRowranges)
      ans = bindRowranges450k(ans)
 ans
}

#' map rownames of an SE to another vocabulary
#' @param se SummarizedExperiment instance
#' @param sourceVocab character(1) must be a keytype of org.Hs.eg.db, defaults to 'ENTREZID'
#' @param targetVocab character(1) must be a column of org.Hs.eg.db
#' @export
replaceRownames = function(se, sourceVocab="ENTREZID", targetVocab="SYMBOL") {
 if (!requireNamespace("org.Hs.eg.db")) stop("install org.Hs.eg.db to use replaceEntrez")
 if (!requireNamespace("AnnotationDbi")) stop("install AnnotationDbi to use replaceEntrez")
 rn = rownames(se)
 ks = AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype=sourceVocab)
 todrop = setdiff(rn, ks)
 if (length(todrop>0)) {
  message(paste(length(todrop), "rows unmapped to ENTREZ, dropped"))
  se = se[-match(todrop, rownames(se)),]
  }
 rownames(se) = AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db, 
   keys=rownames(se), keytype=sourceVocab, "ENTREZID",column=targetVocab)
 se
}


bindRowranges450k = function(se,
   platformObject = FDb.InfiniumMethylation.hg19::FDb.InfiniumMethylation.hg19,
   geneObject = EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75) {
  mdass = S4Vectors::metadata(se)$assay
  if (!requireNamespace("FDb.InfiniumMethylation.hg19"))
    stop("please install FDb.InfiniumMethylation.hg19 to use bindRowranges450k")
  if (!requireNamespace("EnsDb.Hsapiens.v75"))
    stop("please install EnsDb.Hsapiens.v75 to use bindRowranges450k")
  feat = GenomicFeatures::features(platformObject)
  mcols(feat) = NULL
  f2use = feat[rownames(se)]
  gmap = GenomicFeatures::genes(geneObject)
  GenomeInfoDb::genome(gmap) = "hg19"
  GenomeInfoDb::seqlevelsStyle(gmap) = "UCSC"
  gmap = resize(gmap,1)
  gmap = gmap[IRanges::nearest(f2use, gmap)]
  names(gmap) = names(f2use)
  SummarizedExperiment::rowRanges(se) = gmap[,c("gene_id", "gene_name", 
    "gene_biotype")]
  se
}
