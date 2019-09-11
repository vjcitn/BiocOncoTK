# this is to be used with Rscript
# Rscript exprByMSI_csv --tumor=[] --gene=[] will
# generate a csv with expression data from
# curatedTCGAData and MSIsensor stratification from
# BiocOncoTK

#exprByMSI_csv = function(tumcode, genesym, alias) {
# library(BiocGenerics)
 options(warn=-1)
 inargs = commandArgs(trailingOnly=TRUE)
 stopifnot(length(inargs)==2)
 parms = strsplit(inargs, "=")
 pnames = sapply(parms, "[", 1)
 pvals = sapply(parms, "[", 2)
 stopifnot(all(c("--tumor", "--gene") %in% pnames))
 names(pvals) = pnames
 tumcode = pvals["--tumor"]
 genesym = pvals["--gene"]
 library(ExperimentHub)
 eca = getExperimentHubOption("CACHE")
 if (!dir.exists(eca)) try(dir.create(getExperimentHubOption("CACHE")))
 mae = curatedTCGAData::curatedTCGAData(tumcode, "RNASeq2GeneNorm", dry.run=FALSE)
 nn = names(MultiAssayExperiment::experiments(mae))
 wh = grep(tumcode, nn) 
 stopifnot(length(wh)==1)
 curse = MultiAssayExperiment::experiments(mae)[[wh]]
 colnames(curse) = substr(colnames(curse),1,12)
 curex = BiocOncoTK::bindMSI(curse)
 ass = SummarizedExperiment::assay(curex[as.character(genesym),])
 ans = data.frame(patient_barcode = colnames(curex),
  acronym=tumcode,
  symbol = genesym,
  alias = genesym,
  log2ex = log2(as.numeric(ass)+1),
  msicode = ifelse(curex[["msiTest"]] >= 4, ">=4", "<4"))
 write.csv(ans, paste0(tumcode, "_", genesym, ".csv"))

