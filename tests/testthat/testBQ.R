
context("buildPancanSE compared to dbGetQuery")

test_that("meth450k data concordant between SE and BQ", {
myq = "SELECT ParticipantBarcode, Study, ID, Beta, sampletype, sampletypelettercode FROM 
`pancancer-atlas.Annotated.jhu_usc_edu_PANCAN_HumanMethylation450_betaValue_whitelisted_annot` 
where Study = 'BLCA' and (ID = 'cg00000029' or ID = 'rs9839873') and 
(ParticipantBarcode = 'TCGA-CU-A0YN' or ParticipantBarcode = 'TCGA-CU-A0YR') and SampleTypeLetterCode = 'TP'"

if (nchar(Sys.getenv("CGC_BILLING"))==0) skip("CGC_BILLING not set")
 bq = try(pancan_BQ())
 if (inherits(bq, "try-error")) skip("can't establish pancan-atlas CGC_BILLING")
 se = buildPancanSE(bq)
 sedat = SummarizedExperiment::assay(se[c("cg00000029", "rs9839873"), c("TCGA-CU-A0YN", "TCGA-CU-A0YR")])
 semat = as.matrix(sedat)
 bqdat = DBI::dbGetQuery(bq, myq)

bqdf = dcast(data=bqdat, formula=ID~ParticipantBarcode, value.var="Beta")
bqmat = data.matrix(bqdf[,-1])
rownames(bqmat) = bqdf[,1]
expect_true(all.equal(bqmat[rownames(semat), colnames(semat)], semat))
})

