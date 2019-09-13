
context("test dockstore scripts")

test_that("exprByMSI_csv produces expected CSV", {
 fn = system.file("scripts/msireg/exprByMSI_csv.R",
    package="BiocOncoTK")
 system2("Rscript", c(fn,
    "--gene=CD8A", "--tumor=BLCA"))
 expect_true(file.exists("BLCA_CD8A.csv"))
 rd = read.csv("BLCA_CD8A.csv")
 expect_true(nrow(rd)==408)
 expect_true(ncol(rd)==8)
})

test_that("rbind utilities work", {
 fn = system.file("scripts/msireg/exprByMSI_csv.R",
    package="BiocOncoTK")
 system2("Rscript", c(fn,
    "--gene=CD8A", "--tumor=BLCA"))
 system2("Rscript", c(fn,
    "--gene=CD8A", "--tumor=SKCM"))
 fn_csv = system.file("scripts/msireg/rbind_csvs_to_RDS.R",
    package="BiocOncoTK")
 fn_rds = system.file("scripts/msireg/rbind_RDS_to_RDS.R",
    package="BiocOncoTK")
 system2("Rscript", c(fn_csv, "demo.rds", "BLCA_CD8A.csv", "SKCM_CD8A.csv"))
 chkdem = readRDS("demo.rds")
 expect_true(nrow(chkdem)==877)
 system2("Rscript", c(fn_rds, "demo2.rds", "demo.rds", "demo.rds"))
 chkd2 = readRDS("demo2.rds")
 expect_true(nrow(chkd2)==2*877)
})
