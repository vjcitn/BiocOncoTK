library(SummarizedExperiment)
library(GenomicRanges)
fn = "https://static-content.springer.com/esm/art%3A10.1038%2Fncomms15180/MediaObjects/41467_2017_BFncomms15180_MOESM263_ESM.xls"
mp_CDS = read.delim(fn, sep="\t", check.names=FALSE)
strsplit(rownames(mp_CDS), "_") -> sp
cn = sapply(sp, "[",1)
startloc = sapply(sp, "[", 2)
endloc = sapply(sp, "[", 3)
gene = sapply(sp, "[", 4)
t5 = sapply(sp, "[", 5)
table(table(gene))
t6 = sapply(sp, "[", 6)
maybe_rn = paste(gene, startloc, sep="_")
molp_CDS = GRanges(cn, IRanges(as.numeric(startloc), as.numeric(endloc)))
mcols(molp_CDS)$gene = gene
mcols(molp_CDS)$full_rn = rownames(mp_CDS)
names(molp_CDS) = maybe_rn
sum(duplicated(maybe_rn))
assy = data.matrix(mp_CDS)
library(SummarizedExperiment)
molpo_CDS = SummarizedExperiment(assy)
names(molp_CDS) = maybe_rn
rowRanges(molpo_CDS) = molp_CDS
names(assays(molpo_CDS)) = "sputnik_based"
metadata(molpo_CDS) = list(sourcepub_url = "https://www.nature.com/articles/ncomms15180", table_url=fn)
molpo_CDS
