library(SummarizedExperiment)
library(GenomicRanges)
fn = "https://static-content.springer.com/esm/art%3A10.1038%2Fncomms15180/MediaObjects/41467_2017_BFncomms15180_MOESM265_ESM.xls"
mp_5utr = read.delim(fn, sep="\t", check.names=FALSE, stringsAsFactors=FALSE)
rownames(mp_5utr) = mp_5utr[,1]
mp_5utr = mp_5utr[,-1]
strsplit(rownames(mp_5utr), "_") -> sp
cn = sapply(sp, "[",1)
startloc = sapply(sp, "[", 2)
endloc = sapply(sp, "[", 3)
gene = sapply(sp, "[", 4)
t5 = sapply(sp, "[", 5)
table(table(gene))
t6 = sapply(sp, "[", 6)
maybe_rn = paste(gene, startloc, sep="_")
molp_5utr = GRanges(cn, IRanges(as.numeric(startloc), as.numeric(endloc)))
mcols(molp_5utr)$gene = gene
mcols(molp_5utr)$full_rn = rownames(mp_5utr)
names(molp_5utr) = maybe_rn
sum(duplicated(maybe_rn))
assy = data.matrix(mp_5utr)
library(SummarizedExperiment)
molpo_5utr = SummarizedExperiment(assy)
names(molp_5utr) = maybe_rn
rowRanges(molpo_5utr) = molp_5utr
names(assays(molpo_5utr)) = "sputnik_based"
metadata(molpo_5utr) = list(sourcepub_url = "https://www.nature.com/articles/ncomms15180", table_url=fn)
molpo_5utr
