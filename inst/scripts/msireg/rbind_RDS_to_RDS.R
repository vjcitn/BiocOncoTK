options(warn=-1)
args = commandArgs(trailingOnly=TRUE)
oname = args[1]
files = args[-1]
stopifnot(length(files)>1)
fl = lapply(files, readRDS)
ans = do.call(rbind, fl)
saveRDS(ans, oname)
