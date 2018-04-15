#' use BiocFileCache discipline to acquire patelGBMSC SummarizedExperiment
#' @param remotePath character(1) identifying remote RDS
#' @param cache instance of BiocFileCache, defaults to BiocFileCache::BiocFileCache()
#' @note The RDS for the SummarizedExperiment is in an AWS S3 bucket.
#' This function will check local cache for the data and will download
#' to cache if not found.  That download is a one-time operation for
#' any given value of \code{cache}.
#' @return a SummarizedExperiment instance
#' @examples
#' loadPatel
#' @export
loadPatel = function(remotePath = "https://s3.us-east-2.amazonaws.com/biocfound-scrna/patelGBMSC.rds", cache=BiocFileCache::BiocFileCache()) {
    if (!checkCache_patel(cache)) message("adding RDS to local cache, future invocations will use local image")
    path = BiocFileCache::bfcrpath(cache, remotePath)
    readRDS(path)
}
    
checkCache_patel = function(cache=BiocFileCache()) {
 if (!requireNamespace("BiocFileCache")) stop("install BiocFileCache to use this function")
 allr = BiocFileCache::bfcinfo(cache)$rname
 "https://s3.us-east-2.amazonaws.com/biocfound-scrna/patelGBMSC.rds" %in% allr
}
