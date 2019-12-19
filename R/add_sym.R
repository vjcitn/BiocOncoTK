#' add symbols in rowData to a SummarizedExperiment that has Entrez IDs for rownames
#' @param x SummarizedExperiment instance
#' @note Will fail if 'symbol' is a column of rowData(x)
#' @examples
#' if (interactive()) {
#'   bq = pancan_BQ()
#'   rnse = try(buildPancanSE(bq, assay="RNASeqv2"))
#'   if (inherits(rnse, "try-error")) stop("probably need CGC_BILLING set in environment or with pancan_BQ")
#'   add_sym(rnse)
#' }
#' @export
add_sym = function (x) 
{
    rd = rowData(x)
    if (ncol(rd)>0 && "symbol" %in% colnames(rd)) stop("symbol already in colnames(rowData(x))")
    n = mapIds(org.Hs.eg.db::org.Hs.eg.db, keys = rownames(x), 
        column = "SYMBOL", keytype = "ENTREZID")
    rowData(x) = cbind(rowData(x), DataFrame(symbol = as.character(n)))
    x
}

