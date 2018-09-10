#' @importFrom SummarizedExperiment colData
#
.load_mock = function(stub) {
 get(load(system.file(sprintf("mocks/%s.rda", stub), package="BiocOncoTK")))
}
#' utilities for mock data (not involving internet access for vignette)
#' @rdname utils
#' @note These functions are provided only for avoiding reliance on internet connectivity for document production.
#' @return a list of DRProfSet instances
#' @examples
#' load_ccleNRAS()
#' dim(load_nrasdf())
#' @export
load_ccleNRAS = function() {
 .load_mock("ccleNRAS")
}
#' @rdname utils
#' @return a data.frame with fields 'Cell_line_primary_name', 'RMA_normalized_expression', 'HGNC_gene_symbol'
#' @export
load_NRAS_AHR = function() {
 .load_mock("NRAS_AHR")
}
#' @rdname utils
#' @return a data.frame
#' @export
load_nrasdf = function() {
 .load_mock("nrasdf")
}

#' bind MSI data to a SummarizedExperiment
#' @param se SummarizedExperiment instance
#' @return SummarizedExperiment instance with expanded colData,
#' samples limited to those with microsatellite instability values.
#' @note This function adds the column \code{msiTest} to
#' \code{colData(se)}.  The contents of the column are given by
#' \code{\link{fireMSI}}.  Samples in \code{se}
#' that do not correspond to a row of \code{\link{fireMSI}}
#' are dropped.  If there is already a column named \code{msiTest}
#' in \code{colData(se)}, it is replaced and samples are filtered
#' as described, and a message is given.  If none of the
#' samples in \code{se} have rows in \code{\link{fireMSI}},
#' an error is thrown.
#' @examples
#' bindMSI
#' @export
bindMSI = function(se) {
 inpat = colnames(se)
 if ("msiTest" %in% names(colData(se)))
   message("'msiTest' found among colData columns, will replace.")
 msi = BiocOncoTK::fireMSI
 msipat = msi$patient_barcode
 ok = intersect(inpat, msipat)
 if (length(ok)==0) stop("no patients in SE found in fireMSI")
 se = se[, ok]
 rownames(msi) = msi$patient_barcode
 se$msiTest = as.character(msi[colnames(se),"msiTest"])
 se
}
