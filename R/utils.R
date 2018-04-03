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
