.load_mock = function(stub) {
 get(load(system.file(sprintf("mocks/%s.rda", stub), package="BiocOncoTK")))
}
#' utilities for mock data (not involving internet access for vignette)
#' @rdname utils
#' @note These functions are provided only for avoiding reliance on internet connectivity for document production.
#' @examples
#' load_ccleNRAS()
#' @export
load_ccleNRAS = function() {
 .load_mock("ccleNRAS")
}
#' @rdname utils
#' @export
load_NRAS_AHR = function() {
 .load_mock("NRAS_AHR")
}
#' @rdname utils
#' @export
load_nrasdf = function() {
 .load_mock("nrasdf")
}
