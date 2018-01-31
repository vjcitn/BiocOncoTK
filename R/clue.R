.testClue = function(key=Sys.getenv("CLUE_KEY")) {
  ans = httr::GET(sprintf('https://api.clue.io/api/cells?filter={"where":{"cell_id":"A375"}}&user_key=%s', key))
  rjson::fromJSON(readBin(ans$content, what="character"))
}

#' generate lists to generate clue API queries
#' @import httr
#' @import rjson
#' @note These are converted to JSON (%20 substituted for embedded blanks.
#' @export
clueDemos = function() list(cells = 
              list(where=list(cell_id="A375")),
             genes = 
              list(where=list(l1000_type="landmark")),
             profiles = 
              list(where=list(pert_iname='sirolimus', 
                              cell_id='MCF7', assay='L1000')),
             perts = 
              list(where=list(pert_iname='erlotinib')),
             sigs =
              list(where= list(pert_desc = 'imatinib',
                   cell_id = 'MCF7')),
             rep_drug_moas = 
              list(where=list(name="calcineurin inhibitor"), 
                  fields=list(pert_iname=TRUE)),
             pcls =
              list(include=list(perts=TRUE), where=list(group_id=
                     "CP_ATP_SYNTHASE_INHIBITOR"))
           )

#' Provide names of some clue.io services for which examples are available in this package.
#' @note See \url{https://clue.io/api}.
clueServiceNames = function() 
   c("cells", "genes", "profiles", "perts", "sigs", "rep_drug_moas")
   
#' run the api.clue.io API to acquire information on LINCS experiments
#' @param service a character(1) service name
#' @param filter a list to be converted to JSON for submission as a GET request
#' @param key character(1) API key provided by clue.io
#' @examples
#' demos = clueDemos()
#' nd = length(demos)
#' chk = lapply(seq_len(nd), function(x) query_clue( service=names(demos)[x],
#'               filter=demos[[x]]) )
#' names(chk) = names(demos)
#' sapply(chk,length)
#' @export
query_clue = function(service = "profiles", 
    filter = list(where=(list(pert_iname='sirolimus',
                     cell_id='MCF7', assay='L1000'))),
    key = Sys.getenv("CLUE_KEY")) {
    requireNamespace("httr")
    requireNamespace("rjson")
    stub = paste("https://api.clue.io/api/", service, "?", collapse="", sep="")
    jfilt = rjson::toJSON(filter)
    quer = paste(stub, "filter=", jfilt, "&user_key=", key, sep="", collapse="")
    quer = gsub(" ", "%20", quer)
    ans = httr::GET(quer)
    rjson::fromJSON(readBin(ans$content, what="character"))
}

#' enumerate perturbagen classes
#' @param key character(1) API key provided by clue.io
#' @examples
#' if (nchar(Sys.getenv("CLUE_KEY"))>0) pc = pertClasses()
#' head(vapply(pc, "[[", character(1), 1))
#' @export
pertClasses = function(key=Sys.getenv("CLUE_KEY")) {
 pc = httr::GET(paste0("https://api.clue.io/api/pcls?user_key=",key))
 rjson::fromJSON(readBin(pc$content, what="character"))
}
