#np = adjp$adjp[order(adjp$index),]
#np = cbind(t.tests,np)
#sig.t = np[which(np[,"BH"]<0.05),]
#sig.t$tumor = as.character(sig.t$tumor)
#sig.t$gene = as.character(sig.t$gene)
#dim(sig.t)
#head(sig.t %>% arrange(BH))
#
#   adv = c("FOXM1", "BIRC5", "TOP2A", "TPX2", "NME1", "CCNB1", "CEP55", 
#   "TYMS", "CENPF", "CDKN3")
#   
#   fav = c("KLRB1", "ITM2B", "CBX7", "CD2", "CREBL2", 
#   "SATB1", "NR3C1", "TMEM66", "KLRK1", "FUCA1", "CD8A")
#
#   options(repr.plot.width=8,repr.plot.height=4)

#' configure a bipartite graph relating tumor type to gene, using graphNEL
#' @import graph
#' @rawNamespace import("Rgraphviz", except=c("from", "to"))
#' @param stattab a data.frame with columns 'tumor', 'gene', and 'tstat'
#' @param genes_adverse a vector of genes whose increased expression is 
#' regarded as adverse
#' @param genes_favorable a vector of genes whose increased expression is 
#' regarded as favorable
#' @param gpar_cex tune size of graph labels
#' @param gpar_lwd tune appearance of node boundaries
#' @return a graphNEL instance (graph package)
#' @examples
#' bipg_tests(k23sig)
#' @export
bipg_tests = function(stattab, genes_adverse=NA, genes_favorable=NA,
 gpar_cex=.65, gpar_lwd = 0 ) {
   stopifnot(inherits(stattab, "data.frame"))
   stopifnot(all(c("tumor", "gene", "tstat") %in% names(stattab)))
   requireNamespace("graph")
   requireNamespace("Rgraphviz")
   g = new("graphNEL", edgemode="directed",
           nodes = unique(c(stattab$tumor,stattab$gene)))
   for (i in 1:nrow(stattab)) 
      g = graph::addEdge(to=stattab$gene[i], from=stattab$tumor[i], g)
   
   graph::graph.par(list(nodes=list(shape="rectangle", 
      cex=gpar_cex, lwd=gpar_lwd)))
   
   gnel = Rgraphviz::layoutGraph(g)
   graph::nodeRenderInfo(gnel)$fill[] = 
         ifelse(graph::nodes(g) %in% genes_favorable, "lightblue", 
            ifelse(graph::nodes(g) %in% genes_adverse, "orange", "transparent"))
   enm = paste0(stattab$tumor,"~",stattab$gene)
   tst = stattab$tstat
   graph::edgeRenderInfo(gnel)$col[enm][] = ifelse(tst<0,"orange", "blue") # see note about directionality above
   Rgraphviz::renderGraph(gnel)
}
