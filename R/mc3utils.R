#' create a GRanges from the MC3 mutation data
#' @import ggplot2 
#' @import IRanges
#' @param bq bigrquery BigQueryConnection instance
#' @param basicfilt a dplyr::filter instance or NULL to convert entire MAF
#' @param maxnrec numeric(1) used with dplyr::as.data.frame en route to GRanges
#' @return a GRanges instance
#' @examples
#' mc3toGR
#' @export
mc3toGR = function (bq, basicfilt= function(data) dplyr::filter(data,Consequence=="non_coding_transcript_exon_variant"), maxnrec=1e5) {
    mc3 = bq %>% dplyr::tbl(pancan_longname("mc3_v0"))
    if (!is.null(basicfilt)) mc3 = mc3 %>% basicfilt()
    mc3 = mc3 %>% dplyr::filter(Chromosome != "") 
    suppressWarnings({xdf = mc3 %>% as.data.frame(n = maxnrec)})
    if (nrow(xdf)==maxnrec) warning("maxnrec records returned, there could be more available")
    str = xdf$STRAND
    gr = GenomicRanges::GRanges(xdf$Chromosome, IRanges::IRanges(xdf$Start_Position, 
        xdf$End_Position), strand = ifelse(str == 1, "+", ifelse(str ==  
        -1, "-", "*")))
    S4Vectors::mcols(gr) = xdf 
    gr  
}

#mc3toGR = function (bq, basicfilt=dplyr::filter(Consequence=="non_coding_transcript_exon_variant"), maxnrec=1e5) {
#    mc3 = bq %>% dplyr::tbl(pancan_longname("mc3_v0"))
#    if (!is.null(basicfilt)) mc3 = mc3 %>% basicfilt
#    x = x %>% dplyr::filter(Chromosome != "") 
#    suppressWarnings({xdf = x %>% dplyr::as.data.frame(n = maxnrec)})
#    str = xdf$STRAND
#    gr = GenomicRanges::GRanges(xdf$Chromosome, IRanges::IRanges(xdf$Start_Position, 
#        xdf$End_Position), strand = ifelse(str == 1, "+", ifelse(str ==  
#        -1, "-", "*")))
#    S4Vectors::mcols(gr) = xdf 
#    gr  
#}

#' make a ggplot with density traces of mutations per base pair, for 'most mutated' tumor types in a given interval
#' @param bq bigrquery BigQueryConnection instance
#' @param basicfilt a dplyr::filter operation, defaulting to select non-coding variants in mc3 MAF
#' @param chrname character(1) chromosome token in NCBI seqlevels style
#' @param start numeric(1) base coordinate to start
#' @param end numeric(1) base coordinate to end
#' @param project_volume numeric(1) tumor types will have 
#' different numbers of contributions; this parameter tells how many
#' tumor types to represent, counting down from the most frequently represented
#' @param maxnrec numeric(1) for as.data.frame
#' @param binwidth numeric(1) passed to geom_freqpoly
#' @param xlab.in character(1) passed to ggplot2::xlab
#' @return instance of ggplot
#' @examples
#' if (interactive()) {
#'  if (!requireNamespace("ggplot2")) stop("install ggplot2 to run this function")
#'  bq = try(pancan_BQ())
#'  if (!inherits(bq, "try-error")) {
#'   ggMutDens(bq)
#'   }
#'  }
#' @export
ggMutDens = function(bq, 
    basicfilt=function(data) dplyr::filter(data,Consequence=="non_coding_transcript_exon_variant"),
    chrname="15", start=20450000, end=20730000, project_volume=5, maxnrec=50000, 
    binwidth=5000, xlab.in=" ") {
 mc3filt = bq %>% dplyr::tbl(pancan_longname("mc3_v0")) 
 if (!is.null(basicfilt)) mc3filt = mc3filt %>% basicfilt()
 onchr = mc3filt %>% dplyr::select(project_short_name,Chromosome, Start_Position) %>% 
   dplyr::filter(Chromosome==chrname) %>% as.data.frame(n=maxnrec) 
 toptum = names(sort(table(onchr$project_short_name), decreasing=TRUE)[seq_len(project_volume)])
 fonchr = dplyr::filter(onchr, project_short_name %in% toptum)
 ggplot2::ggplot( fonchr, ggplot2::aes(x=Start_Position, ggplot2::stat(density), colour=project_short_name)) +
    ggplot2::geom_freqpoly(binwidth=binwidth) + ggplot2::xlim(c(start, end)) + 
       ggplot2::xlab(xlab.in) + ggplot2::ylab("mutation\ndensity")
}

#' create ggplot for density of starts of a GRanges in an interval
#' @param gr GRanges instance of interest
#' @param mcolvbl character(1) mcols(gr) has this variable that will 
#' be used to specify different groups for computing/colouring the density traces
#' @param chrname character(1) chromosome/seqname
#' @param start numeric(1) start of interval
#' @param end numeric(1) end of interval
#' @param binwidth.in numeric(1) for geom_freqpoly binwidth setting
#' @param basicfilt a dplyr::filter operation, defaulting to select non-coding variants in mc3 MAF
#' @param ylab.in character(1) label for y axis
#' @param slstyle character(1) for GenomeInfoDb::seqlevelsStyle
#' @return ggplot instance
#' @examples
#' ggFeatDens
#' @export
ggFeatDens = function(gr, mcolvbl,
  chrname="chr15", start=20450000, end=20730000, binwidth.in=5000,
  basicfilt=function(data) dplyr::filter(data,Consequence=="non_coding_transcript_exon_variant"),
  ylab.in = "feature\ndensity", slstyle="UCSC") {
 GenomeInfoDb::seqlevelsStyle(gr) = slstyle
 restr = GenomicRanges::GRanges(chrname, IRanges::IRanges(start,end))
 nearGR = subsetByOverlaps(gr, restr)
 myd = data.frame(tfstart=GenomicRanges::start(nearGR), class=mcols(nearGR)[[mcolvbl]])
 ggplot(myd, ggplot2::aes(x=tfstart, stat(density), colour=class)) + geom_freqpoly(binwidth=binwidth.in) + 
       xlim(c(start, end)) + ylab(ylab.in)
}

#' generate a ggplot of segments of gene-like regions
#' @param chrname character(1) chromosome tag
#' @param start numeric(1) start of interval
#' @param end numeric(1) end of interval
#' @param db EnsDb instance for example
#' @param slstyle character(1) tag for seqlevelsStyle
#' @param ylab.in character(1) for use as y axis tag
#' @note Most annotation is turned off with element_blank()
#' @return ggplot instance
#' @examples
#' ggFeatureSegs
#' @export
ggFeatureSegs = function(chrname="chr15", start=20450000, end=20730000, 
   db=EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75,
   slstyle="UCSC", ylab.in="ensembl\nnoncoding") {
 eg = genes(db)
 GenomeInfoDb::seqlevelsStyle(eg) = slstyle
 stopifnot("symbol" %in% names(mcols(eg)))
 nb = subsetByOverlaps(eg, GenomicRanges::GRanges(chrname, IRanges::IRanges(start, end)))
 ndf = as.data.frame(nb)
 ndf$pos = seq(10, 10*nrow(ndf), 10) 
 ggplot(ndf, aes(x=start, xend=end, y=pos, yend=pos, colour=symbol)) + geom_segment() + xlim(c(start,end)) +
   theme(axis.line.y=element_blank(), axis.text.y=element_blank(), 
   axis.ticks.y=element_blank()) + xlab(chrname) + ylab(ylab.in)
}

