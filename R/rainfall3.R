#' @import ggplot2
#' @import S4Vectors
#' @import TxDb.Hsapiens.UCSC.hg19.knownGene
#' @import TxDb.Hsapiens.UCSC.hg18.knownGene

midpts = function(x) {
# given n interval boundaries obtain n-1 midpts
c(x[1]/2, x[-length(x)] + diff(x)/2)
}

chrbounds_basic = function(genome="hg18") {
 if (genome=="hg19") {
    requireNamespace("TxDb.Hsapiens.UCSC.hg19.knownGene")
    info = seqinfo(TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene)[paste0("chr", c(1:22, "X", "Y")),]
    }
 else if (genome=="hg18") {
    requireNamespace("TxDb.Hsapiens.UCSC.hg18.knownGene")
    info = seqinfo(TxDb.Hsapiens.UCSC.hg18.knownGene::TxDb.Hsapiens.UCSC.hg18.knownGene)[paste0("chr", c(1:22, "X", "Y")),]
    }
 cumsum(as.numeric(seqlengths(info)))
}

kataColors = function () 
{
    cmap = c("blue", "black", "red", "purple", "yellow", "green")
    names(cmap) = c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
    cmap
}
subt = function(ref, a1, a2) {
        alt = ifelse(a1 != ref, a1, a2)
        tmp = ref
        needsw = which(alt %in% c("C", "T"))
        ref[needsw] = alt[needsw]
        alt[needsw] = tmp[needsw]
        paste(ref, alt, sep = ">")
}

.rainfall.bq.df = function(bq, studytag="LUAD", VariantType="SNP", id="TCGA-05-4398", colmap=kataColors(),
  dropDupChg=TRUE) {
     select = dplyr::select
     init = bq %>% tbl("Somatic_Mutation_calls") %>% 
         select(ParticipantBarcode, Tumor_SampleTypeLetterCode, Start_Position, Chromosome,
          Variant_Classification, Variant_Type, Study, Reference_Allele, NCBI_Build, Hugo_Symbol,
          Tumor_Seq_Allele1, Tumor_Seq_Allele2, cDNA_Change) %>% 
         filter(Study==studytag) %>% filter(ParticipantBarcode==id) %>%
         filter(Variant_Type==VariantType) %>% as.data.frame()
     init$Subst = with(init, subt(Reference_Allele, Tumor_Seq_Allele1, Tumor_Seq_Allele2) )
     init$Chromosome = paste0("chr", init$Chromosome)
     init = init[ which(init$Subst %in% names(colmap)), ]
     if (init$NCBI_Build[1] == "36") {
          requireNamespace("TxDb.Hsapiens.UCSC.hg18.knownGene")
          lenbase = TxDb.Hsapiens.UCSC.hg18.knownGene::TxDb.Hsapiens.UCSC.hg18.knownGene
          }
     else if (init$NCBI_Build[1] == "37") {
          requireNamespace("TxDb.Hsapiens.UCSC.hg19.knownGene")
          lenbase = TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
          }
     else stop("can't decode init[['NCBI_Build']][1] (typically 36 or 37 in 2016)")
     canonseqs = paste0("chr", c(1:22, "X", "Y"))
     slens = seqlengths(lenbase)[canonseqs]
     soff = c(0, cumsum(as.numeric(slens[-length(slens)])))
     names(soff) = canonseqs
     init$totalgd = init$Start_Position + soff[ as.character(init$Chromosome) ]
# put in natural genomic order
     if (dropDupChg && any(duplicated(init$cDNA_Change))) init = init[-which(duplicated(init$cDNA_Change)),]
     sdf = split(init, as.character(init$Chromosome))
     sdf = lapply(sdf, function(x) x[order(x$Start_Position),,drop=FALSE])
     nm = names(sdf)
     o = match(canonseqs, nm)
     do.call(rbind, sdf[o])
}

rainfallBQ = function(bq, studytag="LUAD", VariantType="SNP", id="TCGA-05-4398", colmap=kataColors(),
    ymax = 9, inylab="-log10 dbp", inxlab="chromosome", intitle, 
    addbounds=TRUE, cnames=TRUE) {
#
# basis is the ISB BigQuery for TCGA
#
  df = .rainfall.bq.df( bq, studytag=studytag, VariantType=VariantType, id=id, colmap=colmap )
  di = c(log10(df$totalgd[1]/2), log10(diff(df$totalgd) + 1))
  df_new = data.frame(totalgd=df$totalgd, ml10dbp=di, Subst=df$Subst, chr=df$Chromosome, start=df$Start_Position,
     sym=df$Hugo_Symbol, stringsAsFactors=FALSE) # drop dist from origin
  gob = ggplot(data=df_new, aes(x=totalgd, y=ml10dbp, colour=Subst, text=chr, loc=start, text2=sym)) +
                  geom_point()
  gcode = ifelse(df$NCBI_Build[1]=="36", "hg18", "hg19")
  ndf = data.frame(cbounds=chrbounds_basic(genome=gcode))
  ndf$chrmid = midpts(ndf$cbounds)
  ndf$chr = c(1:22, "X", "Y")
  ndf = cbind(ndf, yloc=0)
  ans = gob + theme( panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank()) +
                         ylab(inylab) + xlab(inxlab)
  if (addbounds) ans = ans +
       geom_vline( aes(xintercept=cbounds, y=yloc), color="green", data=ndf )  +
       scale_x_continuous(expand=c(.01,.01))
  if (!missing(intitle)) ans = ans + ggtitle(intitle)
   else ans = ans + ggtitle(paste(studytag, id))
  if (cnames) ans = ans +
         geom_text(aes(x=chrmid,y=yloc,label=chr), data=ndf, inherit.aes=FALSE, size=3)
  ans
}


.rainfall.maeGRL.df = function(maemut, ind=1, colmap = kataColors(), ...) {
  # assumes we are using the TCGA Mutations component of
  # a multiassay experiment, this will be a GRangesList
  # we will get one "sample" selected by index ind
  #
  thecall = match.call()
  df = cbind( as.data.frame( unname(maemut[[ind]]) ),
                  metadata(maemut)[[ind]] )
  df = df[which(df$variant_type == "SNP"), ]
  df$Subst = with(df, subt(reference_allele, tumor_seq_allele1, tumor_seq_allele2) )
  df = df[ which(df$Subst %in% names(colmap)), ]
  if (genome(maemut)[1] == "36") lenbase = TxDb.Hsapiens.UCSC.hg18.knownGene::TxDb.Hsapiens.UCSC.hg18.knownGene
  if (genome(maemut)[1] == "37") lenbase = TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
  canonseqs = paste0("chr", c(1:22, "X", "Y"))
  slens = seqlengths(lenbase)[canonseqs]
  soff = c(0, cumsum(as.numeric(slens[-length(slens)])))
  names(soff) = canonseqs
  df$totalgd = df$end + soff[ as.character(df$seqnames) ]
  #badinds = which(df$end > (tl <- seqlengths(lenbase)[as.character(df$seqnames)]))
  #if (length(badinds)>0) df$end[badinds] = tl[badinds] # does trim
  #gr = trim(GRanges( as.character(df$seqnames), IRanges(df$start, df$end) ))
  #seqinfo(gr) = seqinfo(lenbase)[seqlevels(gr),]
  #df$totalgd = totalgd(gr)$totalgd
# put in natural genomic order
  sdf = split(df, as.character(df$seqnames))
  sdf = lapply(sdf, function(x) x[order(x$start),,drop=FALSE])
  nm = names(sdf)
  o = match(canonseqs, nm)
  do.call(rbind, sdf[o])
}

rainfall = function( mut, ind, type="maeGRL", colmap = kataColors(),
    legcex = 0.8, legy = 8.5, legxdenom = 3, 
    ymax = 9, inylab="-log10 dbp", inxlab="chromosome", intitle, dotrim=TRUE,
    addbounds=TRUE, cnames=TRUE) {
#
# deal with MultiAssayExperiment mutation Elist
#
  if (type=="maeGRL") {
      df = .rainfall.maeGRL.df( mut, ind = ind, colmap=colmap )
      gcode = ifelse(genome(mut)[1]=="36", "hg18", "hg19")
      if (missing(intitle)) intitle = names(mut)[[ind]]
   }
  di = c(log10(df$totalgd[1]/2), log10(diff(df$totalgd) + 1))
  df_new = data.frame(totalgd=df$totalgd, ml10dbp=di, Subst=df$Subst, chr=df$seqnames, start=df$start,
     sym=df$hugo_symbol, stringsAsFactors=FALSE) # drop dist from origin
  gob = ggplot(data=df_new, aes(x=totalgd, y=ml10dbp, colour=Subst, text=chr, loc=start, text2=sym)) + 
                  geom_point()
  ndf = data.frame(cbounds=chrbounds_basic(genome=gcode))
  ndf$chrmid = midpts(ndf$cbounds)
  ndf$chr = c(1:22, "X", "Y")
  ndf = cbind(ndf, yloc=0)
  ans = gob + theme( panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank()) +
                         ylab(inylab) + xlab(inxlab) 
  if (addbounds) ans = ans +
       geom_vline( aes(xintercept=cbounds, y=yloc), color="green", data=ndf )  +
       scale_x_continuous(expand=c(.01,.01))
  if (!is.null(intitle)) ans = ans + ggtitle(intitle) 
  if (cnames) ans = ans +
         geom_text(aes(x=chrmid,y=yloc,label=chr), data=ndf, inherit.aes=FALSE, size=3)
  ans
}
  
