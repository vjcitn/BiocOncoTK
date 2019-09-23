

.flexbi = function(prcomp_output, which=c(1,2), nvar=5, shr=.6, ...) {
  proj = prcomp_output$x[,which]
  rot = prcomp_output$rot[,which]
  sss = function(x) sum(x^2)
  lens = apply(rot,1,sss)
  kprot = rot[order(lens,decreasing=TRUE)[1:nvar],]
  sca = max(abs(proj))
  fac = sca/max(abs(kprot[,1]))
  pcs = paste0("PC", which)
print(pcs)
  bas = ggplot(data.frame(proj),aes_string(pcs[1],pcs[2])) + geom_point(alpha=.4)
  df2 = data.frame(v1=shr*fac*kprot[,1],v2=shr*fac*kprot[,2])
  df2$tag = rownames(kprot)
  bas+geom_text(data=df2,aes(x=v1,y=v2,label=tag)) + 
    geom_segment(data=df2, aes(x=0, y=0, xend=v1, 
                 yend=v2), colour=muted("red"), alpha=.4,
                 arrow=arrow(length=unit(.1,"inches")))
}

flexbi = function(tcode, indata, which=c(1,2), nvar=5, shr=.6, ...) {
 pp = prc(tcode, indata=indata)
 .flexbi(pp$prcomp,indata=indata, which=which, nvar=nvar, shr=shr, ...)
}


#' log10(x+p) transformation for use with scales/ggplot2
#' @importFrom scales trans_new
#' @importFrom scales muted
#' @param p value of shift before taking log10
#' @return an instance of custom trans() for scales package
#' @export
log10pl1 = function (p=1) {
    trans <- function(x) {
        log10(x+p)
     }
    inv <- function(x) {
        (10^(x))-p
    }
    trans_new("log10pl1", trans, inv)
}

prc = function(inacro="STAD", indata) {
 nd = names(indata)
print(nd)
 stopifnot(all(c("patient_barcode", "symbol", "msival", "acronym") %in% nd))
 dd = reshape2::dcast(indata, formula=patient_barcode~symbol, subset=plyr::.(acronym==inacro), value.var="log2ex") 
 mx3 = indata[-which(duplicated(indata$patient_barcode)),]
 ee = right_join(mx3[,c("patient_barcode", "msival")],dd)
 rr = data.matrix(ee[,-c(1,2)])
 list(prcomp=prcomp(rr), msival=ee[,2])
}

#' visualize aspects of MSIsensor/expression relationships
#' @import ggplot2
#' @importFrom dplyr filter
#' @import magrittr
#' @importFrom ggpubr ggarrange 
#' @importFrom car avPlots
#' @param tum a TCGA tumor code
#' @param gene a gene symbol used in the indata data.frame
#' @param intrans an instance of the trans() transformation method of scales package
#' @param inmeth a valid setting for method parameter for geom_smooth
#' @param topmsi maximum numeric value for x-axis when plotting against MSI value
#' @param indata a data.frame instance with values for acronym, gene, msival
#' @param nvar numeric() number of variables to show in biplot
#' @export
multiviz = function (tum = "MESO", gene = "TYMS", intrans = log10pl1(p = 1), 
    inmeth = "auto", topmsi = Inf, indata, nvar=6) 
{
    requireNamespace("dplyr")
    requireNamespace("magrittr")
    g1 = ggbox(tum, gene, indata=indata)
    g1 = ggplot(indata %>% filter(acronym==tum & gene==gene), aes(x=msival)) + geom_density(bw=.1) + scale_x_continuous(trans=log10pl1())
    g2 = ggscat(tum, gene, intrans = intrans, inmeth = inmeth, 
        topmsi = topmsi, indata=indata)
    g3 = flexbi(tcode=tum, indata=indata, nvar=nvar)
    g4 = flexbi(tcode=tum, indata=indata, nvar=nvar, which=c(2,3))
    ggpubr::ggarrange(g1,g2,g3,g4,nrow=2, ncol=2)
}
ggbox = function (tumor, gene, titlepref = "", indata) {
  ggplot(indata %>% filter(acronym == tumor & symbol == gene), aes(y = log2ex, 
    x = msicode)) + geom_boxplot() + ggtitle(paste0(titlepref, 
    tumor)) + ylab(gene) + xlab("MSIsensor category")
}
ggscat = function (tumor, gene, titlepref = "", intrans = log10pl1(p = 1), 
    inmeth = "auto", topmsi = Inf, indata) 
ggplot(indata %>% filter(acronym == tumor & symbol == gene & msival < 
    topmsi), aes(y = log2ex, x = msival)) + geom_hex() + ggtitle(paste0(titlepref, 
    tumor)) + ylab(gene) + scale_x_continuous(trans = intrans) + 
    geom_smooth(method = inmeth) + xlab("MSIsensor score")

ggscat_av = function (tumor, gene, titlepref = "", intrans = log10pl1(p = 1), 
    inmeth = "auto", topmsi = Inf, indata)  {
 dat = indata %>% filter(acronym == tumor & symbol == gene & msival < topmsi)
 nplates = length(unique(dat$plate))
 if (nplates==1) return(ggscat(tumor, gene, titlepref, intrans, inmeth, topmsi, indata))
 m1 = lm(log2ex~log(msival + 1)+factor(plate), data=dat)
 png(file=tempfile())
 zz = car::avPlots(m1, terms="log(msival + 1)")  
 dev.off()
 md = data.frame(tmsi=zz[[1]][,1],log2exa=zz[[1]][,2])
 ggplot(md, aes(x=tmsi,y=log2exa)) + geom_hex() + geom_abline(slope=m1$coef[2], intercept=0)
}
 

#multiviz(indata=ex21_t33_wplate, inmeth=MASS::rlm)

#' small app to survey MSIsensor against expression
#' @rawNamespace import("shiny", except=c("dataTableOutput", "renderDataTable"))
#' @param df a data.frame instance
#' @param inmeth a method for geom_smooth
#' @param nvar number of variables to show in biplot
#' @note Use ask=FALSE if running example.
#' @examples
#' if (interactive()) viz_msi_raw(BiocOncoTK::small_msi, nvar=3)
#' @export
viz_msi_raw = function(df, inmeth=MASS::rlm, nvar=3) {
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    selectInput("tumor", "tumor", choices=sort(unique(df$acronym))),
    selectInput("gene", "gene", choices=sort(unique(df$symbol)))
    ),
   mainPanel(
    plotOutput("abc")
   )
  )
 )
 server = function(input, output) {
  output$abc = renderPlot(
  multiviz(indata=df, inmeth=inmeth, tum=input$tumor,
    gene=input$gene, nvar=nvar))
 }
 runApp(list(ui=ui, server=server))
}

#viz_msi_raw(small_msi, nvar=3)
