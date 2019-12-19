acronym_to_system = function(accs) {
   data(map_tcga_ncit)
   otree = ontoProc::getOncotreeOnto()
   controlindex = which(map_tcga_ncit[,1]=="CNTL")
   tcgacodes = map_tcga_ncit[-controlindex,1]
   ncitsites = map_tcga_ncit[-controlindex,3]
   ssi = strsplit(ncitsites, "\\|")
   sites = sapply(ssi, "[", 1)
   simpmap = data.frame(code=tcgacodes, oncotr_site=otree$name[sites], ncit=sites,
     stringsAsFactors=FALSE)
   simpmap[sample(seq_len(nrow(simpmap)),5),]
   poss_sys = otree$children["NCIT:C3263"][[1]] # all possible systems
   allanc = otree$ancestors[simpmap$ncit]
   specific = sapply(allanc, function(x) intersect(x, poss_sys)[1]) # ignore multiplicities
   sys = unlist(otree$name[specific])
   mapper = cbind(simpmap, sys=sys)
   co = mapper$code
   sys = mapper$sys
   names(sys) = co
   sys[accs]
}
