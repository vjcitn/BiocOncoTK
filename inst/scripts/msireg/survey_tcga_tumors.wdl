import "https://raw.githubusercontent.com/vjcitn/BiocOncoTK/master/inst/scripts/msireg/report_patients_by_txg.wdl" as sub

# survey_tcga_tumors.wdl
# this is an approach to programming nested scatter operations with cromwell
# 

task collect_over_tumors {
  Array[File] infiles
  File aggscr = "gs://vjc_scripts/rbind_csvs_to_RDS.R"
  command {
   Rscript ${aggscr} demo.rds ${sep=' ' infiles}
  }
  output {
   File rdsbytum = "demo.rds"
   }
  runtime {
    continueOnReturnCode: true
    docker: "vjcitn/vjconco:v3"
    disks: "local-disk 40 HDD"
    bootDiskSizeGb: 50
    memory: "24G"
    }   
 }

task collect_over_genes {
  Array[File] inrds
  File concscr = "gs://vjc_scripts/rbind_RDS_to_RDS.R"
  command {
   Rscript ${concscr} final.rds ${sep=' ' inrds}
  }
 output {
  File concatDF = "final.rds"
  }
  runtime {
    continueOnReturnCode: true
    docker: "vjcitn/vjconco:v3"
    disks: "local-disk 40 HDD"
    bootDiskSizeGb: 50
    memory: "24G"
    }   
}

workflow survey_tumors_within_genes {
  Array[String] genes # gets binding from 'inputs' json
  Array[String] tumors 
  scatter (g in genes) {
   call sub.report_patients_by_txg {
    input: gene = g, tumors=tumors
    }
  }
  scatter (f in report_patients_by_txg.csvs) {
    call collect_over_tumors { input: infiles = f }
   }
  call collect_over_genes { input: inrds = collect_over_tumors.rdsbytum }
  output {
   Array[Array[File]] allout = report_patients_by_txg.csvs
  }
}
