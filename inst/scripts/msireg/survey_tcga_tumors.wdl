import "https://raw.githubusercontent.com/vjcitn/msiboxes/master/simp4.wdl" as sub

# survey_tcga_tumors.wdl
# this is an approach to programming nested scatter operations with cromwell
# 

task agt {
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

task agg {
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

workflow genes {
  Array[String] genes # gets binding from 'inputs' json
  Array[String] tumors 
  scatter (g in genes) {
   call sub.tumors {
    input: gene = g, tumors=tumors
    }
  }
  scatter (f in files_per_tumor {
    call agt { input: infiles = f }
   }
  call agg { input: inrds = agt.rdsbytum }
  output {
   Array[Array[File]] allout = files_per_tumor
  }
}
