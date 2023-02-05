
# report_patients_by_txg
# defines task to perform on subordinate scatter
# (included in scatter over genes)

workflow report_patients_by_txg {
  Array[String] tumors
  String gene
  scatter (t in tumors) {
    call onetum { input: tum1 = t, g1=gene }
   }
  output {
    Array[File] csvs = onetum.csv_per_tumor_per_gene
  }
}

task onetum {
  String tum1 
  String g1
  File scr
  command {
    Rscript  ${scr}  --tumor=${tum1} --gene=${g1}
  }
  output {
    File csv_per_tumor_per_gene = "${tum1}_${g1}.csv"
  }
  runtime {
    continueOnReturnCode: true
    docker: "vjcitn/vjconco:3.16.0"
    disks: "local-disk 40 HDD"
    bootDiskSizeGb: 50
    memory: "24G"
    maxRetries: 3
    }   
}

