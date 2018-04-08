#' provide bigrquery connection to pancancer Annotated datasets
#' @import bigrquery
#' @param dataset character(1) dataset name
#' @param billing character(1) Google cloud platform billing code; authentication will be attempted when using the resulting connection
#' @return BigQueryConnection instance
#' @examples
#' pancan_BQ
#' @export
pancan_BQ = function (dataset="Annotated", 
              billing=Sys.getenv("CGC_BILLING")) 
{
    con <- DBI::dbConnect(bigrquery::dbi_driver(), project = "pancancer-atlas", 
        dataset = dataset, billing = billing)
    con
}

#' give an interface to tablenames
#' @return interactive datatable from DT
#' @examples
#' if (interactive()) pancan_clinicalTabVarnames()
#' @export
pancan_clinicalTabVarnames = function() {
 if (!requireNamespace("DT")) stop("install DT package to use this function")
 DT::datatable(BiocOncoTK::pancan.clin.varnames)
}

#' tabulate a variable in a table
#' @param dataset character(1) dataset name
#' @param tblname character(1) table name in dataset
#' @param vblname character(1) field name in table
#' @return instance of tbl_dbi, constituting summarise result
#' @examples
#' if (interactive()) pancan_tabulate(tblname=
#'     "clinical_PANCAN_patient_with_followup", vblname="icd_10")
#' @export
pancan_tabulate = function(dataset="Annotated", tblname, vblname) {
 pancan_BQ(dataset=dataset) %>% tbl(tblname) %>% select_(vblname) %>%
   group_by_(vblname) %>% summarise(n=n())
}

#' provide a shiny app to 'glimpse' structure and content of pancan atlas
#' @import DT
#' @param dataset character(1) name of a BigQuery dataset in the pancan-atlas project
#' @param nrecs numeric(1) number of records to request (limited through the n= parameter to as.data.table
#' @return currently only as a side effect of starting app
#' @examples
#' if (interactive()) pancan_app()
#' @export
pancan_app = function(dataset="Annotated", nrecs=5) {
 tbls = pancan_BQ(dataset=dataset) %>% dbListTables()
 if (dataset=="Annotated") tbls = BiocOncoTK::annotTabs
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText(paste("BiocOncoTK pancan_app provides high-level view of",
      "tables and records in the BigQuery pancan-atlas project of",
      "April 2018.")),
    helpText("See ", a(href="http://isb-cancer-genomics-cloud.readthedocs.io/en/latest/sections/PanCancer-Atlas-Mirror.html", "the ISB documentation on this project"), "for more details on the underlying data."),
    helpText(paste("Tab 'recs' presents a small number of records",
      "from the selected table; tab 'fullnames' shows the internal",
      "name of the table, which includes some relevant metadata.")),
    helpText("Tab allvbls is a searchable list of table fields"),
    selectInput("table", "Select a table", tbls),
    width=2
   ),
   mainPanel(
    tabsetPanel(
     tabPanel("recs.", 
      tableOutput("chk")
     ),
     tabPanel("fullnames", 
      tableOutput("fullnames")
     ),
     tabPanel("allvbls", 
      dataTableOutput("allvbls")
     )
    )
   )
  )
 )
 server = function(input, output) {
  gettab = reactive({
   pancan_BQ(dataset=dataset) %>% tbl(input$table) %>% as.data.frame(n=nrecs)
  })
  output$chk = renderTable({
   gettab()
  })
  output$allvbls = renderDataTable({
   data.frame(vbls=names(gettab()))
  })
  output$fullnames = renderTable({
   data.frame(short=names(tbls), fullnames=tbls)
  })
 }
 shinyApp(ui=ui, server=server)
} 
