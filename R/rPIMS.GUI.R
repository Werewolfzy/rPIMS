#' @export
#' @import shiny
#' @import shinythemes
#' @import shinyjs
#' @import DT
#' @import shinyWidgets
#' @import colourpicker
#' @import RColorBrewer
#' @import ggplot2
#' @import data.table
#' @import sommer
#' @import ggtree
#' @import ape
#' @import pbapply
#' @import parallel
#' @import infotheo
#' @import caret
#' @import class
#' @import randomForest
#' @import xgboost
#' @import ranger
#' @import ROCR
#' @import smacof
#' @import umap
#' @import phangorn
#' @import shinyalert
#' @import leaflet
#' @import e1071
#' @import kernlab
#' @import LEA

rPIMS.GUI <- function() {
  suppressPackageStartupMessages({
    library(shiny)
    library(shinythemes)
    library(shinyjs)
    library(DT)
    library(shinyWidgets)
    library(colourpicker)
    library(RColorBrewer)
    library(ggplot2)
    library(data.table)
    library(sommer)
    library(ggtree)
    library(ape)
    library(pbapply)
    library(parallel)
    library(infotheo)
    library(caret)
    library(class)
    library(randomForest)
    library(xgboost)
    library(ranger)
    library(ROCR)
    library(smacof)
    library(umap)
    library(phangorn)
    library(shinyalert)
    library(leaflet)
    library(e1071)
    library(kernlab)
    library(LEA)
  })

  shinyEnv <- new.env()

  source("R/data.R", local = shinyEnv)
  source("R/pca.R", local = shinyEnv)
  source("R/PhyloTree.R", local = shinyEnv)
  source("R/PredNewInd.R", local = shinyEnv)
  source("R/TrainModel.R", local = shinyEnv)
  source("R/Structure.R", local = shinyEnv)

  ui <- fluidPage(
    useShinyjs(),
    tags$head(
      tags$script(HTML("
        $(document).on('click', '.navbar-brand', function() {
          $('#navbar').find('a[data-value=\"WELCOME\"]').tab('show');
        });
      "))
    ),

    navbarPage(
      "rPIMS",
      theme = shinythemes::shinytheme("flatly"),
      id = "navbar",

      tabPanel("WELCOME",
               tags$div(style = "text-align: center;",
                        h1("Welcome Page"),
                        tags$br()
               ),
               tags$div(style = "margin-left: 50px; margin-right: 50px;",
                        p("The genetic diversity within livestock breeds constitutes an invaluable genetic resource. However, numerous breeds are currently endangered, facing threats such as emerging diseases, climate change, and competition from international commercial breeds. Accurate breed identification is essential to implement effective conservation strategies and mitigate the risk of extinction."),
                        p("The *rPIMS* package was developed with the goal of equipping breed conservationists with a comprehensive, user-friendly toolkit to rapidly and accurately build breed identification models using a variety of machine learning approaches. This package accepts genomic data as well as simple tabular text files, integrating a wide array of analysis modules to ensure thorough and precise breed identification."),
                        p("The key modules of *rPIMS* include:"),
                        p("1. **Data Input Module (DATA):** Facilitates the import of genomic data, breed-related information, and geographic sampling details."),
                        p("2. **Dimensionality Reduction Module (DimRed):** Offers tools for dimensionality reduction, enabling the visualization and interpretation of complex datasets."),
                        p("3. **Phylogenetic Tree Construction Module (PhyloTree):** Constructs evolutionary trees, providing insights into the genetic relationships among different breeds."),
                        p("4. **Population Structure Analysis Module (Structure):** Uncovers genetic structure and historical patterns of population stratification."),
                        p("5. **Machine Learning Model Training Module (TrainModel):** Implements various machine learning algorithms to train effective breed classification models, generating molecular identity cards for specific breeds."),
                        p("6. **Model Application Module (PredNewind):** Enables the application of trained models for precise breed identification."),
                        p("The *rPIMS* package was developed and is currently maintained by Dr. Yuhetian Zhao of the Institute of Animal Science, Chinese Academy of Agricultural Sciences. For any questions or suggestions for further improvement, please contact Dr. Zhao at zyuhetian@163.com.")
               ),
               id = "welcome_tab"
      ),


      shinyEnv$data_ui(),
      shinyEnv$pca_ui(),
      shinyEnv$PhyloTree_ui(),
      shinyEnv$Structure_ui(),
      shinyEnv$TrainModel_ui(),
      shinyEnv$PredNewInd_ui(),
    )
  )

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 100*1024^3)
    rvdataclass <- reactiveValues(classification_data = NULL)
    rvdatageno <- reactiveValues(genotype_data = NULL)
    rvdatacloca <- reactiveValues(location_data = NULL)
    rvpcaresultdata <- reactiveValues(pcaresult_data = NULL)
    rvtreeresultdata <- reactiveValues(treeresult_data = NULL)
    rvstructureresultdata <- reactiveValues(treeresult_data = NULL)
    shinyEnv$data_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca)
    shinyEnv$pca_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata)
    shinyEnv$PhyloTree_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata)
    shinyEnv$Structure_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata)
    shinyEnv$TrainModel_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata)
    shinyEnv$PredNewInd_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata)

  }
  shinyApp(ui = ui, server = server)
}

