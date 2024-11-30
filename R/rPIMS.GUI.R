#' @export
#' @import shiny
#' @import shinythemes
#' @import shinyWidgets
#' @import RColorBrewer
#' @import ggplot2
#' @import data.table
#' @import sommer
#' @import ape
#' @import pbapply
#' @import parallel
#' @import infotheo
#' @import caret
#' @import class
#' @import xgboost
#' @import ROCR
#' @import smacof
#' @import umap
#' @import phangorn
#' @import leaflet
#' @import e1071
#' @importFrom randomForest classCenter combine getTree grow importance MDSplot na.roughfix outlier partialPlot randomForest rfcv rfImpute rfNews treesize tuneRF varImpPlot varUsed
#' @importFrom kernlab alphaindex anovadot as.kernelMatrix Asymbound AsympH0 b besseldot buffer centers coef convergence couple cross csi diagresidues dual edgegraph eig error fitted gausspr H0 how inchol inlearn ipop kcall kcca kcor kernelf kernelFast kernelMatrix kernelMult kernelPol kfa kha kkmeans kmmd kpar kpca kqr ksvm laplacedot lev lssvm maxresiduals mlike mmdstats nSV nvar obj onlearn param pcv pivots plot polydot predgain predict primal prior prob.model R Radbound ranking rbfdot rho rotated RVindex rvm scaling sigest size specc splinedot stringdot SVindex tanhdot truegain type vanilladot withinss xcoef xmatrix ycoef ymatrix
#' @importFrom shinyjs addClass addCssClass click delay disable disabled enable extendShinyjs hidden hide hideElement html info inlineCSS logjs onclick onevent refresh removeClass removeCssClass removeEvent reset runcodeServer runcodeUI runjs showElement showLog toggle toggleClass toggleCssClass toggleElement toggleState useShinyjs
#' @importFrom colourpicker colourInput colourPicker colourWidget plotHelper updateColourInput
#' @importFrom DT %>% addRow clearSearch coerceValue colReorder datatable dataTableAjax dataTableProxy doColumnSearch doGlobalSearch DTOutput editData formatCurrency formatDate formatPercentage formatRound formatSignif formatString formatStyle hideCols JS reloadData renderDT replaceData saveWidget selectCells selectColumns selectPage selectRows showCols styleColorBar styleEqual styleInterval styleRow styleValue tableFooter tableHeader updateCaption updateFilters updateSearch
#' @importFrom shinyalert closeAlert shinyalert useShinyalert
#' @importFrom ranger csrf deforest getTerminalNodeIDs holdoutRF importance_pvalues predictions ranger timepoints treeInfo
#' @importFrom ggtree %+>% %<% %<+% %>% add_colorbar aes arrow as.polytomy collapse Date2decimal decimal2Date expand facet_data facet_labeller facet_plot facet_widths flip fortify geom_aline geom_balance geom_cladelab geom_cladelabel geom_cladelabel2 geom_facet geom_highlight geom_hilight geom_inset geom_label geom_label2 geom_motif geom_nodelab geom_nodelab2 geom_nodepoint geom_point geom_point2 geom_range geom_rootedge geom_rootpoint geom_segment2 geom_strip geom_striplab geom_taxalink geom_text geom_text2 geom_tiplab geom_tiplab2 geom_tippoint geom_tree geom_tree2 geom_treescale geom_zoom_clade get.path get_clade_position get_heatmap_column_position get_taxa_name ggdensitree ggexpand ggplot ggsave ggtree gheatmap groupClade groupOTU guide_legend gzoom hexpand identify inset label_pad layout_circular layout_dendrogram layout_fan layout_inward_circular layout_rectangular MRCA msaplot multiplot nodebar nodeid nodelab nodepie open_tree plot_list range_format read.tree revts rotate_tree rtree scale_color scale_color_manual scale_color_subtree scale_colour_manual scale_colour_subtree scale_fill_manual scale_x_continuous scale_x_ggtree scale_x_range scaleClade set_hilight_legend td_filter td_mutate td_unnest theme theme_dendrogram theme_inset theme_tree theme_tree2 unit vexpand viewClade xlim xlim_expand xlim_tree zoomClade
#' @importFrom LEA ancestrymap2geno ancestrymap2lfmm barchart combine.lfmmProject combine.snmfProject create.dataset cross.entropy cross.entropy.estimation export.lfmmProject export.pcaProject export.snmfProject G genetic.gap genetic.offset geno2lfmm import.lfmmProject import.pcaProject import.snmfProject lfmm lfmm.pvalues lfmm2 lfmm2.test lfmm2geno load.lfmmProject load.pcaProject load.snmfProject pca ped2geno ped2lfmm plot Q read.env read.geno read.lfmm read.zscore remove.lfmmProject remove.pcaProject remove.snmfProject show snmf snmf.pvalues struct2geno summary tracy.widom vcf2geno vcf2lfmm write.env write.geno write.lfmm z.scores

rPIMS.GUI <- function() {

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


      data_ui(),
      pca_ui(),
      PhyloTree_ui(),
      Structure_ui(),
      TrainModel_ui(),
      PredNewInd_ui(),
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
    data_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca)
    pca_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata)
    PhyloTree_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata)
    Structure_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata)
    TrainModel_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata)
    PredNewInd_server(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata)

  }
  shinyApp(ui = ui, server = server)
}

