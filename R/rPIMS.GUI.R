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
#' @import pwr
#' @importFrom FNN knnx.dist KL.dist knn.index KLx.divergence knnx.index mutinfo KL.divergence KLx.dist knn.dist ownn get.knnx crossentropy knn.reg get.knn
#' @importFrom randomForest classCenter combine getTree grow importance MDSplot na.roughfix outlier partialPlot randomForest rfcv rfImpute rfNews treesize tuneRF varImpPlot varUsed
#' @importFrom kernlab alphaindex anovadot as.kernelMatrix Asymbound AsympH0 b besseldot buffer centers coef convergence couple cross csi diagresidues dual edgegraph eig error fitted gausspr H0 how inchol inlearn ipop kcall kcca kcor kernelf kernelFast kernelMatrix kernelMult kernelPol kfa kha kkmeans kmmd kpar kpca kqr ksvm laplacedot lev lssvm maxresiduals mlike mmdstats nSV nvar obj onlearn param pcv pivots plot polydot predgain predict primal prior prob.model R Radbound ranking rbfdot rho rotated RVindex rvm scaling sigest size specc splinedot stringdot SVindex tanhdot truegain type vanilladot withinss xcoef xmatrix ycoef ymatrix
#' @importFrom shinyjs addClass addCssClass click delay disable disabled enable extendShinyjs hidden hide hideElement html info inlineCSS logjs onclick onevent refresh removeClass removeCssClass removeEvent reset runcodeServer runcodeUI runjs showElement showLog toggle toggleClass toggleCssClass toggleElement toggleState useShinyjs
#' @importFrom colourpicker colourInput colourPicker colourWidget plotHelper updateColourInput
#' @importFrom DT %>% addRow clearSearch coerceValue colReorder datatable dataTableAjax dataTableProxy doColumnSearch doGlobalSearch DTOutput editData formatCurrency formatDate formatPercentage formatRound formatSignif formatString formatStyle hideCols JS reloadData renderDT replaceData saveWidget selectCells selectColumns selectPage selectRows showCols styleColorBar styleEqual styleInterval styleRow styleValue tableFooter tableHeader updateCaption updateFilters updateSearch
#' @importFrom shinyalert closeAlert shinyalert useShinyalert
#' @importFrom ranger csrf deforest getTerminalNodeIDs holdoutRF importance_pvalues predictions ranger timepoints treeInfo
#' @importFrom ggtree %+>% %<% %<+% %>% add_colorbar aes arrow as.polytomy collapse Date2decimal decimal2Date expand facet_data facet_labeller facet_plot facet_widths flip fortify geom_aline geom_balance geom_cladelab geom_cladelabel geom_cladelabel2 geom_facet geom_highlight geom_hilight geom_inset geom_label geom_label2 geom_motif geom_nodelab geom_nodelab2 geom_nodepoint geom_point geom_point2 geom_range geom_rootedge geom_rootpoint geom_segment2 geom_strip geom_striplab geom_taxalink geom_text geom_text2 geom_tiplab geom_tiplab2 geom_tippoint geom_tree geom_tree2 geom_treescale geom_zoom_clade get.path get_clade_position get_heatmap_column_position get_taxa_name ggdensitree ggexpand ggplot ggsave ggtree gheatmap groupClade groupOTU guide_legend gzoom hexpand identify inset label_pad layout_circular layout_dendrogram layout_fan layout_inward_circular layout_rectangular MRCA msaplot multiplot nodebar nodeid nodelab nodepie open_tree plot_list range_format read.tree revts rotate_tree rtree scale_color scale_color_manual scale_color_subtree scale_colour_manual scale_colour_subtree scale_fill_manual scale_x_continuous scale_x_ggtree scale_x_range scaleClade set_hilight_legend td_filter td_mutate td_unnest theme theme_dendrogram theme_inset theme_tree theme_tree2 unit vexpand viewClade xlim xlim_expand xlim_tree zoomClade
#' @importFrom LEA ancestrymap2geno ancestrymap2lfmm barchart combine.lfmmProject combine.snmfProject create.dataset cross.entropy cross.entropy.estimation export.lfmmProject export.pcaProject export.snmfProject G genetic.gap genetic.offset geno2lfmm import.lfmmProject import.pcaProject import.snmfProject lfmm lfmm.pvalues lfmm2 lfmm2.test lfmm2geno load.lfmmProject load.pcaProject load.snmfProject pca ped2geno ped2lfmm plot Q read.env read.geno read.lfmm read.zscore remove.lfmmProject remove.pcaProject remove.snmfProject show snmf snmf.pvalues struct2geno summary tracy.widom vcf2geno vcf2lfmm write.env write.geno write.lfmm z.scores
#' @importFrom dplyr slice_max expr src db_desc group_by_drop_default db_commit coalesce data_frame summarize_if mutate_ db_create_index join_by check_dbplyr src_tbls db_query_rows dense_rank slice_head tbl last_col consecutive_id arrange dplyr_row_slice sql_semi_join is.grouped_df summarise_each group_vars bind_cols last_dplyr_warnings distinct_at mutate_all relocate compute mutate_if distinct_ group_indices_ eval_tbls2 rename_all count do_ do cumany wrap_dbplyr_obj recode_factor same_src top_frac select_vars desc db_create_indexes lead mutate_at as_tibble distinct_prepare src_sqlite distinct_if add_row .data cur_column semi_join filter_if sql_translate_env arrange_ case_match sql_set_op eval_tbls ensyms arrange_at add_count dplyr_reconstruct slice_min new_rowwise_df cross_join setequal select distinct_all src_local as.tbl summarise type_sum db_drop_table rows_patch cur_data row_number groups anti_join tbl_vars cur_group_id auto_copy sql_subquery group_indices progress_estimated transmute_all all_of new_grouped_df add_tally_ enquo group_data contains rows_insert all_equal group_split tbl_ptype glimpse inner_join db_insert_into db_save_query id summarise_all ensym db_begin left_join everything summarize_each_ dplyr_col_modify compare_tbls2 filter enquos union_all summarize_all slice_tail n quo add_count_ summarize_each show_query group_cols summarise_at n_distinct transmute_at rename reframe select_vars_ near pick num_range sym n_groups across all_vars select_all na_if select_if grouped_df tbl_df tally_ is.src intersect db_create_table %>% mutate_each group_keys add_rownames rename_vars select_at is.tbl arrange_all summarize quo_name cummean rename_with group_map current_vars with_groups syms rowwise group_size union nth src_postgres enexprs dim_desc transmute any_of transmute_if case_when arrange_if order_by with_order group_rows any_vars db_analyze db_data_type as_data_frame top_n add_tally enexpr db_query_fields tribble group_trim select_ lag mutate_each_ percent_rank distinct as_label group_by_prepare db_explain summarise_each_ tbl_nongroup_vars bench_tbls is_grouped_df right_join summarise_if filter_ cur_group_rows src_mysql summarize_at lst rows_delete ntile location symdiff sql_escape_string rename_vars_ nest_by db_rollback quos filter_at collapse db_write_table db_has_table starts_with ungroup group_by funs vars if_all tibble slice_sample recode validate_rowwise_df rename_ cur_group group_nest filter_all one_of src_df summarise_ group_by_ copy_to ends_with cur_data_all if_else setdiff transmute_ funs_ summarize_ failwith compare_tbls sample_frac matches group_by_all explain rename_if sample_n group_by_if validate_grouped_df common_by group_walk changes make_tbl rows_upsert cume_dist ident rows_append rows_update select_var db_list_tables rename_at sql_escape_ident group_by_at pull mutate nest_join slice_ count_ group_modify if_any tally bind_rows c_across cumall collect min_rank sql sql_select sql_join full_join

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

