PhyloTree_ui <- function() {
  tabPanel("PhyloTree",
           fluidRow(
             column(width = 4,
                    tags$head(
                      tags$style(HTML("
  .selectize-dropdown-content .option[data-value='RdYlBu'] { background: linear-gradient(to right, #215fbb, #99d594, #e8c1a0, #f7fcb9); }
  .selectize-dropdown-content .option[data-value='Spectral'] { background: linear-gradient(to right, #9e0142, #d53e4f, #f46d43, #fdae61, #fee08b, #e6f598, #abdda4, #66c2a5, #3288bd, #5e4fa2); }
  .selectize-dropdown-content .option[data-value='Paired'] { background: linear-gradient(to right, #a6cee3, #1f78b4, #b2df8a, #33a02c, #fb9a99, #e31a1c, #fdbf6f, #ff7f00, #cab2d6, #6a3d9a); }
  .selectize-dropdown-content .option[data-value='Set3'] { background: linear-gradient(to right, #8dd3c7, #ffffb3, #bebada, #fb8072, #80b1d3, #fdb462, #b3de69, #fccde5, #d9d9d9, #bc80bd, #ccebc5, #ffed6f); }
  .selectize-dropdown-content .option[data-value='Dark2'] { background: linear-gradient(to right, #1b9e77, #d95f02, #7570b3, #e7298a, #66a61e, #e6ab02, #a6761d, #666666); }
  .selectize-dropdown-content .option[data-value='Set1'] { background: linear-gradient(to right, #e41a1c, #377eb8, #4daf4a, #984ea3, #ff7f00, #ffff33, #a65628, #f781bf, #999999); }
  .selectize-input { height: 2.38em; line-height: 2.38em; }
  .selectize-dropdown { top: 2.38em; }
"))
                    ),
                    tags$div(
                      style = "background-color: #f2f2f2; padding: 10px; border-radius: 10px; box-shadow: none;",
                      div(style = "display: flex; align-items: center;",
                          selectInput("data_type_PhyloTree", "Select Data", choices = c("Please select..." = "", "Raw Data" = "Raw Data", "PCA Data" = "PCA Data")),
                          actionButton("refresh_PhyloTree", label = icon("refresh"), style = "background-color: transparent; border: none; color: #337ab7; margin-left: 10px;")
                      ),
                      uiOutput("classification_data_status_PhyloTree"),
                      selectInput("color_palette_PhyloTree", "Select Color Palette",
                                  choices = c("Please select..." = "",
                                              "RdYlBu" = "RdYlBu",
                                              "Spectral" = "Spectral",
                                              "Paired" = "Paired",
                                              "Set3" = "Set3",
                                              "Dark2" = "Dark2",
                                              "Set1" = "Set1")),
                      uiOutput("color_palette_status_PhyloTree"),
                      pickerInput(
                        inputId = "selected_breeds_PhyloTree",
                        label = "Select Breeds",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")
                      ),
                      selectInput("tree_construction_selector", "Select Phylogenetic Tree Construction Method",
                                  choices = c("Please select..." = "",
                                              "Neighbor-Joining" = "Neighbor-Joining",
                                              "FastME" = "FastME",
                                              "BioNJ" = "BioNJ",
                                              "UPGMA" = "UPGMA"),
                                  selected = ""),
                      selectInput("select_palette_PhyloTree", "Select Tree Palette",
                                  choices = c("Please select..." = "",
                                              "rectangular_tree" = "rectangular_tree",
                                              "slanted_tree" = "slanted_tree",
                                              "circular_tree" = "circular_tree",
                                              "radial_tree" = "radial_tree"),
                                  selected = "rectangular_tree"),
                      selectInput("perform_bootstrap", "Do you want to perform Bootstrap computation?",
                                  choices = c("Please select..." = "",
                                              "Yes" = "yes",
                                              "No" = "no"),
                                  selected = "no"),
                      actionButton("calculate_PhyloTree", "Calculate", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                    ),
                    tags$div(
                      style = "background-color: #f2f2f2; padding: 10px; border-radius: 10px; box-shadow: none; margin-top: 20px;",
                      div(
                        pickerInput("image_format_PhyloTree", "Select Image Format",
                                    choices = c("PDF", "TIFF", "PNG", "JPEG"),
                                    selected = c("PDF", "TIFF", "PNG", "JPEG"),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE)),
                        downloadButton("downloadZip_PhyloTree", "Download ZIP", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                      )
                    )
             ),
             useShinyjs(),
             column(width = 8,
                    uiOutput("conditionalUI_PhyloTree"),
                    plotOutput("plot_PhyloTree"),
                    div(id = "radioButtonsContainer", style = "position: relative;",
                        column(width = 4, class = "text-center",
                               actionButton("recalculate_PhyloTree", "ReCalculate", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;"),
                               div(id = "cover_PhyloTree", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 4, class = "text-center",
                               downloadButton("downloadTable_PhyloTree", "DownloadTable", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;"),
                               div(id = "cover_PhyloTree3", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 3,
                               radioButtons("data_view_PhyloTree", label = "Display:",
                                            choices = c("Head" = "head", "All" = "all"),
                                            selected = "all"),
                               div(id = "cover_PhyloTree2", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        div(id = "cover_PhyloTree", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")
                    ),
                    DTOutput("ResultsTable_PhyloTree")
             )
           )
  )
}
PhyloTree_server <- function(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata) {
  values <- reactiveValues(showText = TRUE, showDataViewOptions = FALSE)
  recalculateresult <- reactiveValues(recalculate1 = NULL)
  rv_PhyloTree <- reactiveValues(breed1 = NULL, global_color_assigned = FALSE)
  rvdown_PhyloTree <- reactiveValues(plot_PhyloTree = NULL)
  rvResults_PhyloTree <- reactiveValues(data = NULL)
  rvpalette_PhyloTree <- reactiveValues(selected_palette = NULL)
  observeEvent(input$refresh_PhyloTree, {
    updateSelectInput(session, "data_type_PhyloTree", selected = "")
    if (!rv_PhyloTree$global_color_assigned) {
      updateSelectInput(session, "color_palette_PhyloTree", selected = "RdYlBu")
    }
    rv_PhyloTree$global_color_assigned <- FALSE
  })
  observeEvent(input$data_type_PhyloTree, {
    output$classification_data_status_PhyloTree <- renderUI({})
    if (input$data_type_PhyloTree == "Raw Data") {
      if (!is.null(rvdataclass$classification_data) && nrow(rvdataclass$classification_data) > 0) {
        class1_PhyloTree <- rvdataclass$classification_data
        rv_PhyloTree$breed1 <- data.frame(breed = unique(class1_PhyloTree$breed))
        breedcount <- nrow(rv_PhyloTree$breed1)
        observeEvent(input$color_palette_PhyloTree, {
          if (!is.null(input$color_palette_PhyloTree)) {
            max_colors <- brewer.pal.info[input$color_palette_PhyloTree, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette_PhyloTree)
            colors <- colorRampPalette(palette)(breedcount)
            rv_PhyloTree$breed1$color <- colors
            rv_PhyloTree$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status_PhyloTree <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    } else if (input$data_type_PhyloTree == "PCA Data") {
      if (!is.null(rvpcaresultdata$pcaresult_data) && nrow(rvpcaresultdata$pcaresult_data) > 0) {
        class1_PhyloTree <- rvdataclass$classification_data
        rv_PhyloTree$breed1 <- data.frame(breed = unique(class1_PhyloTree$breed))
        breedcount <- nrow(rv_PhyloTree$breed1)
        observeEvent(input$color_palette_PhyloTree, {
          if (!is.null(input$color_palette_PhyloTree)) {
            max_colors <- brewer.pal.info[input$color_palette_PhyloTree, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette_PhyloTree)
            colors <- colorRampPalette(palette)(breedcount)
            rv_PhyloTree$breed1$color <- colors
            rv_PhyloTree$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status_PhyloTree <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    }
    observe({
      if (!is.null(rvdataclass$classification_data) && nrow(rvdataclass$classification_data) > 0) {
        if (input$data_type_PhyloTree == "PCA Data") {
          pcadataresult <- rvpcaresultdata$pcaresult_data
          pcadataresult_class <- rvdataclass$classification_data
          pcadataresult_class <- pcadataresult_class[pcadataresult_class$ID %in% pcadataresult$ID, ]
          breedChoices <- unique(pcadataresult_class$breed)
        } else {
          breedChoices <- unique(rvdataclass$classification_data$breed)
        }
        updatePickerInput(session, "selected_breeds_PhyloTree",
                          choices = breedChoices,
                          selected = breedChoices)
      }
    })
  })
  observeEvent(input$calculate_PhyloTree, {
    withProgress(message = 'Calculating...', value = 0, {
      setProgress(value = 0.01)
      if (!is.null(rv_PhyloTree$breed1) && nrow(rv_PhyloTree$breed1) > 0) {
        class1_PhyloTree <- rvdataclass$classification_data
        rv_PhyloTree$breed1 <- data.frame(breed = unique(class1_PhyloTree$breed))
        breedcount <- nrow(rv_PhyloTree$breed1)
        max_colors <- brewer.pal.info[input$color_palette_PhyloTree, "maxcolors"]
        palette <- brewer.pal(max_colors, input$color_palette_PhyloTree)
        colors <- colorRampPalette(palette)(breedcount)
        rv_PhyloTree$breed1$color <- colors
        calculate1 <- rv_PhyloTree$breed1
        calculateclass1 <- rvdataclass$classification_data
        merged_calculateclass <- merge(calculateclass1, calculate1, by = "breed")
        setProgress(value = 0.1)
        if (is.null(input$selected_breeds_PhyloTree) || length(input$selected_breeds_PhyloTree) == 0) {
          showNotification("Please select at least one breed before calculating.", type = "error")
          return()
        }
        merged_calculateclass_select <- merged_calculateclass[merged_calculateclass$breed %in% input$selected_breeds_PhyloTree, ]
        setProgress(value = 0.15)
        if (nrow(merged_calculateclass_select) == 0) {
          showNotification("No data available for selected breeds.", type = "error")
          return()
        }
        if (!is.null(rvdatageno$genotype_data) && nrow(rvdatageno$genotype_data) > 0) {
          calculate2 <- rvdatageno$genotype_data
          setProgress(value = 0.2)
          selected_IDs <- merged_calculateclass_select$ID
          selected_columns <- c(names(calculate2)[1:11], as.character(selected_IDs))
          setProgress(value = 0.25)
          if (is.data.table(calculate2)) {
            calculate3 <- as.data.frame(calculate2[, ..selected_columns])
          } else if (is.data.frame(calculate2)) {
            calculate3 <- calculate2[, selected_columns, drop = FALSE]
          } else {
            stop("calculate2 is neither a data.frame nor a data.table")
          }
          setProgress(value = 0.3)
          convert_genotypes_dt <- function(df) {
            setDT(df)
            df_with_slash <- df[grepl("/", alleles), ]
            df_without_slash <- df[!grepl("/", alleles), ]
            df_with_slash <- df_with_slash[order(alleles)]
            unique_alleles <- unique(df_with_slash$alleles)
            df_processed <- data.table()
            for (allele in unique_alleles) {
              sub_df <- df_with_slash[alleles == allele, ]
              alleles_parts <- strsplit(allele, "/", fixed = TRUE)[[1]]
              map <- setNames(c(0, 2, NA, 1), c(alleles_parts, "N", "other"))
              genotype_cols <- names(sub_df)[12:length(names(sub_df))]
              sub_df[, (genotype_cols) := lapply(.SD, function(x) map[ifelse(x %in% names(map), x, "other")]), .SDcols = genotype_cols]
              df_processed <- rbindlist(list(df_processed, sub_df), use.names = TRUE)
            }
            if (nrow(df_without_slash) > 0) {
              genotype_cols <- names(df_without_slash)[12:length(names(df_without_slash))]
              df_without_slash[, (genotype_cols) := lapply(.SD, function(x) ifelse(x == alleles, 0, ifelse(x == "N", NA, 1))), .SDcols = genotype_cols]
            }
            df_final <- rbindlist(list(df_processed, df_without_slash), use.names = TRUE)
            return(df_final)
          }
          calculate_converted <- convert_genotypes_dt(calculate3)
          setProgress(value = 0.35)
          calculate_converted2 <- calculate_converted[, -c(2:11)]
          new_row <- as.list(colnames(calculate_converted2))
          setProgress(value = 0.4)
          calculate_converted2 <- rbind(new_row, calculate_converted2)
          calculate_transposed <- transpose(calculate_converted2)
          setProgress(value = 0.45)
          col_names <- as.character(calculate_transposed[1, ])
          setProgress(value = 0.5)
          colnames(calculate_transposed) <- col_names
          calculate_transposed <- calculate_transposed[-1, ]
          colnames(calculate_transposed)[1] <- "IID"
          if (input$data_type_PhyloTree == "PCA Data") {
            pcadataresult <- rvpcaresultdata$pcaresult_data
            calculate_transposed <- calculate_transposed[calculate_transposed$IID %in% pcadataresult$ID, ]
            merged_calculateclass_select_pcadata <- merged_calculateclass_select[merged_calculateclass_select$ID %in% pcadataresult$ID, ]
          } else {
            merged_calculateclass_select_pcadata <- merged_calculateclass_select
          }
          calculate_transposed$FID <- calculate_transposed$IID
          setProgress(value = 0.55)
          calculate_transposed <- calculate_transposed[, c("FID", setdiff(names(calculate_transposed), "FID")), with = FALSE]
          fid = calculate_transposed$FID
          iid = calculate_transposed$IID
          rownames(calculate_transposed) = calculate_transposed$IID
          calculate_transposed$IID = NULL
          calculate_transposed$FID = NULL
          original_row_names <- rownames(calculate_transposed)
          setProgress(value = 0.6)
          calculate_transposed_numeric <- apply(calculate_transposed, 2, as.numeric) - 1
          rownames(calculate_transposed_numeric) <- original_row_names
          setProgress(value = 0.65)
          dist_matrix <- dist(calculate_transposed_numeric, method = "euclidean")
          dist_matrix <- as.matrix(dist_matrix)
          setProgress(value = 0.7)
          tree_method <- input$tree_construction_selector
          if (tree_method == "Neighbor-Joining") {
            phylo_tree <- nj(dist_matrix)
          } else if (tree_method == "FastME") {
            phylo_tree <- fastme.bal(dist_matrix)
          } else if (tree_method == "BioNJ") {
            phylo_tree <- bionj(dist_matrix)
          } else if (tree_method == "UPGMA") {
            phylo_tree <- upgma(dist_matrix)
          }
          setProgress(value = 0.75)
          if (!inherits(phylo_tree, "phylo")) {
            stop("Provided tree is not a valid 'phylo' object.")
          }
          if (input$perform_bootstrap == "yes") {
            B <- 1000
            boot_res <- boot.phylo(phylo_tree, dist_matrix, function(xx) nj(as.dist(xx)), B = B)
            if (is.null(boot_res)) {
              stop("Bootstrap values could not be generated.")
            }
            bootstrap_values <- pmin(1, (boot_res / B) * 100)
            num_internal_nodes <- Nnode(phylo_tree)
            if (length(bootstrap_values) != num_internal_nodes) {
              stop("The number of bootstrap values does not match the number of internal nodes.")
            }
            phylo_tree$node.label <- bootstrap_values
            setProgress(value = 0.8)
            color_vector <- setNames(merged_calculateclass_select_pcadata$color, merged_calculateclass_select_pcadata$ID)
            tree_data <- fortify(phylo_tree)
            internal_nodes <- tree_data$isTip == FALSE
            tree_data$node.label <- NA
            tree_data$node.label[internal_nodes] <- phylo_tree$node.label
            tree_data$color <- color_vector[as.character(tree_data$label)]
            if (rvpalette_PhyloTree$selected_palette == "rectangular_tree") {
              layout <- "rectangular"
            } else if (rvpalette_PhyloTree$selected_palette == "slanted_tree") {
              layout <- "slanted"
            } else if (rvpalette_PhyloTree$selected_palette == "circular_tree") {
              layout <- "circular"
            } else if (rvpalette_PhyloTree$selected_palette == "radial_tree") {
              layout <- "radial"
            } else {
              return()
            }
            plot_PhyloTree <- ggtree(phylo_tree, layout=layout) %<+% tree_data +
              geom_tiplab(aes(label=label, color=label), size=2) +
              geom_nodelab(aes(label=node.label), size=3, vjust=-0.5) +
              geom_tree(aes(color = label), size = 1) +
              scale_color_manual(values=color_vector) +
              ggtitle("Phylogenetic Tree with Bootstrap Values and Colors") +
              theme(
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                legend.position = "none"
              )
          } else {
            setProgress(value = 0.8)
            color_vector <- setNames(merged_calculateclass_select_pcadata$color, merged_calculateclass_select_pcadata$ID)
            tree_data <- fortify(phylo_tree)
            internal_nodes <- tree_data$isTip == FALSE
            tree_data$node.label <- NA
            tree_data$color <- color_vector[as.character(tree_data$label)]
            if (rvpalette_PhyloTree$selected_palette == "rectangular_tree") {
              layout <- "rectangular"
            } else if (rvpalette_PhyloTree$selected_palette == "slanted_tree") {
              layout <- "slanted"
            } else if (rvpalette_PhyloTree$selected_palette == "circular_tree") {
              layout <- "circular"
            } else if (rvpalette_PhyloTree$selected_palette == "radial_tree") {
              layout <- "radial"
            } else {
              return()
            }
            plot_PhyloTree <- ggtree(phylo_tree, layout=layout) %<+% tree_data +
              geom_tiplab(aes(label=label, color=label), size=2) +
              geom_tree(aes(color = label), size = 1) +
              scale_color_manual(values=color_vector) +
              ggtitle("Phylogenetic Tree with Colors") +
              theme(
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                legend.position = "none"
              )
          }
          setProgress(value = 0.85)
          rvdown_PhyloTree$plot_PhyloTree <- plot_PhyloTree
          setProgress(value = 0.9)
          edges <- phylo_tree$edge
          edge.lengths <- phylo_tree$edge.length
          total_lengths <- numeric(length(phylo_tree$tip.label))
          for (i in seq_along(phylo_tree$tip.label)) {
            node <- i
            total_length <- 0
            while (node != NROW(phylo_tree$edge)/2 + 1) {
              edge_index <- which(edges[,2] == node)
              if (length(edge_index) == 1) {
                total_length <- total_length + edge.lengths[edge_index]
                node <- edges[edge_index, 1]
              } else {
                break
              }
            }
            total_lengths[i] <- total_length
          }
          total_lengths_df <- data.frame(ID = phylo_tree$tip.label, total_length = total_lengths)
          values$showDataViewOptions <- TRUE
          rvResults_PhyloTree$data <- total_lengths_df
          rvtreeresultdata$treeresult_data <- total_lengths_df
          output$plot_PhyloTree <- renderPlot({ plot_PhyloTree })
          setProgress(value = 0.95)
          values$showText <- FALSE
          outputOptions(output, "showText", suspendWhenHidden = FALSE)
          setProgress(value = 1)
        } else {
          output$classification_data_status_PhyloTree <- renderUI({
            tags$p("Please upload and process genotype data before calculating.", style = "color: red;")
          })
        }
      } else {
        output$classification_data_status_PhyloTree <- renderUI({
          tags$p("Please upload and process classification data before calculating.", style = "color: red;")
        })
      }
    })
  })
  observeEvent(input$recalculate_PhyloTree, {
    withProgress(message = 'Calculating...', value = 0, {
      setProgress(value = 0.01)
      if (!is.null(rv_PhyloTree$breed1) && nrow(rv_PhyloTree$breed1) > 0) {
        class1_PhyloTree <- rvdataclass$classification_data
        rv_PhyloTree$breed1 <- data.frame(breed = unique(class1_PhyloTree$breed))
        breedcount <- nrow(rv_PhyloTree$breed1)
        max_colors <- brewer.pal.info[input$color_palette_PhyloTree, "maxcolors"]
        palette <- brewer.pal(max_colors, input$color_palette_PhyloTree)
        colors <- colorRampPalette(palette)(breedcount)
        rv_PhyloTree$breed1$color <- colors
        calculate1 <- rv_PhyloTree$breed1
        calculateclass1 <- rvdataclass$classification_data
        merged_calculateclass <- merge(calculateclass1, calculate1, by = "breed")
        setProgress(value = 0.1)
        if (is.null(input$selected_breeds_PhyloTree) || length(input$selected_breeds_PhyloTree) == 0) {
          showNotification("Please select at least one breed before calculating.", type = "error")
          return()
        }
        merged_calculateclass_select_old <- merged_calculateclass[merged_calculateclass$breed %in% input$selected_breeds_PhyloTree, ]
        selected_recalculate_ids <- recalculateresult$recalculate1$ID
        merged_calculateclass_select <- merged_calculateclass_select_old[merged_calculateclass_select_old$ID %in% selected_recalculate_ids, ]
        setProgress(value = 0.15)
        if (nrow(merged_calculateclass_select) == 0) {
          showNotification("No data available for selected breeds.", type = "error")
          return()
        }
        if (!is.null(rvdatageno$genotype_data) && nrow(rvdatageno$genotype_data) > 0) {
          calculate2 <- rvdatageno$genotype_data
          setProgress(value = 0.2)
          selected_IDs <- merged_calculateclass_select$ID
          selected_columns <- c(names(calculate2)[1:11], as.character(selected_IDs))
          setProgress(value = 0.25)
          if (is.data.table(calculate2)) {
            calculate3 <- as.data.frame(calculate2[, ..selected_columns])
          } else if (is.data.frame(calculate2)) {
            calculate3 <- calculate2[, selected_columns, drop = FALSE]
          } else {
            stop("calculate2 is neither a data.frame nor a data.table")
          }
          setProgress(value = 0.3)
          convert_genotypes_dt <- function(df) {
            setDT(df)
            df_with_slash <- df[grepl("/", alleles), ]
            df_without_slash <- df[!grepl("/", alleles), ]
            df_with_slash <- df_with_slash[order(alleles)]
            unique_alleles <- unique(df_with_slash$alleles)
            df_processed <- data.table()
            for (allele in unique_alleles) {
              sub_df <- df_with_slash[alleles == allele, ]
              alleles_parts <- strsplit(allele, "/", fixed = TRUE)[[1]]
              map <- setNames(c(0, 2, NA, 1), c(alleles_parts, "N", "other"))
              genotype_cols <- names(sub_df)[12:length(names(sub_df))]
              sub_df[, (genotype_cols) := lapply(.SD, function(x) map[ifelse(x %in% names(map), x, "other")]), .SDcols = genotype_cols]
              df_processed <- rbindlist(list(df_processed, sub_df), use.names = TRUE)
            }
            if (nrow(df_without_slash) > 0) {
              genotype_cols <- names(df_without_slash)[12:length(names(df_without_slash))]
              df_without_slash[, (genotype_cols) := lapply(.SD, function(x) ifelse(x == alleles, 0, ifelse(x == "N", NA, 1))), .SDcols = genotype_cols]
            }
            df_final <- rbindlist(list(df_processed, df_without_slash), use.names = TRUE)
            return(df_final)
          }
          calculate_converted <- convert_genotypes_dt(calculate3)
          setProgress(value = 0.35)
          calculate_converted2 <- calculate_converted[, -c(2:11)]
          new_row <- as.list(colnames(calculate_converted2))
          setProgress(value = 0.4)
          calculate_converted2 <- rbind(new_row, calculate_converted2)
          calculate_transposed <- transpose(calculate_converted2)
          setProgress(value = 0.45)
          col_names <- as.character(calculate_transposed[1, ])
          setProgress(value = 0.5)
          colnames(calculate_transposed) <- col_names
          calculate_transposed <- calculate_transposed[-1, ]
          colnames(calculate_transposed)[1] <- "IID"
          if (input$data_type_PhyloTree == "PCA Data") {
            pcadataresult <- rvpcaresultdata$pcaresult_data
            calculate_transposed <- calculate_transposed[calculate_transposed$IID %in% pcadataresult$ID, ]
            merged_calculateclass_select_pcadata <- merged_calculateclass_select[merged_calculateclass_select$ID %in% pcadataresult$ID, ]
          } else {
            merged_calculateclass_select_pcadata <- merged_calculateclass_select
          }
          calculate_transposed$FID <- calculate_transposed$IID
          setProgress(value = 0.55)
          calculate_transposed <- calculate_transposed[, c("FID", setdiff(names(calculate_transposed), "FID")), with = FALSE]
          fid = calculate_transposed$FID
          iid = calculate_transposed$IID
          rownames(calculate_transposed) = calculate_transposed$IID
          calculate_transposed$IID = NULL
          calculate_transposed$FID = NULL
          original_row_names <- rownames(calculate_transposed)
          setProgress(value = 0.6)
          calculate_transposed_numeric <- apply(calculate_transposed, 2, as.numeric) - 1
          rownames(calculate_transposed_numeric) <- original_row_names
          setProgress(value = 0.65)
          dist_matrix <- dist(calculate_transposed_numeric, method = "euclidean")
          dist_matrix <- as.matrix(dist_matrix)
          setProgress(value = 0.7)
          tree_method <- input$tree_construction_selector
          if (tree_method == "Neighbor-Joining") {
            phylo_tree <- nj(dist_matrix)
          } else if (tree_method == "FastME") {
            phylo_tree <- fastme.bal(dist_matrix)
          } else if (tree_method == "BioNJ") {
            phylo_tree <- bionj(dist_matrix)
          } else if (tree_method == "UPGMA") {
            phylo_tree <- upgma(dist_matrix)
          }
          setProgress(value = 0.75)
          if (!inherits(phylo_tree, "phylo")) {
            stop("Provided tree is not a valid 'phylo' object.")
          }
          if (input$perform_bootstrap == "yes") {
            B <- 1000
            boot_res <- boot.phylo(phylo_tree, dist_matrix, function(xx) nj(as.dist(xx)), B = B)
            if (is.null(boot_res)) {
              stop("Bootstrap values could not be generated.")
            }
            bootstrap_values <- pmin(1, (boot_res / B) * 100)
            num_internal_nodes <- Nnode(phylo_tree)
            if (length(bootstrap_values) != num_internal_nodes) {
              stop("The number of bootstrap values does not match the number of internal nodes.")
            }
            phylo_tree$node.label <- bootstrap_values
            setProgress(value = 0.8)
            color_vector <- setNames(merged_calculateclass_select_pcadata$color, merged_calculateclass_select_pcadata$ID)
            tree_data <- fortify(phylo_tree)
            internal_nodes <- tree_data$isTip == FALSE
            tree_data$node.label <- NA
            tree_data$node.label[internal_nodes] <- phylo_tree$node.label
            tree_data$color <- color_vector[as.character(tree_data$label)]
            if (rvpalette_PhyloTree$selected_palette == "rectangular_tree") {
              layout <- "rectangular"
            } else if (rvpalette_PhyloTree$selected_palette == "slanted_tree") {
              layout <- "slanted"
            } else if (rvpalette_PhyloTree$selected_palette == "circular_tree") {
              layout <- "circular"
            } else if (rvpalette_PhyloTree$selected_palette == "radial_tree") {
              layout <- "radial"
            } else {
              return()
            }
            plot_PhyloTree <- ggtree(phylo_tree, layout=layout) %<+% tree_data +
              geom_tiplab(aes(label=label, color=label), size=2) +
              geom_nodelab(aes(label=node.label), size=3, vjust=-0.5) +
              geom_tree(aes(color = label), size = 1) +
              scale_color_manual(values=color_vector) +
              ggtitle("Phylogenetic Tree with Bootstrap Values and Colors") +
              theme(
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                legend.position = "none"
              )
          } else {
            setProgress(value = 0.8)
            color_vector <- setNames(merged_calculateclass_select_pcadata$color, merged_calculateclass_select_pcadata$ID)
            tree_data <- fortify(phylo_tree)
            internal_nodes <- tree_data$isTip == FALSE
            tree_data$node.label <- NA
            tree_data$color <- color_vector[as.character(tree_data$label)]
            if (rvpalette_PhyloTree$selected_palette == "rectangular_tree") {
              layout <- "rectangular"
            } else if (rvpalette_PhyloTree$selected_palette == "slanted_tree") {
              layout <- "slanted"
            } else if (rvpalette_PhyloTree$selected_palette == "circular_tree") {
              layout <- "circular"
            } else if (rvpalette_PhyloTree$selected_palette == "radial_tree") {
              layout <- "radial"
            } else {
              return()
            }
            plot_PhyloTree <- ggtree(phylo_tree, layout=layout) %<+% tree_data +
              geom_tiplab(aes(label=label, color=label), size=2) +
              geom_tree(aes(color = label), size = 1) +
              scale_color_manual(values=color_vector) +
              ggtitle("Phylogenetic Tree with Colors") +
              theme(
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                legend.position = "none"
              )
          }
          setProgress(value = 0.85)
          rvdown_PhyloTree$plot_PhyloTree <- plot_PhyloTree
          setProgress(value = 0.9)
          edges <- phylo_tree$edge
          edge.lengths <- phylo_tree$edge.length
          total_lengths <- numeric(length(phylo_tree$tip.label))
          for (i in seq_along(phylo_tree$tip.label)) {
            node <- i
            total_length <- 0
            while (node != NROW(phylo_tree$edge)/2 + 1) {
              edge_index <- which(edges[,2] == node)
              if (length(edge_index) == 1) {
                total_length <- total_length + edge.lengths[edge_index]
                node <- edges[edge_index, 1]
              } else {
                break
              }
            }
            total_lengths[i] <- total_length
          }
          total_lengths_df <- data.frame(ID = phylo_tree$tip.label, total_length = total_lengths)
          rvtreeresultdata$treeresult_data <- total_lengths_df
          output$plot_PhyloTree <- renderPlot({ plot_PhyloTree })
          setProgress(value = 0.95)
          values$showText <- FALSE
          outputOptions(output, "showText", suspendWhenHidden = FALSE)
          setProgress(value = 1)
        } else {
          output$classification_data_status_PhyloTree <- renderUI({
            tags$p("Please upload and process genotype data before calculating.", style = "color: red;")
          })
        }
      } else {
        output$classification_data_status_PhyloTree <- renderUI({
          tags$p("Please upload and process classification data before calculating.", style = "color: red;")
        })
      }
    })
  })
  observeEvent(input$select_palette_PhyloTree, {
    selected <- input$select_palette_PhyloTree
    if (selected != "") {
      rvpalette_PhyloTree$selected_palette <- switch(selected,
                                                     "rectangular_tree" = "rectangular_tree",
                                                     "slanted_tree" = "slanted_tree",
                                                     "circular_tree" = "circular_tree",
                                                     "radial_tree" = "radial_tree")
    }
  })
  output$conditionalUI_PhyloTree <- renderUI({
    if (values$showText) {
      conditionalPanel(
        condition = "true",
        h1("PhyloTree Analysis"),
        p("Perform PhyloTree analysis here and display the results.")
      )
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_PhyloTree")
    } else {
      shinyjs::show("cover_PhyloTree")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_PhyloTree2")
    } else {
      shinyjs::show("cover_PhyloTree2")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_PhyloTree3")
    } else {
      shinyjs::show("cover_PhyloTree3")
    }
  })
  output$ResultsTable_PhyloTree <- renderDT({
    req(rvResults_PhyloTree$data)
    DataToShow_PhyloTree <- rvResults_PhyloTree$data
    if(input$data_view_PhyloTree == "head") {
      DataToShow_PhyloTree <- head(DataToShow_PhyloTree, 10)
    }
    datatable(DataToShow_PhyloTree, options = list(
      pageLength = -1,
      scrollX = TRUE,
      columnDefs = list(
        list(
          targets = 0,
          render = JS(
            "function(data, type, full, meta) {",
            "var checked = window.selectedRowsData_PhyloTree && window.selectedRowsData_PhyloTree.map(row => row[0]).includes(data) ? 'checked' : '';",
            "return '<input type=\"checkbox\" class=\"dt-checkbox\" '+checked+' value=\"'+data+'\">';",
            "}"
          ),
          title = '<input type="checkbox" id="check-all-PhyloTree">',
          orderable = FALSE
        )
      ),
      rowCallback = JS(
        "function(row, data, index) {",
        "if (window.selectedRowsData_PhyloTree && window.selectedRowsData_PhyloTree.map(row => row[0]).includes(data[0])) {",
        "$(row).addClass('selected');",
        "$('input.dt-checkbox', row).prop('checked', true);",
        "} else {",
        "$(row).removeClass('selected');",
        "$('input.dt-checkbox', row).prop('checked', false);",
        "}",
        "}"
      ),
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      initComplete = JS(
        "function(settings, json) {",
        "if (!window.selectedRowsData_PhyloTree) {",
        "window.selectedRowsData_PhyloTree = [];",
        "}",
        "$('#check-all-PhyloTree').click(function() {",
        "var rows = settings.aoData;",
        "var currentPageData = rows.map(row => row._aData);",
        "if ($(this).is(':checked')) {",
        "currentPageData.forEach(function(rowData) {",
        "if (!window.selectedRowsData_PhyloTree.some(row => row[0] === rowData[0])) {",
        "window.selectedRowsData_PhyloTree.push(rowData);",
        "}",
        "});",
        "} else {",
        "currentPageData.forEach(function(rowData) {",
        "window.selectedRowsData_PhyloTree = window.selectedRowsData_PhyloTree.filter(row => row[0] !== rowData[0]);",
        "});",
        "}",
        "$('input.dt-checkbox').prop('checked', $(this).is(':checked'));",
        "Shiny.setInputValue('selectedRowsData_PhyloTree', window.selectedRowsData_PhyloTree);",
        "});",
        "$(settings.nTable).on('change', 'input.dt-checkbox', function() {",
        "var $checkbox = $(this);",
        "var rowIndex = $(this).closest('tr').index();",
        "var rowData = settings.aoData[rowIndex]._aData;",
        "if ($checkbox.is(':checked')) {",
        "if (!window.selectedRowsData_PhyloTree.some(row => row[0] === rowData[0])) {",
        "window.selectedRowsData_PhyloTree.push(rowData);",
        "}",
        "} else {",
        "window.selectedRowsData_PhyloTree = window.selectedRowsData_PhyloTree.filter(row => row[0] !== rowData[0]);",
        "}",
        "Shiny.setInputValue('selectedRowsData_PhyloTree', window.selectedRowsData_PhyloTree);",
        "});",
        "}"
      )
    ), escape = FALSE)
  }, server = TRUE)
  observeEvent(input$selectedRowsData_PhyloTree, {
    if (length(input$selectedRowsData_PhyloTree) %% 3 != 0) {
      #print("Input data format is incorrect")
      return()
    }
    matrix_data <- matrix(input$selectedRowsData_PhyloTree, ncol = 3, byrow = TRUE)
    data_df <- as.data.frame(matrix_data, stringsAsFactors = FALSE)
    names(data_df) <- c("Index", "ID", "total_length")
    recalculateresult$recalculate1 <- data_df
  })
  output$downloadTable_PhyloTree <- downloadHandler(
    filename = function() {
      paste("PhyloTree-Data-Table-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- recalculateresult$recalculate1
      if (is.null(data)) {
        return()
      }
      selected_data <- data[, c("ID", "total_length")]
      write.csv(selected_data, file, row.names = FALSE)
    }
  )
  output$showText <- reactive({ values$showText })
  output$showText <- reactive({ values$showText })
  output$downloadZip_PhyloTree <- downloadHandler(
    filename = function() {
      "PhyloTree_results.zip"
    },
    content = function(file) {
      tempDir <- tempdir()
      formats <- input$image_format_PhyloTree
      plotName <- "plot_PhyloTree"
      filesToRemove <- list.files(tempDir, full.names = TRUE)
      if (length(filesToRemove) > 0) {
        suppressWarnings(file.remove(filesToRemove))
      }
      for (format in formats) {
        filePath <- file.path(tempDir, paste0(plotName, ".", tolower(format)))
        suppressMessages(ggsave(filePath, plot = rvdown_PhyloTree$plot_PhyloTree, device = tolower(format)))
      }
      pattern <- paste0("\\.(", paste(tolower(formats), collapse="|"), ")$")
      imageFiles <- list.files(tempDir, pattern = pattern, full.names = TRUE)
      suppressMessages(zip(file, files = imageFiles, flags = "-j"))
    },
    contentType = "application/zip"
  )
}
