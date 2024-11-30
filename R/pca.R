#' @export
pca_ui <- function() {
  tabPanel("DimRed",
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
                          selectInput("data_type", "Select Data", choices = c("Please select..." = "", "Raw Data" = "Raw Data")),
                          actionButton("refresh", label = icon("refresh"), style = "background-color: transparent; border: none; color: #337ab7; margin-left: 10px;")
                      ),
                      uiOutput("classification_data_status"),
                      selectInput("color_palette", "Select Color Palette",
                                  choices = c("Please select..." = "",
                                              "RdYlBu" = "RdYlBu",
                                              "Spectral" = "Spectral",
                                              "Paired" = "Paired",
                                              "Set3" = "Set3",
                                              "Dark2" = "Dark2",
                                              "Set1" = "Set1")),
                      uiOutput("color_palette_status"),
                      pickerInput(
                        inputId = "selected_breeds",
                        label = "Select Breeds",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")
                      ),
                      selectInput("dim_reduction_selector", "Select Dimensionality Reduction Method",
                                  choices = c("Please select..." = "",
                                              "PCA" = "PCA",
                                              "MDS" = "MDS",
                                              "UMAP" = "UMAP"),
                                  selected = "PCA"),
                      selectInput("pcaselect_palette", "Select Dimensions Palette",
                                  choices = c("Please select..." = "",
                                              "PCA1_vs_PCA2" = "PCA1_vs_PCA2",
                                              "PCA1_vs_PCA3" = "PCA1_vs_PCA3",
                                              "PCA2_vs_PCA3" = "PCA2_vs_PCA3"),
                                  selected = "PCA1_vs_PCA2"),
                      actionButton("calculate", "Calculate", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                    ),
                    tags$div(
                      style = "background-color: #f2f2f2; padding: 10px; border-radius: 10px; box-shadow: none; margin-top: 20px;",
                      div(
                        pickerInput("image_format", "Select Image Format",
                                    choices = c("PDF", "TIFF", "PNG", "JPEG"),
                                    selected = c("PDF", "TIFF", "PNG", "JPEG"),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE)),
                        downloadButton("downloadZip", "Download ZIP", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                      )
                    )
             ),
             useShinyjs(),
             column(width = 8,
                    uiOutput("conditionalUI"),
                    plotOutput("plot"),
                    div(id = "radioButtonsContainer", style = "position: relative;",
                        column(width = 4, class = "text-center",
                               actionButton("recalculate", "ReCalculate", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;"),
                               div(id = "cover", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 4, class = "text-center",
                               downloadButton("downloadTable", "DownloadTable", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;"),
                               div(id = "cover3", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 3,
                               radioButtons("data_view_pca", label = "Display:",
                                            choices = c("Head" = "head", "All" = "all"),
                                            selected = "all"),
                               div(id = "cover2", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        div(id = "cover", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")
                    ),
                    DTOutput("pcaResultsTable")
             )
           )
  )
}

#' @export
pca_server <- function(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata) {
  values <- reactiveValues(showText = TRUE, showDataViewOptions = FALSE)
  recalculateresult <- reactiveValues(recalculate1 = NULL)
  rvpca1 <- reactiveValues(breed1 = NULL, global_color_assigned = FALSE)
  rvpcadown <- reactiveValues(pcaplot = NULL)
  rvPcaResults <- reactiveValues(data = NULL)
  rvPcapalette <- reactiveValues(selected_palette = NULL)
  observeEvent(input$refresh, {
    updateSelectInput(session, "data_type", selected = "")
    if (!rvpca1$global_color_assigned) {
      updateSelectInput(session, "color_palette", selected = "RdYlBu")
    }
    rvpca1$global_color_assigned <- FALSE
  })
  observeEvent(input$data_type, {
    output$classification_data_status <- renderUI({})
    if (input$data_type == "Raw Data") {
      if (!is.null(rvdataclass$classification_data) && nrow(rvdataclass$classification_data) > 0) {
        pcaclass1 <- rvdataclass$classification_data
        rvpca1$breed1 <- data.frame(breed = unique(pcaclass1$breed))
        breedcount <- nrow(rvpca1$breed1)
        observeEvent(input$color_palette, {
          if (!is.null(input$color_palette)) {
            max_colors <- brewer.pal.info[input$color_palette, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette)
            colors <- colorRampPalette(palette)(breedcount)
            rvpca1$breed1$color <- colors
            rvpca1$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    }
    observe({
      if (!is.null(rvdataclass$classification_data) && nrow(rvdataclass$classification_data) > 0) {
        breedChoices <- unique(rvdataclass$classification_data$breed)
        updatePickerInput(session, "selected_breeds",
                          choices = breedChoices,
                          selected = breedChoices)
      }
    })
  })
  observeEvent(input$calculate, {
    withProgress(message = 'Calculating...', value = 0, {
      setProgress(value = 0.01)
      if (!is.null(rvpca1$breed1) && nrow(rvpca1$breed1) > 0) {
        calculate1 <- rvpca1$breed1
        calculateclass1 <- rvdataclass$classification_data
        merged_calculateclass <- merge(calculateclass1, calculate1, by = "breed")
        setProgress(value = 0.1)
        if (is.null(input$selected_breeds) || length(input$selected_breeds) == 0) {
          showNotification("Please select at least one breed before calculating.", type = "error")
          return()
        }
        merged_calculateclass_select <- merged_calculateclass[merged_calculateclass$breed %in% input$selected_breeds, ]
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
          Gmat <- A.mat(calculate_transposed_numeric)
          selected_dim <- input$dim_reduction_selector
          if (selected_dim == "PCA") {
            re = eigen(Gmat)
            por = re$values/sum(re$values)
            pca_re1 = re$vectors[,1:3]
            pca_re2 = data.frame(pca_re1, Ind = iid)
            pca_re2$ID = fid
            dim_re_df = pca_re2
          } else if (selected_dim == "MDS") {
            Gmat[is.na(Gmat)] <- apply(Gmat, 2, function(x) mean(x, na.rm = TRUE))
            diss_matrix <- dist(Gmat)
            mds_re = cmdscale(diss_matrix, k = 3, eig = TRUE)
            mds_re2 = data.frame(mds_re$points)
            colnames(mds_re2) <- c("PCA1", "PCA2", "PCA3")
            mds_re2$Ind = iid
            mds_re2$ID = fid
            dim_re_df = mds_re2
            mds_eigenvalues = mds_re$eig[mds_re$eig > 0]
            por = mds_eigenvalues / sum(mds_eigenvalues)
          } else if (selected_dim == "UMAP") {
            Gmat[is.na(Gmat)] <- apply(Gmat, 2, function(x) mean(x, na.rm = TRUE))
            umap_re = umap(Gmat, n_components = 3)
            umap_re2 = data.frame(umap_re$layout, Ind = iid)
            umap_re2$ID = fid
            dim_re_df = umap_re2
            umap_distances = dist(umap_re$layout)
            umap_re = eigen(as.matrix(umap_distances))
            por = umap_re$values / sum(umap_re$values)
          }
          calculateresult <- merge(dim_re_df, merged_calculateclass_select, by = "ID", all.x = TRUE)
          setProgress(value = 0.75)
          colnames(calculateresult)[colnames(calculateresult) %in% c("X1", "X2", "X3")] <- c("PCA1", "PCA2", "PCA3")
          pcaresult <- calculateresult[, c("ID", "PCA1", "PCA2", "PCA3")]
          values$showDataViewOptions <- TRUE
          rvPcaResults$data <- pcaresult
          rvpcaresultdata$pcaresult_data <- pcaresult
          xlab = paste0("PC1 (", round(por[1] * 100, 2), "%)")
          ylab = paste0("PC2 (", round(por[2] * 100, 2), "%)")
          zlab = paste0("PC3 (", round(por[3] * 100, 2), "%)")
          setProgress(value = 0.8)
          if (rvPcapalette$selected_palette == "PCA1_vs_PCA2") {
            pcaplot <- ggplot(calculateresult, aes(x = PCA1, y = PCA2, color = breed)) +
              geom_point(size = 2) +
              scale_color_manual(values = setNames(calculateresult$color, calculateresult$breed)) +
              stat_ellipse(aes(fill = breed), type = "norm", geom = "polygon", alpha = 0.2, color = NA) +
              geom_hline(yintercept = 0) +
              geom_vline(xintercept = 0) +
              labs(x = xlab, y = ylab, color = "Breed") +
              guides(fill = "none") +
              theme_bw() +
              ggtitle("PCA_RESULT") +
              theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 3/4)
          } else if (rvPcapalette$selected_palette == "PCA1_vs_PCA3") {
            pcaplot <- ggplot(calculateresult, aes(x = PCA1, y = PCA3, color = breed)) +
              geom_point(size = 2) +
              scale_color_manual(values = setNames(calculateresult$color, calculateresult$breed)) +
              stat_ellipse(aes(fill = breed), type = "norm", geom = "polygon", alpha = 0.2, color = NA) +
              geom_hline(yintercept = 0) +
              geom_vline(xintercept = 0) +
              labs(x = xlab, y = zlab, color = "Breed") +
              guides(fill = "none") +
              theme_bw() +
              ggtitle("PCA_RESULT") +
              theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 3/4)
          } else if (rvPcapalette$selected_palette == "PCA2_vs_PCA3") {
            pcaplot <- ggplot(calculateresult, aes(x = PCA2, y = PCA3, color = breed)) +
              geom_point(size = 2) +
              scale_color_manual(values = setNames(calculateresult$color, calculateresult$breed)) +
              stat_ellipse(aes(fill = breed), type = "norm", geom = "polygon", alpha = 0.2, color = NA) +
              geom_hline(yintercept = 0) +
              geom_vline(xintercept = 0) +
              labs(x = ylab, y = zlab, color = "Breed") +
              guides(fill = "none") +
              theme_bw() +
              ggtitle("PCA_RESULT") +
              theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 3/4)
          } else {
            return()
          }
          setProgress(value = 0.85)
          rvpcadown$pcaplot <- pcaplot
          setProgress(value = 0.9)
          output$plot <- renderPlot({ pcaplot })
          setProgress(value = 0.95)
          values$showText <- FALSE
          outputOptions(output, "showText", suspendWhenHidden = FALSE)
          setProgress(value = 1)
        } else {
          output$classification_data_status <- renderUI({
            tags$p("Please upload and process genotype data before calculating.", style = "color: red;")
          })
        }
      } else {
        output$classification_data_status <- renderUI({
          tags$p("Please upload and process classification data before calculating.", style = "color: red;")
        })
      }
    })
  })
  observeEvent(input$recalculate, {
    withProgress(message = 'Calculating...', value = 0, {
      setProgress(value = 0.01)
      if (!is.null(rvpca1$breed1) && nrow(rvpca1$breed1) > 0) {
        calculate1 <- rvpca1$breed1
        calculateclass1 <- rvdataclass$classification_data
        merged_calculateclass <- merge(calculateclass1, calculate1, by = "breed")
        setProgress(value = 0.1)
        if (is.null(input$selected_breeds) || length(input$selected_breeds) == 0) {
          showNotification("Please select at least one breed before calculating.", type = "error")
          return()
        }
        merged_calculateclass_select_old <- merged_calculateclass[merged_calculateclass$breed %in% input$selected_breeds, ]
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
          Gmat <- A.mat(calculate_transposed_numeric)
          selected_dim <- input$dim_reduction_selector
          if (selected_dim == "PCA") {
            re = eigen(Gmat)
            por = re$values/sum(re$values)
            pca_re1 = re$vectors[,1:3]
            pca_re2 = data.frame(pca_re1, Ind = iid)
            pca_re2$ID = fid
            dim_re_df = pca_re2
          } else if (selected_dim == "MDS") {
            Gmat[is.na(Gmat)] <- apply(Gmat, 2, function(x) mean(x, na.rm = TRUE))
            diss_matrix <- dist(Gmat)
            mds_re = cmdscale(diss_matrix, k = 3, eig = TRUE)
            mds_re2 = data.frame(mds_re$points)
            colnames(mds_re2) <- c("PCA1", "PCA2", "PCA3")
            mds_re2$Ind = iid
            mds_re2$ID = fid
            dim_re_df = mds_re2
            mds_eigenvalues = mds_re$eig[mds_re$eig > 0]
            por = mds_eigenvalues / sum(mds_eigenvalues)
          } else if (selected_dim == "UMAP") {
            Gmat[is.na(Gmat)] <- apply(Gmat, 2, function(x) mean(x, na.rm = TRUE))
            umap_re = umap(Gmat, n_components = 3)
            umap_re2 = data.frame(umap_re$layout, Ind = iid)
            umap_re2$ID = fid
            dim_re_df = umap_re2
            umap_distances = dist(umap_re$layout)
            umap_re = eigen(as.matrix(umap_distances))
            por = umap_re$values / sum(umap_re$values)
          }
          calculateresult <- merge(dim_re_df, merged_calculateclass_select, by = "ID", all.x = TRUE)
          setProgress(value = 0.75)
          colnames(calculateresult)[colnames(calculateresult) %in% c("X1", "X2", "X3")] <- c("PCA1", "PCA2", "PCA3")
          pcaresult <- calculateresult[, c("ID", "PCA1", "PCA2", "PCA3")]
          values$showDataViewOptions <- TRUE
          rvpcaresultdata$pcaresult_data <- pcaresult
          View(pcaresult)
          xlab = paste0("PC1 (", round(por[1] * 100, 2), "%)")
          ylab = paste0("PC2 (", round(por[2] * 100, 2), "%)")
          zlab = paste0("PC3 (", round(por[3] * 100, 2), "%)")
          setProgress(value = 0.8)
          if (rvPcapalette$selected_palette == "PCA1_vs_PCA2") {
            pcaplot <- ggplot(calculateresult, aes(x = PCA1, y = PCA2, color = breed)) +
              geom_point(size = 2) +
              scale_color_manual(values = setNames(calculateresult$color, calculateresult$breed)) +
              stat_ellipse(aes(fill = breed), type = "norm", geom = "polygon", alpha = 0.2, color = NA) +
              geom_hline(yintercept = 0) +
              geom_vline(xintercept = 0) +
              labs(x = xlab, y = ylab, color = "Breed") +
              guides(fill = "none") +
              theme_bw() +
              ggtitle("PCA_RESULT") +
              theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 3/4)
          } else if (rvPcapalette$selected_palette == "PCA1_vs_PCA3") {
            pcaplot <- ggplot(calculateresult, aes(x = PCA1, y = PCA3, color = breed)) +
              geom_point(size = 2) +
              scale_color_manual(values = setNames(calculateresult$color, calculateresult$breed)) +
              stat_ellipse(aes(fill = breed), type = "norm", geom = "polygon", alpha = 0.2, color = NA) +
              geom_hline(yintercept = 0) +
              geom_vline(xintercept = 0) +
              labs(x = xlab, y = zlab, color = "Breed") +
              guides(fill = "none") +
              theme_bw() +
              ggtitle("PCA_RESULT") +
              theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 3/4)
          } else if (rvPcapalette$selected_palette == "PCA2_vs_PCA3") {
            pcaplot <- ggplot(calculateresult, aes(x = PCA2, y = PCA3, color = breed)) +
              geom_point(size = 2) +
              scale_color_manual(values = setNames(calculateresult$color, calculateresult$breed)) +
              stat_ellipse(aes(fill = breed), type = "norm", geom = "polygon", alpha = 0.2, color = NA) +
              geom_hline(yintercept = 0) +
              geom_vline(xintercept = 0) +
              labs(x = ylab, y = zlab, color = "Breed") +
              guides(fill = "none") +
              theme_bw() +
              ggtitle("PCA_RESULT") +
              theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 3/4)
          } else {
            return()
          }
          setProgress(value = 0.85)
          rvpcadown$pcaplot <- pcaplot
          setProgress(value = 0.9)
          output$plot <- renderPlot({ pcaplot })
          setProgress(value = 0.95)
          values$showText <- FALSE
          outputOptions(output, "showText", suspendWhenHidden = FALSE)
          setProgress(value = 1)
        } else {
          output$classification_data_status <- renderUI({
            tags$p("Please upload and process genotype data before calculating.", style = "color: red;")
          })
        }
      } else {
        output$classification_data_status <- renderUI({
          tags$p("Please upload and process classification data before calculating.", style = "color: red;")
        })
      }
    })
  })
  observeEvent(input$pcaselect_palette, {
    selected <- input$pcaselect_palette
    if (selected != "") {
      rvPcapalette$selected_palette <- switch(selected,
                                              "PCA1_vs_PCA2" = "PCA1_vs_PCA2",
                                              "PCA1_vs_PCA3" = "PCA1_vs_PCA3",
                                              "PCA2_vs_PCA3" = "PCA2_vs_PCA3")
    }
  })
  output$conditionalUI <- renderUI({
    if (values$showText) {
      conditionalPanel(
        condition = "true",
        h1("PCA Analysis"),
        p("Perform PCA analysis here and display the results.")
      )
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover")
    } else {
      shinyjs::show("cover")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover2")
    } else {
      shinyjs::show("cover2")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover3")
    } else {
      shinyjs::show("cover3")
    }
  })
  output$pcaResultsTable <- renderDT({
    req(rvPcaResults$data)
    pcaDataToShow <- rvPcaResults$data
    if(input$data_view_pca == "head") {
      pcaDataToShow <- head(pcaDataToShow, 10)
    }
    datatable(pcaDataToShow, options = list(
      pageLength = -1,
      scrollX = TRUE,
      columnDefs = list(
        list(
          targets = 0,
          render = JS(
            "function(data, type, full, meta) {",
            "var checked = window.selectedRowsData && window.selectedRowsData.map(row => row[0]).includes(data) ? 'checked' : '';",
            "return '<input type=\"checkbox\" class=\"dt-checkbox\" '+checked+' value=\"'+data+'\">';",
            "}"
          ),
          title = '<input type="checkbox" id="check-all">',
          orderable = FALSE
        )
      ),
      rowCallback = JS(
        "function(row, data, index) {",
        "if (window.selectedRowsData && window.selectedRowsData.map(row => row[0]).includes(data[0])) {",
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
        "if (!window.selectedRowsData) {",
        "window.selectedRowsData = [];",
        "}",
        "$('#check-all').click(function() {",
        "var rows = settings.aoData;",
        "var currentPageData = rows.map(row => row._aData);",
        "if ($(this).is(':checked')) {",
        "currentPageData.forEach(function(rowData) {",
        "if (!window.selectedRowsData.some(row => row[0] === rowData[0])) {",
        "window.selectedRowsData.push(rowData);",
        "}",
        "});",
        "} else {",
        "currentPageData.forEach(function(rowData) {",
        "window.selectedRowsData = window.selectedRowsData.filter(row => row[0] !== rowData[0]);",
        "});",
        "}",
        "$('input.dt-checkbox').prop('checked', $(this).is(':checked'));",
        "Shiny.setInputValue('selectedRowsData', window.selectedRowsData);",
        "});",
        "$(settings.nTable).on('change', 'input.dt-checkbox', function() {",
        "var $checkbox = $(this);",
        "var rowIndex = $(this).closest('tr').index();",
        "var rowData = settings.aoData[rowIndex]._aData;",
        "if ($checkbox.is(':checked')) {",
        "if (!window.selectedRowsData.some(row => row[0] === rowData[0])) {",
        "window.selectedRowsData.push(rowData);",
        "}",
        "} else {",
        "window.selectedRowsData = window.selectedRowsData.filter(row => row[0] !== rowData[0]);",
        "}",
        "Shiny.setInputValue('selectedRowsData', window.selectedRowsData);",
        "});",
        "}"
      )
    ), escape = FALSE)
  }, server = TRUE)
  observeEvent(input$selectedRowsData, {
    if (length(input$selectedRowsData) %% 5 != 0) {
      print("Input data format is incorrect")
      return()
    }
    matrix_data <- matrix(input$selectedRowsData, ncol = 5, byrow = TRUE)
    data_df <- as.data.frame(matrix_data, stringsAsFactors = FALSE)
    names(data_df) <- c("Index", "ID", "PCA1", "PCA2", "PCA3")
    recalculateresult$recalculate1 <- data_df
  })
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("Pca-Data-Table-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- recalculateresult$recalculate1
      if (is.null(data)) {
        return()
      }
      selected_data <- data[, c("ID", "PCA1", "PCA2", "PCA3")]
      write.csv(selected_data, file, row.names = FALSE)
    }
  )
  output$showText <- reactive({ values$showText })
  output$downloadZip <- downloadHandler(
    filename = function() {
      "PCA_results.zip"
    },
    content = function(file) {
      tempDir <- tempdir()
      formats <- input$image_format
      plotName <- "pcaplot"
      filesToRemove <- list.files(tempDir, full.names = TRUE)
      if (length(filesToRemove) > 0) {
        suppressWarnings(file.remove(filesToRemove))
      }
      for (format in formats) {
        filePath <- file.path(tempDir, paste0(plotName, ".", tolower(format)))
        suppressMessages(ggsave(filePath, plot = rvpcadown$pcaplot, device = tolower(format)))
      }
      pattern <- paste0("\\.(", paste(tolower(formats), collapse="|"), ")$")
      imageFiles <- list.files(tempDir, pattern = pattern, full.names = TRUE)
      suppressMessages(zip(file, files = imageFiles, flags = "-j"))
    },
    contentType = "application/zip"
  )
}
