TrainModel_ui <- function() {
  tabPanel("TrainModel",
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
                          selectInput("data_type_TrainModel", "Select Data", choices = c("Please select..." = "", "Raw Data" = "Raw Data", "PCA Data" = "PCA Data", "Tree Data" = "Tree Data")),
                          actionButton("refresh_TrainModel", label = icon("refresh"), style = "background-color: transparent; border: none; color: #337ab7; margin-left: 10px;")
                      ),
                      uiOutput("classification_data_status_TrainModel"),
                      selectInput("color_palette_TrainModel", "Select Color Palette",
                                  choices = c("Please select..." = "",
                                              "RdYlBu" = "RdYlBu",
                                              "Spectral" = "Spectral",
                                              "Paired" = "Paired",
                                              "Set3" = "Set3",
                                              "Dark2" = "Dark2",
                                              "Set1" = "Set1")),
                      uiOutput("color_palette_status_TrainModel"),
                      pickerInput(
                        inputId = "selected_breeds_TrainModel",
                        label = "Select Breeds",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")
                      ),
                      numericInput("seed_selector_TrainModel", "Select Random Seed", value = 123, min = 1, max = .Machine$integer.max, step = 1),
                      selectInput("select_palette_TrainModel", "Select Feature Selection Method",
                                  choices = c("Please select..." = "",
                                              "None" = "None",
                                              "Chi_square_Test" = "Chi_square_Test",
                                              "ANOVA_Test" = "ANOVA_Test",
                                              "F_Test" = "F_Test",
                                              "Mutual_Information_Method" = "Mutual_Information_Method",
                                              "Fisher_Exact_Test" = "Fisher_Exact_Test",
                                              "Pearson_Correlation_Test" = "Pearson_Correlation_Test",
                                              "Variance_Test" = "Variance_Test"),
                                  selected = "Chi_square_Test"),
                      numericInput("p_selector", "Select Training Set Proportion", value = 0.8, min = 0.3, max = 0.95, step = 0.05),
                      numericInput("cv_selector", "Select Number of Cross-Validation Folds", value = 10, min = 2, max = 10, step = 1),
                      selectInput("model_selector", "Select Machine Learning Model",
                                  choices = c("Please select..." = "",
                                              "KNN" = "KNN",
                                              "Random_Forest" = "Random_Forest",
                                              "XGBoost" = "XGBoost",
                                              "SVM" = "SVM"),
                                  selected = "KNN"),
                      actionButton("calculate_TrainModel", "Calculate", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                    ),
                    tags$div(
                      style = "background-color: #f2f2f2; padding: 10px; border-radius: 10px; box-shadow: none; margin-top: 20px;",
                      div(
                        pickerInput("image_format_TrainModel", "Select Image Format",
                                    choices = c("PDF", "TIFF", "PNG", "JPEG"),
                                    selected = c("PDF", "TIFF", "PNG", "JPEG"),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE)),
                        pickerInput("model_format_TrainModel", "Select Model Format",
                                    choices = c("RDS"),
                                    selected = "RDS",
                                    multiple = FALSE,
                                    options = list(`actions-box` = TRUE)),
                        downloadButton("downloadZip_TrainModel", "Download ZIP", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                      )
                    ),
                    tags$script(HTML(
                      "Shiny.addCustomMessageHandler('updateSelectedRows', function(selectedIndices) {
                        var checkboxes = $('.dt-checkbox');
                        checkboxes.prop('checked', false);
                        selectedIndices.forEach(function(index) {
                          checkboxes.filter('[value=' + index + ']').prop('checked', true);
                        });
                        checkboxes.trigger('change');
                      });"
                    ))
             ),
             useShinyjs(),
             column(width = 8,
                    uiOutput("conditionalUI_TrainModel"),
                    plotOutput("plot_TrainModel"),
                    verbatimTextOutput("confusion_matrix_output"),
                    div(id = "radioButtonsContainer", style = "display: flex; align-items: center; justify-content: space-between;",
                        column(width = 4, class = "text-center",
                               actionButton("recalculate_TrainModel", "ReCalculate", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;"),
                               div(id = "cover_TrainModel", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 4, class = "text-center",
                               downloadButton("downloadTable_TrainModel", "DownloadTable", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;"),
                               div(id = "cover_TrainModel3", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 4,
                               radioButtons("data_view_TrainModel", label = "Display:",
                                            choices = c("Head" = "head", "All" = "all"),
                                            selected = "all"),
                               div(id = "cover_TrainModel2", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        #div(id = "cover_TrainModel", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")
                    ),
                    div(style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                        column(width = 2,
                               tags$span("Top"),
                               div(id = "cover_TrainModel4", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 4,
                               sliderInput("top_percentage", label = NULL, min = 1, max = 100, value = 10, step = 1, post = "%", width = "200px"),
                               div(id = "cover_TrainModel5", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 2,
                               tags$span("or"),
                               div(id = "cover_TrainModel6", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                        column(width = 4,
                               numericInput("top_number", label = NULL, value = 10, min = 1, step = 1, width = "100px"),
                               div(id = "cover_TrainModel7", style = "position: absolute; background-color: white; top: 0; left: 0; right: 0; bottom: 0; z-index: 100;")),
                    ),
                    DTOutput("ResultsTable_TrainModel")
             )
           )
  )
}
TrainModel_server <- function(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata) {
  values <- reactiveValues(showText = TRUE, showDataViewOptions = FALSE)
  recalculateresult <- reactiveValues(recalculate1 = NULL)
  rv_TrainModel <- reactiveValues(breed1 = NULL, global_color_assigned = FALSE)
  rvdown_TrainModel <- reactiveValues(plot_TrainModel = NULL)
  rvResults_TrainModel <- reactiveValues(data = NULL)
  rvmodelsave <- reactiveValues(data = NULL)
  rvsnpsave <- reactiveValues(data = NULL)
  rvpalette_TrainModel <- reactiveValues(selected_palette = NULL)
  rv_significant_data <- reactiveValues(data = NULL)
  rv_merged_calculateclass_select_selectdata <- reactiveValues(data = NULL)
  observeEvent(input$refresh_TrainModel, {
    updateSelectInput(session, "data_type_TrainModel", selected = "")
    if (!rv_TrainModel$global_color_assigned) {
      updateSelectInput(session, "color_palette_TrainModel", selected = "RdYlBu")
    }
    rv_TrainModel$global_color_assigned <- FALSE
  })
  observeEvent(input$data_type_TrainModel, {
    output$classification_data_status_TrainModel <- renderUI({})
    if (input$data_type_TrainModel == "Raw Data") {
      if (!is.null(rvdataclass$classification_data) && nrow(rvdataclass$classification_data) > 0) {
        class1_TrainModel <- rvdataclass$classification_data
        rv_TrainModel$breed1 <- data.frame(breed = unique(class1_TrainModel$breed))
        breedcount <- nrow(rv_TrainModel$breed1)
        observeEvent(input$color_palette_TrainModel, {
          if (!is.null(input$color_palette_TrainModel)) {
            max_colors <- brewer.pal.info[input$color_palette_TrainModel, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette_TrainModel)
            colors <- colorRampPalette(palette)(breedcount)
            rv_TrainModel$breed1$color <- colors
            rv_TrainModel$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status_TrainModel <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    } else if (input$data_type_TrainModel == "PCA Data") {
      if (!is.null(rvpcaresultdata$pcaresult_data) && nrow(rvpcaresultdata$pcaresult_data) > 0) {
        class1_TrainModel <- rvdataclass$classification_data
        rv_TrainModel$breed1 <- data.frame(breed = unique(class1_TrainModel$breed))
        breedcount <- nrow(rv_TrainModel$breed1)
        observeEvent(input$color_palette_TrainModel, {
          if (!is.null(input$color_palette_TrainModel)) {
            max_colors <- brewer.pal.info[input$color_palette_TrainModel, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette_TrainModel)
            colors <- colorRampPalette(palette)(breedcount)
            rv_TrainModel$breed1$color <- colors
            rv_TrainModel$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status_TrainModel <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    } else if (input$data_type_TrainModel == "Tree Data") {
      if (!is.null(rvtreeresultdata$treeresult_data) && nrow(rvtreeresultdata$treeresult_data) > 0) {
        class1_TrainModel <- rvdataclass$classification_data
        rv_TrainModel$breed1 <- data.frame(breed = unique(class1_TrainModel$breed))
        breedcount <- nrow(rv_TrainModel$breed1)
        observeEvent(input$color_palette_TrainModel, {
          if (!is.null(input$color_palette_TrainModel)) {
            max_colors <- brewer.pal.info[input$color_palette_TrainModel, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette_TrainModel)
            colors <- colorRampPalette(palette)(breedcount)
            rv_TrainModel$breed1$color <- colors
            rv_TrainModel$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status_TrainModel <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    }
    observe({
      if (!is.null(rvdataclass$classification_data) && nrow(rvdataclass$classification_data) > 0) {
        if (input$data_type_TrainModel == "PCA Data") {
          pcadataresult <- rvpcaresultdata$pcaresult_data
          pcadataresult_class <- rvdataclass$classification_data
          pcadataresult_class <- pcadataresult_class[pcadataresult_class$ID %in% pcadataresult$ID, ]
          breedChoices <- unique(pcadataresult_class$breed)
        } else if (input$data_type_TrainModel == "Tree Data") {
          treedataresult <- rvtreeresultdata$treeresult_data
          treedataresult_class <- rvdataclass$classification_data
          treedataresult_class <- treedataresult_class[treedataresult_class$ID %in% treedataresult$ID, ]
          breedChoices <- unique(treedataresult_class$breed)
        }  else {
          breedChoices <- unique(rvdataclass$classification_data$breed)
        }
        updatePickerInput(session, "selected_breeds_TrainModel",
                          choices = breedChoices,
                          selected = breedChoices)
      }
    })
  })
  observeEvent(input$calculate_TrainModel, {
    withProgress(message = 'Calculating...', value = 0, {
      setProgress(value = 0.01)
      if (!is.null(rv_TrainModel$breed1) && nrow(rv_TrainModel$breed1) > 0) {
        class1_TrainModel <- rvdataclass$classification_data
        rv_TrainModel$breed1 <- data.frame(breed = unique(class1_TrainModel$breed))
        breedcount <- nrow(rv_TrainModel$breed1)
        max_colors <- brewer.pal.info[input$color_palette_TrainModel, "maxcolors"]
        palette <- brewer.pal(max_colors, input$color_palette_TrainModel)
        colors <- colorRampPalette(palette)(breedcount)
        rv_TrainModel$breed1$color <- colors
        calculate1 <- rv_TrainModel$breed1
        calculateclass1 <- rvdataclass$classification_data
        merged_calculateclass <- merge(calculateclass1, calculate1, by = "breed")
        setProgress(value = 0.1)
        if (is.null(input$selected_breeds_TrainModel) || length(input$selected_breeds_TrainModel) == 0) {
          showNotification("Please select at least one breed before calculating.", type = "error")
          return()
        }
        merged_calculateclass_select <- merged_calculateclass[merged_calculateclass$breed %in% input$selected_breeds_TrainModel, ]
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
          if (input$data_type_TrainModel == "PCA Data") {
            pcadataresult <- rvpcaresultdata$pcaresult_data
            calculate_transposed <- calculate_transposed[calculate_transposed$IID %in% pcadataresult$ID, ]
            merged_calculateclass_select_selectdata <- merged_calculateclass_select[merged_calculateclass_select$ID %in% pcadataresult$ID, ]
          } else if (input$data_type_TrainModel == "Tree Data") {
            treedataresult <- rvtreeresultdata$treeresult_data
            calculate_transposed <- calculate_transposed[calculate_transposed$IID %in% treedataresult$ID, ]
            merged_calculateclass_select_selectdata <- merged_calculateclass_select[merged_calculateclass_select$ID %in% treedataresult$ID, ]
          } else {
            merged_calculateclass_select_selectdata <- merged_calculateclass_select
          }
          rv_merged_calculateclass_select_selectdata$data <- merged_calculateclass_select_selectdata
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
          matched_breeds <- merged_calculateclass_select_selectdata[merged_calculateclass_select_selectdata$ID %in% original_row_names, "breed"]
          matched_data <- data.frame(class = calculate_transposed_numeric[original_row_names, ], breed = matched_breeds)
          column_names <- colnames(calculate_transposed_numeric)
          column_names <- append(column_names, "class")
          colnames(matched_data) <- column_names
          setDT(matched_data)
          map <- setNames(c(0), c("NA"))
          matched_data[] <- lapply(matched_data, function(x) {
            key <- ifelse(is.na(x), "NA", as.character(x))
            ifelse(key %in% names(map), map[key], x)
          })
          no_cores <- detectCores() - 1
          cl <- makeCluster(no_cores)
          clusterExport(cl, varlist = c("matched_data"), envir = environment())
          selected_palette <- input$select_palette_TrainModel
          if (selected_palette == "None") {
            significant_data <- matched_data
          } else {
            if (rvpalette_TrainModel$selected_palette == "Chi_square_Test") {
              p_values <- parLapply(cl, 1:(ncol(matched_data) - 1), function(i) {
                tbl <- table(matched_data[[i]], matched_data$class)
                test_result <- tryCatch({
                  test <- chisq.test(tbl)
                  test$p.value
                }, warning = function(w) {
                  NA
                }, error = function(e) {
                  NA
                }, silent = TRUE)
                test_result
              })
            } else if (rvpalette_TrainModel$selected_palette == "ANOVA_Test") {
              p_values <- parLapply(cl, 1:(ncol(matched_data) - 1), function(i) {
                test_result <- tryCatch({
                  test <- aov(matched_data[[i]] ~ matched_data$class)
                  summary(test)[[1]]$`Pr(>F)`[1]
                }, warning = function(w) {
                  NA
                }, error = function(e) {
                  NA
                }, silent = TRUE)
                test_result
              })
            } else if (rvpalette_TrainModel$selected_palette == "F_Test") {
              p_values <- parLapply(cl, 1:(ncol(matched_data) - 1), function(i) {
                test_result <- tryCatch({
                  test <- var.test(matched_data[[i]] ~ matched_data$class)
                  test$p.value
                }, warning = function(w) {
                  NA
                }, error = function(e) {
                  NA
                }, silent = TRUE)
                test_result
              })
            } else if (rvpalette_TrainModel$selected_palette == "Mutual_Information_Method") {
              p_values <- parLapply(cl, 1:(ncol(matched_data) - 1), function(i) {
                test_result <- tryCatch({
                  mi_result <- mutualInformation(matched_data[[i]], matched_data$class)
                  p_value <- mi_result$p.value
                  p_value
                }, warning = function(w) {
                  NA
                }, error = function(e) {
                  NA
                }, silent = TRUE)
                test_result
              })
            } else if (rvpalette_TrainModel$selected_palette == "Fisher_Exact_Test") {
              p_values <- parLapply(cl, 1:(ncol(matched_data) - 1), function(i) {
                tbl <- table(matched_data[[i]], matched_data$class)
                test_result <- tryCatch({
                  test <- fisher.test(tbl)
                  test$p.value
                }, warning = function(w) {
                  NA
                }, error = function(e) {
                  NA
                }, silent = TRUE)
                test_result
              })
            } else if (rvpalette_TrainModel$selected_palette == "Pearson_Correlation_Test") {
              p_values <- parLapply(cl, 1:(ncol(matched_data) - 1), function(i) {
                test_result <- tryCatch({
                  test <- cor.test(matched_data[[i]], matched_data$class, method = "pearson")
                  test$p.value
                }, warning = function(w) {
                  NA
                }, error = function(e) {
                  NA
                }, silent = TRUE)
                test_result
              })
            } else if (rvpalette_TrainModel$selected_palette == "Variance_Test") {
              p_values <- parLapply(cl, 1:(ncol(matched_data) - 1), function(i) {
                test_result <- tryCatch({
                  test <- bartlett.test(matched_data[[i]], matched_data$class)
                  test$p.value
                }, warning = function(w) {
                  NA
                }, error = function(e) {
                  NA
                }, silent = TRUE)
                test_result
              })
            } else {
              return()
            }
            stopCluster(cl)
            setProgress(value = 0.7)
            p_values <- unlist(p_values)
            feature_names <- names(matched_data)[1:(ncol(matched_data) - 1)]
            sorted_indices <- order(p_values, na.last = NA)
            sorted_feature_names <- feature_names[sorted_indices]
            sorted_p_values <- p_values[sorted_indices]
            significant_features <- sorted_feature_names[sorted_p_values < 0.05]
            min_features <- 500
            max_features <- 5000
            if (length(significant_features) < min_features) {
              additional_features <- sorted_feature_names[1:min_features]
              significant_features <- unique(c(significant_features, additional_features))
            }
            if (length(significant_features) > max_features) {
              significant_features <- significant_features[1:max_features]
            }
            significant_features <- c(significant_features, "class")
            significant_features <- significant_features[!is.na(significant_features)]
            significant_features <- significant_features[significant_features %in% colnames(matched_data)]
            if (length(significant_features) < min_features && length(feature_names) >= min_features) {
              remaining_features <- setdiff(feature_names, significant_features)
              if (length(remaining_features) >= (min_features - length(significant_features))) {
                additional_features <- sample(remaining_features, min_features - length(significant_features))
              } else {
                additional_features <- remaining_features
              }
              significant_features <- unique(c(significant_features, additional_features))
            }
            if (length(significant_features) == 0) {
              stop("No significant features found in matched_data.")
            }
            significant_data <- matched_data[, ..significant_features, drop = FALSE]
          }
          rv_significant_data$data <- significant_data
          setProgress(value = 0.75)
          seed <- input$seed_selector_TrainModel
          set.seed(seed)
          p <- input$p_selector
          index <- createDataPartition(significant_data$class, p = p, list = TRUE)
          train_data <- significant_data[index$Resample1, ]
          test_data <- significant_data[-index$Resample1, ]
          train_data$class <- as.factor(train_data$class)
          test_data$class <- as.factor(test_data$class)
          train_data <- as.data.frame(train_data)
          x <- train_data[, -which(names(train_data) == "class")]
          y <- train_data$class
          setProgress(value = 0.8)
          selected_model <- input$model_selector
          if (selected_model == "KNN") {
            number_cv = input$cv_selector
            control_knn <- trainControl(method = "cv", number = number_cv, savePredictions = "final")
            tuneGrid_knn <- expand.grid(k = seq(1, 20, by = 2))
            knn_model <- train(x = x, y = y, method = "knn", trControl = control_knn, tuneGrid = tuneGrid_knn)
            predictions <- predict(knn_model, test_data, type = "prob")
            class_levels <- levels(test_data$class)
            n_classes <- length(class_levels)
            fpr <- list()
            tpr <- list()
            roc_auc <- vector("numeric", n_classes)
            knn_importance <- varImp(knn_model, scale = FALSE)
            knn_importance_table <- as.data.frame(knn_importance$importance)
            knn_importance_table <- cbind(Feature = rownames(knn_importance_table), Importance = rowMeans(knn_importance_table))
            rownames(knn_importance_table) <- NULL
            knn_importance_table <- as.data.frame(knn_importance_table)
            importance_table <- knn_importance_table
            selected_features <- importance_table$Feature
            selected_features <- c(selected_features, "class")
            selected_data_model <- significant_data[, ..selected_features]
            predictions_knn <- factor(predict(knn_model, test_data), levels = levels(test_data$class))
            confMat_knn <- confusionMatrix(predictions_knn, test_data$class)
            confMat_model <- confMat_knn
            model_to_save <- knn_model
          } else if (selected_model == "Random_Forest") {
            number_cv <- input$cv_selector
            control_rf <- trainControl(method = "cv", number = number_cv, savePredictions = "final", classProbs = TRUE)
            tuneGrid_rf <- expand.grid(
              mtry = c(1, floor(sqrt(ncol(train_data) - 1)), ncol(train_data) - 1),
              splitrule = c("gini", "extratrees"),
              min.node.size = c(1, 5, 10)
            )
            rf_model <- train(x = x, y = y, method = "ranger", trControl = control_rf, tuneGrid = tuneGrid_rf, importance = 'impurity')
            predictions <- predict(rf_model, test_data, type = "prob")
            if (is.data.frame(predictions)) {
              #print("Predictions are returned as a data frame with class probabilities.")
            } else {
              #print("Error in prediction results: not a data frame.")
            }
            class_levels <- levels(test_data$class)
            n_classes <- length(class_levels)
            fpr <- list()
            tpr <- list()
            roc_auc <- vector("numeric", n_classes)
            rf_importance <- varImp(rf_model, scale = FALSE)
            rf_importance_table <- as.data.frame(rf_importance$importance)
            rf_importance_table <- cbind(Feature = rownames(rf_importance_table), Importance = rf_importance_table$Overall)
            rownames(rf_importance_table) <- NULL
            rf_importance_table <- as.data.frame(rf_importance_table)
            rf_importance_table$Feature <- gsub("`", "", rf_importance_table$Feature)
            importance_table <- rf_importance_table
            selected_features <- importance_table$Feature
            selected_features <- c(selected_features, "class")
            selected_data_model <- significant_data[, ..selected_features]
            predictions_rf <- factor(predict(rf_model, test_data), levels = levels(test_data$class))
            confMat_rf <- confusionMatrix(predictions_rf, test_data$class)
            confMat_model <- confMat_rf
            model_to_save <- rf_model
          } else if (selected_model == "XGBoost") {
            number_cv <- input$cv_selector
            control_xgb <- trainControl(method = "cv", number = number_cv, savePredictions = "final", classProbs = TRUE)
            tuneGrid_xgb <- expand.grid(
              nrounds = c(100, 200),
              max_depth = c(3, 6, 9),
              eta = c(0.01, 0.05, 0.1),
              gamma = c(0, 0.1, 0.2),
              colsample_bytree = c(0.5, 0.75, 1),
              min_child_weight = c(1, 2, 3),
              subsample = c(0.5, 0.75, 1)
            )
            xgb_model <- train(x = x, y = y, method = "xgbTree", trControl = control_xgb, tuneGrid = tuneGrid_xgb, verbosity = 0)
            predictions <- predict(xgb_model, test_data, type = "prob")
            class_levels <- levels(test_data$class)
            n_classes <- length(class_levels)
            fpr <- list()
            tpr <- list()
            roc_auc <- vector("numeric", n_classes)
            xgb_importance <- xgb.importance(model = xgb_model$finalModel)
            xgb_importance_table <- xgb_importance[, c("Feature", "Gain")]
            colnames(xgb_importance_table) <- c("Feature", "Importance")
            xgb_importance_table$Feature <- gsub("`", "", xgb_importance_table$Feature)
            importance_table <- xgb_importance_table
            selected_features <- importance_table$Feature
            selected_features <- c(selected_features, "class")
            selected_data_model <- significant_data[, ..selected_features]
            predictions_xgb <- factor(predict(xgb_model, test_data), levels = levels(test_data$class))
            confMat_xgb <- confusionMatrix(predictions_xgb, test_data$class)
            confMat_model <- confMat_xgb
            model_to_save <- xgb_model
          } else if (selected_model == "SVM") {
            number_cv <- input$cv_selector
            control_svm <- trainControl(method = "cv", number = number_cv, savePredictions = "final", classProbs = TRUE)
            svm_model <- train(x = x, y = y, method = "knn", trControl = control_svm)
            predictions <- predict(svm_model, test_data, type = "prob")
            class_levels <- levels(test_data$class)
            n_classes <- length(class_levels)
            fpr <- list()
            tpr <- list()
            roc_auc <- vector("numeric", n_classes)
            svm_importance <- varImp(svm_model, scale = FALSE)
            svm_importance_table <- as.data.frame(svm_importance$importance)
            svm_importance_table <- cbind(Feature = rownames(svm_importance_table), svm_importance_table)
            rownames(svm_importance_table) <- NULL
            svm_importance_table$Importance <- rowMeans(svm_importance_table[, setdiff(names(svm_importance_table), "Feature")], na.rm = TRUE)
            svm_importance_table <- svm_importance_table[, c("Feature", "Importance")]
            svm_importance_table$Feature <- as.character(svm_importance_table$Feature)
            svm_importance_table$Importance <- as.numeric(svm_importance_table$Importance)
            importance_table <- svm_importance_table
            importance_table <- as.data.frame(importance_table)
            feature_vector <- as.character(importance_table$Feature)
            selected_features <- feature_vector
            selected_features <- c(selected_features, "class")
            selected_data_model <- significant_data[, ..selected_features]
            predictions_svm <- factor(predict(svm_model, test_data), levels = levels(test_data$class))
            confMat_svm <- confusionMatrix(predictions_svm, test_data$class)
            confMat_model <- confMat_svm
            model_to_save <- svm_model
          }
          rvmodelsave$data <- model_to_save
          colors <- unique(merged_calculateclass_select_selectdata$color)
          setProgress(value = 0.85)
          plot_TrainModel <- ggplot() +
            xlim(c(0, 1)) + ylim(c(0, 1)) +
            xlab("False Positive Rate") + ylab("True Positive Rate") +
            ggtitle("ROC Curves by Class") +
            theme_minimal()
          for(i in 1:n_classes) {
            test_class_binary <- ifelse(test_data$class == class_levels[i], "yes", "no")
            pred <- prediction(predictions[, i], test_class_binary)
            perf <- performance(pred, "tpr", "fpr")
            fpr <- perf@x.values[[1]]
            tpr <- perf@y.values[[1]]
            roc_auc[i] <- performance(pred, measure = "auc")@y.values[[1]]
            roc_data <- data.frame(fpr = fpr, tpr = tpr, class = factor(class_levels[i]))
            plot_TrainModel <- plot_TrainModel + geom_line(data = roc_data, aes(x = fpr, y = tpr, color = class), linewidth = 2)
          }
          plot_TrainModel <- plot_TrainModel + scale_color_manual(values = colors, name = "Class")
          plot_TrainModel <- plot_TrainModel + theme(plot.title = element_text(hjust = 0.5))
          #total_lengths_df <- data.frame(ID = phylo_tree$tip.label, total_length = total_lengths)
          values$showDataViewOptions <- TRUE
          rvResults_TrainModel$data <- importance_table
          rvsnpsave$data <- importance_table
          updateNumericInput(session, "top_number", max = nrow(rvResults_TrainModel$data), value = min(input$top_number, nrow(rvResults_TrainModel$data)))
          rvdown_TrainModel$plot_TrainModel <- plot_TrainModel
          output$plot_TrainModel <- renderPlot({ plot_TrainModel })
          output$confusion_matrix_output <- renderPrint({
            print(confMat_model)
          })
          setProgress(value = 0.95)
          values$showText <- FALSE
          outputOptions(output, "showText", suspendWhenHidden = FALSE)
          setProgress(value = 1)
        } else {
          output$classification_data_status_TrainModel <- renderUI({
            tags$p("Please upload and process genotype data before calculating.", style = "color: red;")
          })
        }
      } else {
        output$classification_data_status_TrainModel <- renderUI({
          tags$p("Please upload and process classification data before calculating.", style = "color: red;")
        })
      }
    })
  })
  observeEvent(input$recalculate_TrainModel, {
    withProgress(message = 'Calculating...', value = 0, {
      setProgress(value = 0.01)
      if (!is.null(rv_TrainModel$breed1) && nrow(rv_TrainModel$breed1) > 0) {
        class1_TrainModel <- rvdataclass$classification_data
        rv_TrainModel$breed1 <- data.frame(breed = unique(class1_TrainModel$breed))
        breedcount <- nrow(rv_TrainModel$breed1)
        max_colors <- brewer.pal.info[input$color_palette_TrainModel, "maxcolors"]
        palette <- brewer.pal(max_colors, input$color_palette_TrainModel)
        colors <- colorRampPalette(palette)(breedcount)
        rv_TrainModel$breed1$color <- colors
        calculate1 <- rv_TrainModel$breed1
        calculateclass1 <- rvdataclass$classification_data
        merged_calculateclass <- merge(calculateclass1, calculate1, by = "breed")
        setProgress(value = 0.1)
        if (is.null(input$selected_breeds_TrainModel) || length(input$selected_breeds_TrainModel) == 0) {
          showNotification("Please select at least one breed before calculating.", type = "error")
          return()
        }
        merged_calculateclass_select_old <- merged_calculateclass[merged_calculateclass$breed %in% input$selected_breeds_TrainModel, ]
        selected_recalculate_ids <- recalculateresult$recalculate1$Feature
        setProgress(value = 0.2)
        selected_features <- c(selected_recalculate_ids, "class")
        significant_data_old <- rv_significant_data$data
        setProgress(value = 0.3)
        significant_data <- significant_data_old[, ..selected_features]
        setProgress(value = 0.4)
        seed <- input$seed_selector_TrainModel
        set.seed(seed)
        p <- input$p_selector
        index <- createDataPartition(significant_data$class, p = p, list = TRUE)
        setProgress(value = 0.5)
        train_data <- significant_data[index$Resample1, ]
        test_data <- significant_data[-index$Resample1, ]
        train_data$class <- as.factor(train_data$class)
        test_data$class <- as.factor(test_data$class)
        train_data <- as.data.frame(train_data)
        x <- train_data[, -which(names(train_data) == "class")]
        y <- train_data$class
        setProgress(value = 0.6)
        selected_model <- input$model_selector
        if (selected_model == "KNN") {
          number_cv = input$cv_selector
          control_knn <- trainControl(method = "cv", number = number_cv, savePredictions = "final")
          tuneGrid_knn <- expand.grid(k = seq(1, 20, by = 2))
          knn_model <- train(x = x, y = y, method = "knn", trControl = control_knn, tuneGrid = tuneGrid_knn)
          predictions <- predict(knn_model, test_data, type = "prob")
          class_levels <- levels(test_data$class)
          n_classes <- length(class_levels)
          fpr <- list()
          tpr <- list()
          roc_auc <- vector("numeric", n_classes)
          knn_importance <- varImp(knn_model, scale = FALSE)
          knn_importance_table <- as.data.frame(knn_importance$importance)
          knn_importance_table <- cbind(Feature = rownames(knn_importance_table), Importance = rowMeans(knn_importance_table))
          rownames(knn_importance_table) <- NULL
          knn_importance_table <- as.data.frame(knn_importance_table)
          importance_table <- knn_importance_table
          selected_features <- importance_table$Feature
          selected_features <- c(selected_features, "class")
          selected_data_model <- significant_data[, ..selected_features]
          predictions_knn <- factor(predict(knn_model, test_data), levels = levels(test_data$class))
          confMat_knn <- confusionMatrix(predictions_knn, test_data$class)
          confMat_model <- confMat_knn
          model_to_save <- knn_model
        } else if (selected_model == "Random_Forest") {
          number_cv <- input$cv_selector
          control_rf <- trainControl(method = "cv", number = number_cv, savePredictions = "final", classProbs = TRUE)
          tuneGrid_rf <- expand.grid(
            mtry = c(1, floor(sqrt(ncol(train_data) - 1)), ncol(train_data) - 1),
            splitrule = c("gini", "extratrees"),
            min.node.size = c(1, 5, 10)
          )
          rf_model <- train(x = x, y = y, method = "ranger", trControl = control_rf, tuneGrid = tuneGrid_rf, importance = 'impurity')
          predictions <- predict(rf_model, test_data, type = "prob")
          if (is.data.frame(predictions)) {
            #print("Predictions are returned as a data frame with class probabilities.")
          } else {
            #print("Error in prediction results: not a data frame.")
          }
          class_levels <- levels(test_data$class)
          n_classes <- length(class_levels)
          fpr <- list()
          tpr <- list()
          roc_auc <- vector("numeric", n_classes)
          rf_importance <- varImp(rf_model, scale = FALSE)
          rf_importance_table <- as.data.frame(rf_importance$importance)
          rf_importance_table <- cbind(Feature = rownames(rf_importance_table), Importance = rf_importance_table$Overall)
          rownames(rf_importance_table) <- NULL
          rf_importance_table <- as.data.frame(rf_importance_table)
          rf_importance_table$Feature <- gsub("`", "", rf_importance_table$Feature)
          importance_table <- rf_importance_table
          selected_features <- importance_table$Feature
          selected_features <- c(selected_features, "class")
          selected_data_model <- significant_data[, ..selected_features]
          predictions_rf <- factor(predict(rf_model, test_data), levels = levels(test_data$class))
          confMat_rf <- confusionMatrix(predictions_rf, test_data$class)
          confMat_model <- confMat_rf
          model_to_save <- rf_model
        } else if (selected_model == "XGBoost") {
          number_cv <- input$cv_selector
          control_xgb <- trainControl(method = "cv", number = number_cv, savePredictions = "final", classProbs = TRUE)
          tuneGrid_xgb <- expand.grid(
            nrounds = c(100, 200),
            max_depth = c(3, 6, 9),
            eta = c(0.01, 0.05, 0.1),
            gamma = c(0, 0.1, 0.2),
            colsample_bytree = c(0.5, 0.75, 1),
            min_child_weight = c(1, 2, 3),
            subsample = c(0.5, 0.75, 1)
          )
          xgb_model <- train(x = x, y = y, method = "xgbTree", trControl = control_xgb, tuneGrid = tuneGrid_xgb, verbosity = 0)
          predictions <- predict(xgb_model, test_data, type = "prob")
          class_levels <- levels(test_data$class)
          n_classes <- length(class_levels)
          fpr <- list()
          tpr <- list()
          roc_auc <- vector("numeric", n_classes)
          xgb_importance <- xgb.importance(model = xgb_model$finalModel)
          xgb_importance_table <- xgb_importance[, c("Feature", "Gain")]
          colnames(xgb_importance_table) <- c("Feature", "Importance")
          xgb_importance_table$Feature <- gsub("`", "", xgb_importance_table$Feature)
          importance_table <- xgb_importance_table
          selected_features <- importance_table$Feature
          selected_features <- c(selected_features, "class")
          selected_data_model <- significant_data[, ..selected_features]
          predictions_xgb <- factor(predict(xgb_model, test_data), levels = levels(test_data$class))
          confMat_xgb <- confusionMatrix(predictions_xgb, test_data$class)
          confMat_model <- confMat_xgb
          model_to_save <- xgb_model
        } else if (selected_model == "SVM") {
          number_cv <- input$cv_selector
          control_svm <- trainControl(method = "cv", number = number_cv, savePredictions = "final", classProbs = TRUE)
          svm_model <- train(x = x, y = y, method = "knn", trControl = control_svm)
          predictions <- predict(svm_model, test_data, type = "prob")
          class_levels <- levels(test_data$class)
          n_classes <- length(class_levels)
          fpr <- list()
          tpr <- list()
          roc_auc <- vector("numeric", n_classes)
          svm_importance <- varImp(svm_model, scale = FALSE)
          svm_importance_table <- as.data.frame(svm_importance$importance)
          svm_importance_table <- cbind(Feature = rownames(svm_importance_table), svm_importance_table)
          rownames(svm_importance_table) <- NULL
          svm_importance_table$Importance <- rowMeans(svm_importance_table[, setdiff(names(svm_importance_table), "Feature")], na.rm = TRUE)
          svm_importance_table <- svm_importance_table[, c("Feature", "Importance")]
          svm_importance_table$Feature <- as.character(svm_importance_table$Feature)
          svm_importance_table$Importance <- as.numeric(svm_importance_table$Importance)
          importance_table <- svm_importance_table
          importance_table <- as.data.frame(importance_table)
          feature_vector <- as.character(importance_table$Feature)
          selected_features <- feature_vector
          selected_features <- c(selected_features, "class")
          selected_data_model <- significant_data[, ..selected_features]
          predictions_svm <- factor(predict(svm_model, test_data), levels = levels(test_data$class))
          confMat_svm <- confusionMatrix(predictions_svm, test_data$class)
          confMat_model <- confMat_svm
          model_to_save <- svm_model
        }
        setProgress(value = 0.7)
        rvmodelsave$data <- model_to_save
        merged_calculateclass_select_selectdata <- rv_merged_calculateclass_select_selectdata$data
        colors <- unique(merged_calculateclass_select_selectdata$color)
        setProgress(value = 0.8)
        plot_TrainModel <- ggplot() +
          xlim(c(0, 1)) + ylim(c(0, 1)) +
          xlab("False Positive Rate") + ylab("True Positive Rate") +
          ggtitle("ROC Curves by Class") +
          theme_minimal()
        for(i in 1:n_classes) {
          test_class_binary <- ifelse(test_data$class == class_levels[i], "yes", "no")
          pred <- prediction(predictions[, i], test_class_binary)
          perf <- performance(pred, "tpr", "fpr")
          fpr <- perf@x.values[[1]]
          tpr <- perf@y.values[[1]]
          roc_auc[i] <- performance(pred, measure = "auc")@y.values[[1]]
          roc_data <- data.frame(fpr = fpr, tpr = tpr, class = factor(class_levels[i]))
          plot_TrainModel <- plot_TrainModel + geom_line(data = roc_data, aes(x = fpr, y = tpr, color = class), linewidth = 2)
        }
        plot_TrainModel <- plot_TrainModel + scale_color_manual(values = colors, name = "Class")
        plot_TrainModel <- plot_TrainModel + theme(plot.title = element_text(hjust = 0.5))
        rvsnpsave$data <- importance_table
        if (!is.null(rvdatageno$genotype_data) && nrow(rvdatageno$genotype_data) > 0) {
          setProgress(value = 0.85)
          rvdown_TrainModel$plot_TrainModel <- plot_TrainModel
          setProgress(value = 0.9)
          output$plot_TrainModel <- renderPlot({ plot_TrainModel })
          output$confusion_matrix_output <- renderPrint({
            print(confMat_model)
          })
          setProgress(value = 0.95)
          values$showText <- FALSE
          outputOptions(output, "showText", suspendWhenHidden = FALSE)
          setProgress(value = 1)
        } else {
          output$classification_data_status_TrainModel <- renderUI({
            tags$p("Please upload and process genotype data before calculating.", style = "color: red;")
          })
        }
      } else {
        output$classification_data_status_TrainModel <- renderUI({
          tags$p("Please upload and process classification data before calculating.", style = "color: red;")
        })
      }
    })
  })
  observeEvent(input$select_palette_TrainModel, {
    selected <- input$select_palette_TrainModel
    if (selected != "") {
      rvpalette_TrainModel$selected_palette <- switch(selected,
                                                      "None" = "None",
                                                      "Chi_square_Test" = "Chi_square_Test",
                                                      "ANOVA_Test" = "ANOVA_Test",
                                                      "F_Test" = "F_Test",
                                                      "Mutual_Information_Method" = "Mutual_Information_Method",
                                                      "
Fisher_Exact_Test" = "Fisher_Exact_Test",
                                                      "
Pearson_Correlation_Test" = "Pearson_Correlation_Test",
                                                      "Variance_Test" = "Variance_Test")
    }
  })
  output$conditionalUI_TrainModel <- renderUI({
    if (values$showText) {
      conditionalPanel(
        condition = "true",
        h1("TrainModel Analysis"),
        p("Perform TrainModel analysis here and display the results.")
      )
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_TrainModel")
    } else {
      shinyjs::show("cover_TrainModel")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_TrainModel2")
    } else {
      shinyjs::show("cover_TrainModel2")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_TrainModel3")
    } else {
      shinyjs::show("cover_TrainModel3")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_TrainModel4")
    } else {
      shinyjs::show("cover_TrainModel4")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_TrainModel5")
    } else {
      shinyjs::show("cover_TrainModel5")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_TrainModel6")
    } else {
      shinyjs::show("cover_TrainModel6")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_TrainModel7")
    } else {
      shinyjs::show("cover_TrainModel7")
    }
  })
  observeEvent(input$top_percentage, {
    req(rvResults_TrainModel$data)
    total_rows <- nrow(rvResults_TrainModel$data)
    selected_percentage <- input$top_percentage
    selected_number <- ceiling(total_rows * (selected_percentage / 100))
    #cat("Total Rows:", total_rows, "Selected Percentage:", selected_percentage, "Selected Number:", selected_number, "\n")
    updateNumericInput(session, "top_number", value = selected_number)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$top_number, {
    req(rvResults_TrainModel$data)
    total_rows <- nrow(rvResults_TrainModel$data)
    selected_number <- input$top_number
    selected_percentage <- floor((selected_number / total_rows) * 100)
    #cat("Total Rows:", total_rows, "Selected Number:", selected_number, "Selected Percentage:", selected_percentage, "\n")
    updateSliderInput(session, "top_percentage", value = selected_percentage)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observe({
    req(rvResults_TrainModel$data)
    sorted_data <- rvResults_TrainModel$data[order(-as.numeric(rvResults_TrainModel$data[[2]])), ]
    top_rows <- head(sorted_data, input$top_number)
    #print(top_rows)
    selected_indices <- as.numeric(row.names(top_rows))
    #cat("Selected Indices:", selected_indices, "\n")
    session$sendCustomMessage("updateSelectedRows", selected_indices)
  })
  output$ResultsTable_TrainModel <- renderDT({
    req(rvResults_TrainModel$data)
    DataToShow_TrainModel <- rvResults_TrainModel$data
    if(input$data_view_TrainModel == "head") {
      DataToShow_TrainModel <- head(DataToShow_TrainModel, 10)
    }
    datatable(DataToShow_TrainModel, options = list(
      pageLength = -1,
      scrollX = TRUE,
      columnDefs = list(
        list(
          targets = 0,
          render = JS(
            "function(data, type, full, meta) {",
            "var checked = window.selectedRowsData_TrainModel && window.selectedRowsData_TrainModel.map(row => row[0]).includes(data) ? 'checked' : '';",
            "return '<input type=\"checkbox\" class=\"dt-checkbox\" '+checked+' value=\"'+data+'\">';",
            "}"
          ),
          title = '<input type="checkbox" id="check-all-TrainModel">',
          orderable = FALSE
        )
      ),
      rowCallback = JS(
        "function(row, data, index) {",
        "if (window.selectedRowsData_TrainModel && window.selectedRowsData_TrainModel.map(row => row[0]).includes(data[0])) {",
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
        "if (!window.selectedRowsData_TrainModel) {",
        "window.selectedRowsData_TrainModel = [];",
        "}",
        "$('#check-all-TrainModel').click(function() {",
        "var rows = settings.aoData;",
        "var currentPageData = rows.map(row => row._aData);",
        "if ($(this).is(':checked')) {",
        "currentPageData.forEach(function(rowData) {",
        "if (!window.selectedRowsData_TrainModel.some(row => row[0] === rowData[0])) {",
        "window.selectedRowsData_TrainModel.push(rowData);",
        "}",
        "});",
        "} else {",
        "currentPageData.forEach(function(rowData) {",
        "window.selectedRowsData_TrainModel = window.selectedRowsData_TrainModel.filter(row => row[0] !== rowData[0]);",
        "});",
        "}",
        "$('input.dt-checkbox').prop('checked', $(this).is(':checked'));",
        "Shiny.setInputValue('selectedRowsData_TrainModel', window.selectedRowsData_TrainModel);",
        "});",
        "$(settings.nTable).on('change', 'input.dt-checkbox', function() {",
        "var $checkbox = $(this);",
        "var rowIndex = $(this).closest('tr').index();",
        "var rowData = settings.aoData[rowIndex]._aData;",
        "if ($checkbox.is(':checked')) {",
        "if (!window.selectedRowsData_TrainModel.some(row => row[0] === rowData[0])) {",
        "window.selectedRowsData_TrainModel.push(rowData);",
        "}",
        "} else {",
        "window.selectedRowsData_TrainModel = window.selectedRowsData_TrainModel.filter(row => row[0] !== rowData[0]);",
        "}",
        "Shiny.setInputValue('selectedRowsData_TrainModel', window.selectedRowsData_TrainModel);",
        "});",
        "}"
      )
    ), escape = FALSE)
  }, server = TRUE)
  observeEvent(input$selectedRowsData_TrainModel, {
    if (length(input$selectedRowsData_TrainModel) %% 3 != 0) {
      #print("Input data format is incorrect")
      return()
    }
    matrix_data <- matrix(input$selectedRowsData_TrainModel, ncol = 3, byrow = TRUE)
    data_df <- as.data.frame(matrix_data, stringsAsFactors = FALSE)
    names(data_df) <- c("Index", "Feature", "Importance")
    recalculateresult$recalculate1 <- data_df
  })
  output$downloadTable_TrainModel <- downloadHandler(
    filename = function() {
      paste("TrainModel-Data-Table-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- recalculateresult$recalculate1
      if (is.null(data)) {
        return()
      }
      selected_data <- data[, c("Feature", "Importance")]
      write.csv(selected_data, file, row.names = FALSE)
    }
  )
  output$showText <- reactive({ values$showText })
  output$showText <- reactive({ values$showText })
  output$downloadZip_TrainModel <- downloadHandler(
    filename = function() {
      "TrainModel_results.zip"
    },
    content = function(file) {
      tempDir <- tempdir()
      formats <- input$image_format_TrainModel
      plotName <- "plot_TrainModel"
      filesToRemove <- list.files(tempDir, full.names = TRUE)
      if (length(filesToRemove) > 0) {
        suppressWarnings(file.remove(filesToRemove))
      }
      for (format in formats) {
        filePath <- file.path(tempDir, paste0(plotName, ".", tolower(format)))
        suppressMessages(ggsave(filePath, plot = rvdown_TrainModel$plot_TrainModel, device = tolower(format)))
      }
      pattern <- paste0("\\.(", paste(tolower(formats), collapse="|"), ")$")
      imageFiles <- list.files(tempDir, pattern = pattern, full.names = TRUE)
      if (!is.null(rvdatacloca$location_data) && nrow(rvdatacloca$location_data) > 0) {
        dataclocation <- rvdatacloca$location_data
      } else {
        dataclocation <- 0
      }
      model_to_save <- rvmodelsave$data
      snp_to_save <- rvsnpsave$data
      model_save <- list(
        model = model_to_save,
        data_location = dataclocation,
        snp_location = snp_to_save
      )
      modelDataFile <- file.path(tempDir, "model_save.rds")
      saveRDS(model_save, file = modelDataFile)
      allFiles <- c(imageFiles, modelDataFile)
      suppressMessages(zip(file, files = allFiles, flags = "-j"))
    },
    contentType = "application/zip"
  )
}
