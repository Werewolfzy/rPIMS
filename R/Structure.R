#' @export
Structure_ui <- function() {
  tabPanel("Structure",
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
                          selectInput("data_type_Structure", "Select Data", choices = c("Please select..." = "", "Raw Data" = "Raw Data", "PCA Data" = "PCA Data", "Tree Data" = "Tree Data")),
                          actionButton("refresh_Structure", label = icon("refresh"), style = "background-color: transparent; border: none; color: #337ab7; margin-left: 10px;")
                      ),
                      uiOutput("classification_data_status_Structure"),
                      selectInput("color_palette_Structure", "Select Color Palette",
                                  choices = c("Please select..." = "",
                                              "RdYlBu" = "RdYlBu",
                                              "Spectral" = "Spectral",
                                              "Paired" = "Paired",
                                              "Set3" = "Set3",
                                              "Dark2" = "Dark2",
                                              "Set1" = "Set1")),
                      uiOutput("color_palette_status_Structure"),
                      pickerInput(
                        inputId = "selected_breeds_Structure",
                        label = "Select Breeds",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")
                      ),
                      numericInput("min_k_selector", "Select Minimum K Value", value = 2, min = 1, max = 5, step = 1),
                      numericInput("max_k_selector", "Select Maximum K Value", value = 5, min = 5, max = 20, step = 1),
                      actionButton("calculate_Structure", "Calculate", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                    ),
                    tags$div(
                      style = "background-color: #f2f2f2; padding: 10px; border-radius: 10px; box-shadow: none; margin-top: 20px;",
                      div(
                        pickerInput("image_format_Structure", "Select Image Format",
                                    choices = c("PDF", "TIFF", "PNG", "JPEG"),
                                    selected = c("PDF", "TIFF", "PNG", "JPEG"),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE)),
                        downloadButton("downloadZip_Structure", "Download ZIP", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
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
                    uiOutput("conditionalUI_Structure"),
                    plotOutput("plot_Structure_cv_error"),
                    plotOutput("plot_Structure_plot_structure"),
             )
           )
  )
}

#' @export
Structure_server <- function(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata) {
  values <- reactiveValues(showText = TRUE, showDataViewOptions = FALSE)
  recalculateresult <- reactiveValues(recalculate1 = NULL)
  rv_Structure <- reactiveValues(breed1 = NULL, global_color_assigned = FALSE)
  rvdown_Structure <- reactiveValues(plot_Structure_cv_error = NULL)
  rvdown_Structure_plot_structure <- reactiveValues(plot_Structure_plot_structure = NULL)
  rvResults_Structure <- reactiveValues(data = NULL)
  rvmodelsave <- reactiveValues(data = NULL)
  rvsnpsave <- reactiveValues(data = NULL)
  rvpalette_Structure <- reactiveValues(selected_palette = NULL)
  rv_significant_data <- reactiveValues(data = NULL)
  rv_merged_calculateclass_select_selectdata <- reactiveValues(data = NULL)
  observeEvent(input$refresh_Structure, {
    updateSelectInput(session, "data_type_Structure", selected = "")
    if (!rv_Structure$global_color_assigned) {
      updateSelectInput(session, "color_palette_Structure", selected = "RdYlBu")
    }
    rv_Structure$global_color_assigned <- FALSE
  })
  observeEvent(input$data_type_Structure, {
    output$classification_data_status_Structure <- renderUI({})
    if (input$data_type_Structure == "Raw Data") {
      if (!is.null(rvdataclass$classification_data) && nrow(rvdataclass$classification_data) > 0) {
        class1_Structure <- rvdataclass$classification_data
        rv_Structure$breed1 <- data.frame(breed = unique(class1_Structure$breed))
        breedcount <- nrow(rv_Structure$breed1)
        observeEvent(input$color_palette_Structure, {
          if (!is.null(input$color_palette_Structure)) {
            max_colors <- brewer.pal.info[input$color_palette_Structure, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette_Structure)
            colors <- colorRampPalette(palette)(breedcount)
            rv_Structure$breed1$color <- colors
            rv_Structure$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status_Structure <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    } else if (input$data_type_Structure == "PCA Data") {
      if (!is.null(rvpcaresultdata$pcaresult_data) && nrow(rvpcaresultdata$pcaresult_data) > 0) {
        class1_Structure <- rvdataclass$classification_data
        rv_Structure$breed1 <- data.frame(breed = unique(class1_Structure$breed))
        breedcount <- nrow(rv_Structure$breed1)
        observeEvent(input$color_palette_Structure, {
          if (!is.null(input$color_palette_Structure)) {
            max_colors <- brewer.pal.info[input$color_palette_Structure, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette_Structure)
            colors <- colorRampPalette(palette)(breedcount)
            rv_Structure$breed1$color <- colors
            rv_Structure$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status_Structure <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    } else if (input$data_type_Structure == "Tree Data") {
      if (!is.null(rvtreeresultdata$treeresult_data) && nrow(rvtreeresultdata$treeresult_data) > 0) {
        class1_Structure <- rvdataclass$classification_data
        rv_Structure$breed1 <- data.frame(breed = unique(class1_Structure$breed))
        breedcount <- nrow(rv_Structure$breed1)
        observeEvent(input$color_palette_Structure, {
          if (!is.null(input$color_palette_Structure)) {
            max_colors <- brewer.pal.info[input$color_palette_Structure, "maxcolors"]
            palette <- brewer.pal(max_colors, input$color_palette_Structure)
            colors <- colorRampPalette(palette)(breedcount)
            rv_Structure$breed1$color <- colors
            rv_Structure$global_color_assigned <- TRUE
          }
        }, ignoreInit = TRUE)
      } else {
        output$classification_data_status_Structure <- renderUI({
          tags$p("Please upload and process classification data first.", style = "color: red;")
        })
      }
    }
    observe({
      if (!is.null(rvdataclass$classification_data) && nrow(rvdataclass$classification_data) > 0) {
        if (input$data_type_Structure == "PCA Data") {
          pcadataresult <- rvpcaresultdata$pcaresult_data
          pcadataresult_class <- rvdataclass$classification_data
          pcadataresult_class <- pcadataresult_class[pcadataresult_class$ID %in% pcadataresult$ID, ]
          breedChoices <- unique(pcadataresult_class$breed)
        } else if (input$data_type_Structure == "Tree Data") {
          treedataresult <- rvtreeresultdata$treeresult_data
          treedataresult_class <- rvdataclass$classification_data
          treedataresult_class <- treedataresult_class[treedataresult_class$ID %in% treedataresult$ID, ]
          breedChoices <- unique(treedataresult_class$breed)
        }  else {
          breedChoices <- unique(rvdataclass$classification_data$breed)
        }
        updatePickerInput(session, "selected_breeds_Structure",
                          choices = breedChoices,
                          selected = breedChoices)
      }
    })
  })
  observeEvent(input$calculate_Structure, {
    withProgress(message = 'Calculating...', value = 0, {
      setProgress(value = 0.01)
      if (!is.null(rv_Structure$breed1) && nrow(rv_Structure$breed1) > 0) {
        class1_Structure <- rvdataclass$classification_data
        rv_Structure$breed1 <- data.frame(breed = unique(class1_Structure$breed))
        breedcount <- nrow(rv_Structure$breed1)
        max_colors <- brewer.pal.info[input$color_palette_Structure, "maxcolors"]
        palette <- brewer.pal(max_colors, input$color_palette_Structure)
        colors <- colorRampPalette(palette)(breedcount)
        rv_Structure$breed1$color <- colors
        calculate1 <- rv_Structure$breed1
        calculateclass1 <- rvdataclass$classification_data
        merged_calculateclass <- merge(calculateclass1, calculate1, by = "breed")
        setProgress(value = 0.1)
        if (is.null(input$selected_breeds_Structure) || length(input$selected_breeds_Structure) == 0) {
          showNotification("Please select at least one breed before calculating.", type = "error")
          return()
        }
        merged_calculateclass_select <- merged_calculateclass[merged_calculateclass$breed %in% input$selected_breeds_Structure, ]
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
          if (input$data_type_Structure == "PCA Data") {
            pcadataresult <- rvpcaresultdata$pcaresult_data
            calculate_transposed <- calculate_transposed[calculate_transposed$IID %in% pcadataresult$ID, ]
            merged_calculateclass_select_selectdata <- merged_calculateclass_select[merged_calculateclass_select$ID %in% pcadataresult$ID, ]
          } else if (input$data_type_Structure == "Tree Data") {
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
          calculate_transposed_numeric <- apply(calculate_transposed, 2, as.numeric)
          rownames(calculate_transposed_numeric) <- original_row_names
          setProgress(value = 0.65)
          min_k <- input$min_k_selector
          max_k <- input$max_k_selector
          geno_data <- calculate_transposed_numeric
          geno_data <- as.data.frame(geno_data)
          temp_file <- file.path(tempdir(), "fixed_filename.lfmm")
          write.table(geno_data, temp_file, quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)
          set.seed(1)
          log_file <- file.path(tempdir(), "fixed_logfile.txt")
          sink(log_file)
          snmf_project <- snmf(temp_file, K = min_k:max_k, entropy = TRUE, repetitions = 40, project = "new")
          sink()
          cv_errors <- numeric(max_k - min_k + 1)
          for (K in min_k:max_k) {
            cv_entropy <- cross.entropy(snmf_project, K = K)
            cv_errors[K - min_k + 1] <- mean(cv_entropy)
          }
          plot_Structure_cv_error <- ggplot(data.frame(K = min_k:max_k, CV_Error = cv_errors), aes(x = K, y = CV_Error)) +
            geom_line() +
            geom_point() +
            xlab("Number of Ancestry Components (K)") +
            ylab("Cross-validation Error") +
            ggtitle("Cross-validation Error by K") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
          K_values <- min_k:max_k
          best_K_index <- which.min(cv_errors)
          best_K <- K_values[best_K_index]
          run_index <- 1
          Q_matrix <- Q(snmf_project, K = best_K, run = run_index)
          Q_df <- data.frame(Q_matrix)
          Q_df$individual <- rownames(calculate_transposed_numeric)
          class1_Structure <- rvdataclass$classification_data
          melted_Q_df <- reshape2::melt(Q_df, id.vars = "individual", variable.name = "Ancestry", value.name = "Proportion")
          melted_Q_dt <- as.data.table(melted_Q_df)
          class1_Structure_dt <- as.data.table(class1_Structure)
          merged_Q_df <- merge(melted_Q_dt, class1_Structure_dt, by.x = "individual", by.y = "ID", all.x = TRUE)
          merged_Q_df[, position := .GRP, by = individual]
          breed_start_end <- merged_Q_df[, .(start = min(position), end = max(position)), by = breed]
          breed_start_end[, mid := (start + end) / 2]
          color_palette <- input$color_palette_Structure
          plot_Structure_plot_structure <- ggplot(merged_Q_df, aes(x = factor(individual, levels = unique(individual)), y = Proportion, fill = Ancestry)) +
            geom_bar(stat = "identity", position = "stack", width = 0.9) +
            labs(x = "Breed", y = "Ancestry Proportion", fill = "Ancestry Group") +
            ggtitle("Population Structure Plot by Individual") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position = "none")
          if (color_palette != "") {
            plot_Structure_plot_structure <- plot_Structure_plot_structure +
              scale_fill_brewer(palette = color_palette)
          }
          for (i in 1:(nrow(breed_start_end) - 1)) {
            plot_Structure_plot_structure <- plot_Structure_plot_structure +
              geom_vline(xintercept = breed_start_end$end[i] + 0.5, linetype = "dotted")
          }
          plot_Structure_plot_structure <- plot_Structure_plot_structure +
            geom_text(data = breed_start_end, aes(x = mid, y = 1.05, label = breed), angle = 0, vjust = -0.5, inherit.aes = FALSE)
          rvdown_Structure$plot_Structure_cv_error <- plot_Structure_cv_error
          output$plot_Structure_cv_error <- renderPlot({ plot_Structure_cv_error })
          rvdown_Structure_plot_structure$plot_Structure_plot_structure <- plot_Structure_plot_structure
          output$plot_Structure_plot_structure <- renderPlot({ plot_Structure_plot_structure })
          temp_file <- file.path(tempdir(), "fixed_filename.lfmm")
          geno_file <- file.path(tempdir(), "fixed_filename.geno")
          snmf_project_file <- file.path(tempdir(), "fixed_filename.snmfProject")
          snmf_folder <- file.path(tempdir(), "fixed_filename.snmf")
          file.remove(temp_file)
          file.remove(geno_file)
          file.remove(snmf_project_file)
          file.remove(log_file)
          unlink(snmf_folder, recursive = TRUE)
          setProgress(value = 0.95)
          values$showText <- FALSE
          outputOptions(output, "showText", suspendWhenHidden = FALSE)
          setProgress(value = 1)
        } else {
          output$classification_data_status_Structure <- renderUI({
            tags$p("Please upload and process genotype data before calculating.", style = "color: red;")
          })
        }
      } else {
        output$classification_data_status_Structure <- renderUI({
          tags$p("Please upload and process classification data before calculating.", style = "color: red;")
        })
      }
    })
  })
  output$conditionalUI_Structure <- renderUI({
    if (values$showText) {
      conditionalPanel(
        condition = "true",
        h1("Structure Analysis"),
        p("Perform Structure analysis here and display the results.")
      )
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_Structure")
    } else {
      shinyjs::show("cover_Structure")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_Structure2")
    } else {
      shinyjs::show("cover_Structure2")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_Structure3")
    } else {
      shinyjs::show("cover_Structure3")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_Structure4")
    } else {
      shinyjs::show("cover_Structure4")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_Structure5")
    } else {
      shinyjs::show("cover_Structure5")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_Structure6")
    } else {
      shinyjs::show("cover_Structure6")
    }
  })
  observe({
    if(!is.null(values$showDataViewOptions) && values$showDataViewOptions == TRUE) {
      shinyjs::hide("cover_Structure7")
    } else {
      shinyjs::show("cover_Structure7")
    }
  })
  observe({
    min_k <- input$min_k_selector
    max_k <- input$max_k_selector
    if (min_k >= max_k) {
      updateNumericInput(session, "max_k_selector", value = min_k + 1)
    }
  })
  output$showText <- reactive({ values$showText })
  output$showText <- reactive({ values$showText })
  output$downloadZip_Structure <- downloadHandler(
    filename = function() {
      "Structure_results.zip"
    },
    content = function(file) {
      tempDir <- tempdir()
      formats <- input$image_format_Structure
      plotNames <- c("plot_Structure_cv_error", "plot_Structure_plot_structure")

      # 获取当前渲染的宽度和高度
      plot_width <- session$clientData$output_plot_Structure_plot_structure_width / 96  # 转换为英寸（假设96dpi）
      plot_height <- session$clientData$output_plot_Structure_plot_structure_height / 96  # 转换为英寸（假设96dpi）

      # 清除临时文件夹中的旧文件
      filesToRemove <- list.files(tempDir, full.names = TRUE)
      if (length(filesToRemove) > 0) {
        suppressWarnings(file.remove(filesToRemove))
      }

      # 保存图片文件到临时目录
      for (plotName in plotNames) {
        for (format in formats) {
          filePath <- file.path(tempDir, paste0(plotName, ".", tolower(format)))
          if (plotName == "plot_Structure_cv_error") {
            suppressMessages(ggsave(filePath, plot = rvdown_Structure$plot_Structure_cv_error, device = tolower(format), width = plot_width, height = plot_height))
          } else if (plotName == "plot_Structure_plot_structure") {
            suppressMessages(ggsave(filePath, plot = rvdown_Structure_plot_structure$plot_Structure_plot_structure, device = tolower(format), width = plot_width, height = plot_height))
          }
        }
      }

      # 打包图片文件到 ZIP 文件
      pattern <- paste0("\\.(", paste(tolower(formats), collapse = "|"), ")$")
      imageFiles <- list.files(tempDir, pattern = pattern, full.names = TRUE)
      suppressMessages(zip(file, files = imageFiles, flags = "-j"))
    },
    contentType = "application/zip"
  )


}
