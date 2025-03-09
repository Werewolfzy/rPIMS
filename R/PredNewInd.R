#' @export
PredNewInd_ui <- function() {
  tabPanel("PredNewInd",
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
                      fileInput("model_file", "Select Model File",
                                buttonLabel = "browse...",
                                placeholder = "No file has been selected",
                                accept = '.rds'),
                      fileInput("pred_genotype_file", "Select Genotype Data for Prediction",
                                buttonLabel = "browse...",
                                placeholder = "No file has been selected",
                                accept = '.hmp.txt'),
                      actionButton("calculate_PredNewInd", "Calculate", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                    ),
                    tags$div(
                      style = "background-color: #f2f2f2; padding: 10px; border-radius: 10px; box-shadow: none; margin-top: 20px;",
                      div(
                        downloadButton("downloadTable_PredNewInd", "Download Result", style = "width: 100%; background-color: #3374AC; color: white; font-weight: bold; font-size: 16px; padding: 10px; margin-top: 10px;")
                      )
                    )
             ),
             useShinyjs(),
             column(width = 8,
                    uiOutput("conditionalUI_PredNewInd"),
                    leafletOutput("map_PredNewInd"),
                    DTOutput("ResultsTable_PredNewInd")
             )
           )
  )
}

#' @export
PredNewInd_server <- function(input, output, session, rvdataclass, rvdatageno, rvdatacloca, rvpcaresultdata, rvtreeresultdata, rvstructureresultdata) {
  rvResults_PredNewInd <- reactiveValues(data = NULL)
  values <- reactiveValues(showText = TRUE, showDataViewOptions = FALSE)

  validate_sample <- function(model, sample, pred_class) {
    if (!is.null(model$meta)) {
      tryCatch({
        result <- switch(
          model$meta$validation_type,
          "distance" = {
            if (!pred_class %in% model$meta$class_centers$Class) return(TRUE)
            center <- model$meta$class_centers[model$meta$class_centers$Class == pred_class, -1]
            sample_vector <- as.numeric(sample)
            center_vector <- as.numeric(center)
            if (any(is.na(sample_vector)) || any(is.na(center_vector))) return(TRUE)
            distance <- sqrt(sum((sample_vector - center_vector)^2))
            threshold <- max(model$meta$distance_thresholds)
            if (is.na(threshold)) return(TRUE)
            distance > threshold
          },
          "probability" = {
            prob <- max(predict(model, sample, type = "prob"))
            threshold <- min(model$meta$prob_thresholds)
            if (is.na(threshold)) return(TRUE)
            is.na(prob) || prob < threshold
          },
          "leaf_path" = {
            leaves <- predict(model$finalModel, as.matrix(sample), predleaf = TRUE)
            global_min_freq <- min(model$meta$min_freq)
            any(sapply(1:ncol(leaves), function(j) {
              leaf_id <- leaves[1, j]
              freq <- model$meta$leaf_frequency[[j]][as.character(leaf_id)]
              is.na(freq) || freq < global_min_freq
            }))
          },
          "margin" = {
            margin <- kernlab::predict(model$finalModel, as.matrix(sample))
            threshold <- min(model$meta$margin_thresholds)
            if (is.na(threshold)) return(TRUE)
            is.na(margin) || margin < threshold
          },
          FALSE
        )
        if (is.null(result)) {
          warning("Unrecognized validation type: ", model$meta$validation_type)
          return(FALSE)
        }
        return(result)
      }, error = function(e) {
        message("Validation error: ", e$message)
        return(TRUE)
      })
    } else {
      return(FALSE)
    }
  }

  mapDisplayed <- reactiveVal(FALSE)

  observeEvent(input$calculate_PredNewInd, {
    withProgress(message = 'Calculating...', value = 0, {
      setProgress(value = 0.1)
      pred_model_file_path <- input$model_file$datapath
      pred_genotype_file_path <- input$pred_genotype_file$datapath
      if (is.null(pred_model_file_path) || is.null(pred_genotype_file_path)) {
        showNotification("Please upload both the model file and the genotype data file.", type = "error")
        return(NULL)
      }
      loaded_object <- readRDS(pred_model_file_path)
      pred_trainmodel <- loaded_object$model
      pred_trainsnp <- loaded_object$snp_location
      pred_trainlocal <- loaded_object$data_location
      setProgress(value = 0.2)
      pred_genotype_file <- read.table(pred_genotype_file_path, header = TRUE, sep = "\t", comment.char = "")
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
      genotype_number <- convert_genotypes_dt(pred_genotype_file)
      genotype_number_fill <- genotype_number[, -c(2:11)]
      new_row <- as.list(colnames(genotype_number_fill))
      genotype_number_conbind <- rbind(new_row, genotype_number_fill)
      genotype_number_transposed <- transpose(genotype_number_conbind)
      col_names <- as.character(genotype_number_transposed[1, ])
      colnames(genotype_number_transposed) <- col_names
      genotype_number_transposed <- genotype_number_transposed[-1, ]
      colnames(genotype_number_transposed)[1] <- "IID"
      rownames(genotype_number_transposed) = genotype_number_transposed$IID
      genotype_number_transposed$IID = NULL
      original_row_names <- rownames(genotype_number_transposed)
      transposed_numeric <- apply(genotype_number_transposed, 2, as.numeric) - 1
      rownames(transposed_numeric) <- original_row_names
      matched_data <- data.frame(class = transposed_numeric)
      column_names <- colnames(transposed_numeric)
      colnames(matched_data) <- column_names
      setDT(matched_data)
      rownames(matched_data) <- original_row_names
      setProgress(value = 0.4)
      matched_data_columns <- colnames(matched_data)
      pred_trainsnp_features <- pred_trainsnp$Feature
      features_in_matched_data <- pred_trainsnp_features %in% matched_data_columns
      all_features_in_matched_data <- all(features_in_matched_data)
      setProgress(value = 0.5)
      if (all_features_in_matched_data) {
        new_matched_data <- matched_data[, ..pred_trainsnp_features]
        if (any(is.na(new_matched_data))) {
          shinyalert(
            title = "Notice",
            text = "NA values found in the data. Continue analysis and fill NA with 0?",
            type = "warning",
            showCancelButton = TRUE,
            confirmButtonText = "Continue",
            cancelButtonText = "Re-upload",
            callbackR = function(value) {
              if (value) {
                na_counts <- rowSums(is.na(new_matched_data))
                map <- setNames(c(0), c("NA"))
                new_matched_data[] <- lapply(new_matched_data, function(x) {
                  key <- ifelse(is.na(x), "NA", as.character(x))
                  ifelse(key %in% names(map), map[key], x)
                })
                output$resultText <- renderText("Continuing analysis, NA values filled with 0")
                withProgress(message = 'Predicting...', value = 0.6, {
                  rownames(new_matched_data) <- original_row_names
                  predicted_results <- predict(pred_trainmodel, new_matched_data)
                  predicted_probabilities <- predict(pred_trainmodel, new_matched_data, type = "prob")
                  adjust_probability <- function(prob, alpha = 2) {
                    adjusted_prob <- prob^alpha / (prob^alpha + (1 - prob)^alpha)
                    if (prob <= 0.5) {
                      adjusted_prob <- adjusted_prob + (0.5 - adjusted_prob) * alpha
                    }
                    return(adjusted_prob)
                  }
                  confidence_info <- lapply(1:nrow(new_matched_data), function(i) {
                    original_na_count <- na_counts[i]
                    total_features <- ncol(new_matched_data)
                    if (original_na_count > total_features / 3) {
                      adjusted_prob <- NA
                      confidence <- "Uncertain"
                    } else {
                      sample_row <- new_matched_data[i, , drop = FALSE]
                      pred_class <- as.character(predicted_results[i])
                      is_uncertain <- validate_sample(pred_trainmodel, sample_row, pred_class)
                      original_prob <- predicted_probabilities[i, pred_class]
                      if (is_uncertain) {
                        adjusted_prob <- NA
                        confidence <- "Uncertain"
                      } else {
                        adjusted_prob <- adjust_probability(original_prob, alpha = 2)
                        confidence <- "Reliable"
                      }
                    }
                    list(
                      z_score = ifelse(is.na(adjusted_prob), NA, formatC(round(adjusted_prob, 2), format = "f", digits = 2)),
                      confidence = confidence
                    )
                  })
                  predicted_probability <- sapply(confidence_info, function(x) x$z_score)
                  confidence <- sapply(confidence_info, function(x) x$confidence)
                  predicted_df <- data.frame(
                    IID = rownames(new_matched_data),
                    predicted_result = predicted_results,
                    z_score = predicted_probability,
                    confidence = confidence
                  )
                  result_df <- merge(predicted_df, pred_trainlocal, by.x = "predicted_result", by.y = "breed", all.x = TRUE)
                  result_df <- result_df[, c("IID", "predicted_result", "z_score", "confidence", "Latitude", "Longitude", "Location")]
                  setDT(result_df)
                  result_df_combined <- result_df[, .(IIDs = paste(IID, collapse = ", ")), by = .(Latitude, Longitude)]
                  rvResults_PredNewInd$data <- result_df
                  output$ResultsTable_PredNewInd <- renderDT({
                    rvResults_PredNewInd$data
                  })
                  output$map_PredNewInd <- renderLeaflet({
                    leaflet(data = result_df_combined) %>%
                      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
                      addMarkers(~Longitude, ~Latitude, popup = ~paste("IIDs:", IIDs))
                  })
                  mapDisplayed(TRUE)
                  setProgress(value = 1)
                })
              } else {
                output$resultText <- renderText("Please re-upload files for analysis")
                setProgress(value = 1)
                return(NULL)
              }
            }
          )
        } else {
          output$resultText <- renderText("No NA values found in the data")
          withProgress(message = 'Predicting...', value = 0.6, {
            rownames(new_matched_data) <- original_row_names
            predicted_results <- predict(pred_trainmodel, new_matched_data)
            predicted_probabilities <- predict(pred_trainmodel, new_matched_data, type = "prob")
            adjust_probability <- function(prob, alpha = 2) {
              adjusted_prob <- prob^alpha / (prob^alpha + (1 - prob)^alpha)
              if (prob <= 0.5) {
                adjusted_prob <- adjusted_prob + (0.5 - adjusted_prob) * alpha
              }
              return(adjusted_prob)
            }
            confidence_info <- lapply(1:nrow(new_matched_data), function(i) {
              sample_row <- new_matched_data[i, , drop = FALSE]
              pred_class <- as.character(predicted_results[i])
              is_uncertain <- validate_sample(pred_trainmodel, sample_row, pred_class)
              original_prob <- predicted_probabilities[i, pred_class]
              if (is_uncertain) {
                adjusted_prob <- NA
                confidence <- "Uncertain"
              } else {
                adjusted_prob <- adjust_probability(original_prob, alpha = 2)
                confidence <- "Reliable"
              }
              list(
                z_score = ifelse(is.na(adjusted_prob), NA, formatC(round(adjusted_prob, 2), format = "f", digits = 2)),
                confidence = confidence
              )
            })
            predicted_probability <- sapply(confidence_info, function(x) x$z_score)
            confidence <- sapply(confidence_info, function(x) x$confidence)
            predicted_df <- data.frame(
              IID = rownames(new_matched_data),
              predicted_result = predicted_results,
              z_score = predicted_probability,
              confidence = confidence
            )
            result_df <- merge(predicted_df, pred_trainlocal, by.x = "predicted_result", by.y = "breed", all.x = TRUE)
            result_df <- result_df[, c("IID", "predicted_result", "z_score", "confidence", "Latitude", "Longitude", "Location")]
            setDT(result_df)
            result_df_combined <- result_df[, .(IIDs = paste(IID, collapse = ", ")), by = .(Latitude, Longitude)]
            rvResults_PredNewInd$data <- result_df
            output$ResultsTable_PredNewInd <- renderDT({
              rvResults_PredNewInd$data
            })
            output$map_PredNewInd <- renderLeaflet({
              leaflet(data = result_df_combined) %>%
                addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
                addMarkers(~Longitude, ~Latitude, popup = ~paste("IIDs:", IIDs))
            })
            mapDisplayed(TRUE)
            setProgress(value = 1)
          })
        }
      } else {
        output$resultText <- renderText("Not all Features in pred_trainsnp are in matched_data columns")
        missing_features <- pred_trainsnp_features[!features_in_matched_data]
        output$missingFeatures <- renderTable(data.frame(Missing_SNP_Features = missing_features))
        shinyalert(
          title = "Notice",
          text = "Some Features in pred_trainsnp are missing in matched_data columns. Continue analysis?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Continue",
          cancelButtonText = "Re-upload",
          callbackR = function(value) {
            if (value) {
              new_matched_data <- data.table(full_0 = rep(0, nrow(matched_data)))
              matched_data_columns <- colnames(matched_data)
              for (feature in pred_trainsnp_features) {
                if (feature %in% matched_data_columns) {
                  current_col <- matched_data[[feature]]
                  if (length(current_col) != nrow(new_matched_data)) {
                    stop(paste("Column", feature, "has incorrect length. Expected", nrow(new_matched_data), "but got", length(current_col)))
                  }
                  new_matched_data[, (feature) := current_col]
                } else {
                  new_matched_data[, (feature) := rep(NA, nrow(new_matched_data))]
                }
              }
              if (any(is.na(new_matched_data))) {
                shinyalert(
                  title = "Notice",
                  text = "NA values found in the data. Continue analysis and fill NA with 0?",
                  type = "warning",
                  showCancelButton = TRUE,
                  confirmButtonText = "Continue",
                  cancelButtonText = "Re-upload",
                  callbackR = function(value) {
                    if (value) {
                      na_counts <- rowSums(is.na(new_matched_data))
                      map <- setNames(c(0), c("NA"))
                      new_matched_data[] <- lapply(new_matched_data, function(x) {
                        key <- ifelse(is.na(x), "NA", as.character(x))
                        ifelse(key %in% names(map), map[key], x)
                      })
                      output$resultText <- renderText("Continuing analysis, NA values filled with 0")
                      withProgress(message = 'Predicting...', value = 0.8, {
                        rownames(new_matched_data) <- original_row_names
                        predicted_results <- predict(pred_trainmodel, new_matched_data)
                        predicted_probabilities <- predict(pred_trainmodel, new_matched_data, type = "prob")
                        adjust_probability <- function(prob, alpha = 2) {
                          adjusted_prob <- prob^alpha / (prob^alpha + (1 - prob)^alpha)
                          if (prob <= 0.5) {
                            adjusted_prob <- adjusted_prob + (0.5 - adjusted_prob) * alpha
                          }
                          return(adjusted_prob)
                        }
                        confidence_info <- lapply(1:nrow(new_matched_data), function(i) {
                          original_na_count <- na_counts[i]
                          total_features <- ncol(new_matched_data)
                          if (original_na_count > total_features / 3) {
                            adjusted_prob <- NA
                            confidence <- "Uncertain"
                          } else {
                            sample_row <- new_matched_data[i, , drop = FALSE]
                            pred_class <- as.character(predicted_results[i])
                            is_uncertain <- validate_sample(pred_trainmodel, sample_row, pred_class)
                            original_prob <- predicted_probabilities[i, pred_class]
                            if (is_uncertain) {
                              adjusted_prob <- NA
                              confidence <- "Uncertain"
                            } else {
                              adjusted_prob <- adjust_probability(original_prob, alpha = 2)
                              confidence <- "Reliable"
                            }
                          }
                          list(
                            z_score = ifelse(is.na(adjusted_prob), NA, formatC(round(adjusted_prob, 2), format = "f", digits = 2)),
                            confidence = confidence
                          )
                        })
                        predicted_probability <- sapply(confidence_info, function(x) x$z_score)
                        confidence <- sapply(confidence_info, function(x) x$confidence)
                        predicted_df <- data.frame(
                          IID = rownames(new_matched_data),
                          predicted_result = predicted_results,
                          z_score = predicted_probability,
                          confidence = confidence
                        )
                        result_df <- merge(predicted_df, pred_trainlocal, by.x = "predicted_result", by.y = "breed", all.x = TRUE)
                        result_df <- result_df[, c("IID", "predicted_result", "z_score", "confidence", "Latitude", "Longitude", "Location")]
                        setDT(result_df)
                        result_df_combined <- result_df[, .(IIDs = paste(IID, collapse = ", ")), by = .(Latitude, Longitude)]
                        rvResults_PredNewInd$data <- result_df
                        output$ResultsTable_PredNewInd <- renderDT({
                          rvResults_PredNewInd$data
                        })
                        output$map_PredNewInd <- renderLeaflet({
                          leaflet(data = result_df_combined) %>%
                            addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
                            addMarkers(~Longitude, ~Latitude, popup = ~paste("IIDs:", IIDs))
                        })
                        mapDisplayed(TRUE)
                        setProgress(value = 1)
                      })
                    } else {
                      output$resultText <- renderText("Please re-upload files for analysis")
                      setProgress(value = 1)
                      return(NULL)
                    }
                  }
                )
              } else {
                output$resultText <- renderText("No NA values found in the data")
                withProgress(message = 'Predicting...', value = 0.8, {
                  print(444)
                  rownames(new_matched_data) <- original_row_names
                  predicted_results <- predict(pred_trainmodel, new_matched_data)
                  predicted_probabilities <- predict(pred_trainmodel, new_matched_data, type = "prob")
                  adjust_probability <- function(prob, alpha = 2) {
                    adjusted_prob <- prob^alpha / (prob^alpha + (1 - prob)^alpha)
                    if (prob <= 0.5) {
                      adjusted_prob <- adjusted_prob + (0.5 - adjusted_prob) * alpha
                    }
                    return(adjusted_prob)
                  }
                  confidence_info <- lapply(1:nrow(new_matched_data), function(i) {
                    original_na_count <- na_counts[i]
                    total_features <- ncol(new_matched_data)
                    if (original_na_count > total_features / 3) {
                      adjusted_prob <- NA
                      confidence <- "Uncertain"
                    } else {
                      sample_row <- new_matched_data[i, , drop = FALSE]
                      pred_class <- as.character(predicted_results[i])
                      is_uncertain <- validate_sample(pred_trainmodel, sample_row, pred_class)
                      original_prob <- predicted_probabilities[i, pred_class]
                      if (is_uncertain) {
                        adjusted_prob <- NA
                        confidence <- "Uncertain"
                      } else {
                        adjusted_prob <- adjust_probability(original_prob, alpha = 2)
                        confidence <- "Reliable"
                      }
                    }
                    list(
                      z_score = ifelse(is.na(adjusted_prob), NA, formatC(round(adjusted_prob, 2), format = "f", digits = 2)),
                      confidence = confidence
                    )
                  })
                  predicted_probability <- sapply(confidence_info, function(x) x$z_score)
                  confidence <- sapply(confidence_info, function(x) x$confidence)
                  predicted_df <- data.frame(
                    IID = rownames(new_matched_data),
                    predicted_result = predicted_results,
                    z_score = predicted_probability,
                    confidence = confidence
                  )
                  result_df <- merge(predicted_df, pred_trainlocal, by.x = "predicted_result", by.y = "breed", all.x = TRUE)
                  result_df <- result_df[, c("IID", "predicted_result", "z_score", "confidence", "Latitude", "Longitude", "Location")]
                  setDT(result_df)
                  result_df_combined <- result_df[, .(IIDs = paste(IID, collapse = ", ")), by = .(Latitude, Longitude)]
                  rvResults_PredNewInd$data <- result_df
                  output$ResultsTable_PredNewInd <- renderDT({
                    rvResults_PredNewInd$data
                  })
                  output$map_PredNewInd <- renderLeaflet({
                    leaflet(data = result_df_combined) %>%
                      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
                      addMarkers(~Longitude, ~Latitude, popup = ~paste("IIDs:", IIDs))
                  })
                  mapDisplayed(TRUE)
                  setProgress(value = 1)
                })
              }
            } else {
              output$resultText <- renderText("Please re-upload files for analysis")
              setProgress(value = 1)
              return(NULL)
            }
          }
        )
      }
    })
  })

  output$downloadTable_PredNewInd <- downloadHandler(
    filename = function() {
      paste("Prediction_Results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rvResults_PredNewInd$data, file, row.names = FALSE)
    }
  )
  output$conditionalUI_PredNewInd <- renderUI({
    if (!mapDisplayed()) {
      div(
        h1("PredNewInd Analysis"),
        p("Perform PredNewInd analysis here and display the results.")
      )
    }
  })
}
