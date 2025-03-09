#' @export
data_ui <- function() {
  tabPanel("DATA",
           fluidRow(
             column(width = 4,
                    tags$div(
                      style = "background-color: #f2f2f2; padding: 10px; border-radius: 10px; box-shadow: none;",
                      tags$div(
                        actionButton("genotype_btn", "Genotype", style = "width: 100%; margin-bottom: 40px; background-color: transparent; border-color: transparent; color: #3374AC; font-weight: bold; font-size: 16px;"),
                        actionButton("classification_btn", "Classification", style = "width: 100%; margin-bottom: 40px; background-color: transparent; border-color: transparent; color: #3374AC; font-weight: bold; font-size: 16px;"),
                        actionButton("location_btn", "Location", style = "width: 100%; margin-bottom: 10px; background-color: transparent; border-color: transparent; color: #3374AC; font-weight: bold; font-size: 16px;")
                      )
                    ),
                    tags$div(
                      style = "background-color: #f2f2f2; padding: 10px; border-radius: 10px; box-shadow: none; margin: 10px 0;",
                      tags$div(
                        actionButton("statistical_btn", "Statistical Test", style = "width: 100%; margin-bottom: 10px; background-color: transparent; border-color: transparent; color: #3374AC; font-weight: bold;")
                      )
                    )
             ),
             column(width = 8,
                    div(id = "upload_prompt", h3("Please upload your data")),
                    uiOutput("data_content")
             )
           )
  )
}

#' @export
data_server <- function(input, output, session, rvdataclass, rvdatageno, rvdatacloca) {
  uploadedFileInfo <- reactiveVal(NULL)
  uploadedFileInfoClassification <- reactiveVal(NULL)
  uploadedFileInfolocation <- reactiveVal(NULL)
  resetButtonStyles <- "$('#genotype_btn, #classification_btn, #location_btn, #statistical_btn').css({'background-color': 'transparent', 'color': '#3374AC'});"
  observeEvent(input$genotype_file, {
    uploadedFileInfo(input$genotype_file)
  })
  observeEvent(input$genotype_btn, {
    runjs(resetButtonStyles)
    runjs('$("#upload_prompt").hide();')
    output$data_content <- renderUI({
      fluidRow(
        column(12, h3("Genotype")),
        column(9,
               div(style = "height: 20px;"),
               HTML("<label for='genotype_file'>Genotypic file<br><span style='font-size: smaller;'>Please open Hapmap (TASSEL) format.</span></label>"),
               fileInput("genotype_file", "", buttonLabel = "browse...", placeholder = "No file has been selected", accept = '.hmp.txt')
        ),
        column(3, style = "align-self: flex-end;", div(style = "height: 20px;"), radioButtons("data_view", label = "Display:", choices = c("Head" = "head", "All" = "all"), selected = "head")),
        column(12, DTOutput("genotype_table"))
      )
    })
    runjs('$("#genotype_btn").css({"background-color": "#3374AC", "color": "white"});')
  })
  output$genotype_table <- renderDT({
    req(input$genotype_file)
    genotypefile <- read.table(input$genotype_file$datapath, header = TRUE, sep = "\t", comment.char = "")
    rvdatageno$genotype_data <- genotypefile

    if(input$data_view == "head" || is.null(input$data_view)) {
      genotypefile <- head(genotypefile, 10)
    }
    datatable(genotypefile, options = list(pageLength = 10, scrollX = TRUE))
  }, server = TRUE)
  observeEvent(input$classification_file, {
    uploadedFileInfoClassification(input$classification_file)
  })
  observeEvent(input$classification_btn, {
    runjs(resetButtonStyles)
    runjs('$("#upload_prompt").hide();')
    output$data_content <- renderUI({
      fluidRow(
        column(12, h3("classification")),
        column(9,
               div(style = "height: 20px;"),
               HTML("<label for='classification_file'>Classification file<br><span style='font-size: smaller;'>Please open .TXT format.</span></label>"),
               fileInput("classification_file", "", buttonLabel = "browse...", placeholder = "No file has been selected", accept = '.txt')
        ),
        column(3, style = "align-self: flex-end;", div(style = "height: 20px;"), radioButtons("data_view", label = "Display:", choices = c("Head" = "head", "All" = "all"), selected = "head")),
        column(12, DTOutput("classification_table"))
      )
    })
    runjs('$("#classification_btn").css({"background-color": "#3374AC", "color": "white"});')
  })
  output$classification_table <- renderDT({
    req(input$classification_file)
    classificationfile <- read.table(input$classification_file$datapath, header = TRUE, sep = "\t", comment.char = "")
    rvdataclass$classification_data <- classificationfile
    if(input$data_view == "head" || is.null(input$data_view)) {
      classificationfile <- head(classificationfile, 10)
    }
    datatable(classificationfile, options = list(pageLength = 10, scrollX = TRUE))
  }, server = TRUE)
  observeEvent(input$location_file, {
    uploadedFileInfolocation(input$location_file)
  })
  observeEvent(input$location_btn, {
    runjs(resetButtonStyles)
    runjs('$("#upload_prompt").hide();')
    output$data_content <- renderUI({
      fluidRow(
        column(12, h3("location")),
        column(9,
               div(style = "height: 20px;"),
               HTML("<label for='location_file'>location file<br><span style='font-size: smaller;'>Please open .TXT format.</span></label>"),
               fileInput("location_file", "", buttonLabel = "browse...", placeholder = "No file has been selected", accept = '.txt')
        ),
        column(3, style = "align-self: flex-end;", div(style = "height: 20px;"), radioButtons("data_view", label = "Display:", choices = c("Head" = "head", "All" = "all"), selected = "head")),
        column(12, DTOutput("location_table"))
      )
    })
    runjs('$("#location_btn").css({"background-color": "#3374AC", "color": "white"});')
  })
  output$location_table <- renderDT({
    req(input$location_file)
    locationfile <- read.table(input$location_file$datapath, header = TRUE, sep = "\t", comment.char = "")
    rvdatacloca$location_data <- locationfile
    if(input$data_view == "head" || is.null(input$data_view)) {
      locationfile <- head(locationfile, 10)
    }
    datatable(locationfile, options = list(pageLength = 10, scrollX = TRUE))
  }, server = TRUE)
  observeEvent(input$statistical_file, {
    uploadedFileInfo(input$statistical_file)
  })
  observeEvent(input$statistical_btn, {
    runjs(resetButtonStyles)
    runjs('$("#upload_prompt").hide();')

    output$data_content <- renderUI({
      fluidRow(
        column(12, h3("Statistical Test (Recommended)")),
        column(
          9,
          # Input parameters: significance level and statistical power
          numericInput("alpha", "Significance Level (alpha)", value = 0.05, step = 0.01),
          numericInput("power", "Statistical Power", value = 0.8, step = 0.01),
          # Calculate button with the same style as #statistical_btn
          actionButton("calculate_statistical", "Calculate",
                       style = "background-color: #3374AC; color: white; border-color: #3374AC;")
        ),
        column(3, style = "align-self: flex-end;",
               div(style = "height: 20px;"),
               radioButtons("data_view", label = "Display:",
                            choices = c("Head" = "head", "All" = "all"), selected = "head")),
        # Add margin-top to DTOutput for spacing
        column(12, div(style = "margin-top: 20px;", DTOutput("statistical_table")))
      )
    })

    runjs('$("#statistical_btn").css({"background-color": "#3374AC", "color": "white"});')
  })
  observeEvent(input$calculate_statistical, {
    withProgress(message = 'Calculating...', value = 0, {
      # Step 1: Extract data
      setProgress(value = 0.01, detail = "Extracting data")
      genotypefile_statistical <- rvdatageno$genotype_data
      classificationfile_statistical <- rvdataclass$classification_data
      req(genotypefile_statistical, classificationfile_statistical)

      # Step 2: Extract sample IDs from the genotype file (Assuming HapMap sample IDs start from column 12)
      setProgress(value = 0.10, detail = "Extracting sample IDs")
      sample_ids_statistical <- colnames(genotypefile_statistical)[12:ncol(genotypefile_statistical)]

      # Step 3: Match sample IDs with classification data
      setProgress(value = 0.20, detail = "Matching classification data")
      matched_data_statistical <- classificationfile_statistical[classificationfile_statistical$ID %in% sample_ids_statistical, ]

      # Step 4: Extract genotype matrix (sample columns)
      setProgress(value = 0.30, detail = "Extracting genotype matrix")
      genotype_matrix_statistical <- genotypefile_statistical[, 12:ncol(genotypefile_statistical)]

      # Define conversion function to convert genotype data to numeric format
      convert_genotypes_dt_statistical <- function(df_statistical) {
        setDT(df_statistical)
        df_with_slash_statistical <- df_statistical[grepl("/", alleles), ]
        df_without_slash_statistical <- df_statistical[!grepl("/", alleles), ]
        df_with_slash_statistical <- df_with_slash_statistical[order(alleles)]
        unique_alleles_statistical <- unique(df_with_slash_statistical$alleles)
        df_processed_statistical <- data.table()
        for (allele_statistical in unique_alleles_statistical) {
          sub_df_statistical <- df_with_slash_statistical[alleles == allele_statistical, ]
          alleles_parts_statistical <- strsplit(allele_statistical, "/", fixed = TRUE)[[1]]
          # Mapping: first allele -> 0, second allele -> 2, "N" -> NA, others -> 1
          map_statistical <- setNames(c(0, 2, NA, 1), c(alleles_parts_statistical, "N", "other"))
          # Assume sample data starts from column 12
          genotype_cols_statistical <- names(sub_df_statistical)[12:ncol(sub_df_statistical)]
          sub_df_statistical[, (genotype_cols_statistical) := lapply(.SD, function(x)
            map_statistical[ifelse(x %in% names(map_statistical), x, "other")]),
            .SDcols = genotype_cols_statistical]
          df_processed_statistical <- rbindlist(list(df_processed_statistical, sub_df_statistical), use.names = TRUE)
        }
        if (nrow(df_without_slash_statistical) > 0) {
          genotype_cols_statistical <- names(df_without_slash_statistical)[12:ncol(df_without_slash_statistical)]
          df_without_slash_statistical[, (genotype_cols_statistical) := lapply(.SD, function(x)
            ifelse(x == alleles, 0, ifelse(x == "N", NA, 1))),
            .SDcols = genotype_cols_statistical]
        }
        df_final_statistical <- rbindlist(list(df_processed_statistical, df_without_slash_statistical), use.names = TRUE)
        return(df_final_statistical)
      }

      # Step 5: Convert genotype data to numeric format
      setProgress(value = 0.35, detail = "Converting genotype data")
      numeric_genotype_statistical <- convert_genotypes_dt_statistical(genotypefile_statistical)

      # Step 6: Ensure sample columns are numeric to avoid errors in mean calculation (starting from column 12)
      setProgress(value = 0.40, detail = "Ensuring numeric conversion")
      genotype_cols_statistical <- names(numeric_genotype_statistical)[12:ncol(numeric_genotype_statistical)]
      numeric_genotype_statistical[, (genotype_cols_statistical) := lapply(.SD, function(x) as.numeric(as.character(x))),
                                   .SDcols = genotype_cols_statistical]

      # ===== Start of calculation section =====
      # Step 7: Calculate group statistics, skipping NA values
      setProgress(value = 0.50, detail = "Calculating group statistics")
      k_statistical <- length(unique(matched_data_statistical$breed))

      # Calculate the mean for each individual (column means)
      individual_means <- colMeans(numeric_genotype_statistical[, ..genotype_cols_statistical], na.rm = TRUE)

      # Calculate the mean for each group
      class_means_statistical <- sapply(unique(matched_data_statistical$breed), function(breed_statistical) {
        samples_statistical <- matched_data_statistical$ID[matched_data_statistical$breed == breed_statistical]
        if(length(samples_statistical) == 0) return(NA)
        mean(individual_means[samples_statistical], na.rm = TRUE)
      })

      # Calculate overall mean and variance
      overall_mean_statistical <- mean(individual_means, na.rm = TRUE)
      overall_var_statistical <- var(individual_means, na.rm = TRUE)

      # Step 8: Calculate effect size (f) using numeric values only
      setProgress(value = 0.70, detail = "Calculating effect size")
      f_statistical <- sqrt(sum((class_means_statistical - overall_mean_statistical)^2, na.rm = TRUE) /
                              (k_statistical * overall_var_statistical))
      # ===== End of calculation section =====

      # Step 9: Perform power analysis to compute required sample size per group
      setProgress(value = 0.80, detail = "Performing power analysis")
      sample_size_result_statistical <- pwr.anova.test(
        k = k_statistical,                   # Number of groups
        f = f_statistical,                   # Effect size
        sig.level = input$alpha,             # Significance level
        power = input$power                  # Statistical power
      )

      # Step 10: Output results
      setProgress(value = 0.90, detail = "Finalizing results")
      recommended_n <- ceiling(sample_size_result_statistical$n) *3
      groups <- unique(matched_data_statistical$breed)
      result_df <- data.frame(
        Group = groups,
        Current_Sample = sapply(groups, function(g) sum(matched_data_statistical$breed == g)),
        Recommended = recommended_n,
        Need_Supplement = sapply(groups, function(g) {
          count <- sum(matched_data_statistical$breed == g)
          if(count < recommended_n) "Yes" else "No"
        }),
        stringsAsFactors = FALSE
      )

      output$statistical_table <- renderDT({
        dt <- if(input$data_view == "head") head(result_df, 10) else result_df

        datatable(dt,
                  rownames = FALSE,
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = 't'
                  )) %>%
          formatStyle(columns = 'Need_Supplement',
                      backgroundColor = styleEqual(c("Yes", "No"), c('#FFE4E1', '#98FB98')))
      })

      output$data_content <- renderUI({
        fluidRow(
          column(12, h3("Statistical Test (Recommended)")),
          column(
            9,
            numericInput("alpha", "Significance Level (alpha)", value = 0.05, step = 0.01),
            numericInput("power", "Statistical Power", value = 0.8, step = 0.01),
            actionButton("calculate_statistical", "Calculate",
                         style = "background-color: #3374AC; color: white;")
          ),
          column(3,
                 radioButtons("data_view", label = "Display:",
                              choices = c("Head" = "head", "All" = "all"),
                              selected = "head")),
          column(12,
                 DTOutput("statistical_table"),
                 uiOutput("sampling_ui")
          ),
          column(12,
                 DTOutput("adjusted_statistical_table")
          )
        )
      })

      observe({
        if (!exists("original_classification_data")) {
          original_classification_data <<- copy(rvdataclass$classification_data)
        }
        if (!exists("original_genotype_data")) {
          original_genotype_data <<- copy(rvdatageno$genotype_data)
        }
      })

      observeEvent(input$apply_sampling, {
        req(original_classification_data, original_genotype_data)

        withProgress(message = 'Adjusting samples...', value = 0, {
          tryCatch({
            class_data <- copy(original_classification_data)
            genotype_data <- copy(original_genotype_data)
            setDT(class_data)

            if (!"breed" %in% names(class_data)) {
              stop("Column 'breed' does not exist in classification data! Actual column names: ", paste(names(class_data), collapse = ", "))
            }

            setorder(class_data, ID)
            setcolorder(genotype_data, c(names(genotype_data)[1:11], class_data$ID))

            group_counts <- class_data[, .N, by = breed]
            groups <- group_counts$breed

            setProgress(value = 10)

            if (input$sampling_target == "recommended") {
              target_counts <- result_df$Recommended[match(groups, result_df$Group)]
            } else {
              target_counts <- sapply(groups, function(g) {
                round(mean(group_counts$N[group_counts$breed != g]))
              })
            }

            setProgress(value = 30)

            if (input$sampling_method == "under") {
              for (i in seq_along(groups)) {
                group <- groups[i]
                target <- target_counts[i]
                current <- group_counts$N[group_counts$breed == group]

                if (current > target) {
                  keep_ids <- class_data[breed == group][
                    sample(.N, target, replace = FALSE), ID]

                  remove_ids <- setdiff(class_data[breed == group, ID], keep_ids)

                  class_data <- class_data[!(ID %in% remove_ids)]

                  genotype_data <- genotype_data[, !(colnames(genotype_data) %in% remove_ids), with = FALSE]
                }
              }

              setProgress(value = 50)

              setorder(class_data, ID)
              genotype_col_order <- c(names(genotype_data)[1:11], class_data$ID)
              genotype_data <- genotype_data[, genotype_col_order, with = FALSE]
            } else if (input$sampling_method == "over") {
              for (i in seq_along(groups)) {
                group <- groups[i]
                target <- target_counts[i]
                current <- group_counts$N[group_counts$breed == group]

                if (current < target) {
                  new_samples <- class_data[breed == group][
                    sample(.N, target - current, replace = TRUE)]
                  new_samples[, ID := paste0(ID, ".", 1:.N)]

                  class_data <- rbind(class_data, new_samples)

                  for (j in 1:(target - current)) {
                    original_id <- new_samples[j, ID]
                    original_id <- sub("\\..*", "", original_id)
                    new_id <- new_samples[j, ID]

                    genotype_data[, (new_id) := genotype_data[[original_id]]]
                  }
                }
              }
            } else if (input$sampling_method == "smote") {
              charSMOTE <- function(data, n_new, k) {
                new_samples <- data.table()
                snp_names <- data$rs.

                for (snp_idx in 1:nrow(data)) {
                  current_row <- data[snp_idx]
                  samples <- unlist(current_row[, -c(1:11), with = FALSE])
                  valid_samples <- which(samples != "N" & !is.na(samples))

                  if (length(valid_samples) < 2) {
                    new_gt <- rep("N", n_new)
                  } else {
                    actual_k <- min(k, length(valid_samples)-1)
                    if(actual_k < 1) actual_k <- 1

                    dist_mat <- adist(samples[valid_samples])

                    mapped_ids <- seq_along(valid_samples)

                    knn <- tryCatch({
                      get.knnx(dist_mat,
                               query = matrix(diag(length(valid_samples)), ncol = length(valid_samples)),
                               k = actual_k)
                    }, error = function(e) {
                      list(nn.index = matrix(rep(1, length(valid_samples)*actual_k),
                                             ncol = actual_k))
                    })

                    new_gt <- character(n_new)
                    for (i in 1:n_new) {
                      idx <- sample(mapped_ids, 1)
                      nn <- sample(knn$nn.index[idx, ], 1)

                      orig_idx <- valid_samples[idx]
                      orig_nn <- valid_samples[nn]

                      candidates <- c(samples[orig_idx], samples[orig_nn])
                      new_gt[i] <- if(length(candidates) > 0) sample(candidates, 1) else "N"
                    }
                  }

                  new_row <- as.list(c(current_row[, 1:11], new_gt))
                  new_samples <- rbindlist(list(new_samples, new_row), fill = TRUE)
                }

                new_cols <- c(names(data)[1:11], paste0("SMOTE_", 1:n_new))
                setnames(new_samples, new_cols)
                return(new_samples)
              }

              for (i in seq_along(groups)) {
                group <- groups[i]
                target <- target_counts[i]
                current <- group_counts$N[group_counts$breed == group]

                if (current < target) {
                  n_new <- target - current
                  group_ids <- class_data[breed == group, ID]

                  if(length(group_ids) == 0) next
                  group_data <- genotype_data[, c(names(genotype_data)[1:11], group_ids),
                                              with = FALSE]

                  if(nrow(group_data) == 0) next

                  set.seed(input$sampling_seed)
                  new_geno <- charSMOTE(group_data, n_new, input$smote_k)

                  if(ncol(new_geno) > 11) {
                    new_ids <- names(new_geno)[12:ncol(new_geno)]
                    genotype_data <- cbind(genotype_data, new_geno[, 12:.N, with = FALSE])

                    new_class <- data.table(
                      ID = new_ids,
                      breed = group
                    )
                    class_data <- rbindlist(list(class_data, new_class), use.names = TRUE)
                  }
                }
              }
            }

            setProgress(value = 75)

            rvdataclass$classification_data <- class_data
            rvdatageno$genotype_data <- genotype_data

            output$adjusted_statistical_table <- renderDT({
              datatable(class_data[, .N, by = breed],
                        rownames = FALSE,
                        colnames = c("Group", "Adjusted Sample Count"))
            })

            setProgress(value = 100)

          }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
          })
        })
      })


      output$sampling_ui <- renderUI({
        div(
          style = "margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 8px;",
          h4(icon("cogs"), "Sample Adjustment", style = "color: #3374AC;"),
          fluidRow(
            column(4,
                   selectInput("sampling_method", "Method:",
                               choices = c("Oversampling" = "over",
                                           "Undersampling" = "under",
                                           "SMOTE" = "smote"))),
            column(4,
                   numericInput("sampling_seed", "Random Seed:",
                                value = 123, min = 1)),
            column(4,
                   actionButton("apply_sampling", "Apply Sampling",
                                style = "margin-top: 25px; width: 100%;",
                                class = "btn-primary"))
          ),
          conditionalPanel(
            condition = "input.sampling_method == 'smote'",
            numericInput("smote_k", "Number of Neighbors (k):",
                         value = 3, min = 1, max = 5),
            helpText("Suggested values: 3-5, adjust based on sample size.")
          ),
          helpText(icon("info-circle"),
                   "Note: Oversampling duplicates samples, SMOTE creates synthetic samples."),
          fluidRow(
            column(6,
                   radioButtons("sampling_target", "Adjust to:",
                                choices = c("Recommended Count" = "recommended",
                                            "Group Average" = "average"),
                                selected = "recommended"))
          )
        )
      })

      setProgress(value = 1, detail = "Completed")
    })
  })
}
