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
  resetButtonStyles <- "$('#genotype_btn, #classification_btn, #location_btn').css({'background-color': 'transparent', 'color': '#3374AC'});"
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
               HTML("<label for='location_file'>location file (optional)<br><span style='font-size: smaller;'>Please open .TXT format.</span></label>"),
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
}
