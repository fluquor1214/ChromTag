set.seed(1234)
server <- function(input, output, session) {
  values <- reactiveValues(
    peakAnnoReady = FALSE,
    DVReady = FALSE
  )
  #清缓存
  session$onSessionEnded(function() {
    custom_cache_files <- list.files(cache_dir, pattern = "^custom_cache_.*\\.rds$", full.names = TRUE)
    if (length(custom_cache_files) > 0) {
      file.remove(custom_cache_files)
    }
  })
  
  observeEvent(input$Selectdata, {
    if (input$Selectdata != "Custom Data") {
      custom_cache_files <- list.files(cache_dir, pattern = "^custom_cache_.*\\.rds$", full.names = TRUE)
      if (length(custom_cache_files) > 0) {
        file.remove(custom_cache_files)
      }
    }
  })
  
  observeEvent(input$file, {
    if (input$Selectdata == "Custom Data") {
      custom_cache_files <- list.files(cache_dir, pattern = "^custom_cache_.*\\.rds$", full.names = TRUE)
      if (length(custom_cache_files) > 0) {
        file.remove(custom_cache_files)
      }
    }
  })
  
  session$onSessionEnded(function() {
    custom_cache_files <- list.files(cache_dir, pattern = "^profilePlots_custom_cache_.*\\.rds$", full.names = TRUE)
    if (length(custom_cache_files) > 0) {
      file.remove(custom_cache_files)
    }
  })
  
  observeEvent(input$Selectdata, {
    if (input$Selectdata != "Custom Data") {
      custom_cache_files <- list.files(cache_dir, pattern = "^profilePlots_custom_cache_.*\\.rds$", full.names = TRUE)
      if (length(custom_cache_files) > 0) {
        file.remove(custom_cache_files)
      }
    }
  })
  
  observeEvent(input$file, {
    if (input$Selectdata == "Custom Data") {
      custom_cache_files <- list.files(cache_dir, pattern = "^profilePlots_custom_cache_.*\\.rds$", full.names = TRUE)
      if (length(custom_cache_files) > 0) {
        file.remove(custom_cache_files)
      }
    }
  })
  
  session$onSessionEnded(function() {
    sinse_cache_files <- list.files(cache_dir, pattern = "_sinse_.*\\.rds$", full.names = TRUE)
    if (length(sinse_cache_files) > 0) {
      file.remove(sinse_cache_files)
    }
  })
  
  observeEvent(input$Selectdata, {
    if (input$Selectdata != "Custom Data") {
      sinse_cache_files <- list.files(cache_dir, pattern = "_sinse_.*\\.rds$", full.names = TRUE)
      if (length(sinse_cache_files) > 0) {
        file.remove(sinse_cache_files)
      }
    }
  })
  
  observeEvent(input$file, {
    if (input$Selectdata == "Custom Data") {
      sinse_cache_files <- list.files(cache_dir, pattern = "_sinse_.*\\.rds$", full.names = TRUE)
      if (length(sinse_cache_files) > 0) {
        file.remove(sinse_cache_files)
      }
    }
  })
  
  values <- reactiveValues(data = NULL)
  # shinyjs::disable(selector = ".sidebar li a[data-value='step1']")
  # shinyjs::disable(selector = ".sidebar li a[data-value='step2']")
  # shinyjs::disable(selector = ".sidebar li a[data-value='step3']")
  # shinyjs::disable(selector = ".sidebar li a[data-value='step4']")
  # shinyjs::disable(selector = ".sidebar li a[data-value='step5']")
  # shinyjs::disable(selector = ".sidebar li a[data-value='step6']")
  # shinyjs::disable(selector = ".sidebar li a[data-value='step7']")
  
  shinyjs::disable("step11")
  shinyjs::disable("step12")
  shinyjs::disable("step21")
  shinyjs::disable("step312")
  shinyjs::disable("step2_to_step3")
  shinyjs::disable("step32")
  shinyjs::disable("step31")
  shinyjs::disable("step4")
  shinyjs::disable("step51")
  shinyjs::disable("step6")
  shinyjs::disable("step7")
  
  # —————————————— 上传文件 ——————————————
  observeEvent(input$Selectdata, {
    if (input$Selectdata == "Custom Data") {
      output$FileInputs <- renderUI({
        tagList(
          fileInput(
            "file", 
            "Choose a File in CSV Format", 
            accept = c(".csv"),
            width = "100%"
          )
        )
      })
      output$Selectspecies <- renderUI({
        tagList(
          selectInput(
            "species", 
            "Choose Species", 
            choices = c("Human", "Mouse", "Drosophila"),
            multiple = FALSE,
            selected = "Human",
            width = "100%"
          )
        )
      })
      
      observeEvent(input$file, {
        req(input$file)
        tryCatch({
          uploaded_file <- input$file$datapath
          values$data <- read.csv(uploaded_file, stringsAsFactors = FALSE)
          values$masterPeak <- GRanges(
            seqnames = Rle(values$data$Chromosome),
            ranges = IRanges(start = values$data$Start, end = values$data$End), 
            strand = Rle(rep("*", nrow(values$data)))
          )
          mcols(values$masterPeak) <- values$data[, 4:ncol(values$data)]
          filtered_gr <- values$masterPeak[grepl("^chr[0-9XY]+$", seqnames(values$masterPeak))]
          values$filtered_gr <- filtered_gr
          
          output$dataPreview <- DT::renderDataTable({
            DT::datatable(
              values$data,
              selection = 'none',
              extensions = 'FixedHeader',
              options = list(
                pageLength = 10,
                autoWidth = F,
                searchHighlight = TRUE,
                scrollX = TRUE,
                FixedHeader = T
              ))
          })
        }, error = function(e) {
          sendSweetAlert(
            session = session,
            title = "Error",
            text = paste("Error reading the file:", e$message),
            type = "error"
          )
        })
      })
      
    } else if (input$Selectdata == "Example Data") {
      output$FileInputs <- renderUI({
        tagList(
          output$FileInputs <- renderUI({
            tagList(
              div(
                "A ", code("CSV file"), 
                "with merged peaks and sample count data is required. The first three columns should specify the chromosome, start, and end positions, followed by count values for each sample. Each group must have at least", 
                code("two biological replicates"), 
                ". Peaks should be pre-merged across all samples for consistency. The provided dataset includes H3K27me3 and H3K4me3 count data, each with", 
                code("two biological replicates"),
                ". Uploaded data can be previewed in the table below.",
                style = "font-size:15px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          })
          
        )
      })
      
      output$Selectspecies <- renderUI({
        NULL
      })
      
      tryCatch({
        values$data <- read.csv(example_data_dir, stringsAsFactors = FALSE)
        values$masterPeak <- GRanges(
          seqnames = Rle(values$data$Chromosome),
          ranges = IRanges(start = values$data$Start, end = values$data$End), 
          strand = Rle(rep("*", nrow(values$data)))
        )
        mcols(values$masterPeak) <- values$data[, 4:ncol(values$data)]
        filtered_gr <- values$masterPeak[grepl("^chr[0-9XY]+$", seqnames(values$masterPeak))]
        values$filtered_gr <- filtered_gr
        
        output$dataPreview <- DT::renderDataTable({
          DT::datatable(
            values$data,
            selection = 'none',
            extensions = 'FixedHeader',
            options = list(
              pageLength = 10,
              autoWidth = F,
              searchHighlight = TRUE,
              scrollX = TRUE,
              FixedHeader = T
            ))
        })
      }, error = function(e) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = paste("Error loading sample data:", e$message),
          type = "error"
        )
      })
    }
  })
  
  #物种数据库
  observeEvent(input$import, {
    if (length(input$species) == 1) {
      
      if (input$Selectdata == "Custom Data") {
        step12_txdb <- switch(input$species,
                              "Human" = TxDb.Hsapiens.UCSC.hg19.knownGene,
                              "Mouse" = TxDb.Mmusculus.UCSC.mm10.knownGene,
                              "Drosophila" = TxDb.Dmelanogaster.UCSC.dm6.ensGene
        )
        txdb <- switch(input$species,
                       "Human" = TxDb.Hsapiens.UCSC.hg38.knownGene,
                       "Mouse" = TxDb.Mmusculus.UCSC.mm10.knownGene,
                       "Drosophila" = TxDb.Dmelanogaster.UCSC.dm6.ensGene
        )
        annoDb <- switch(input$species,
                         "Human" = "org.Hs.eg.db",
                         "Mouse" = "org.Mm.eg.db",
                         "Drosophila" = "org.Dm.eg.db"
        )
        Organism <- switch(input$species,
                           "Human" = 'hsa',
                           "Mouse" = 'mmu',
                           "Drosophila" = "dme"
        )
        BSgenome <- switch(input$species,
                           "Human" = BSgenome.Hsapiens.UCSC.hg38,
                           "Mouse" = BSgenome.Mmusculus.UCSC.mm10,
                           "Drosophila" = BSgenome.Dmelanogaster.UCSC.dm6
        )
        values$annoDb <- annoDb
        values$txdb <- txdb
        values$step12_txdb <- step12_txdb
        values$Organism <- Organism
        values$BSgenome <- BSgenome
      }
      # 数据来源
    } else if (input$Selectdata == "Example Data") {
      values$annoDb <- "org.Hs.eg.db"
      values$txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
      values$step12_txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
      values$Organism <- "hsa"
      values$BSgenome <- BSgenome.Hsapiens.UCSC.hg38
    }
    
  })
  
  #禁用按钮
  observeEvent(input$import, {
    shinyjs::enable(selector = ".sidebar li a[data-value='step1']")
    js.1 <- '
        $(document).ready(function(){
            $("a[data-value=step1]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.1)
    
    shinyjs::enable(selector = ".sidebar li a[data-value='step2']")
    js.2 <- '
        $(document).ready(function(){
            $("a[data-value=step2]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.2)
  })
  
  observeEvent(input$step2_to_step3, {
    shinyjs::enable(selector = ".sidebar li a[data-value='step3']")
    js.3 <- '
        $(document).ready(function(){
            $("a[data-value=step3]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.3)
  })
  
  observeEvent(input$step32, {
    shinyjs::enable(selector = ".sidebar li a[data-value='step4']")
    js.4 <- '
        $(document).ready(function(){
            $("a[data-value=step4]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.4)
  })
  
  observeEvent(input$step4, {
    shinyjs::enable(selector = ".sidebar li a[data-value='step5']")
    js.5 <- '
        $(document).ready(function(){
            $("a[data-value=step5]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.5)
  })
  
  observeEvent(input$step51, {
    shinyjs::enable(selector = ".sidebar li a[data-value='step6']")
    js.6 <- '
        $(document).ready(function(){
            $("a[data-value=step6]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.6)
  })
  
  observeEvent(input$step51, {
    shinyjs::enable(selector = ".sidebar li a[data-value='step7']")
    js.7 <- '
        $(document).ready(function(){
            $("a[data-value=step7]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.7)
  })
  
  # 跳转到 step1 页面,按钮的id为import
  observeEvent(input$import, {
    cleanFunction(TRUE)
    shinyjs::enable("step11")
    shinyjs::enable("step12")
    shinyjs::enable("step21")
    updateTabItems(session, "menuitem", "step1")
  })
  
  # —————————————— Peaks Visualization ——————————————
  observe({
    req(values$data)
    
    unidata <- unique(values$data$Chromosome)
    filtered_data <- unidata[grepl("^chr[0-9XY]+$", unidata)]
    
    updateSelectInput(
      session, "weightCol1",
      choices = colnames(values$data)[4:ncol(values$data)],
      selected = colnames(values$data)[4]
    )
    updateSelectInput(
      session, "weightCol2",
      choices = colnames(values$data)[4:ncol(values$data)],
      selected = colnames(values$data)[4]
    )
    updateSelectInput(
      session, "chrs",
      # choices = unique(values$data$Chromosome),
      choices = c(filtered_data,"All"),
      selected = unique(values$data$Chromosome)[1]
    )
  })
  
  observeEvent(input$step11, {
    shinyjs::disable("step11")
    shinyjs::disable("step12")
    req(values$masterPeak, input$weightCol1, input$chrs)
    
    filtered_gr <- values$masterPeak[grepl("^chr[0-9XY]+$", seqnames(values$masterPeak))]
    
    tryCatch({
      values$coveragePlots <- list()
      
      plot_output_list <- lapply(input$weightCol1, function(sample) {
        file_id <- digest::digest(values$data) 
        if (input$Selectdata == "Example Data") {
          cache_file <- file.path(cache_dir, paste0("example_cache_", file_id, "_", sample, "_", input$chrs, ".rds"))
          if (file.exists(cache_file)) {
            p <- readRDS(cache_file)
          } else {
            if(input$chrs == "All"){
              p <- covplot2(
                filtered_gr,
                weightCol = as.character(sample),
                title = paste("Peaks over Chromosomes", sample)
              )
            }else{
              p <- covplot1(
                filtered_gr,
                weightCol = as.character(sample),
                chrs = as.character(input$chrs),
                title = paste("Peaks over Chromosomes for", sample)
              )
            }
            saveRDS(p, cache_file)
          }
        } else if (input$Selectdata == "Custom Data") {
          showModal(modalDialog(
            title = "Drawing the plot",
            div(
              style = "text-align: center;",
              h4("Please wait..."),
              shiny::tags$a(img(src = "ZZ5H.gif", height = "100px"))
            ),
            footer = NULL,
            easyClose = F
          ))
          cache_file <- file.path(cache_dir, paste0("custom_cache_", file_id, "_", digest::digest(list(sample, input$chrs)), ".rds"))
          if (file.exists(cache_file)) {
            p <- readRDS(cache_file)
          } else {
            if(input$chrs == "All"){
              p <- covplot2(
                filtered_gr,
                weightCol = as.character(sample),
                title = paste("Peaks over Chromosomes", sample)
              )
            }else{
              p <- covplot1(
                filtered_gr,
                weightCol = as.character(sample),
                chrs = as.character(input$chrs),
                title = paste("Peaks over Chromosomes for", sample)
              )
            }
            saveRDS(p, cache_file)
          }
        }
        values$coveragePlots[[sample]] <- p
        
        plotname <- paste("covplot_", sample, sep = "")
        output[[plotname]] <- renderPlot(print(p))
        plotOutput(plotname) %>% withSpinner()
      })
      
      output$covplot <- renderUI(do.call(tagList, plot_output_list))
      
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during analysis:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal()
      shinyjs::enable("step11")
      shinyjs::enable("step12")
    })
  })
  
  #生成谱图
  observeEvent(input$step12, {
    shinyjs::disable("step11")
    shinyjs::disable("step12")
    req(values$masterPeak, input$weightCol2, input$upstream, input$downstream)
    
    promoter <- getPromoters(
      TxDb = values$step12_txdb,
      upstream = as.numeric(input$upstream),
      downstream = as.numeric(input$downstream)
    )
    
    tryCatch({
      values$profilePlots <- list()
      
      plot_output_list <- lapply(input$weightCol2, function(sample) {
        file_id <- digest::digest(values$data)
        if (input$Selectdata == "Example Data") {
          cache_file <- file.path(cache_dir, paste0("profilePlots_example_cache_", file_id, "_", sample, "_upstream_", input$upstream, "_downstream_", input$downstream, ".rds"))
          if (file.exists(cache_file)) {
            p <- readRDS(cache_file)
          } else {
            tagMatrix <- getTagMatrix(
              values$masterPeak,
              windows = promoter,
              weightCol = as.character(sample)
            )
            p <- plotAvgProf(
              tagMatrix,
              xlim = c(-as.numeric(input$upstream), as.numeric(input$downstream)),
              xlab = "Genomic Region (5' -> 3')",
              ylab = "Read Count Frequency"
            )
            saveRDS(p, cache_file)
          }
        } else if (input$Selectdata == "Custom Data") {
          showModal(modalDialog(
            title = "Drawing the plot",
            div(
              style = "text-align: center;",
              h4("Please wait..."),
              shiny::tags$a(img(src = "ZZ5H.gif", height = "100px"))
            ),
            footer = NULL,
            easyClose = F
          ))
          
          cache_file <- file.path(cache_dir, paste0("profilePlots_custom_cache_", file_id, "_", sample, "_upstream_", input$upstream, "_downstream_", input$downstream, ".rds"))
          if (file.exists(cache_file)) {
            p <- readRDS(cache_file)
          } else {
            tagMatrix <- getTagMatrix(
              values$masterPeak,
              windows = promoter,
              weightCol = as.character(sample)
            )
            p <- plotAvgProf(
              tagMatrix,
              xlim = c(-as.numeric(input$upstream), as.numeric(input$downstream)),
              xlab = "Genomic Region (5' -> 3')",
              ylab = "Read Count Frequency"
            )
            saveRDS(p, cache_file)
          }
        }
        
        values$profilePlots[[sample]] <- p
        
        plotname <- paste("plot_", sample, sep = "")
        output[[plotname]] <- renderPlot(print(p))
        plotOutput(plotname) %>% withSpinner()
      })
      
      output$profileplot <- renderUI(do.call(tagList, plot_output_list))
      
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during filtering:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal()
      shinyjs::enable("step11")
      shinyjs::enable("step12")
    })
  })
  
  
  #-----------------Filter------------------
  output$dataPreviewstep21 <- DT::renderDataTable({
    DT::datatable(
      values$data,
      selection = 'none',
      extensions = 'FixedHeader',
      options = list(
        pageLength = 5,
        autoWidth = F,
        searchHighlight = TRUE,
        scrollX = TRUE,
        FixedHeader = T
      )
    )
  })
  
  observe({
    req(values$masterPeak)
    masterPeak <- values$masterPeak
    countMat <- as.matrix(mcols(values$masterPeak)[, 1:ncol(mcols(values$masterPeak))])
    values$countMat <- countMat
    values$dataS <- countMat
    values$filtered_masterPeak <- masterPeak
  })
  
  observeEvent(input$step21, {
    req(values$masterPeak)
    clean_filter(T)
    shinyjs::disable("step21")
    countMat <- values$countMat
    tryCatch({
      selectR <- which(rowSums(countMat) > input$threshold)
      dataS <- countMat[selectR,]
      values$dataS <- dataS
      filtered_masterPeak <- values$masterPeak[selectR]
      values$filtered_masterPeak <- filtered_masterPeak
      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste0("Filtering completed successfully! Rows retained: ", nrow(dataS)),
        type = "success"
      )
      
      output$dataPreviewstep22 <- DT::renderDataTable({
        DT::datatable(
          as.data.frame(values$filtered_masterPeak),
          selection = 'none',
          extensions = 'FixedHeader',
          width = '80%',
          options = list(
            pageLength = 5,
            autoWidth = F,
            searchHighlight = TRUE,
            scrollX = TRUE,
            FixedHeader = T
          )
        )
      })
      
      updateTabsetPanel(session, "datapreview", selected = "After Filter")
      
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during filtering:", e$message),
        type = "error"
      )
    }, finally = {
      shinyjs::enable("step21")
    })
  })
  
  
  #-----------------Group------------------
  observe({
    req(values$data)
    updateSelectInput(
      session, "samples",
      choices = colnames(values$data)[4:ncol(values$data)]
    )
  })
  
  observeEvent(input$addGroup, {
    req(input$samples, input$groupName)
    if (input$groupName %in% names(values$groups)) {
      showNotification("Group name already exists!", type = "error")
      return()
    }
    values$groups[[input$groupName]] <- input$samples
    values$groupCount <- length(values$groups)
    updateSelectizeInput(session, "samples", selected = NULL)
    updateTextInput(session, "groupName", value = "")
  })
  
  observeEvent(input$clearGroups, {
    values$groups <- list()
    values$groupCount <- 0
  })
  
  output$currentGroups <- renderPrint({
    if (length(values$groups) == 0) {
      shinyjs::disable("step2_to_step3")
      return("No groups defined yet.")
    }
    shinyjs::enable("step2_to_step3")
    shinyjs::enable("step31")
    values$groups
  })
  
  observeEvent(input$step2_to_step3, {
    req(length(values$groups) > 0)  
    clean_group(T)
    sendSweetAlert(
      session = session,
      title = "Success",
      text = "Groups have been successfully created.",
      type = "success"
    )
    updateTabItems(session, "menuitem", "step3")
  })
  
  #-----------分组后更新ui界面------------
  observeEvent(input$step31, {
    output$dynamic_tabs_step3 <- renderUI({ NULL })
    if (values$groupCount == 2) {
      updateTabsetPanel(session, "visualization_tabs")
      output$dynamic_tabs_step3 <- renderUI({
        shinydashboard::tabBox(
          title = shiny::tagList(icon("table"), "Analysis Results"),
          width = 12,
          selected = "Before Filter",
          side = "right",
          tabsetPanel(
            id = "analysis_results",
            tabPanel(
              shiny::tagList("Before Filter"),
              fluidRow(
                column(3,
                       textInput(
                         inputId = "pvalueThreshold",
                         label = "P-value Threshold:",
                         value = "0.05"
                       ),
                       textInput(
                         inputId = "log2fcThreshold",
                         label = "Log2 Fold Change Threshold:",
                         value = "2"
                       ),
                       actionButton(
                         inputId = "step32",
                         label = "Submit",
                         icon = icon("check")
                       )
                ),
                column(9,
                       verbatimTextOutput("summaryText"),
                       DT::dataTableOutput("deseqResult"),
                       downloadButton(
                         outputId = "downloadDeseqResult1",
                         label = "Download"
                       )
                )
              )
            ),
            tabPanel(
              shiny::tagList("After Filter"),
              fluidRow(
                column(12,
                       DT::dataTableOutput("deseqResult2"),
                       downloadButton(
                         outputId = "downloadDeseqResult2",
                         label = "Download"
                       )
                )
              )
            )
          )
        )
      })
    } else if (values$groupCount > 2) {
      output$dynamic_tabs_step3 <- renderUI({
        group_names <- names(values$groups)
        comparisons <- combn(group_names, 2, simplify = FALSE)
        tabPanels <- lapply(comparisons, function(comp) {
          tabPanel(
            shiny::tagList(comp[1], " vs ", comp[2]),
            fluidRow(
              column(12,
                     verbatimTextOutput(outputId = paste0("summary_", comp[1], "_vs_", comp[2])),
                     DT::dataTableOutput(outputId = paste0("deseqResult_", comp[1], "_vs_", comp[2])),
                     downloadButton(
                       outputId = paste0("downloadDeseqResult_", comp[1], "_vs_", comp[2]),
                       label = "Download"
                     )
              )
            )
          )
        })
        comparison_keys <- sapply(comparisons, function(comp) paste(comp, collapse = "_vs_"))
        comparison_display <- sapply(comparisons, function(comp) paste(comp, collapse = " vs "))
        shiny::tagList(
          shinydashboard::tabBox(
            title = shiny::tagList(icon("table"), "Analysis Results"),
            width = 12,
            selected = comparison_keys[1],
            side = "right",
            do.call(tabsetPanel, c(list(id = "analysis_results"), tabPanels))
          ),
          shinydashboard::tabBox(
            title = shiny::tagList(icon("table"), "Select Comparison and Filter",
                                   bsButton("comparisonhelp", label = "", icon = icon("question"), size = "extra-small"),
                                   bsPopover(
                                     id = "comparisonhelp", 
                                     title = "Select Comparison",
                                     content = "Only one comparison group can be selected for filtering and subsequent analysis.",
                                     placement = "right", 
                                     trigger = "hover", 
                                     options = list(container = "body")
                                   )
            ),
            side = "right",
            width = 12,
            tabsetPanel(
              tabPanel(
                shiny::tagList("Filter Result"),
                fluidRow(
                  column(3,
                         selectInput(
                           inputId = "selectedComparison",
                           label = "Selected Comparison：",
                           choices = setNames(comparison_keys, comparison_display),
                           selected = comparison_keys[1]
                         ),
                         textInput(
                           inputId = "pvalueThreshold",
                           label = "P-value Threshold:",
                           value = "0.05"
                         ),
                         textInput(
                           inputId = "log2fcThreshold",
                           label = "Log2 Fold Change Threshold:",
                           value = "2"
                         ),
                         actionButton(
                           inputId = "step32",
                           label = "Submit",
                           icon = icon("check")
                         )
                  ),
                  column(9,
                         DT::dataTableOutput("filteredResult"),
                         downloadButton(
                           outputId = "downloadFilteredResult",
                           label = "Download"
                         )
                  )
                )
              )
            )
          )
        )
      })
    }
  })
  
  allowStep32 <- reactiveVal(FALSE)
  observe({
    if (!allowStep32() && !is.null(input$step32)) {
      shinyjs::disable("step32")
    }
  })
  
  #-----------Peak Differential Analysis------------
  observeEvent(input$step31, {
    req(length(values$groups) > 0)
    allowStep32(FALSE)
    dataS <- values$dataS
    showModal(modalDialog(
      title = "Running Differential Analysis",
      div(
        style = "text-align: center;",
        h4("Please wait..."),
        shiny::tags$a(img(src = "ZZ5H.gif", height = "100px"))
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    groupedSamples <- unlist(values$groups)
    filteredDataS <- dataS[, groupedSamples, drop = FALSE]
    values$filteredDataS <- filteredDataS
    
    condition <- factor(unlist(lapply(names(values$groups), function(g) {
      rep(g, length(values$groups[[g]]))
    })))
    values$condition <- condition
    
    tryCatch({
      dds <- DESeqDataSetFromMatrix(
        countData = filteredDataS,
        colData = DataFrame(condition),
        design = ~ condition
      )
      values$DDS <- DESeq(dds)
      updateTabItems(session, "menuitem", "step3")
      sendSweetAlert(
        session = session,
        title = "Success",
        text = "Differential Analysis completed successfully!",
        type = "success"
      )
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during analysis:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal()
      shinyjs::enable("step312")
    })
  })
  
  observeEvent(input$step312, {
    req(values$groups)
    clean_DAR(T)
    filteredDataS <- values$filteredDataS
    filtered_masterPeak <- values$filtered_masterPeak
    
    tryCatch({
      DDS <- values$DDS
      normDDS <- counts(DDS, normalized = TRUE)
      colnames(normDDS) <- paste0(colnames(normDDS), "_norm")
      
      if (values$groupCount == 2) {
        res <- results(DDS, 
                       independentFiltering = input$independentFiltering, 
                       altHypothesis = input$altHypothesis,
                       lfcThreshold = input$lfcThreshold,
                       alpha = input$alpha,
                       pAdjustMethod = input$pAdjustMethod)
        
        countMatDiff <- cbind(filteredDataS, normDDS, res)
        filtered_masterPeak_df <- as.data.frame(filtered_masterPeak)
        filtered_masterPeak_info <- filtered_masterPeak_df[, c("seqnames", "start", "end")]
        merged_countMatDiff <- cbind(filtered_masterPeak_info, countMatDiff)
        values$deseqResult <- merged_countMatDiff
        
        new_colnames <- colnames(merged_countMatDiff)
        new_colnames[new_colnames == "baseMean"] <- "average expression"
        new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
        new_colnames[new_colnames == "stat"] <- "Wald Statistic"
        
        output$summaryText <- renderText({
          summary_str <- capture.output(summary(res))
          formatted_summary <- paste(head(summary_str[summary_str != ""], 6), collapse = "\n")
          formatted_summary
        })
        
        output$deseqResult <- DT::renderDataTable({
          DT::datatable(
            merged_countMatDiff,
            colnames = new_colnames, 
            selection = 'none',
            extensions = 'FixedHeader',
            options = list(
              pageLength = 10,
              autoWidth = F,
              searchHighlight = TRUE,
              scrollX = TRUE,
              FixedHeader = TRUE
            )
          )
        })
        
      } else if (values$groupCount > 2) {
        # 获取所有分组名称，并生成两两组合
        group_names <- levels(values$condition)
        comparisons <- combn(group_names, 2, simplify = FALSE)
        
        results_list <- list()         # 存放每个比较的 results 对象
        merged_results_list <- list()  # 存放合并后的结果表
        summary_list <- list()         # 存放每个比较的摘要信息
        
        for (comp in comparisons) {
          comp_name <- paste(comp, collapse = "_vs_")
          comp_res <- results(DDS, contrast = c("condition", comp[1], comp[2]),
                              independentFiltering = input$independentFiltering, 
                              altHypothesis = input$altHypothesis,
                              lfcThreshold = input$lfcThreshold,
                              alpha = input$alpha,
                              pAdjustMethod = input$pAdjustMethod)
          results_list[[comp_name]] <- comp_res
          
          countMatDiff <- cbind(filteredDataS, normDDS, comp_res)
          filtered_masterPeak_df <- as.data.frame(filtered_masterPeak)
          filtered_masterPeak_info <- filtered_masterPeak_df[, c("seqnames", "start", "end")]
          merged_countMatDiff <- cbind(filtered_masterPeak_info, countMatDiff)
          merged_results_list[[comp_name]] <- merged_countMatDiff
          
          summary_list[[comp_name]] <- capture.output(summary(comp_res))
        }
        values$deseqResultList <- merged_results_list
        
        for (comp_name in names(merged_results_list)) {
          local({
            cn <- comp_name
            
            new_colnames <- colnames(merged_results_list[[cn]])
            new_colnames[new_colnames == "baseMean"] <- "average expression"
            new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
            new_colnames[new_colnames == "stat"] <- "Wald Statistic"
            
            output[[paste0("deseqResult_", cn)]] <- DT::renderDataTable({
              DT::datatable(
                merged_results_list[[cn]],
                colnames = new_colnames,
                selection = 'none',
                extensions = 'FixedHeader',
                options = list(
                  pageLength = 10,
                  autoWidth = F,
                  searchHighlight = TRUE,
                  scrollX = TRUE,
                  FixedHeader = TRUE
                )
              )
            })
            
            output[[paste0("summary_", cn)]] <- renderText({
              summary_str <- summary_list[[cn]]
              formatted_summary <- paste(head(summary_str[summary_str != ""], 6), collapse = "\n")
              formatted_summary
            })
          })
        }
      }
      allowStep32(TRUE)
      shinyjs::enable("step32")
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during analysis:", e$message),
        type = "error"
      )
    })
  })
  
  #-------------过滤-------------
  observeEvent(input$pvalueThreshold, {
    # 如果为空、NA 或转换为数字后不在 0 到 1 范围内
    if (is.null(input$pvalueThreshold) ||
        is.na(as.numeric(input$pvalueThreshold)) ||
        as.numeric(input$pvalueThreshold) < 0 ||
        as.numeric(input$pvalueThreshold) > 1) {
      showFeedbackDanger(
        inputId = "pvalueThreshold",
        text = "P-value threshold must be a number between 0 and 1."
      )
    } else {
      hideFeedback("pvalueThreshold")
      pvalueThreshold <- as.numeric(input$pvalueThreshold)
      values$pvalueThreshold <- pvalueThreshold
    }
  })
  
  observeEvent(input$log2fcThreshold, {
    # 如果为空、NA 或转换为数字后小于0
    if (is.null(input$log2fcThreshold) ||
        is.na(as.numeric(input$log2fcThreshold)) ||
        as.numeric(input$log2fcThreshold) < 0) {
      showFeedbackDanger(
        inputId = "log2fcThreshold",
        text = "Log2 fold change threshold must be a positive number."
      )
    } else {
      hideFeedback("log2fcThreshold")
      log2fcThreshold <- as.numeric(input$log2fcThreshold)
      values$log2fcThreshold <- log2fcThreshold
    }
  })
  
  observeEvent(input$step32, {
    req(values$log2fcThreshold,values$pvalueThreshold)
    clean_DRF(T)
    log2fcThreshold <- values$log2fcThreshold
    pvalueThreshold <- values$pvalueThreshold
    tryCatch({
      
      if (values$groupCount == 2) {
        req(values$deseqResult)
        dataToFilter <- values$deseqResult
      } else if (values$groupCount > 2) {
        req(values$deseqResultList)
        selectedComp <- input$selectedComparison
        if (is.null(selectedComp) || !(selectedComp %in% names(values$deseqResultList))) {
          showNotification("Please select a valid comparison result for filtering!", type = "error")
          return()
        }
        dataToFilter <- values$deseqResultList[[selectedComp]]
      }
      
      sig_peaks <- dataToFilter[
        dataToFilter$pvalue < pvalueThreshold &
          abs(dataToFilter$log2FoldChange) > log2fcThreshold, 
      ]
      
      values$sig_peaks <- sig_peaks
      
      new_colnames <- colnames(sig_peaks)
      new_colnames[new_colnames == "baseMean"] <- "average expression"
      new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
      new_colnames[new_colnames == "stat"] <- "Wald Statistic"
      
      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste0("Filtering completed successfully! Rows retained: ", nrow(sig_peaks)),
        type = "success"
      )
      
      if (values$groupCount == 2) {
        output$deseqResult2 <- DT::renderDataTable({
          DT::datatable(
            as.data.frame(sig_peaks),
            colnames = new_colnames,
            selection = 'none',
            extensions = 'FixedHeader',
            width = '80%',
            options = list(
              pageLength = 10,
              autoWidth = F,
              searchHighlight = TRUE,
              scrollX = TRUE,
              FixedHeader = TRUE
            )
          )
        })
        updateTabsetPanel(session, "analysis_results", selected = "After Filter")
      } else if (values$groupCount > 2) {
        output$filteredResult <- DT::renderDataTable({
          DT::datatable(
            as.data.frame(sig_peaks),
            colnames = new_colnames,
            selection = 'none',
            extensions = 'FixedHeader',
            width = '80%',
            options = list(
              pageLength = 10,
              autoWidth = FALSE,
              searchHighlight = TRUE,
              scrollX = TRUE,
              FixedHeader = TRUE
            )
          )
        })
      }
      shinyjs::enable("step4")
      
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during filtering:", e$message),
        type = "error"
      )
    })
  })
  
  #-----------基因注释------------
  observe({
    req(values$sig_peaks)
    sig_peaks <- values$sig_peaks
    sig_peaks_gr <- GRanges(
      seqnames = sig_peaks$seqnames, 
      ranges = IRanges(start = sig_peaks$start, end = sig_peaks$end),
      mcols = sig_peaks[, -c(1:3)]
    )
    values$sig_peaks_gr <- sig_peaks_gr
  })
  
  observeEvent(input$step4, {
    req(values$sig_peaks_gr, input$upstream2, input$downstream2)
    clean_peakanno(T)
    sig_peaks_gr <- values$sig_peaks_gr
    upstream <- as.numeric(input$upstream2)
    downstream <- as.numeric(input$downstream2)
    
    tssRegion <- c(-downstream, upstream)
    
    showModal(modalDialog(
      title = "Annotating",
      div(
        style = "text-align: center;",
        h4("Please wait..."),
        shiny::tags$a(img(src = "ZZ5H.gif", height = "100px"))
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      peakAnno <- annotatePeak(sig_peaks_gr,
                               TxDb = values$txdb,
                               annoDb = values$annoDb,
                               tssRegion = tssRegion
      )
      print(values$txdb)
      print(values$annoDb)
      values$peakAnno <- peakAnno
      peakAnno_df <- as.data.frame(peakAnno)
      colnames(peakAnno_df) <- gsub("^mcols\\.", "", colnames(peakAnno_df))
      peakAnno_df <- peakAnno_df[, !colnames(peakAnno_df) %in% "geneChr"]
      values$peakAnno_df <- peakAnno_df
      
      new_colnames <- colnames(peakAnno_df)
      new_colnames[new_colnames == "baseMean"] <- "average expression"
      new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
      new_colnames[new_colnames == "stat"] <- "Wald Statistic"
      
      output$annotationTable <- DT::renderDataTable({
        DT::datatable(
          peakAnno_df,
          colnames = new_colnames,
          selection = 'none',
          extensions = 'FixedHeader',
          options = list(
            pageLength = 5,
            autoWidth = F,
            searchHighlight = TRUE,
            scrollX = TRUE,
            FixedHeader = T
          )
        )
      })
      values$DVReady <- TRUE
      values$peakAnnoReady <- TRUE
      
      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste("Annotation completed successfully!"),
        type = "success"
      )
      
      shinyjs::enable("step51")
      
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during analysis:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal() 
    })
  })
  
  observe({
    # Tab 1: Annotation Pie Chart
    output$plotAnnoPie <- renderPlot({
      req(values$peakAnno)
      req(values$peakAnnoReady)
      print(plotAnnoPie(values$peakAnno))
    })
    
    # Tab 2: Annotation Bar Chart
    output$plotAnnoBar <- renderPlot({
      req(values$peakAnno)
      req(values$peakAnnoReady)
      print(plotAnnoBar(values$peakAnno))
    })
    
    # Tab 3: Distance to TSS
    output$plotDistToTSS <- renderPlot({
      req(values$peakAnno)
      req(values$peakAnnoReady)
      print(plotDistToTSS(values$peakAnno, title = "Distribution of peaks relative to TSS"))
    })
    
    # Tab 4: Upset Plot
    output$upsetPlot <- renderPlot({
      req(values$peakAnno)
      req(values$peakAnnoReady)
      print(upsetplot(values$peakAnno, vennpie = TRUE))
    })
  })
  
  #-----------差异分析可视化------------
  # 初始化存储参数的响应式值
  volcano_params <- reactiveValues(
    FCcutoff1 = 1.0,  # 默认值需与UI控件一致
    FCcutoff2 = 1.0,
    pCutoff = 10^-0.05,  # 假设input$pCutoff默认0.05
    pointSize = 2.0,
    labSize = 5.0,
    initialized = FALSE  # 首次渲染标志
  )
  
  # 首次自动渲染逻辑（不需要按钮）
  observe({
    req(values$peakAnno_df)  # 确保数据存在
    req(values$DVReady)
    if (!volcano_params$initialized) {
      # 使用当前UI控件的值初始化参数（首次加载时）
      volcano_params$FCcutoff1 <- input$FCcutoff1
      volcano_params$FCcutoff2 <- input$FCcutoff2
      volcano_params$pCutoff <- 10^(-input$pCutoff)
      volcano_params$pointSize <- input$pointSize
      volcano_params$labSize <- input$labSize
      volcano_params$initialized <- TRUE
    }
  })
  
  observeEvent(input$step51, {
    clean_DV(T)
    volcano_params$FCcutoff1 <- input$FCcutoff1
    volcano_params$FCcutoff2 <- input$FCcutoff2
    volcano_params$pCutoff <- 10^(-input$pCutoff)
    volcano_params$pointSize <- input$pointSize
    volcano_params$labSize <- input$labSize
    values$DVReady <- TRUE
    
    tryCatch({
      req(input$pCutoff, input$FCcutoff1, input$FCcutoff2)
      peakAnno_df <- values$peakAnno_df
      fc_cutoff1 <- input$FCcutoff1
      fc_cutoff2 <- input$FCcutoff2
      p_cutoff <- 10^(-input$pCutoff)
      red_peaks_up <- peakAnno_df[peakAnno_df$pvalue <= p_cutoff & peakAnno_df$log2FoldChange >= fc_cutoff2, ]
      red_peaks_down <- peakAnno_df[peakAnno_df$pvalue <= p_cutoff & peakAnno_df$log2FoldChange <= -fc_cutoff1, ]
      
      red_peaks_upgenes <- unique(red_peaks_up$SYMBOL)
      red_peaks_downgenes <- unique(red_peaks_down$SYMBOL)
      
      red_peaks_upgenes <- red_peaks_upgenes[!is.na(red_peaks_upgenes)]
      red_peaks_downgenes <- red_peaks_downgenes[!is.na(red_peaks_downgenes)]
      
      red_peaks <- rbind(red_peaks_up, red_peaks_down)
      red_peaks_genes <- union(red_peaks_upgenes, red_peaks_downgenes)
      
      values$red_peaks_up <- red_peaks_up
      values$red_peaks_down <- red_peaks_down
      values$red_peaks_upgenes <- red_peaks_upgenes
      values$red_peaks_downgenes <- red_peaks_downgenes
      values$red_peaks <- red_peaks
      values$red_peaks_genes <- red_peaks_genes
      
      red_peaks_up_filter <- red_peaks_up[grepl("^chr[0-9XY]+$", red_peaks_up$seqnames), ]
      red_peaks_down_filter <- red_peaks_down[grepl("^chr[0-9XY]+$", red_peaks_down$seqnames), ]
      
      uppeak_strings <- red_peaks_up_filter %>%
        arrange(desc(log2FoldChange)) %>%  
        head(min(200, nrow(red_peaks_up_filter)))
      
      downpeak_strings <- red_peaks_down_filter %>%
        arrange(log2FoldChange) %>%  
        head(min(200, nrow(red_peaks_down_filter)))
      
      uppeak_strings <- paste(
        uppeak_strings$seqnames,
        uppeak_strings$start,
        uppeak_strings$end,
        sep = "-"
      )
      downpeak_strings <- paste(
        downpeak_strings$seqnames,
        downpeak_strings$start,
        downpeak_strings$end,
        sep = "-"
      )
      values$uppeak_strings <- uppeak_strings
      values$downpeak_strings <- downpeak_strings
      
      observe({
        if (nrow(red_peaks_up) > 0) {
          updateTextAreaInput(
            session,
            "upgenes_list",
            value = paste(red_peaks_upgenes, collapse = "\n")
          )
        } else {
          updateTextAreaInput(
            session,
            "upgenes_list",
            value = "No upregulated genes"
          )
        }
      })
      
      new_colnames <- colnames(red_peaks_up)
      new_colnames[new_colnames == "baseMean"] <- "average expression"
      new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
      new_colnames[new_colnames == "stat"] <- "Wald Statistic"
      output$upgenes_table <- DT::renderDataTable({
        DT::datatable(
          red_peaks_up,
          colnames = new_colnames,
          selection = 'none',
          extensions = 'FixedHeader',
          width = '80%',
          options = list(
            pageLength = 5,
            autoWidth = F,
            searchHighlight = TRUE,
            scrollX = TRUE,
            FixedHeader = T
          )
        )
      })
      
      observe({
        if (nrow(red_peaks_down) > 0) {
          updateTextAreaInput(
            session,
            "downgenes_list",
            value = paste(red_peaks_downgenes, collapse = "\n")
          )
        } else {
          updateTextAreaInput(
            session,
            "downgenes_list",
            value = "No downregulated genes"
          )
        }
      })
      
      new_colnames <- colnames(red_peaks_down)
      new_colnames[new_colnames == "baseMean"] <- "average expression"
      new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
      new_colnames[new_colnames == "stat"] <- "Wald Statistic"
      output$downgenes_table <- DT::renderDataTable({
        DT::datatable(
          red_peaks_down,
          colnames = new_colnames,
          selection = 'none',
          extensions = 'FixedHeader',
          width = '80%',
          options = list(
            pageLength = 5,
            autoWidth = F,
            searchHighlight = TRUE,
            scrollX = TRUE,
            FixedHeader = T
          )
        )
      })
      
      #富集分析基因表
      observe({
        if (nrow(red_peaks_up) > 0) {
          updateTextAreaInput(
            session,
            "upgenes_list2",
            value = paste(red_peaks_upgenes, collapse = "\n")
          )
        } else {
          updateTextAreaInput(
            session,
            "upgenes_list2",
            value = "No upregulated genes"
          )
        }
      })
      
      observe({
        if (nrow(red_peaks_down) > 0) {
          updateTextAreaInput(
            session,
            "downgenes_list2",
            value = paste(red_peaks_downgenes, collapse = "\n")
          )
        } else {
          updateTextAreaInput(
            session,
            "downgenes_list2",
            value = "No downregulated genes"
          )
        }
      })
      
      observe({
        if (nrow(red_peaks) > 0) {
          updateTextAreaInput(
            session,
            "GSEA_genelist",
            value = paste(red_peaks_genes, collapse = "\n")
          )
        } else {
          updateTextAreaInput(
            session,
            "GSEA_genelist",
            value = "No significant genes"
          )
        }
      })
      
      # Motif module gene list
      observe({
        if (length(uppeak_strings) > 0) {
          updateTextAreaInput(
            session,
            "upgenes_list3",
            value = paste(uppeak_strings, collapse = "\n")
          )
        } else {
          updateTextAreaInput(
            session,
            "upgenes_list3",
            value = "No upregulated peaks"
          )
        }
      })
      
      observe({
        if (length(downpeak_strings) > 0) {
          updateTextAreaInput(
            session,
            "downgenes_list3",
            value = paste(downpeak_strings, collapse = "\n")
          )
        } else {
          updateTextAreaInput(
            session,
            "downgenes_list3",
            value = "No downregulated peaks"
          )
        }
      })
      
      shinyjs::enable("step6")
      shinyjs::enable("step7")
      
      sendSweetAlert(
        session = session,
        title = "Success",
        text = "Successfully filtered upregulated and downregulated genes!",
        type = "success"
      )
      
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred:", e$message),
        type = "error"
      )
    })
  })
  
  # Tab 1: Volcanoplot
  output$volcanoplot <- renderPlot({
    req(values$peakAnno_df, volcano_params$initialized)
    req(values$DVReady)
    
    if (values$groupCount == 2) {
      group_names <- names(values$groups)
      if (length(group_names) >= 2) {
        plotTitle <- paste(group_names[1], "vs", group_names[2])
      } else {
        plotTitle <- "Volcano Plot"
      }
    } else if (values$groupCount > 2) {
      if (!is.null(input$selectedComparison)) {
        plotTitle <- gsub("_vs_", " vs ", input$selectedComparison)
      } else {
        plotTitle <- "Volcano Plot"
      }
    }
    enhancedVolcano(
      values$peakAnno_df,
      lab = values$peakAnno_df$SYMBOL,
      x = 'log2FoldChange',
      y = 'pvalue',
      title = plotTitle,
      pCutoff = volcano_params$pCutoff,
      FCcutoff1 = volcano_params$FCcutoff1,
      FCcutoff2 = volcano_params$FCcutoff2,
      pointSize = volcano_params$pointSize,
      labSize = volcano_params$labSize
    )
  })
  
  
  # Tab 2: MA
  output$maplot <- renderPlot({
    req(values$peakAnno_df)
    req(values$DVReady)
    peakAnno_df2 <- values$peakAnno_df
    peakAnno_df2$change <- ifelse(values$peakAnno_df$log2FoldChange > 0 & values$peakAnno_df$padj < 0.05, "UP",
                                  ifelse(values$peakAnno_df$log2FoldChange < 0 & values$peakAnno_df$padj < 0.05, "DOWN", NA))
    
    top_upregulated_genes <- peakAnno_df2 %>%
      arrange(desc(log2FoldChange)) %>%
      head(input$topUpGenes)
    top_downregulated_genes <- peakAnno_df2 %>%
      arrange(log2FoldChange) %>%
      head(input$topDownGenes)
    top_genes <- rbind(top_upregulated_genes, top_downregulated_genes)
    
    print(
      ggplot(peakAnno_df2, aes(x = baseMean, y = log2FoldChange)) +
        geom_point(aes(color = change), alpha = 0.5) +
        scale_color_manual(values = c("DOWN" = "lightblue", "UP" = "lightcoral")) +
        scale_x_log10() +  # 采用 log10 刻度显示基因的平均表达量
        labs(title = "MA Plot", x = "Base Mean", y = "Log2 Fold Change") +
        theme_minimal() +
        geom_text_repel(data = top_genes, aes(label = SYMBOL),
                        size = input$size,  # 从输入框获取 size
                        color = "black",
                        box.padding = input$boxPadding,  # 从输入框获取 box.padding
                        max.overlaps = input$maxOverlaps)  # 从输入框获取 max.overlaps
      
    )
  })
  
  # Tab 3: PCA
  output$pcaplot <- renderPlot({
    req(values$peakAnno_df)
    req(values$DVReady)
    condition <- values$condition
    rlogData <- rlog(values$DDS) 
    pcaData <- prcomp(t(assay(rlogData)))
    pcaDF <- as.data.frame(pcaData$x)
    ggplot(pcaDF, aes(x = PC1, y = PC2)) +
      geom_point(aes(color = condition),size=4) +
      theme_minimal() +
      labs(title = "PCA Plot", x = "PC1", y = "PC2") +
      theme(legend.position = "top")
  })
  
  # Tab 4: Heatmap
  output$heatmapPlot <- renderPlot({
    req(values$peakAnno_df,values$filteredDataS)
    req(values$DVReady)
    sample_cols <- colnames(values$filteredDataS)#[4:ncol(values$peakAnno_df)]
    summarized_data <- values$peakAnno_df %>%
      dplyr::select(annotation, all_of(sample_cols)) %>%
      mutate(annotation = case_when(
        grepl("^Promoter", annotation) ~ "Promoter",
        grepl("^Intron", annotation) ~ "Intron",
        grepl("^Exon", annotation) ~ "Exon",
        TRUE ~ annotation
      ))
    summarized_data <- summarized_data %>%
      group_by(annotation) %>%
      summarise(across(all_of(sample_cols), sum))
    summarized_data_df <- as.data.frame(summarized_data)
    rownames(summarized_data_df) <- summarized_data_df$annotation
    data_for_heatmap <- summarized_data_df %>% dplyr::select(-annotation)
    print(pheatmap(data_for_heatmap,
                   scale = "row",  # 可以选择对行进行标准化
                   clustering_distance_rows = "euclidean",
                   clustering_distance_cols = "euclidean",
                   clustering_method = "complete",  # 聚类方法
                   main = "Peak Count in Regions"))
  })
  
  
  #-------------------富集分析-------------------
  observeEvent(input$step6, {
    output$dynamic_tabs <- renderUI({NULL})
    clean_enrichment(T)
    # GO analysis type
    if (input$analysis_type == "GO") {
      updateTabsetPanel(session, "visualization_tabs")
      output$dynamic_tabs <- renderUI({
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("table"), "Upregulated Genes Result Table"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("enrichment_result_table_up"), #%>% withSpinner(),
            downloadButton("Downloadenrichmenttableup", "Download")
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Downregulated Genes Result Table"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("enrichment_result_table_down"), #%>% withSpinner(),
            downloadButton("Downloadenrichmenttabledown", "Download")
          ),
          shinydashboard::tabBox(
            title = tagList(icon("chart-column"), "Enrichment Result Visualization"),
            width = 12,
            selected = "GOdotplot",
            side = "right",
            tabsetPanel(
              id = "visualization_tabs",
              tabPanel(
                tagList(icon("chart-bar"), "Dot Plot"),
                fluidRow(
                  column(3,
                         br(),
                         div(
                           h4("Upregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6GOdotplot1", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadGOdotplot1", "Download")))
                  ),
                  column(9,
                         plotOutput("GOdotplot1", width = "100%") %>% withSpinner()
                  ),
                  column(3,
                         br(),
                         div(
                           h4("Downregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6GOdotplot2", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadGOdotplot2", "Download")))
                  ),
                  column(9,
                         plotOutput("GOdotplot2", width = "100%") %>% withSpinner()
                  )
                ),
                value = "GOdotplot"
              ),
              tabPanel(
                title = tagList(icon("chart-bar"), "Bar Plot"),
                fluidRow(
                  column(3,
                         br(),
                         div(
                           h4("Upregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6GObarplot1", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadGObarplot1", "Download")))
                  ),
                  column(9,
                         plotOutput("GObarplot1", width = "100%") %>% withSpinner()
                  ),
                  column(3,
                         br(),
                         div(
                           h4("Downregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6GObarplot2", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadGObarplot2", "Download")))
                  ),
                  column(9,
                         plotOutput("GObarplot2", width = "100%") %>% withSpinner()
                  )
                ),
                value = "GObarplot"
              ),
              tabPanel(
                title = tagList(icon("sitemap"), "Map Plot"),
                fluidRow(
                  column(3,
                         br(),
                         div(
                           h4("Upregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6GOmapplot1", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadGOmapplot1", "Download")))
                  ),
                  column(9,
                         plotOutput("GOmapplot1", width = "100%") %>% withSpinner()
                  ),
                  column(3,
                         br(),
                         div(
                           h4("Downregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6GOmapplot2", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadGOmapplot2", "Download")))
                  ),
                  column(9,
                         plotOutput("GOmapplot2", width = "100%") %>% withSpinner()
                  )
                ),
                value = "GOmapplot"
              )
            )
          )
        )
      })
    }
    
    # KEGG analysis type
    else if (input$analysis_type == "KEGG") {
      updateTabsetPanel(session, "visualization_tabs")
      output$dynamic_tabs <- renderUI({
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("table"), "Upregulated Genes Result Table"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("enrichment_result_table_up"), #%>% withSpinner(),
            downloadButton("Downloadenrichmenttableup", "Download")
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Downregulated Genes Result Table"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("enrichment_result_table_down"), #%>% withSpinner(),
            downloadButton("Downloadenrichmenttabledown", "Download")
          ),
          shinydashboard::tabBox(
            title = tagList(icon("chart-column"), "Enrichment Result Visualization"),
            width = 12,
            selected = "KEGGdotplot",
            side = "right",
            tabsetPanel(
              id = "visualization_tabs",
              tabPanel(
                title = tagList(icon("chart-bar"), "Dot Plot"),
                fluidRow(
                  column(3,
                         br(),
                         div(
                           h4("Upregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6KEGGdotplot1", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadKEGGdotplot1", "Download")))
                  ),
                  column(9,
                         plotOutput("KEGGdotplot1", width = "100%") %>% withSpinner()
                  ),
                  column(3,
                         br(),
                         div(
                           h4("Downregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6KEGGdotplot2", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadKEGGdotplot2", "Download")))
                  ),
                  column(9,
                         plotOutput("KEGGdotplot2", width = "100%") %>% withSpinner()
                  )
                ),
                value = "KEGGdotplot"
              ),
              tabPanel(
                title = tagList(icon("chart-bar"), "Bar Plot"),
                fluidRow(
                  column(3,
                         br(),
                         div(
                           h4("Upregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6KEGGbarplot1", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadKEGGbarplot1", "Download")))
                  ),
                  column(9,
                         plotOutput("KEGGbarplot1", width = "100%") %>% withSpinner()
                  ),
                  column(3,
                         br(),
                         div(
                           h4("Downregulated Genes"),
                           style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                         ),
                         br(),
                         br(),
                         fluidRow(column(12, radioButtons("extPlot6KEGGbarplot2", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadKEGGbarplot2", "Download")))
                  ),
                  column(9,
                         plotOutput("KEGGbarplot2", width = "100%") %>% withSpinner()
                  )
                ),
                value = "KEGGbarplot"
              )
            )
          )
        )
      })
    }
    
    # GSEA analysis type
    else if (input$analysis_type == "GSEA") {
      updateTabsetPanel(session, "visualization_tabs")
      output$dynamic_tabs <- renderUI({
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("table"), "Result Table"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("enrichment_result_table_gsea"), #%>% withSpinner(),
            downloadButton("Downloadenrichmenttablegsea", "Download")
          ),
          shinydashboard::tabBox(
            title = tagList(icon("chart-column"), "Enrichment Result Visualization"),
            width = 12,
            selected = "gseaplot",
            side = "right",
            tabsetPanel(
              id = "visualization_tabs",
              tabPanel(
                title = tagList(icon("chart-line"), "GSEA Plot"),
                fluidRow(
                  column(3,
                         fluidRow(column(12, radioButtons("extPlot6gseaplot", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("Downloadgseaplot", "Download")))
                  ),
                  column(9,
                         plotOutput("gseaplot", width = "100%") %>% withSpinner()
                  )
                ),
                value = "gseaplot"
              ),
              tabPanel(
                title = tagList(icon("chart-bar"), "Dot Plot"),
                fluidRow(
                  column(3,
                         fluidRow(column(12, radioButtons("extPlot6GSEAdotplot", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadGSEAdotplot", "Download")))
                  ),
                  column(9,
                         plotOutput("GSEAdotplot", width = "100%") %>% withSpinner()
                  )
                ),
                value = "GSEAdotplot"
              ),
              tabPanel(
                title = tagList(icon("mountain"), "Ridge Plot"),
                fluidRow(
                  column(3,
                         fluidRow(column(12, radioButtons("extPlot6GSEAridgeplot", "Output Format",
                                                          c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                         fluidRow(column(12, downloadButton("DownloadGSEAridgeplot", "Download")))
                  ),
                  column(9,
                         plotOutput("GSEAridgeplot", width = "100%") %>% withSpinner()
                  )
                ),
                value = "GSEAridgeplot"
              )
            )
          )
        )
      })
    }
    
  })
  #######################################################
  output$GSEA_geneset_ui <- renderUI({
    if (input$analysis_type == "GSEA") {
      if (input$Selectdata == "Example Data" || input$species == "Human") {
        tagList(
          selectInput("GSEA_geneset", "Select Gene Set for GSEA:",
                      choices = c("H: hallmark gene sets" = "H", 
                                  "C1: positional gene sets" = "C1",
                                  "C2: curated gene sets" = "C2",
                                  "C3: regulatory target gene sets" = "C3",
                                  "C4: computational gene sets" = "C4",
                                  "C5: ontology gene sets" = "C5",
                                  "C6: oncogenic signature gene sets" = "C6",
                                  "C7: immunologic signature gene sets" = "C7",
                                  "C8: cell type signature gene sets" = "C8",
                                  "All gene sets" = "ALL"),
                      selected = "H"),
          numericInput("GSEA_pvalue_cutoff", "GSEA p-value Cutoff:",
                       value = 0.1, min = 0, max = 1, step = 0.01)
        )
      } else if (input$species == "Mouse" && input$Selectdata != "Example Data") {
        tagList(
          selectInput("GSEA_geneset", "Select Gene Set for GSEA:",
                      choices = c("MH: hallmark gene sets" = "MH", 
                                  "M1: positional gene sets" = "M1",
                                  "M2: curated gene sets" = "M2",
                                  "M3: regulatory target gene sets" = "M3",
                                  "M5: ontology gene sets" = "M5",
                                  "M8: cell type signature gene sets" = "M8",
                                  "All gene sets" = "ALL"),
                      selected = "MH"),
          numericInput("GSEA_pvalue_cutoff", "GSEA p-value Cutoff:",
                       value = 0.1, min = 0, max = 1, step = 0.01)
        )
      } else if (input$species == "Drosophila" && input$Selectdata != "Example Data") {
        tagList(
          selectInput("GSEA_geneset", "Select Gene Set for GSEA:",
                      choices = c("GO", "KEGG"),
                      selected = "GO"),
          conditionalPanel(
            condition = "input.GSEA_geneset == 'GO'",
            selectInput("GSEA_go_ontology", "GO Ontology for GSEA:",
                        choices = c("Biological Process" = "BP",
                                    "Molecular Function" = "MF",
                                    "Cellular Component" = "CC",
                                    "All" = "ALL"),
                        selected = "ALL")
          ),
          numericInput("GSEA_pvalue_cutoff", "GSEA p-value Cutoff:",
                       value = 0.1, min = 0, max = 1, step = 0.01)
        )
      }
    }
  })
  
  observeEvent(input$step6, {
    showModal(modalDialog(
      title = "Run Enrichment analysis",
      div(
        style = "text-align: center;",
        h4("Please wait..."),
        shiny::tags$a(img(src = "ZZ5H.gif", height = "100px"))
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    selected_row <- NULL
    
    tryCatch({
      upgenes_list_enrichment <- input$upgenes_list2
      downgenes_list_enrichment <- input$downgenes_list2
      GSEA_genelist_enrichment <- input$GSEA_genelist
      # 处理upgenes_list_enrichment
      if (length(upgenes_list_enrichment) == 0 || upgenes_list_enrichment == "No upregulated genes") {
        upgenes_list_enrichment <- NULL
      } else {
        upgenes_list_enrichment <- unlist(strsplit(upgenes_list_enrichment, "\n"))
        upgenes_list_enrichment = bitr(upgenes_list_enrichment, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = values$annoDb)
        upgenes_list_enrichment = dplyr::distinct(upgenes_list_enrichment, ENTREZID, .keep_all = F)
        upgenes_list_enrichment <- as.character(upgenes_list_enrichment$ENTREZID)
        if(!is.null(input$species) && input$species == "Drosophila"){
          upgenes_list_enrichment_dm_kegg <- input$upgenes_list2
          upgenes_list_enrichment_dm_kegg <- unlist(strsplit(upgenes_list_enrichment_dm_kegg, "\n"))
          upgenes_list_enrichment_dm_kegg = bitr(upgenes_list_enrichment_dm_kegg, fromType = "SYMBOL", toType = "FLYBASECG", OrgDb = values$annoDb)
          upgenes_list_enrichment_dm_kegg = dplyr::distinct(upgenes_list_enrichment_dm_kegg, FLYBASECG, .keep_all = F)
          upgenes_list_enrichment_dm_kegg$FLYBASECG <- paste0("Dmel_", upgenes_list_enrichment_dm_kegg$FLYBASECG)
          upgenes_list_enrichment_dm_kegg <- as.character(upgenes_list_enrichment_dm_kegg$FLYBASECG)
        }
      }
      
      # 处理downgenes_list_enrichment
      if (length(downgenes_list_enrichment) == 0 || downgenes_list_enrichment == "No downregulated genes") {
        downgenes_list_enrichment <- NULL
      } else {
        downgenes_list_enrichment <- unlist(strsplit(downgenes_list_enrichment, "\n"))
        downgenes_list_enrichment = bitr(downgenes_list_enrichment, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = values$annoDb)
        downgenes_list_enrichment = dplyr::distinct(downgenes_list_enrichment, ENTREZID, .keep_all = F)
        downgenes_list_enrichment <- as.character(downgenes_list_enrichment$ENTREZID)
        if(!is.null(input$species) && input$species == "Drosophila"){
          downgenes_list_enrichment_dm_kegg <- input$downgenes_list2
          downgenes_list_enrichment_dm_kegg <- unlist(strsplit(downgenes_list_enrichment_dm_kegg, "\n"))
          downgenes_list_enrichment_dm_kegg = bitr(downgenes_list_enrichment_dm_kegg, fromType = "SYMBOL", toType = "FLYBASECG", OrgDb = values$annoDb)
          downgenes_list_enrichment_dm_kegg = dplyr::distinct(downgenes_list_enrichment_dm_kegg, FLYBASECG, .keep_all = F)
          downgenes_list_enrichment_dm_kegg$FLYBASECG <- paste0("Dmel_", downgenes_list_enrichment_dm_kegg$FLYBASECG)
          downgenes_list_enrichment_dm_kegg <- as.character(downgenes_list_enrichment_dm_kegg$FLYBASECG)
        }
      }
      # 处理GSEA genelist
      #人类小鼠
      if(input$species == "Human" || input$species == "Mouse" || input$Selectdata == "Example Data"){
        if (length(GSEA_genelist_enrichment) == 0 || GSEA_genelist_enrichment == "No significant genes") {
          geneList_vector <- NULL
        } else {
          GSEA_genelist_enrichment <- unlist(strsplit(GSEA_genelist_enrichment, "\n"))
          GSEA_genelist_enrichment <- unique(GSEA_genelist_enrichment)
          geneList_gsea <- values$peakAnno_df[values$peakAnno_df$SYMBOL %in% GSEA_genelist_enrichment, c("SYMBOL", "log2FoldChange")]
          geneList_gsea <- geneList_gsea[!is.na(geneList_gsea$SYMBOL) & !is.na(geneList_gsea$log2FoldChange), ]
          geneList_gsea <- geneList_gsea %>%
            group_by(SYMBOL) %>%
            summarise(log2FoldChange = mean(log2FoldChange)) %>%
            arrange(desc(log2FoldChange))
          geneList_vector <- geneList_gsea$log2FoldChange
          names(geneList_vector) <- as.character(geneList_gsea$SYMBOL)
          print(geneList_vector)
        }
        #果蝇
      }else if(!is.null(input$species) && input$species == "Drosophila"){
        if (length(GSEA_genelist_enrichment) == 0 || GSEA_genelist_enrichment == "No significant genes") {
          geneList_vector <- NULL
        } else {
          # GSEA_genelist_enrichment <- unlist(strsplit(GSEA_genelist_enrichment, "\n"))
          # GSEA_genelist_enrichment <- unique(GSEA_genelist_enrichment)
          # geneList_gsea <- values$peakAnno_df[values$peakAnno_df$SYMBOL %in% GSEA_genelist_enrichment, c("SYMBOL", "geneId", "log2FoldChange")]
          # geneList_gsea <- geneList_gsea[!is.na(geneList_gsea$geneId) & !is.na(geneList_gsea$log2FoldChange), ]
          # geneList_gsea <- geneList_gsea %>%
          #   group_by(geneId) %>%
          #   summarise(log2FoldChange = mean(log2FoldChange)) %>%
          #   arrange(desc(log2FoldChange))
          # geneList_vector <- geneList_gsea$log2FoldChange
          # names(geneList_vector) <- as.character(geneList_gsea$geneId)
          GSEA_genelist_enrichment <- unlist(strsplit(GSEA_genelist_enrichment, "\n"))
          GSEA_genelist_enrichment <- unique(GSEA_genelist_enrichment)
          geneList_gsea <- values$peakAnno_df[values$peakAnno_df$SYMBOL %in% GSEA_genelist_enrichment, c("SYMBOL", "log2FoldChange")]
          geneList_gsea <- geneList_gsea[!is.na(geneList_gsea$SYMBOL) & !is.na(geneList_gsea$log2FoldChange), ]
          
          gene_conversion1 <- bitr(geneList_gsea$SYMBOL, 
                                   fromType = "SYMBOL", 
                                   toType = "ENTREZID", 
                                   OrgDb = values$annoDb)
          geneList_gsea1 <- merge(geneList_gsea, gene_conversion1, by = "SYMBOL")
          geneList_gsea1 <- geneList_gsea1 %>%
            group_by(ENTREZID) %>%
            summarise(log2FoldChange = mean(log2FoldChange)) %>%
            arrange(desc(log2FoldChange))
          geneList_vector1 <- geneList_gsea1$log2FoldChange
          names(geneList_vector1) <- as.character(geneList_gsea1$ENTREZID)
          print(geneList_vector1)
          
          gene_conversion2 <- bitr(geneList_gsea$SYMBOL, 
                                   fromType = "SYMBOL", 
                                   toType = "FLYBASECG", 
                                   OrgDb = values$annoDb)
          geneList_gsea2 <- merge(geneList_gsea, gene_conversion2, by = "SYMBOL")
          geneList_gsea2 <- geneList_gsea2 %>%
            group_by(FLYBASECG) %>%
            summarise(log2FoldChange = mean(log2FoldChange)) %>%
            arrange(desc(log2FoldChange))
          geneList_vector2 <- geneList_gsea2$log2FoldChange
          geneList_gsea2$FLYBASECG <- paste0("Dmel_", geneList_gsea2$FLYBASECG)
          names(geneList_vector2) <- as.character(geneList_gsea2$FLYBASECG)
          print(geneList_vector2)
        }
      }
      
      if(input$analysis_type == "GO") {
        ##########GO##########
        ontology <- switch(input$go_ontology,
                           "BP" = "BP",
                           "MF" = "MF",
                           "CC" = "CC",
                           "ALL" = "ALL")
        
        # ---------up------------------
        if (is.null(upgenes_list_enrichment) || length(upgenes_list_enrichment) == 0) {
          output$enrichment_result_table_up <- DT::renderDataTable({
            DT::datatable(data.frame(Message = "Upregulated gene list is empty."),selection = 'none')
          })
          output$GOdotplot1 <- renderPlot({
            plot.new()
            text(0.5, 0.5, "Upregulated gene list is empty.", cex = 1.5)
          })
          output$GObarplot1 <- renderPlot({
            plot.new()
            text(0.5, 0.5, "Upregulated gene list is empty.", cex = 1.5)
          })
          output$GOmapplot1 <- renderPlot({
            plot.new()
            text(0.5, 0.5, "Upregulated gene list is empty.", cex = 1.5)
          })
        } else {
          
          go_results_up <- enrichGO(
            gene = upgenes_list_enrichment,
            OrgDb = values$annoDb,
            ont = ontology,
            pAdjustMethod = "BH",
            pvalueCutoff = input$pvalue_cutoff,
            qvalueCutoff = input$qvalue_cutoff
          )
          go_results_up<- setReadable(go_results_up, OrgDb = values$annoDb ,keyType = 'ENTREZID')
          values$go_results_up <- go_results_up
          output$enrichment_result_table_up <- DT::renderDataTable({
            DT::datatable(as.data.frame(go_results_up),
                          selection = 'none',
                          extensions = 'FixedHeader',
                          options = list(
                            pageLength = 5,
                            autoWidth = F,
                            searchHighlight = TRUE,
                            scrollX = TRUE,
                            FixedHeader = T
                          )
            )
          })
          
          if (nrow(go_results_up)>0) {
            
            output$GOdotplot1 <- renderPlot({
              dotplot(go_results_up)
            })
            
            output$GObarplot1 <- renderPlot({
              barplot(go_results_up)
            })
            
            output$GOmapplot1 <- renderPlot({
              GO_result_plot_up <- pairwise_termsim(go_results_up)
              emapplot(GO_result_plot_up, showCategory = 20, label_format = 30)
            })
            
          }else{
            output$GOdotplot1 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "No enriched term found.", cex = 1.5)
            })
            output$GObarplot1 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "No enriched term found.", cex = 1.5)
            })
            output$GOmapplot1 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "No enriched term found.", cex = 1.5)
            })
          }
          
        }
        #---------down------------------
        if (is.null(downgenes_list_enrichment) || length(downgenes_list_enrichment) == 0) {
          output$enrichment_result_table_down <- DT::renderDataTable({
            DT::datatable(data.frame(Message = "Downregulated gene list is empty."),selection = 'none')
          })
          output$GOdotplot2 <- renderPlot({
            plot.new()
            text(0.5, 0.5, "Downregulated gene list is empty.", cex = 1.5)
          })
          output$GObarplot2 <- renderPlot({
            plot.new()
            text(0.5, 0.5, "Downregulated gene list is empty.", cex = 1.5)
          })
          output$GOmapplot2 <- renderPlot({
            plot.new()
            text(0.5, 0.5, "Downregulated gene list is empty.", cex = 1.5)
          })
        } else {
          
          go_results_down <- enrichGO(
            gene = downgenes_list_enrichment,
            OrgDb = values$annoDb,
            ont = ontology,
            pAdjustMethod = "BH",
            pvalueCutoff = input$pvalue_cutoff,
            qvalueCutoff = input$qvalue_cutoff
          )
          go_results_down<- setReadable(go_results_down, OrgDb = values$annoDb ,keyType = 'ENTREZID')
          values$go_results_down <- go_results_down
          output$enrichment_result_table_down <- DT::renderDataTable({
            DT::datatable(as.data.frame(go_results_down),
                          selection = 'none',
                          extensions = 'FixedHeader',
                          options = list(
                            pageLength = 5,
                            autoWidth = F,
                            searchHighlight = TRUE,
                            scrollX = TRUE,
                            FixedHeader = T
                          )
            )
          })
          
          if (nrow(go_results_down)>0) {
            
            output$GOdotplot2 <- renderPlot({
              dotplot(go_results_down)
            })
            
            output$GObarplot2 <- renderPlot({
              barplot(go_results_down)
            })
            
            output$GOmapplot2 <- renderPlot({
              GO_result_plot_down <- pairwise_termsim(go_results_down)
              emapplot(GO_result_plot_down, showCategory = 20, label_format = 30)
            })
            
          }else{
            output$GOdotplot2 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "No enriched term found.", cex = 1.5)
            })
            output$GObarplot2 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "No enriched term found.", cex = 1.5)
            })
            output$GOmapplot2 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "No enriched term found.", cex = 1.5)
            })
          }
        }
        
      } else if(input$analysis_type == "KEGG") {
        ##########KEGG##########
        # ---------up------------------
        if(input$species == "Human" || input$species == "Mouse" || input$Selectdata == "Example Data"){
          if (is.null(upgenes_list_enrichment) || length(upgenes_list_enrichment) == 0) {
            output$enrichment_result_table_up <- DT::renderDataTable({
              DT::datatable(
                data.frame(Message = "Upregulated gene list is empty."),selection = 'none'
              )
            })
            output$KEGGdotplot1 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Upregulated gene list is empty.", cex = 1.5)
              
            })
            output$KEGGbarplot1 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Upregulated gene list is empty.", cex = 1.5)
            })
          } else {
            
            kegg_results_up <- enrichKEGG(
              gene = upgenes_list_enrichment,
              organism = values$Organism, 
              pAdjustMethod = "BH", 
              pvalueCutoff = input$pvalue_cutoff,
              qvalueCutoff = input$qvalue_cutoff
            )
            kegg_results_up<- setReadable(kegg_results_up, OrgDb = values$annoDb ,keyType = 'ENTREZID')
            values$kegg_results_up <- kegg_results_up
            output$enrichment_result_table_up <- DT::renderDataTable({
              DT::datatable(
                as.data.frame(kegg_results_up),
                selection = 'none',
                extensions = 'FixedHeader',
                options = list(
                  pageLength = 5,
                  autoWidth = F,
                  searchHighlight = TRUE,
                  scrollX = TRUE,
                  FixedHeader = T
                )
              )
            })
            
            if (nrow(kegg_results_up)>0) {
              
              output$KEGGdotplot1 <- renderPlot({
                dotplot(kegg_results_up)
              })
              
              output$KEGGbarplot1 <- renderPlot({
                barplot(kegg_results_up)
              })
              
            }else{
              output$KEGGdotplot1 <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
              output$KEGGbarplot1 <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
            }
          }
          
          # ---------down------------------
          if (is.null(downgenes_list_enrichment) || length(downgenes_list_enrichment) == 0) {
            output$enrichment_result_table_down <- DT::renderDataTable({
              DT::datatable(
                data.frame(Message = "Downregulated gene list is empty."),selection = 'none'
              )
            })
            output$KEGGdotplot2 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Downregulated gene list is empty.", cex = 1.5)
            })
            
            output$KEGGbarplot2 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Downregulated gene list is empty.", cex = 1.5)
            })
          } else {
            
            kegg_results_down <- enrichKEGG(
              gene = downgenes_list_enrichment,
              organism = values$Organism, 
              pAdjustMethod = "BH", 
              pvalueCutoff = input$pvalue_cutoff,
              qvalueCutoff = input$qvalue_cutoff
            )
            kegg_results_down<- setReadable(kegg_results_down, OrgDb = values$annoDb ,keyType = 'ENTREZID')
            values$kegg_results_down <- kegg_results_down
            output$enrichment_result_table_down <- DT::renderDataTable({
              DT::datatable(
                as.data.frame(kegg_results_down),
                selection = 'none',
                extensions = 'FixedHeader',
                options = list(
                  pageLength = 5,
                  autoWidth = F,
                  searchHighlight = TRUE,
                  scrollX = TRUE,
                  FixedHeader = T
                )
              )
            })
            
            if (nrow(kegg_results_down)>0) {
              
              output$KEGGdotplot2 <- renderPlot({
                dotplot(kegg_results_down)
              })
              
              output$KEGGbarplot2 <- renderPlot({
                barplot(kegg_results_down)
              })
              
            }else{
              output$KEGGdotplot2 <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
              output$KEGGbarplot2 <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
            }
          }
        } else if(!is.null(input$species) && input$species == "Drosophila"){
          upgenes_list_enrichment <- upgenes_list_enrichment_dm_kegg
          downgenes_list_enrichment <- downgenes_list_enrichment_dm_kegg
          
          if (is.null(upgenes_list_enrichment) || length(upgenes_list_enrichment) == 0) {
            output$enrichment_result_table_up <- DT::renderDataTable({
              DT::datatable(
                data.frame(Message = "Upregulated gene list is empty."),selection = 'none'
              )
            })
            output$KEGGdotplot1 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Upregulated gene list is empty.", cex = 1.5)
              
            })
            output$KEGGbarplot1 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Upregulated gene list is empty.", cex = 1.5)
            })
          } else {
            
            kegg_results_up <- enrichKEGG(
              gene = upgenes_list_enrichment,
              organism = values$Organism, 
              pAdjustMethod = "BH", 
              pvalueCutoff = input$pvalue_cutoff,
              qvalueCutoff = input$qvalue_cutoff
            )
            # kegg_results_up<- setReadable(kegg_results_up, OrgDb = values$annoDb ,keyType = 'ENTREZID')
            values$kegg_results_up <- kegg_results_up
            output$enrichment_result_table_up <- DT::renderDataTable({
              DT::datatable(
                as.data.frame(kegg_results_up),
                selection = 'none',
                extensions = 'FixedHeader',
                options = list(
                  pageLength = 5,
                  autoWidth = F,
                  searchHighlight = TRUE,
                  scrollX = TRUE,
                  FixedHeader = T
                )
              )
            })
            
            if (nrow(kegg_results_up)>0) {
              
              output$KEGGdotplot1 <- renderPlot({
                dotplot(kegg_results_up)
              })
              
              output$KEGGbarplot1 <- renderPlot({
                barplot(kegg_results_up)
              })
              
            }else{
              output$KEGGdotplot1 <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
              output$KEGGbarplot1 <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
            }
          }
          
          # ---------down------------------
          if (is.null(downgenes_list_enrichment) || length(downgenes_list_enrichment) == 0) {
            output$enrichment_result_table_down <- DT::renderDataTable({
              DT::datatable(
                data.frame(Message = "Downregulated gene list is empty."),selection = 'none'
              )
            })
            output$KEGGdotplot2 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Downregulated gene list is empty.", cex = 1.5)
            })
            
            output$KEGGbarplot2 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Downregulated gene list is empty.", cex = 1.5)
            })
          } else {
            
            kegg_results_down <- enrichKEGG(
              gene = downgenes_list_enrichment,
              organism = values$Organism, 
              pAdjustMethod = "BH", 
              pvalueCutoff = input$pvalue_cutoff,
              qvalueCutoff = input$qvalue_cutoff
            )
            # kegg_results_down<- setReadable(kegg_results_down, OrgDb = values$annoDb ,keyType = 'ENTREZID')
            values$kegg_results_down <- kegg_results_down
            output$enrichment_result_table_down <- DT::renderDataTable({
              DT::datatable(
                as.data.frame(kegg_results_down),
                selection = 'none',
                extensions = 'FixedHeader',
                options = list(
                  pageLength = 5,
                  autoWidth = F,
                  searchHighlight = TRUE,
                  scrollX = TRUE,
                  FixedHeader = T
                )
              )
            })
            
            if (nrow(kegg_results_down)>0) {
              
              output$KEGGdotplot2 <- renderPlot({
                dotplot(kegg_results_down)
              })
              
              output$KEGGbarplot2 <- renderPlot({
                barplot(kegg_results_down)
              })
              
            }else{
              output$KEGGdotplot2 <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
              output$KEGGbarplot2 <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
            }
          }
          
        }
      }else if(input$analysis_type == "GSEA") {
        if(input$species == "Human" || input$species == "Mouse" || input$Selectdata == "Example Data"){
          
          print(paste("Analysis Type:", input$analysis_type))
          print(paste("Species:", input$species))
          print(paste("Selectdata:", input$Selectdata))
          print(paste("GSEA geneset:", input$GSEA_geneset))
          
          if(input$species == "Human" || input$Selectdata == "Example Data"){
            GSEA_geneset <- switch(input$GSEA_geneset,
                                   "H" = h_human, 
                                   "C1" = c1_human,
                                   "C2" = c2_human,
                                   "C3" = c3_human,
                                   "C4" = c4_human,
                                   "C5" = c5_human,
                                   "C6" = c6_human,
                                   "C7" = c7_human,
                                   "C8" = c8_human,
                                   "ALL" = msigdb_human
            )
          }else if(input$species == "Mouse"){
            GSEA_geneset <- switch(input$GSEA_geneset,
                                   "MH" = mh_mouse, 
                                   "M1" = m1_mouse,
                                   "M2" = m2_mouse,
                                   "M3" = m3_mouse,
                                   "M5" = m5_mouse,
                                   "M8" = m8_mouse,
                                   "ALL" = msigdb_mouse
            )
          }
          
          if (is.null(geneList_vector) || length(geneList_vector) == 0) {
            output$enrichment_result_table_gsea <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "Gene list is empty."), selection = 'none')
            })
            output$GSEAdotplot <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
            })
            output$GSEAridgeplot <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
            })
            output$gseaplot <- renderPlot({
              plot.new()
              text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
            })
          } else {
            gsea_result <- GSEA(geneList_vector, 
                                TERM2GENE = GSEA_geneset, 
                                verbose = T,
                                minGSSize = 5,
                                maxGSSize = 1000,
                                eps = 0,
                                pvalueCutoff = input$GSEA_pvalue_cutoff,
                                pAdjustMethod = "BH",
                                nPermSimple = 1000000)   
            values$gsea_result <- gsea_result
            output$enrichment_result_table_gsea <- DT::renderDataTable({
              gsea_result_df <- as.data.frame(gsea_result)
              
              if (nrow(gsea_result_df) == 0) {
                return(DT::datatable(data.frame(gsea_result), 
                                     selection = 'none',
                                     extensions = 'FixedHeader',
                                     options = list(
                                       pageLength = 5,
                                       autoWidth = FALSE,
                                       searchHighlight = TRUE,
                                       scrollX = TRUE,
                                       FixedHeader = TRUE
                                     )))
              } else {
                rownames(gsea_result_df) <- seq(1, nrow(gsea_result_df))
                return(DT::datatable(gsea_result_df,
                                     selection = 'single',
                                     extensions = 'FixedHeader',
                                     options = list(
                                       pageLength = 5,
                                       autoWidth = FALSE,
                                       searchHighlight = TRUE,
                                       scrollX = TRUE,
                                       FixedHeader = TRUE
                                     )))
              }
            })
            
            if (nrow(gsea_result) > 0) {
              
              output$GSEAdotplot <- renderPlot({
                dotplot(gsea_result)
              })
              
              output$GSEAridgeplot <- renderPlot({
                ridgeplot(gsea_result, label_format = 100)
              })
              
              output$gseaplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "Please select an entry from the Enrichment Result Table.", cex = 1.5)
              })
              
              observeEvent(input$enrichment_result_table_gsea_rows_selected, {
                selected_row <- input$enrichment_result_table_gsea_rows_selected
                
                if (is.null(selected_row) || length(selected_row) == 0) {
                  output$gseaplot <- renderPlot({
                    plot.new()
                    text(0.5, 0.5, "Please select an entry from the Enrichment Result Table.", cex = 1.5)
                  })
                } else {
                  selected_term <- gsea_result[selected_row, ]
                  output$gseaplot <- renderPlot({
                    gseaplot2(gsea_result, selected_row, pvalue_table = TRUE)
                  })
                }
              })
              
            } else {
              output$GSEAdotplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
              output$GSEAridgeplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
              output$gseaplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "No enriched term found.", cex = 1.5)
              })
            }}
        } else if(!is.null(input$species) && input$species == "Drosophila"){
          if(input$GSEA_geneset == "GO") {
            geneList_vector <- geneList_vector1
            ontology <- switch(input$GSEA_go_ontology,
                               "BP" = "BP",
                               "MF" = "MF",
                               "CC" = "CC",
                               "ALL" = "ALL")
            if (is.null(geneList_vector) || length(geneList_vector) == 0) {
              output$enrichment_result_table_gsea <- DT::renderDataTable({
                DT::datatable(data.frame(Message = "Gene list is empty."), selection = 'none')
              })
              output$GSEAdotplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
              })
              output$GSEAridgeplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
              })
              output$gseaplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
              })
            } else {
              gsea_result <- gseGO(geneList_vector, 
                                   OrgDb = values$annoDb, 
                                   ont = ontology, 
                                   pvalueCutoff = input$GSEA_pvalue_cutoff)
              gsea_result<- setReadable(gsea_result, OrgDb = values$annoDb ,keyType = 'ENTREZID')
              values$gsea_result <- gsea_result
              output$enrichment_result_table_gsea <- DT::renderDataTable({
                DT::datatable(as.data.frame(gsea_result),
                              selection = 'single',
                              extensions = 'FixedHeader',
                              options = list(
                                pageLength = 5,
                                autoWidth = F,
                                searchHighlight = TRUE,
                                scrollX = TRUE,
                                FixedHeader = TRUE
                              ))
              })
              
              if (nrow(gsea_result) > 0) {
                
                output$GSEAdotplot <- renderPlot({
                  dotplot(gsea_result)
                })
                
                output$GSEAridgeplot <- renderPlot({
                  ridgeplot(gsea_result, label_format = 100)
                })
                
                output$gseaplot <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "Please select an entry from the Enrichment Result Table.", cex = 1.5)
                })
                
                observeEvent(input$enrichment_result_table_gsea_rows_selected, {
                  selected_row <- input$enrichment_result_table_gsea_rows_selected
                  
                  if (is.null(selected_row) || length(selected_row) == 0) {
                    output$gseaplot <- renderPlot({
                      plot.new()
                      text(0.5, 0.5, "Please select an entry from the Enrichment Result Table.", cex = 1.5)
                    })
                  } else {
                    selected_term <- gsea_result[selected_row, ]
                    output$gseaplot <- renderPlot({
                      gseaplot2(gsea_result, selected_row, pvalue_table = TRUE)
                    })
                  }
                })
                
              } else {
                output$GSEAdotplot <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "No enriched term found.", cex = 1.5)
                })
                output$GSEAridgeplot <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "No enriched term found.", cex = 1.5)
                })
                output$gseaplot <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "No enriched term found.", cex = 1.5)
                })
              }
            }
            
          }else if(input$GSEA_geneset == "KEGG") {
            geneList_vector <- geneList_vector2
            if (is.null(geneList_vector) || length(geneList_vector) == 0) {
              output$enrichment_result_table_gsea <- DT::renderDataTable({
                DT::datatable(data.frame(Message = "Gene list is empty."), selection = 'none')
              })
              output$GSEAdotplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
              })
              output$GSEAridgeplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
              })
              output$gseaplot <- renderPlot({
                plot.new()
                text(0.5, 0.5, "Gene list is empty.", cex = 1.5)
              })
            } else {
              gsea_result <- gseKEGG(geneList_vector, 
                                     organism = values$Organism, 
                                     pvalueCutoff = input$GSEA_pvalue_cutoff)
              # gsea_result<- setReadable(gsea_result, OrgDb = values$annoDb ,keyType = 'FLYBASECG')
              values$gsea_result <- gsea_result
              output$enrichment_result_table_gsea <- DT::renderDataTable({
                DT::datatable(as.data.frame(gsea_result),
                              selection = 'single',
                              extensions = 'FixedHeader',
                              options = list(
                                pageLength = 5,
                                autoWidth = F,
                                searchHighlight = TRUE,
                                scrollX = TRUE,
                                FixedHeader = TRUE
                              ))
              })
              
              if (nrow(gsea_result) > 0) {
                
                output$GSEAdotplot <- renderPlot({
                  dotplot(gsea_result)
                })
                
                output$GSEAridgeplot <- renderPlot({
                  ridgeplot(gsea_result, label_format = 100)
                })
                
                output$gseaplot <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "Please select an entry from the Enrichment Result Table.", cex = 1.5)
                })
                
                observeEvent(input$enrichment_result_table_gsea_rows_selected, {
                  selected_row <- input$enrichment_result_table_gsea_rows_selected
                  
                  if (is.null(selected_row) || length(selected_row) == 0) {
                    output$gseaplot <- renderPlot({
                      plot.new()
                      text(0.5, 0.5, "Please select an entry from the Enrichment Result Table.", cex = 1.5)
                    })
                  } else {
                    selected_term <- gsea_result[selected_row, ]
                    output$gseaplot <- renderPlot({
                      gseaplot2(gsea_result, selected_row, pvalue_table = TRUE)
                    })
                  }
                })
                
              } else {
                output$GSEAdotplot <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "No enriched term found.", cex = 1.5)
                })
                output$GSEAridgeplot <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "No enriched term found.", cex = 1.5)
                })
                output$gseaplot <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "No enriched term found.", cex = 1.5)
                })
              }
            }
            
          }
        }
      }
      
      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste0(input$analysis_type, " Enrichment analysis completed successfully!"),
        type = "success"
      )
      
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during analysis:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal()
    })
  })
  
  #-------------------motif---------------------
  observeEvent(input$step7, {
    req(values$annoDb, values$peakAnno_df)
    
    clean_motif(T)
    
    showModal(modalDialog(
      title = "Run Motif Enrichment analysis",
      div(
        style = "text-align: center;",
        h4("Please wait..."),
        shiny::tags$a(img(src = "ZZ5H.gif", height = "100px"))
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      up_peaks_input <- strsplit(input$upgenes_list3, "\n")[[1]]
      down_peaks_input <- strsplit(input$downgenes_list3, "\n")[[1]]
      
      validate_peak_format <- function(peaks_input) {
        pattern <- "^chr[0-9XY]+-\\d+-\\d+$"
        invalid_peaks <- peaks_input[!grepl(pattern, peaks_input)]
        return(invalid_peaks)
      }
      
      if (length(up_peaks_input) == 0 || all(up_peaks_input == "No upregulated peaks")) {
        up_peaks_input <- NULL
      } else {
        invalid_up_peaks <- validate_peak_format(up_peaks_input)
        error_messages <- character(0)
        if (length(invalid_up_peaks) > 0) {
          error_messages <- c(error_messages, paste("Invalid upregulated peaks format:", 
                                                    paste(invalid_up_peaks, collapse = ", ")))
        }
        if (length(error_messages) > 0) {
          stop(paste(error_messages, collapse = "\n"))
        }
      }
      
      if (length(down_peaks_input) == 0 || all(down_peaks_input == "No downregulated peaks")) {
        down_peaks_input <- NULL
      } else {
        invalid_down_peaks <- validate_peak_format(down_peaks_input)
        error_messages <- character(0)
        if (length(invalid_down_peaks) > 0) {
          error_messages <- c(error_messages, paste("Invalid downregulated peaks format:", 
                                                    paste(invalid_down_peaks, collapse = ", ")))
        }
        if (length(error_messages) > 0) {
          stop(paste(error_messages, collapse = "\n"))
        }
      }
      
      if (setequal(up_peaks_input, specific_up_peaks) && setequal(down_peaks_input, specific_down_peaks)) {
        rds_file_up <- file.path(cache_dir, "example_upsinse_2685517c3b2da425b272e16da9cb9de1f34340c39447b822ce60934df3451b77.rds")
        rds_file_down <- file.path(cache_dir, "example_downsinse_2685517c3b2da425b272e16da9cb9de1f34340c39447b822ce60934df3451b77.rds")
        
        up_sinse <- readRDS(rds_file_up)
        down_sinse <- readRDS(rds_file_down)
        
        saveRDS(up_sinse, rds_file_up)
        values$up_sinse <- up_sinse
        up_pv <- data.frame(
          TF = rownames(assay(up_sinse, "negLog10Padj")),
          Log2_Enrichment_Ratio = assay(up_sinse, "log2enr")[, 1],
          Adjusted_P_Value = assay(up_sinse, "negLog10Padj")[, 1],
          Negative_Log10_P_Value = assay(up_sinse, "negLog10P")[, 1],
          Pearson_Residual = assay(up_sinse, "pearsonResid")[, 1],
          Expected_Foreground_Weight_With_Hits = assay(up_sinse, "expForegroundWgtWithHits")[, 1],
          Sum_Foreground_Weight_With_Hits = assay(up_sinse, "sumForegroundWgtWithHits")[, 1],
          Sum_Background_Weight_With_Hits = assay(up_sinse, "sumBackgroundWgtWithHits")[, 1]
        ) %>%
          na.omit() %>%
          dplyr::arrange(desc(Adjusted_P_Value))
        colnames(up_pv) <- gsub("_", " ", colnames(up_pv))
        rownames(up_pv) <- NULL
        values$up_pv <- up_pv
        
        output$motifEnrichmentTable1 <- DT::renderDataTable({
          DT::datatable(up_pv,
                        selection = 'none',
                        extensions = 'FixedHeader',
                        options = list(
                          pageLength = 5,
                          autoWidth = F,
                          searchHighlight = TRUE,
                          scrollX = TRUE,
                          FixedHeader = T
                        )
          )
        })
        
        output$motifplot1 <- renderPlot({
          plotMotifHeatmaps(
            x = up_sinse[up_pv$TF[1:input$num_top_tfs],],
            which.plots = c("log2enr", "negLog10Padj"),
            width = 1.8,
            width.seqlogo = 3,
            maxEnr = 2,
            maxSig = 10,
            cluster = as.logical(input$enable_clustering),
            show_dendrogram = TRUE,
            show_seqlogo = TRUE,
            show_motif_GC = as.logical(input$show_motif_GC)
          )
        })
        
        saveRDS(down_sinse, rds_file_down)
        values$down_sinse <- down_sinse
        down_pv <- data.frame(
          TF = rownames(assay(down_sinse, "negLog10Padj")),
          Log2_Enrichment_Ratio = assay(down_sinse, "log2enr")[, 1],
          Adjusted_P_Value = assay(down_sinse, "negLog10Padj")[, 1],
          Negative_Log10_P_Value = assay(down_sinse, "negLog10P")[, 1],
          Pearson_Residual = assay(down_sinse, "pearsonResid")[, 1],
          Expected_Foreground_Weight_With_Hits = assay(down_sinse, "expForegroundWgtWithHits")[, 1],
          Sum_Foreground_Weight_With_Hits = assay(down_sinse, "sumForegroundWgtWithHits")[, 1],
          Sum_Background_Weight_With_Hits = assay(down_sinse, "sumBackgroundWgtWithHits")[, 1]
        ) %>%
          na.omit() %>%
          dplyr::arrange(desc(Adjusted_P_Value))
        colnames(down_pv) <- gsub("_", " ", colnames(down_pv))
        rownames(down_pv) <- NULL
        values$down_pv <- down_pv
        
        output$motifEnrichmentTable2 <- DT::renderDataTable({
          DT::datatable(down_pv,
                        selection = 'none',
                        extensions = 'FixedHeader',
                        options = list(
                          pageLength = 5,
                          autoWidth = F,
                          searchHighlight = TRUE,
                          scrollX = TRUE,
                          FixedHeader = T
                        )
          )
        })
        
        output$motifplot2 <- renderPlot({
          plotMotifHeatmaps(
            x = down_sinse[down_pv$TF[1:input$num_top_tfs],],
            which.plots = c("log2enr", "negLog10Padj"),
            width = 1.8,
            width.seqlogo = 3,
            maxEnr = 2,
            maxSig = 10,
            cluster = as.logical(input$enable_clustering),
            show_dendrogram = TRUE,
            show_seqlogo = TRUE,
            show_motif_GC = as.logical(input$show_motif_GC)
          )
        })
        
      } else {
        input_hash <- digest(paste(c(up_peaks_input, down_peaks_input), collapse = "\n"), algo = "sha256")
        
        rds_file_up <- file.path(cache_dir, paste0("up_sinse_", input_hash, ".rds"))
        rds_file_down <- file.path(cache_dir, paste0("down_sinse_", input_hash, ".rds"))
        
        if (input$Selectdata == "Example Data") {
          pwms <- readRDS(vertebrates_pwms_dir)
        } else if (input$Selectdata == "Custom Data") {
          if (input$species == "Human" || input$species == "Mouse") {
            pwms <- readRDS(vertebrates_pwms_dir)
          } else if (input$species == "Drosophila") {
            pwms <- readRDS(insects_pwms_dir)
          }
        }
        
        if (file.exists(rds_file_up) && file.exists(rds_file_down)) {
          up_sinse <- readRDS(rds_file_up)
          down_sinse <- readRDS(rds_file_down)
        }  else {
          if (is.null(up_peaks_input) || length(up_peaks_input) == 0) {
            output$motifEnrichmentTable1 <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "peak list is empty."), selection = 'none')
            })
            output$motifplot1 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "peak list is empty.", cex = 1.5)
            })
          } else {
            up_gr <- GRanges(
              seqnames = gsub("-.*", "", up_peaks_input), 
              ranges = IRanges(
                start = as.integer(gsub(".*-(\\d+)-.*", "\\1", up_peaks_input)),
                end = as.integer(gsub(".*-(\\d+)$", "\\1", up_peaks_input))
              )
            )
            
            up_singleSet <- resize(up_gr, width = median(width(up_gr)), fix = "center")
            up_singleseqs <- getSeq(values$BSgenome, up_singleSet)
            up_sinse <- calcBinnedMotifEnrR(seqs = up_singleseqs,
                                            pwmL = pwms,
                                            background = "genome",
                                            genome = values$BSgenome,
                                            genome.regions = NULL,
                                            genome.oversample = 4,
                                            BPPARAM = BiocParallel::SerialParam(RNGseed = 42),
                                            verbose = TRUE)
            saveRDS(up_sinse, rds_file_up)
            values$up_sinse <- up_sinse
            up_pv <- data.frame(
              TF = rownames(assay(up_sinse, "negLog10Padj")),
              Log2_Enrichment_Ratio = assay(up_sinse, "log2enr")[, 1],
              Adjusted_P_Value = assay(up_sinse, "negLog10Padj")[, 1],
              Negative_Log10_P_Value = assay(up_sinse, "negLog10P")[, 1],
              Pearson_Residual = assay(up_sinse, "pearsonResid")[, 1],
              Expected_Foreground_Weight_With_Hits = assay(up_sinse, "expForegroundWgtWithHits")[, 1],
              Sum_Foreground_Weight_With_Hits = assay(up_sinse, "sumForegroundWgtWithHits")[, 1],
              Sum_Background_Weight_With_Hits = assay(up_sinse, "sumBackgroundWgtWithHits")[, 1]
            ) %>%
              na.omit() %>%
              dplyr::arrange(desc(Adjusted_P_Value))
            colnames(up_pv) <- gsub("_", " ", colnames(up_pv))
            rownames(up_pv) <- NULL
            values$up_pv <- up_pv
            
            output$motifEnrichmentTable1 <- DT::renderDataTable({
              DT::datatable(up_pv,
                            selection = 'none',
                            extensions = 'FixedHeader',
                            options = list(
                              pageLength = 5,
                              autoWidth = F,
                              searchHighlight = TRUE,
                              scrollX = TRUE,
                              FixedHeader = T
                            )
              )
            })
            
            output$motifplot1 <- renderPlot({
              plotMotifHeatmaps(
                x = up_sinse[up_pv$TF[1:input$num_top_tfs],],
                which.plots = c("log2enr", "negLog10Padj"),
                width = 1.8,
                width.seqlogo = 3,
                maxEnr = 2,
                maxSig = 10,
                cluster = as.logical(input$enable_clustering),
                show_dendrogram = TRUE,
                show_seqlogo = TRUE,
                show_motif_GC = as.logical(input$show_motif_GC)
              )
            })
          }
          
          if (is.null(down_peaks_input) || length(down_peaks_input) == 0) {
            output$motifEnrichmentTable2 <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "peak list is empty."), selection = 'none')
            })
            output$motifplot2 <- renderPlot({
              plot.new()
              text(0.5, 0.5, "peak list is empty.", cex = 1.5)
            })
          } else {
            down_gr <- GRanges(
              seqnames = gsub("-.*", "", down_peaks_input),
              ranges = IRanges(
                start = as.integer(gsub(".*-(\\d+)-.*", "\\1", down_peaks_input)),
                end = as.integer(gsub(".*-(\\d+)$", "\\1", down_peaks_input))
              )
            )
            
            down_singleSet <- resize(down_gr, width = median(width(down_gr)), fix = "center")
            down_singleseqs <- getSeq(values$BSgenome, down_singleSet)
            down_sinse <- calcBinnedMotifEnrR(seqs = down_singleseqs,
                                              pwmL = pwms,
                                              background = "genome",
                                              genome = values$BSgenome,
                                              genome.regions = NULL,
                                              genome.oversample = 4,
                                              BPPARAM = BiocParallel::SerialParam(RNGseed = 42),
                                              verbose = TRUE)
            saveRDS(down_sinse, rds_file_down)
            values$down_sinse <- down_sinse
            down_pv <- data.frame(
              TF = rownames(assay(down_sinse, "negLog10Padj")),
              Log2_Enrichment_Ratio = assay(down_sinse, "log2enr")[, 1],
              Adjusted_P_Value = assay(down_sinse, "negLog10Padj")[, 1],
              Negative_Log10_P_Value = assay(down_sinse, "negLog10P")[, 1],
              Pearson_Residual = assay(down_sinse, "pearsonResid")[, 1],
              Expected_Foreground_Weight_With_Hits = assay(down_sinse, "expForegroundWgtWithHits")[, 1],
              Sum_Foreground_Weight_With_Hits = assay(down_sinse, "sumForegroundWgtWithHits")[, 1],
              Sum_Background_Weight_With_Hits = assay(down_sinse, "sumBackgroundWgtWithHits")[, 1]
            ) %>%
              na.omit() %>%
              dplyr::arrange(desc(Adjusted_P_Value))
            colnames(down_pv) <- gsub("_", " ", colnames(down_pv))
            rownames(down_pv) <- NULL
            values$down_pv <- down_pv
            
            output$motifEnrichmentTable2 <- DT::renderDataTable({
              DT::datatable(down_pv,
                            selection = 'none',
                            extensions = 'FixedHeader',
                            options = list(
                              pageLength = 5,
                              autoWidth = F,
                              searchHighlight = TRUE,
                              scrollX = TRUE,
                              FixedHeader = T
                            )
              )
            })
            
            output$motifplot2 <- renderPlot({
              plotMotifHeatmaps(
                x = down_sinse[down_pv$TF[1:input$num_top_tfs],],
                which.plots = c("log2enr", "negLog10Padj"),
                width = 1.8,
                width.seqlogo = 3,
                maxEnr = 2,
                maxSig = 10,
                cluster = as.logical(input$enable_clustering),
                show_dendrogram = TRUE,
                show_seqlogo = TRUE,
                show_motif_GC = as.logical(input$show_motif_GC)
              )
            })
          }
        }
      }
      
      sendSweetAlert(
        session = session,
        title = "Success",
        text = "Motif Enrichment analysis completed successfully!",
        type = "success"
      )
      
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = paste("An error occurred during analysis:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal()
    })
  })

  
  #-------------验证输入值------------
  observeEvent(input$upstream, {
    if (is.null(input$upstream) || is.na(input$upstream) || input$upstream <= 0) {
      showFeedbackDanger(
        inputId = "upstream",
        text = "Value must be greater than 0"
      )
    } else {
      hideFeedback("upstream")
    }
  })
  
  observeEvent(input$downstream, {
    if (is.null(input$downstream) || is.na(input$downstream) || input$downstream <= 0) {
      showFeedbackDanger(
        inputId = "downstream",
        text = "Value must be greater than 0"
      )
    } else {
      hideFeedback("downstream")
    }
  })
  
  observeEvent(input$lfcThreshold, {
    if (is.null(input$lfcThreshold) || is.na(input$lfcThreshold) || input$lfcThreshold < 0 || input$lfcThreshold > 10) {
      showFeedbackDanger(
        inputId = "lfcThreshold",
        text = "Log2 Fold Change Threshold must be between 0 and 10"
      )
    } else {
      hideFeedback("lfcThreshold")
    }
  })
  
  observeEvent(input$alpha, {
    if (is.null(input$alpha) || is.na(input$alpha) || input$alpha <= 0 || input$alpha > 1) {
      showFeedbackDanger(
        inputId = "alpha",
        text = "Significance Level must be > 0 and ≤ 1"
      )
    } else {
      hideFeedback("alpha")
    }
  })
  
  observeEvent(input$pvalueThreshold, {
    if (is.null(input$pvalueThreshold) ||
        is.na(as.numeric(input$pvalueThreshold)) ||
        as.numeric(input$pvalueThreshold) < 0 ||
        as.numeric(input$pvalueThreshold) > 1) {
      showFeedbackDanger(
        inputId = "pvalueThreshold",
        text = "P-value threshold must be a number between 0 and 1."
      )
    } else {
      hideFeedback("pvalueThreshold")
      pvalueThreshold <- as.numeric(input$pvalueThreshold)
      values$pvalueThreshold <- pvalueThreshold
    }
  })
  
  observeEvent(input$log2fcThreshold, {
    if (is.null(input$log2fcThreshold) ||
        is.na(as.numeric(input$log2fcThreshold)) ||
        as.numeric(input$log2fcThreshold) < 0) {
      showFeedbackDanger(
        inputId = "log2fcThreshold",
        text = "Log2 fold change threshold must be a positive number."
      )
    } else {
      hideFeedback("log2fcThreshold")
      log2fcThreshold <- as.numeric(input$log2fcThreshold)
      values$log2fcThreshold <- log2fcThreshold
    }
  })
  
  observeEvent(input$upstream2, {
    if (is.null(input$upstream2) || is.na(input$upstream2) || input$upstream2 <= 0) {
      showFeedbackDanger(
        inputId = "upstream2",
        text = "Upstream2 must be greater than 0"
      )
    } else {
      hideFeedback("upstream2")
    }
  })
  
  observeEvent(input$downstream2, {
    if (is.null(input$downstream2) || is.na(input$downstream2) || input$downstream2 <= 0) {
      showFeedbackDanger(
        inputId = "downstream2",
        text = "Downstream must be greater than 0"
      )
    } else {
      hideFeedback("downstream2")
    }
  })
  
  observeEvent(input$pCutoff, {
    if (is.null(input$pCutoff) || is.na(input$pCutoff) || input$pCutoff < 0) {
      showFeedbackDanger(
        inputId = "pCutoff",
        text = "-log10(p) Cutoff must be a number ≥ 0"
      )
    } else {
      hideFeedback("pCutoff")
    }
  })
  
  observeEvent(input$FCcutoff2, {
    if (is.null(input$FCcutoff2) || is.na(input$FCcutoff2) || input$FCcutoff2 < 0) {
      showFeedbackDanger(
        inputId = "FCcutoff2",
        text = "Positive Log2 FC Threshold must be ≥ 0"
      )
    } else {
      hideFeedback("FCcutoff2")
    }
  })
  
  observeEvent(input$FCcutoff1, {
    if (is.null(input$FCcutoff1) || is.na(input$FCcutoff1) || input$FCcutoff1 < 0) {
      showFeedbackDanger(
        inputId = "FCcutoff1",
        text = "Negative Log2 FC Threshold must be ≥ 0"
      )
    } else {
      hideFeedback("FCcutoff1")
    }
  })
  
  observeEvent(input$pointSize, {
    if (is.null(input$pointSize) || is.na(input$pointSize) || input$pointSize <= 0) {
      showFeedbackDanger(
        inputId = "pointSize",
        text = "Point Size must be must be > 0"
      )
    } else {
      hideFeedback("pointSize")
    }
  })
  
  observeEvent(input$labSize, {
    if (is.null(input$labSize) || is.na(input$labSize) || input$labSize < 0) {
      showFeedbackDanger(
        inputId = "labSize",
        text = "Label Size must be must be ≥ 0"
      )
    } else {
      hideFeedback("labSize")
    }
  })
  
  observeEvent(input$size, {
    if (is.null(input$size) || is.na(input$size) || input$size < 0) {
      showFeedbackDanger(
        inputId = "size",
        text = "Label Size must be greater than or equal to 0."
      )
    } else {
      hideFeedback("size")
    }
  })
  
  observeEvent(input$boxPadding, {
    if (is.null(input$boxPadding) || is.na(input$boxPadding) || input$boxPadding < 0) {
      showFeedbackDanger(
        inputId = "boxPadding",
        text = "Box Padding must be greater than or equal to 0."
      )
    } else {
      hideFeedback("boxPadding")
    }
  })
  
  observeEvent(input$maxOverlaps, {
    if (is.null(input$maxOverlaps) || is.na(input$maxOverlaps) || input$maxOverlaps < 0) {
      showFeedbackDanger(
        inputId = "maxOverlaps",
        text = "Max Overlaps must be greater than or equal to 0."
      )
    } else {
      hideFeedback("maxOverlaps")
    }
  })
  
  observeEvent(input$topUpGenes, {
    if (is.null(input$topUpGenes) || is.na(input$topUpGenes) || input$topUpGenes < 0) {
      showFeedbackDanger(
        inputId = "topUpGenes",
        text = "Top Upregulated Genes must be greater than or equal to 0."
      )
    } else {
      hideFeedback("topUpGenes")
    }
  })
  
  observeEvent(input$topDownGenes, {
    if (is.null(input$topDownGenes) || is.na(input$topDownGenes) || input$topDownGenes < 0) {
      showFeedbackDanger(
        inputId = "topDownGenes",
        text = "Top Downregulated Genes must be greater than or equal to 0."
      )
    } else {
      hideFeedback("topDownGenes")
    }
  })
  
  observeEvent(input$pvalue_cutoff, {
    if (is.null(input$pvalue_cutoff) || is.na(input$pvalue_cutoff) ||
        input$pvalue_cutoff < 0 || input$pvalue_cutoff > 1) {
      showFeedbackDanger(
        inputId = "pvalue_cutoff",
        text = "p-value Cutoff must be between 0 and 1."
      )
    } else {
      hideFeedback("pvalue_cutoff")
    }
  })
  
  observeEvent(input$qvalue_cutoff, {
    if (is.null(input$qvalue_cutoff) || is.na(input$qvalue_cutoff) ||
        input$qvalue_cutoff < 0 || input$qvalue_cutoff > 1) {
      showFeedbackDanger(
        inputId = "qvalue_cutoff",
        text = "q-value Cutoff must be between 0 and 1."
      )
    } else {
      hideFeedback("qvalue_cutoff")
    }
  })
  
  observeEvent(input$GSEA_pvalue_cutoff, {
    if (is.null(input$GSEA_pvalue_cutoff) || is.na(input$GSEA_pvalue_cutoff) ||
        input$GSEA_pvalue_cutoff < 0 || input$GSEA_pvalue_cutoff > 1) {
      showFeedbackDanger(
        inputId = "GSEA_pvalue_cutoff",
        text = "p-value Cutoff must be between 0 and 1."
      )
    } else {
      hideFeedback("GSEA_pvalue_cutoff")
    }
  })
  
  observeEvent(input$num_top_tfs, {
    if (is.null(input$num_top_tfs) || is.na(input$num_top_tfs) || input$num_top_tfs <= 0) {
      showFeedbackDanger(
        inputId = "num_top_tfs",
        text = "Number of Top Transcription Factors must be greater than 0."
      )
    } else {
      hideFeedback("num_top_tfs")
    }
  })
  
  
  
  #------------图片下载-----------
  # coverage_plot
  output$download_coverageplot <- downloadHandler(
    filename = function() {
      paste("coverage_plot", Sys.Date(), input$extPlot11, sep = ".")
    },
    content = function(file) {
      req(length(values$coveragePlots) > 0)
      ext <- tolower(input$extPlot11)
      plots <- values$coveragePlots
      
      # 设置图形参数
      n <- length(plots)
      width <- 16
      height <- if(n > 1) 12 * n else 12  # 自动调整高度
      
      # 创建图形设备
      switch(ext,
             "pdf" = pdf(file, width = width, height = height),
             "png" = png(file, width = width * 100, height = height * 100, res = 300),
             "jpeg" = jpeg(file, width = width * 100, height = height * 100, 
                           res = 300, quality = 90))
      
      # 组合图形（如果多个）
      if(n > 1) {
        gridExtra::grid.arrange(
          grobs = plots,
          ncol = 1
        )
      } else {
        print(plots[[1]])
      }
      
      dev.off()
    }
  )
  
  # profile_plot
  output$download_profileplot <- downloadHandler(
    filename = function() {
      paste("profile_plot", Sys.Date(), input$extPlot12, sep = ".")
    },
    content = function(file) {
      req(length(values$profilePlots) > 0)
      ext <- tolower(input$extPlot12)
      plots <- values$profilePlots
      
      # 设置图形参数
      n <- length(plots)
      width <- 16
      height <- if(n > 1) 12 * n else 12  # 自动调整高度
      
      # 创建图形设备
      switch(ext,
             "pdf" = pdf(file, width = width, height = height),
             "png" = png(file, width = width * 100, height = height * 100, res = 300),
             "jpeg" = jpeg(file, width = width * 100, height = height * 100, 
                           res = 300, quality = 90))
      
      # 组合图形（如果多个）
      if(n > 1) {
        gridExtra::grid.arrange(
          grobs = plots,
          ncol = 1
        )
      } else {
        print(plots[[1]])
      }
      dev.off()
    }
  )
  
  output$Downloadafterfilter <- downloadHandler(
    filename = function() {
      paste("After_Filter_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$filtered_masterPeak)
      write.csv(as.data.frame(values$filtered_masterPeak), file, row.names = FALSE)
    }
  )
  
  # For two-group analysis: Download handlers for "Before Filter" and "After Filter"
  observe({
    req(values$groupCount)
    if (values$groupCount == 2) {
      output$downloadDeseqResult1 <- downloadHandler(
        filename = function() {
          paste0("deseqResult_before_filter_", Sys.Date(), ".csv")
        },
        content = function(file) {
          req(values$deseqResult)
          # 复制数据框，不改变原始数据
          dataToWrite <- values$deseqResult
          # 替换列名
          new_colnames <- colnames(dataToWrite)
          new_colnames[new_colnames == "baseMean"] <- "average expression"
          new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
          new_colnames[new_colnames == "stat"] <- "Wald Statistic"
          colnames(dataToWrite) <- new_colnames
          write.csv(dataToWrite, file, row.names = FALSE)
        }
      )
      
      output$downloadDeseqResult2 <- downloadHandler(
        filename = function() {
          paste0("deseqResult_after_filter_", Sys.Date(), ".csv")
        },
        content = function(file) {
          req(values$sig_peaks)
          dataToWrite <- values$sig_peaks
          new_colnames <- colnames(dataToWrite)
          new_colnames[new_colnames == "baseMean"] <- "average expression"
          new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
          new_colnames[new_colnames == "stat"] <- "Wald Statistic"
          colnames(dataToWrite) <- new_colnames
          write.csv(dataToWrite, file, row.names = FALSE)
        }
      )
    }
  })
  
  # For multi-group analysis: Download handlers for each comparison result and the filtered result
  observe({
    req(values$groupCount)
    if (values$groupCount > 2) {
      # 为每个比较结果生成下载按钮对应的 downloadHandler
      for(comp_name in names(values$deseqResultList)) {
        local({
          cn <- comp_name
          output[[paste0("downloadDeseqResult_", cn)]] <- downloadHandler(
            filename = function() {
              paste0("deseqResult_", cn, "_", Sys.Date(), ".csv")
            },
            content = function(file) {
              req(values$deseqResultList[[cn]])
              dataToWrite <- values$deseqResultList[[cn]]
              new_colnames <- colnames(dataToWrite)
              new_colnames[new_colnames == "baseMean"] <- "average expression"
              new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
              new_colnames[new_colnames == "stat"] <- "Wald Statistic"
              colnames(dataToWrite) <- new_colnames
              write.csv(dataToWrite, file, row.names = FALSE)
            }
          )
        })
      }
      
      # 针对过滤后的结果生成下载按钮对应的 downloadHandler
      output$downloadFilteredResult <- downloadHandler(
        filename = function() {
          paste0("deseqResult_after_filter_", input$selectedComparison, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
          req(values$sig_peaks)
          dataToWrite <- values$sig_peaks
          new_colnames <- colnames(dataToWrite)
          new_colnames[new_colnames == "baseMean"] <- "average expression"
          new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
          new_colnames[new_colnames == "stat"] <- "Wald Statistic"
          colnames(dataToWrite) <- new_colnames
          write.csv(dataToWrite, file, row.names = FALSE)
        }
      )
    }
  })
  
  output$downloadannotationTable <- downloadHandler(
    filename = function() {
      paste("Annotation_Results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$peakAnno_df)
      dataToWrite <- values$peakAnno_df
      new_colnames <- colnames(dataToWrite)
      new_colnames[new_colnames == "baseMean"] <- "average expression"
      new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
      new_colnames[new_colnames == "stat"] <- "Wald Statistic"
      colnames(dataToWrite) <- new_colnames
      write.csv(dataToWrite, file, row.names = FALSE)
    }
  )
  
  output$Download_Pie <- downloadHandler(
    filename = function() {
      paste("Annotation_Pie_", Sys.Date(), ".", input$extPlot1, sep = "")
    },
    content = function(file) {
      if (input$extPlot1 == "pdf") {
        pdf(file, width = 8, height = 6)
      } else if (input$extPlot1 == "png") {
        png(file, width = 8, height = 6, units = "in", res = 1000)
      } else {
        jpeg(file, width = 8, height = 6, units = "in", res = 1000)
      }
      plotAnnoPie(values$peakAnno)
      dev.off()
    }
  )
  
  output$Download_Bar <- downloadHandler(
    filename = function() {
      paste("Annotation_Bar_", Sys.Date(), ".", input$extPlot2, sep = "")
    },
    content = function(file) {
      if (input$extPlot2 == "pdf") {
        pdf(file, width = 8, height = 6)
      } else if (input$extPlot2 == "png") {
        png(file, width = 8, height = 6, units = "in", res = 1000)
      } else {
        jpeg(file, width = 8, height = 6, units = "in", res = 1000)
      }
      print(plotAnnoBar(values$peakAnno))
      dev.off()
    }
  )
  
  output$Download_TSS <- downloadHandler(
    filename = function() {
      paste("DistToTSS_", Sys.Date(), ".", input$extPlot3, sep = "")
    },
    content = function(file) {
      if (input$extPlot3 == "pdf") {
        pdf(file, width = 8, height = 6)
      } else if (input$extPlot3 == "png") {
        png(file, width = 8, height = 6, units = "in", res = 1000)
      } else {
        jpeg(file, width = 8, height = 6, units = "in", res = 1000)
      }
      print(plotDistToTSS(values$peakAnno))
      dev.off()
    }
  )
  
  output$Download_Upset <- downloadHandler(
    filename = function() {
      paste("Upset_Plot_", Sys.Date(), ".", input$extPlot4, sep = "")
    },
    content = function(file) {
      if (input$extPlot4 == "pdf") {
        pdf(file, width = 8, height = 6)
      } else if (input$extPlot4 == "png") {
        png(file, width = 8, height = 6, units = "in", res = 1000)
      } else {
        jpeg(file, width = 8, height = 6, units = "in", res = 1000)
      }
      print(upsetplot(values$peakAnno, vennpie = TRUE))
      dev.off()
    }
  )
  
  #火山图下载
  output$Download_Volcano <- downloadHandler(
    filename = function() {
      paste("Volcano_Plot_", Sys.Date(), ".", input$extPlot51, sep = "")
    },
    content = function(file) {
      if (values$groupCount == 2) {
        group_names <- names(values$groups)
        if (length(group_names) >= 2) {
          plotTitle <- paste(group_names[1], "vs", group_names[2])
        } else {
          plotTitle <- "Volcano Plot"
        }
      } else if (values$groupCount > 2) {
        if (!is.null(input$selectedComparison)) {
          plotTitle <- gsub("_vs_", " vs ", input$selectedComparison)
        } else {
          plotTitle <- "Volcano Plot"
        }
      }
      
      if (input$extPlot51 == "pdf") {
        pdf(file, width = 8, height = 6)
      } else if (input$extPlot51 == "png") {
        png(file, width = 8, height = 6, units = "in", res = 1000)
      } else {
        jpeg(file, width = 8, height = 6, units = "in", res = 1000)
      }
      print(
        enhancedVolcano(
          values$peakAnno_df,
          lab = values$peakAnno_df$SYMBOL,
          x = 'log2FoldChange',
          y = 'pvalue',
          title = plotTitle,
          pCutoff = volcano_params$pCutoff,
          FCcutoff1 = volcano_params$FCcutoff1,
          FCcutoff2 = volcano_params$FCcutoff2,
          pointSize = volcano_params$pointSize,
          labSize = volcano_params$labSize
        )
      )
      dev.off() 
    }
  )
  
  
  #MA图下载
  output$Download_MA <- downloadHandler(
    filename = function() {
      paste("MA_Plot_", Sys.Date(), ".", input$extPlot52, sep = "")
    },
    content = function(file) {
      if (input$extPlot52 == "pdf") {
        pdf(file, width = 8, height = 6)
      } else if (input$extPlot52 == "png") {
        png(file, width = 8, height = 6, units = "in", res = 1000)
      } else {
        jpeg(file, width = 8, height = 6, units = "in", res = 1000)
      }
      peakAnno_df2 <- values$peakAnno_df
      peakAnno_df2$change <- ifelse(peakAnno_df2$log2FoldChange > 0 & peakAnno_df2$padj < 0.05, "UP",
                                    ifelse(peakAnno_df2$log2FoldChange < 0 & peakAnno_df2$padj < 0.05, "DOWN", NA))
      top_upregulated_genes <- peakAnno_df2 %>%
        arrange(desc(log2FoldChange)) %>%
        head(input$topUpGenes)
      top_downregulated_genes <- peakAnno_df2 %>%
        arrange(log2FoldChange) %>%
        head(input$topDownGenes)
      top_genes <- rbind(top_upregulated_genes, top_downregulated_genes)
      print(
        ggplot(peakAnno_df2, aes(x = baseMean, y = log2FoldChange)) +
          geom_point(aes(color = change), alpha = 0.5) +
          scale_color_manual(values = c("DOWN" = "lightblue", "UP" = "lightcoral")) +
          scale_x_log10() +
          labs(title = "MA Plot", x = "Base Mean", y = "Log2 Fold Change") +
          theme_minimal() +
          geom_text_repel(data = top_genes, aes(label = SYMBOL), 
                          size = input$size,  
                          color = "black", 
                          box.padding = input$boxPadding,  
                          max.overlaps = input$maxOverlaps)
      )
      dev.off()
    }
  )
  
  
  #PCA
  output$Download_PCA <- downloadHandler(
    filename = function() {
      paste("PCA_Plot_", Sys.Date(), ".", input$extPlot53, sep = "")
    },
    content = function(file) {
      if (input$extPlot53 == "pdf") {
        pdf(file, width = 8, height = 6)
      } else if (input$extPlot53 == "png") {
        png(file, width = 8, height = 6, units = "in", res = 1000)
      } else {
        jpeg(file, width = 8, height = 6, units = "in", res = 1000)
      }
      rlogData <- rlog(values$DDS) 
      pcaData <- prcomp(t(assay(rlogData)))
      pcaDF <- as.data.frame(pcaData$x)
      
      print(
        ggplot(pcaDF, aes(x = PC1, y = PC2)) +
          geom_point(aes(color = values$condition), size = 4) +
          theme_minimal() +
          labs(title = "PCA Plot", x = "PC1", y = "PC2") +
          theme(legend.position = "top")
      )
      dev.off()
    }
  )
  
  #热图
  output$Download_Heatmap <- downloadHandler(
    filename = function() {
      paste("Heatmap_Plot_", Sys.Date(), ".", input$extPlot54, sep = "")
    },
    content = function(file) {
      if (input$extPlot54 == "pdf") {
        pdf(file, width = 8, height = 6)
      } else if (input$extPlot54 == "png") {
        png(file, width = 8, height = 6, units = "in", res = 1000)
      } else {
        jpeg(file, width = 8, height = 6, units = "in", res = 1000)
      }
      sample_cols <- colnames(values$data)[4:ncol(values$data)]
      summarized_data <- values$peakAnno_df %>%
        dplyr::select(annotation, all_of(sample_cols)) %>%  
        mutate(annotation = case_when(
          grepl("^Promoter", annotation) ~ "Promoter",
          grepl("^Intron", annotation) ~ "Intron",
          grepl("^Exon", annotation) ~ "Exon",
          TRUE ~ annotation
        )) 
      summarized_data <- summarized_data %>%
        group_by(annotation) %>%
        summarise(across(all_of(sample_cols), sum))
      
      summarized_data_df <- as.data.frame(summarized_data)
      rownames(summarized_data_df) <- summarized_data_df$annotation
      data_for_heatmap <- summarized_data_df %>% dplyr::select(-annotation)
      
      print(
        pheatmap(data_for_heatmap,
                 scale = "row", 
                 clustering_distance_rows = "euclidean", 
                 clustering_distance_cols = "euclidean",  
                 clustering_method = "complete",  
                 main = "Peak Count in Regions")
      )
      dev.off()
    }
  )
  
  #上调基因表格
  output$downloadupgenes_table <- downloadHandler(
    filename = function() {
      paste("Up_Genes_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$red_peaks_up)
      dataToWrite <- values$red_peaks_up
      new_colnames <- colnames(dataToWrite)
      new_colnames[new_colnames == "baseMean"] <- "average expression"
      new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
      new_colnames[new_colnames == "stat"] <- "Wald Statistic"
      colnames(dataToWrite) <- new_colnames
      write.csv(as.data.frame(dataToWrite), file, row.names = FALSE)
    }
  )
  
  #下调基因表格
  output$downloaddowngenes_table <- downloadHandler(
    filename = function() {
      paste("Down_Genes_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$red_peaks_down)
      dataToWrite <- values$red_peaks_down
      new_colnames <- colnames(dataToWrite)
      new_colnames[new_colnames == "baseMean"] <- "average expression"
      new_colnames[new_colnames == "lfcSE"] <- "log2 FC Std. Error"
      new_colnames[new_colnames == "stat"] <- "Wald Statistic"
      colnames(dataToWrite) <- new_colnames
      write.csv(as.data.frame(dataToWrite), file, row.names = FALSE)
    }
  )
  
  #-------------------富集分析下载----------------------
  
  # 下载富集分析结果表格up
  output$Downloadenrichmenttableup <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Enrichment_Analysis_Up_Genes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(input$analysis_type == "GO") {
        write.csv(as.data.frame(values$go_results_up), file, row.names = FALSE)
      } else if(input$analysis_type == "KEGG") {
        write.csv(as.data.frame(values$kegg_results_up), file, row.names = FALSE)
      }
    }
  )
  
  # 下载富集分析结果表格down
  output$Downloadenrichmenttabledown <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Enrichment_Analysis_Down_Genes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(input$analysis_type == "GO") {
        write.csv(as.data.frame(values$go_results_down), file, row.names = FALSE)
      } else if(input$analysis_type == "KEGG") {
        write.csv(as.data.frame(values$kegg_results_down), file, row.names = FALSE)
      }
    }
  )
  
  # 下载富集分析结果表格gsea
  output$Downloadenrichmenttablegsea <- downloadHandler(
    filename = function() {
      paste("GSEA_Enrichment_Analysis_Results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(as.data.frame(values$gsea_result), file, row.names = FALSE)
    }
  )
  
  # 下载GO:dotplot1
  output$DownloadGOdotplot1 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Dotplot_Up_Genes_", Sys.Date(), ".", input$extPlot6GOdotplot1, sep = "")
    },
    content = function(file) {
      if (input$extPlot6GOdotplot1 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6GOdotplot1 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(dotplot(values$go_results_up))
      dev.off()
    }
  )
  
  # 下载GO:dotplot2
  output$DownloadGOdotplot2 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Dotplot_Down_Genes_", Sys.Date(), ".", input$extPlot6GOdotplot2, sep = "")
    },
    content = function(file) {
      if (input$extPlot6GOdotplot2 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6GOdotplot2 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(dotplot(values$go_results_down))
      dev.off()
    }
  )
  
  # 下载GO:barplot1
  output$DownloadGObarplot1 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Barplot_Up_Genes_", Sys.Date(), ".", input$extPlot6GObarplot1, sep = "")
    },
    content = function(file) {
      if (input$extPlot6GObarplot1 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6GObarplot1 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(barplot(values$go_results_up))
      dev.off()
    }
  )
  
  # 下载GO:barplot2
  output$DownloadGObarplot2 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Barplot_Down_Genes_", Sys.Date(), ".", input$extPlot6GObarplot2, sep = "")
    },
    content = function(file) {
      if (input$extPlot6GObarplot2 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6GObarplot2 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(barplot(values$go_results_down))
      dev.off()
    }
  )
  
  # 下载GO:mapplot1
  output$DownloadGOmapplot1 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Mapplot_Up_Genes_", Sys.Date(), ".", input$extPlot6GOmapplot1, sep = "")
    },
    content = function(file) {
      if (input$extPlot6GOmapplot1 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6GOmapplot1 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      GO_result_plot_up <- pairwise_termsim(values$go_results_up)
      print(emapplot(GO_result_plot_up, showCategory = 20, label_format = 30))
      dev.off()
    }
  )
  
  # 下载GO:mapplot2
  output$DownloadGOmapplot2 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Mapplot_Down_Genes_", Sys.Date(), ".", input$extPlot6GOmapplot2, sep = "")
    },
    content = function(file) {
      if (input$extPlot6GOmapplot2 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6GOmapplot2 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      GO_result_plot_down <- pairwise_termsim(values$go_results_down)
      print(emapplot(GO_result_plot_down, showCategory = 20, label_format = 30))
      dev.off()
    }
  )
  
  # 下载KEGG:dotplot1
  output$DownloadKEGGdotplot1 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Dotplot_Up_Genes_", Sys.Date(), ".", input$extPlot6KEGGdotplot1, sep = "")
    },
    content = function(file) {
      if (input$extPlot6KEGGdotplot1 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6KEGGdotplot1 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(dotplot(values$kegg_results_up))
      dev.off()
    }
  )
  
  # 下载KEGG:dotplot2
  output$DownloadKEGGdotplot2 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Dotplot_Down_Genes_", Sys.Date(), ".", input$extPlot6KEGGdotplot2, sep = "")
    },
    content = function(file) {
      if (input$extPlot6KEGGdotplot2 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6KEGGdotplot2 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(dotplot(values$kegg_results_down))
      dev.off()
    }
  )
  
  # 下载KEGG:barplot1
  output$DownloadKEGGbarplot1 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Barplot_Up_Genes_", Sys.Date(), ".", input$extPlot6KEGGbarplot1, sep = "")
    },
    content = function(file) {
      if (input$extPlot6KEGGbarplot1 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6KEGGbarplot1 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(barplot(values$kegg_results_up))
      dev.off()
    }
  )
  
  # 下载KEGG:barplot2
  output$DownloadKEGGbarplot2 <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Barplot_Down_Genes_", Sys.Date(), ".", input$extPlot6KEGGbarplot2, sep = "")
    },
    content = function(file) {
      if (input$extPlot6KEGGbarplot2 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6KEGGbarplot2 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(barplot(values$kegg_results_down))
      dev.off()
    }
  )
  
  # 下载GSEA:gseaplot
  output$Downloadgseaplot <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_gseaplot_", Sys.Date(), ".", input$extPlot6gseaplot, sep = "")
    },
    content = function(file) {
      if (input$extPlot6gseaplot == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6gseaplot == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      selected_row <- input$enrichment_result_table_gsea_rows_selected
      selected_term <- values$gsea_result[selected_row, ]
      print(gseaplot2(values$gsea_result, selected_row, pvalue_table = TRUE))
      dev.off()
    }
  )
  
  # 下载GSEA:dotplot
  output$DownloadGSEAdotplot <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Dotplot_", Sys.Date(), ".", input$extPlot6GSEAdotplot, sep = "")
    },
    content = function(file) {
      if (input$extPlot6GSEAdotplot == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6GSEAdotplot == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(dotplot(values$gsea_result))
      dev.off()
    }
  )
  
  # 下载GSEA:ridgeplot
  output$DownloadGSEAridgeplot <- downloadHandler(
    filename = function() {
      paste0(input$analysis_type, "_Ridgeplot_", Sys.Date(), ".", input$extPlot6GSEAridgeplot, sep = "")
    },
    content = function(file) {
      if (input$extPlot6GSEAridgeplot == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot6GSEAridgeplot == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      print(ridgeplot(values$gsea_result, label_format = 100))
      dev.off()
    }
  )
  
  #下载motif表格
  output$downloadmotifEnrichmentTable1  <- downloadHandler(
    filename = function() {
      paste("upregulated_peaks_motif_enrichment_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$up_pv, file, row.names = FALSE)
    }
  )
  output$downloadmotifEnrichmentTable2 <- downloadHandler(
    filename = function() {
      paste("downregulated_peaks_motif_enrichment_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$down_pv, file, row.names = FALSE)
    }
  )
  
  #下载motif热图
  output$Downloadmotifplot1 <- downloadHandler(
    filename = function() {
      paste("upregulated_peaks_motif_heatmap_", Sys.Date(), ".", input$extPlot71, sep = "")
    },
    content = function(file) {
      if (input$extPlot71 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot71 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      plotMotifHeatmaps(
        x = values$up_sinse[values$up_pv$TF[1:input$num_top_tfs],],
        which.plots = c("log2enr", "negLog10Padj"),
        width = 1.8,
        width.seqlogo = 3,
        maxEnr = 2,
        maxSig = 10,
        cluster = as.logical(input$enable_clustering),
        show_dendrogram = TRUE,
        show_seqlogo = TRUE,
        show_motif_GC = as.logical(input$show_motif_GC)
      )
      dev.off()
    }
  )
  
  output$Downloadmotifplot2 <- downloadHandler(
    filename = function() {
      paste("downregulated_peaks_motif_heatmap_", Sys.Date(), ".", input$extPlot72, sep = "")
    },
    content = function(file) {
      if (input$extPlot72 == "pdf") {
        pdf(file, width = 14, height = 8)
      } else if (input$extPlot72 == "png") {
        png(file, width = 14, height = 8, units = "in", res = 1000)
      } else {
        jpeg(file, width = 14, height = 8, units = "in", res = 1000)
      }
      plotMotifHeatmaps(
        x = values$down_sinse[values$down_pv$TF[1:input$num_top_tfs],],
        which.plots = c("log2enr", "negLog10Padj"),
        width = 1.8,
        width.seqlogo = 3,
        maxEnr = 2,
        maxSig = 10,
        cluster = as.logical(input$enable_clustering),
        show_dendrogram = TRUE,
        show_seqlogo = TRUE,
        show_motif_GC = as.logical(input$show_motif_GC)
      )
      dev.off()
    }
  )
  
  #----------------清空变量--------------------
  cleanFunction <- function(fromDataInput) {
    if (fromDataInput){
      #step1图像
      values$coveragePlots <- list()
      output$covplot <- renderUI({ NULL })
      values$profilePlots <- list()
      output$profileplot <- renderUI({ NULL })
      #step2过滤
      updateTabsetPanel(session, "datapreview", selected = "Before Filter")
      #step2分组
      values$groups <- list()
      values$groupCount <- 0
      updateSelectizeInput(session, "samples", selected = NULL)
      updateTextInput(session, "groupName", value = "")
      #step3
      updateTabsetPanel(session, "analysis_results", selected = "Before Filter")
      output$dynamic_tabs_step3 <- renderUI({ NULL })
      output$summaryText <- renderText({ NULL })
      output$deseqResult <- DT::renderDataTable({ NULL })
      output$deseqResult2 <- DT::renderDataTable({ NULL })
      output$filteredResult <- DT::renderDataTable({ NULL })
      if (!is.null(values$deseqResultList)) {
        for (comp_name in names(values$deseqResultList)) {
          output[[paste0("deseqResult_", comp_name)]] <- DT::renderDataTable({ NULL })
          output[[paste0("summary_", comp_name)]] <- renderText({ NULL })
        }
      }
      #step4
      output$annotationTable <- DT::renderDataTable({ NULL })
      values$peakAnnoReady <- FALSE
      #step5
      values$DVReady <- FALSE
      output$upgenes_table <- DT::renderDataTable({ NULL })
      output$downgenes_table <- DT::renderDataTable({ NULL })
      updateTextAreaInput(session, "upgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "downgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "upgenes_list2", value = "")
      updateTextAreaInput(session, "downgenes_list2", value = "")
      updateTextAreaInput(session, "GSEA_upgenelist", value = "")
      updateTextAreaInput(session, "GSEA_downgenelist", value = "")
      updateTextAreaInput(session, "upgenes_list3", value = "")
      updateTextAreaInput(session, "downgenes_list3", value = "")
      #step6
      output$dynamic_tabs <- renderUI({ NULL })
      output$enrichment_result_table_up <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_down <- DT::renderDataTable({ NULL })
      #step7
      output$motifEnrichmentTable1 <- DT::renderDataTable({ NULL })
      output$motifEnrichmentTable2 <- DT::renderDataTable({ NULL })
      output$motifplot1 <- renderPlot({ NULL })
      output$motifplot2 <- renderPlot({ NULL })
      #禁用按钮
      shinyjs::disable("step11")
      shinyjs::disable("step12")
      shinyjs::disable("step21")
      shinyjs::disable("step312")
      shinyjs::disable("step2_to_step3")
      shinyjs::disable("step32")
      shinyjs::disable("step31")
      shinyjs::disable("step4")
      shinyjs::disable("step51")
      shinyjs::disable("step6")
      shinyjs::disable("step7")
      #侧边栏更新
      shinyjs::runjs('
        $(document).ready(function(){
            $("a[data-value=step3]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step4]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step5]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step6]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step7]").css("color", "#b1b1b1");  // 设置灰色
        });
      ')
    }
  }
  
  #step21过滤按钮
  clean_filter <- function(fromDataInput) {
    if (fromDataInput){
      #step3
      updateTabsetPanel(session, "analysis_results", selected = "Before Filter")
      output$dynamic_tabs_step3 <- renderUI({ NULL })
      output$summaryText <- renderText({ NULL })
      output$deseqResult <- DT::renderDataTable({ NULL })
      output$deseqResult2 <- DT::renderDataTable({ NULL })
      output$filteredResult <- DT::renderDataTable({ NULL })
      if (!is.null(values$deseqResultList)) {
        for (comp_name in names(values$deseqResultList)) {
          output[[paste0("deseqResult_", comp_name)]] <- DT::renderDataTable({ NULL })
          output[[paste0("summary_", comp_name)]] <- renderText({ NULL })
        }
      }
      #step4
      output$annotationTable <- DT::renderDataTable({ NULL })
      values$peakAnnoReady <- FALSE
      #step5
      values$DVReady <- FALSE
      output$upgenes_table <- DT::renderDataTable({ NULL })
      output$downgenes_table <- DT::renderDataTable({ NULL })
      updateTextAreaInput(session, "upgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "downgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "upgenes_list2", value = "")
      updateTextAreaInput(session, "downgenes_list2", value = "")
      updateTextAreaInput(session, "GSEA_upgenelist", value = "")
      updateTextAreaInput(session, "GSEA_downgenelist", value = "")
      updateTextAreaInput(session, "upgenes_list3", value = "")
      updateTextAreaInput(session, "downgenes_list3", value = "")
      #step6
      output$dynamic_tabs <- renderUI({ NULL })
      output$enrichment_result_table_up <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_down <- DT::renderDataTable({ NULL })
      #step7
      output$motifEnrichmentTable1 <- DT::renderDataTable({ NULL })
      output$motifEnrichmentTable2 <- DT::renderDataTable({ NULL })
      output$motifplot1 <- renderPlot({ NULL })
      output$motifplot2 <- renderPlot({ NULL })
      #禁用按钮
      shinyjs::disable("step312")
      shinyjs::disable("step32")
      shinyjs::disable("step4")
      shinyjs::disable("step51")
      shinyjs::disable("step6")
      shinyjs::disable("step7")
      #侧边栏更新
      shinyjs::runjs('
        $(document).ready(function(){
            $("a[data-value=step3]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step4]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step5]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step6]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step7]").css("color", "#b1b1b1");  // 设置灰色
        });
      ')
    }
  }
  
  #step2_to_step3分组
  clean_group <- function(fromDataInput) {
    if (fromDataInput){
      #step3
      updateTabsetPanel(session, "analysis_results", selected = "Before Filter")
      output$dynamic_tabs_step3 <- renderUI({ NULL })
      output$summaryText <- renderText({ NULL })
      output$deseqResult <- DT::renderDataTable({ NULL })
      output$deseqResult2 <- DT::renderDataTable({ NULL })
      output$filteredResult <- DT::renderDataTable({ NULL })
      if (!is.null(values$deseqResultList)) {
        for (comp_name in names(values$deseqResultList)) {
          output[[paste0("deseqResult_", comp_name)]] <- DT::renderDataTable({ NULL })
          output[[paste0("summary_", comp_name)]] <- renderText({ NULL })
        }
      }
      #step4
      output$annotationTable <- DT::renderDataTable({ NULL })
      values$peakAnnoReady <- FALSE
      #step5
      values$DVReady <- FALSE
      output$upgenes_table <- DT::renderDataTable({ NULL })
      output$downgenes_table <- DT::renderDataTable({ NULL })
      updateTextAreaInput(session, "upgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "downgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "upgenes_list2", value = "")
      updateTextAreaInput(session, "downgenes_list2", value = "")
      updateTextAreaInput(session, "GSEA_upgenelist", value = "")
      updateTextAreaInput(session, "GSEA_downgenelist", value = "")
      updateTextAreaInput(session, "upgenes_list3", value = "")
      updateTextAreaInput(session, "downgenes_list3", value = "")
      #step6
      output$dynamic_tabs <- renderUI({ NULL })
      output$enrichment_result_table_up <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_down <- DT::renderDataTable({ NULL })
      #step7
      output$motifEnrichmentTable1 <- DT::renderDataTable({ NULL })
      output$motifEnrichmentTable2 <- DT::renderDataTable({ NULL })
      output$motifplot1 <- renderPlot({ NULL })
      output$motifplot2 <- renderPlot({ NULL })
      #禁用按钮
      shinyjs::disable("step312")
      shinyjs::disable("step32")
      shinyjs::disable("step4")
      shinyjs::disable("step51")
      shinyjs::disable("step6")
      shinyjs::disable("step7")
      #侧边栏更新
      shinyjs::runjs('
        $(document).ready(function(){
            $("a[data-value=step4]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step5]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step6]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step7]").css("color", "#b1b1b1");  // 设置灰色
        });
      ')
    }
  }
  
  #step312差异结果
  clean_DAR <- function(fromDataInput) {
    if (fromDataInput){
      #step3
      updateTabsetPanel(session, "analysis_results", selected = "Before Filter")
      output$deseqResult2 <- DT::renderDataTable({ NULL })
      output$filteredResult <- DT::renderDataTable({ NULL })
      if (!is.null(values$deseqResultList)) {
        for (comp_name in names(values$deseqResultList)) {
          output[[paste0("deseqResult_", comp_name)]] <- DT::renderDataTable({ NULL })
          output[[paste0("summary_", comp_name)]] <- renderText({ NULL })
        }
      }
      #step4
      output$annotationTable <- DT::renderDataTable({ NULL })
      values$peakAnnoReady <- FALSE
      #step5
      values$DVReady <- FALSE
      output$upgenes_table <- DT::renderDataTable({ NULL })
      output$downgenes_table <- DT::renderDataTable({ NULL })
      updateTextAreaInput(session, "upgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "downgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "upgenes_list2", value = "")
      updateTextAreaInput(session, "downgenes_list2", value = "")
      updateTextAreaInput(session, "GSEA_upgenelist", value = "")
      updateTextAreaInput(session, "GSEA_downgenelist", value = "")
      updateTextAreaInput(session, "upgenes_list3", value = "")
      updateTextAreaInput(session, "downgenes_list3", value = "")
      #step6
      output$dynamic_tabs <- renderUI({ NULL })
      output$enrichment_result_table_up <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_down <- DT::renderDataTable({ NULL })
      #step7
      output$motifEnrichmentTable1 <- DT::renderDataTable({ NULL })
      output$motifEnrichmentTable2 <- DT::renderDataTable({ NULL })
      output$motifplot1 <- renderPlot({ NULL })
      output$motifplot2 <- renderPlot({ NULL })
      #禁用按钮
      shinyjs::disable("step4")
      shinyjs::disable("step51")
      shinyjs::disable("step6")
      shinyjs::disable("step7")
      #侧边栏更新
      shinyjs::runjs('
        $(document).ready(function(){
            $("a[data-value=step4]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step5]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step6]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step7]").css("color", "#b1b1b1");  // 设置灰色
        });
      ')
    }
  }
  
  #step32差异结果过滤
  clean_DRF <- function(fromDataInput) {
    if (fromDataInput){
      #step4
      output$annotationTable <- DT::renderDataTable({ NULL })
      values$peakAnnoReady <- FALSE
      #step5
      values$DVReady <- FALSE
      output$upgenes_table <- DT::renderDataTable({ NULL })
      output$downgenes_table <- DT::renderDataTable({ NULL })
      updateTextAreaInput(session, "upgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "downgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "upgenes_list2", value = "")
      updateTextAreaInput(session, "downgenes_list2", value = "")
      updateTextAreaInput(session, "GSEA_upgenelist", value = "")
      updateTextAreaInput(session, "GSEA_downgenelist", value = "")
      updateTextAreaInput(session, "upgenes_list3", value = "")
      updateTextAreaInput(session, "downgenes_list3", value = "")
      #step6
      output$dynamic_tabs <- renderUI({ NULL })
      output$enrichment_result_table_up <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_down <- DT::renderDataTable({ NULL })
      #step7
      output$motifEnrichmentTable1 <- DT::renderDataTable({ NULL })
      output$motifEnrichmentTable2 <- DT::renderDataTable({ NULL })
      output$motifplot1 <- renderPlot({ NULL })
      output$motifplot2 <- renderPlot({ NULL })
      #禁用按钮
      shinyjs::disable("step51")
      shinyjs::disable("step6")
      shinyjs::disable("step7")
      #侧边栏更新
      shinyjs::runjs('
        $(document).ready(function(){
            $("a[data-value=step5]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step6]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step7]").css("color", "#b1b1b1");  // 设置灰色
        });
      ')
    }
  }
  
  #step4注释
  clean_peakanno <- function(fromDataInput) {
    if (fromDataInput){
      #step5
      values$DVReady <- FALSE
      output$upgenes_table <- DT::renderDataTable({ NULL })
      output$downgenes_table <- DT::renderDataTable({ NULL })
      updateTextAreaInput(session, "upgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "downgenes_list", value = "Please first use the volcano plot to filter upregulated and downregulated genes.")
      updateTextAreaInput(session, "upgenes_list2", value = "")
      updateTextAreaInput(session, "downgenes_list2", value = "")
      updateTextAreaInput(session, "GSEA_upgenelist", value = "")
      updateTextAreaInput(session, "GSEA_downgenelist", value = "")
      updateTextAreaInput(session, "upgenes_list3", value = "")
      updateTextAreaInput(session, "downgenes_list3", value = "")
      #step6
      output$dynamic_tabs <- renderUI({ NULL })
      output$enrichment_result_table_up <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_down <- DT::renderDataTable({ NULL })
      #step7
      output$motifEnrichmentTable1 <- DT::renderDataTable({ NULL })
      output$motifEnrichmentTable2 <- DT::renderDataTable({ NULL })
      output$motifplot1 <- renderPlot({ NULL })
      output$motifplot2 <- renderPlot({ NULL })
      #禁用按钮
      shinyjs::disable("step6")
      shinyjs::disable("step7")
      #侧边栏更新
      shinyjs::runjs('
        $(document).ready(function(){
            $("a[data-value=step6]").css("color", "#b1b1b1");  // 设置灰色
            $("a[data-value=step7]").css("color", "#b1b1b1");  // 设置灰色
        });
      ')
    }
  }
  
  #step5差异可视化
  clean_DV <- function(fromDataInput) {
    if (fromDataInput){
      #step6
      output$dynamic_tabs <- renderUI({ NULL })
      output$enrichment_result_table_up <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_down <- DT::renderDataTable({ NULL })
      #step7
      output$motifEnrichmentTable1 <- DT::renderDataTable({ NULL })
      output$motifEnrichmentTable2 <- DT::renderDataTable({ NULL })
      output$motifplot1 <- renderPlot({ NULL })
      output$motifplot2 <- renderPlot({ NULL })
    }
  }
  
  #step6富集分析
  clean_enrichment <- function(fromDataInput) {
    if (fromDataInput){
      #step6
      output$dynamic_tabs <- renderUI({ NULL })
      output$enrichment_result_table_up <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_down <- DT::renderDataTable({ NULL })
      output$enrichment_result_table_gsea <- DT::renderDataTable({ NULL })
    }
  }
  
  #step7 motif富集分析
  clean_motif <- function(fromDataInput) {
    if (fromDataInput){
      output$motifplot1 <- renderPlot({NULL})
      output$motifplot2 <- renderPlot({NULL})
      output$motifEnrichmentTable1 <- DT::renderDataTable({NULL})
      output$motifEnrichmentTable2 <- DT::renderDataTable({NULL})
    }
  }
  
}

shinyServer(server)

