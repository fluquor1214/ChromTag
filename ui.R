ui <- dashboardPage(
  title="ChromTag",
  dashboardHeader(
    title = shiny::tags$a(img(
      src = "logo6.png", height = 50
    )),
    titleWidth = 250
  ),
  sidebar = shinydashboard::dashboardSidebar(
    width=250,
    sidebarMenu(
      id="menuitem",
      menuItem("Home Module", tabName = "home", icon = icon("home")),
      menuItem("Peaks Visualization", tabName = "step1", icon = icon("chart-area")), 
      menuItem("Filter And Group", tabName = "step2", icon = icon("filter")), 
      menuItem("Differential Peak Detection", tabName = "step3", icon = icon("search")), 
      menuItem("Gene Annotation", tabName = "step4", icon = icon("edit")),
      menuItem("Differential Results Visualization", tabName = "step5", icon = icon("chart-bar")), 
      menuItem("Enrichment Analysis", tabName = "step6", icon = icon("sitemap")), 
      menuItem("Motif Enrichment Analysis", tabName = "step7", icon = icon("dna")), 
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
      
    ),
    shiny::tags$head(
      shiny::tags$script(HTML("
      $(document).ready(function() {
        $('body').css('zoom', '90%');
      });
    "))
    ),
    shiny::tags$head(
      shiny::tags$style(HTML("
    .tiny-button {
      padding: 2px 4px !important;
      font-size: 7px !important;
    }
   "))
    ),
    shiny::tags$head(
      shiny::tags$style(HTML("
      /* 修改未选中标签的背景颜色 */
      .nav-tabs > li > a {
        background-color: #F1F1F1;  /* 设置未选择标签的背景颜色为灰色 */
        color: #888;  /* 设置未选择标签的文字颜色为灰色 */
      }

      /* 修改选中标签的颜色 */
      .nav-tabs > li.active > a {
        background-color: #4CAF50;  /* 选中的标签背景颜色 */
        color: white;  /* 选中的标签文字颜色 */
      }

      /* 修改标签栏的边框颜色 */
      .nav-tabs {
        border-bottom: 2px solid #4CAF50;  /* 改变边框颜色 */
      }
    "))
    ),
    shiny::tags$head(
      shiny::tags$style(
        HTML(
          "
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='Home'] {
            font-size: 13px
    }
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step1'] {
            color: #b1b1b1; /* 灰色 */
            font-size: 13px
    }
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step2'] {
            color: #b1b1b1; /* 灰色 */
            font-size: 13px
    }


    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step3'] {
            color: #b1b1b1; /* 灰色 */
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step4'] {
            color: #b1b1b1; /* 灰色 */
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step5'] {
            color: #b1b1b1; /* 灰色 */
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step6'] {
            color: #b1b1b1; /* 灰色 */
            font-size: 13px
    }
    
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step7'] {
            color: #b1b1b1; /* 灰色 */
            font-size: 13px
    }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='Help'] {
            font-size: 13px
    }
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='About'] {
            font-size: 13px
    }
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
}

    .skin-blue .main-sidebar {
        position: fixed;
        width: 250px;
      }

    .skin-blue .sidebar {        #侧边栏与标题栏的距离
        position: fixed;
        top: 50px;
        bottom: 0;
        width:250px;
    }


      /*固定标题*/
    .skin-blue .main-header .logo{
        position: fixed;
        height: 50px;
        width: 250px;
        
      }
    .skin-blue .navbar-static-top {
        position: fixed;
        width: 100%;
      }

"
        )
      )
    )
  ),
  body=dashboardBody(
    shiny::tags$div(style = "height: 50px;"),
    shinyjs::useShinyjs(),
    fresh::use_theme(mytheme),
    useShinyFeedback(),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          
          shinydashboard::box(
            title = tagList(icon("window-restore"), "Overview"),
            width = 8,
            height = 530,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            
            fluidRow(column(
              12,
              align = "center",
              shiny::tags$img(src = "workflow1.png", style = "max-height:460px; width:auto;max-width:100%;")
            ))
            
          ),
          shinydashboard::box(
            title = tagList(icon("file-upload"), "Import Data"),
            width = 4,
            height = 530,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            selectInput("Selectdata", label = ("Data source："), choices = list("Example Data","Custom Data"), selected = "Example Data"),
            uiOutput("Selectspecies"),
            uiOutput("FileInputs"),
            div(
              style = "position: absolute; bottom: 20px; left: 1%; width: 100%; text-align: center;",
              actionButton(
                inputId = "import",
                label = "START",
                icon = icon("play"),
                style = "width: 80%;background-color: white; color: black;"
              )
            ),
            shiny::tags$style("#import {box-shadow: 0px 2px 5px #888888;}")
          ),
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Instruction"),
            width = 12,
            #height = 500,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            div(
              code("ChromTag"),
              "is a user-friendly, interactive, and open-source R-Shiny application for the analysis and visualization of",  
              code("CUT&Tag and ChIP-seq data"),  
              ". It streamlines data processing, from peak visualization and filtering to",  
              code("differential peak detection"),  
              ", gene annotation, and enrichment analysis. Users can upload a pre-merged peak count matrix and explore various analysis modules with just a few clicks. The application offers multiple visualization tools, including chromosome coverage plots, heatmaps, and volcano plots, making data interpretation more intuitive. Additionally, motif enrichment analysis helps",  
              code("identify potential regulatory elements linked to differentially enriched peaks"),  
              ". ",  
              code("Chromtag"),  
              " provides a comprehensive and accessible solution for researchers studying chromatin modifications and transcriptional regulation.",  
              style = "font-size:16px;font-style:calibri;color:black;",
              align = "justify"
            )
            
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Data Preview"),
            width = 12,
            #height = 500,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("dataPreview")
          )
        )
        
      ),
      tabItem(
        tabName = "step1",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Instruction"),
            width = 12,
            #height = 500,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                strong("The Peaks Visualization"), 
                " module displays the distribution of ",  
                strong("peaks"),  
                " across the genome. It includes the ",  
                strong("Chromosome Coverage Plot"),  
                " for visualizing peak coverage along chromosomes and the ",  
                strong("Profile Plot"),  
                " for showing read distribution around promoter regions. Users can select samples, define regions, and generate plots to explore peak data efficiently. ",  
                strong("This step can be skipped if peak distribution analysis is not required."),  
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Chromosome Coverage Plot"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(3,
                     fluidRow(
                       column(
                         12,
                         selectizeInput("weightCol1", "Select Sample:", choices = NULL, 
                                        multiple = TRUE,
                                        options = list(
                                          plugins = list("remove_button")  # 启用 "remove_button" 插件
                                        )),
                         selectInput("chrs", "Select Chromosome:", choices = NULL),
                         actionButton("step11", "Submit", icon = icon("check"))
                       )),
                     fluidRow(
                       column(
                         12,
                         radioButtons(
                           inputId = "extPlot11",
                           label = helpText("Output Format"),
                           choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                           inline = TRUE
                         )
                       )),
                     fluidRow(
                       column(
                         12,
                         downloadButton("download_coverageplot", "Download")
                       )
                     )
              ),
              column(9,
                     uiOutput("covplot"),# %>% withSpinner(),
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Profile Plot"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(3,
                     fluidRow(
                       column(
                         12,
                         selectizeInput("weightCol2", "Select Sample:", choices = NULL, 
                                        multiple = TRUE,
                                        options = list(
                                          plugins = list("remove_button")  
                                        )),
                         numericInput("upstream", "Upstream (bp):", value = 3000),
                         numericInput("downstream", "Downstream (bp):", value = 3000),
                         actionButton("step12", "Submit", icon = icon("check"))
                       )),
                     fluidRow(
                       column(
                         12,
                         radioButtons(
                           inputId = "extPlot12",
                           label = helpText("Output Format"),
                           choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                           inline = TRUE
                         )
                       )),
                     fluidRow(
                       column(
                         12,
                         downloadButton("download_profileplot", "Download")
                       )
                     )
              ),
              column(9,
                     uiOutput("profileplot"),# %>% withSpinner(),
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "step2",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Instruction"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                strong("The Filter and Group module"), 
                " allows for data refinement and sample organization before downstream analysis. The ",  
                strong("filtering step"),  
                " removes low-count peaks based on a user-defined threshold, ensuring that only peaks with sufficient read support are retained. The ",  
                strong("grouping step"),  
                " enables the assignment of selected samples into user-defined groups for comparative analysis. Users can preview data before and after filtering, adjust the count threshold, create and modify sample groups, and proceed with structured comparisons, ensuring a well-organized dataset for ",  
                strong("differential analysis"),  
                ".",  
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::tabBox(
            title = tagList(icon("table"), "Data Preview"),
            width = 12,
            selected = "Before Filter",
            side = "right",
            tabsetPanel(
              id = "datapreview",
              tabPanel(
                tagList("Before Filter"),
                fluidRow(
                  column(3,
                         div(
                           style = "display:inline-block; vertical-align:middle;",
                           shiny::tags$strong("Count Threshold:")
                         ),
                         div(
                           style = "display:inline-block; vertical-align:middle; margin-left:5px;",
                           bsButton("thresholdHelp", label = "", icon = icon("question"), size = "extra-small", class = "tiny-button")
                         ),
                         numericInput("threshold", label = NULL, value = 5),
                         bsPopover(
                           id = "thresholdHelp", 
                           title = NULL, 
                           content = "This value represents the minimum total count across all samples for each row. Only rows with a sum of counts greater than this threshold will be retained for further analysis.",
                           placement = "right", 
                           trigger = "hover", 
                           options = list(container = "body")
                         ),
                         actionButton("step21", "Submit", icon = icon("check"))
                  ),
                  column(9,
                         DT::dataTableOutput("dataPreviewstep21")
                  )
                )
              ),
              tabPanel(
                tagList("After Filter"),
                fluidRow(column(12,
                                DT::dataTableOutput("dataPreviewstep22"),
                                downloadButton("Downloadafterfilter", "Download")
                )
                )
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("exchange-alt"), "Sample Grouping"),
            width = 12,
            #height = 500,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            selectizeInput(
              inputId = "samples", 
              label = "Select Samples:", 
              choices = NULL, 
              multiple = TRUE,
              options = list(
                plugins = list("remove_button")  # 启用移除按钮插件
              )
            ),
            textInput("groupName", "Group Name:", value = ""),
            actionButton("addGroup", "Add Group"),
            actionButton("clearGroups", "Clear Groups"),
            hr(),
            h4("Current Groups:"),
            verbatimTextOutput("currentGroups"),
            actionButton(
              inputId = "step2_to_step3",
              label = "Submit",
              icon = icon("check")
            )
          )
        )
      ),
      tabItem(
        tabName = "step3",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Instruction"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                strong("The Differential Peak Detection module"), 
                " identifies significant changes in peak intensity between different sample groups. Users can specify statistical parameters such as the ",  
                strong("alternative hypothesis, significance level, log2 fold change threshold, and p-value adjustment method"),  
                " to refine the analysis. The module supports comparisons between two or more groups, automatically performing differential analysis and generating results in an interactive table. Significant peaks can be filtered based on user-defined thresholds, ensuring that only biologically relevant differences are retained for downstream analysis.",  
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            ),
            div(
              style = "text-align: center;",
              actionButton(
                inputId = "step31",
                label = "Run Differential Analysis",
                icon = icon("play"),
                style = "width: 50%;background-color: #6aa7a6; color: white;"
              ),
              div(
                id = "loading",
                style = "display: none;",
                h3("Running differential analysis... Please wait."),
                withSpinner(uiOutput("progressUI"))
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("mouse-pointer"), "Select Analysis Results Parameters"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(column(4,
                            selectInput(
                              inputId = "altHypothesis",
                              label = "Choose Alternative Hypothesis for Differential Expression:",
                              choices = list(
                                "Detect Upregulated Genes" = "greater",
                                "Detect Downregulated Genes" = "less",
                                "Detect Significant Changes (up or down)" = "greaterAbs"
                              ),
                              selected = "greaterAbs"
                              # width = "100%"
                            )),
                     column(2,
                            radioButtons(
                              inputId = "independentFiltering",
                              label = "Apply Independent Filtering for Low Expression Genes",
                              choices = list("Yes" = TRUE, "No" = FALSE),
                              selected = FALSE,  # Default option
                              inline = TRUE  # Make it appear in a row
                            )
                     ),
                     column(2,
                            numericInput(
                              inputId = "alpha",
                              label = "Set Significance Level:",
                              value = 0.01,
                              min = 0,
                              max = 1,
                              step = 0.01
                            )),
                     column(2,
                            numericInput(
                              inputId = "lfcThreshold",
                              label = "Set Log2 Fold Change Threshold:",
                              value = 0,
                              min = 0,
                              max = 10
                            )),
                     column(2,
                            selectInput(
                              inputId = "pAdjustMethod",
                              label = "Select p-value Adjustment Method:",
                              choices = c("BH" = "BH", "Holm" = "holm"),
                              selected = "BH",
                              width = "100%"
                            )),
                     column(12,
                            div(
                              style = "text-align: center;",
                              actionButton(
                                inputId = "step312",
                                label = "Submit",
                                icon = icon("check")
                                # class = "btn-primary",
                                # style = "color: white;"
                              )
                            ))
            )
          ),
          uiOutput("dynamic_tabs_step3")
          
        )
      ),
      tabItem(
        tabName = "step4",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Instruction"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                strong("The Gene Annotation module"),  
                " assigns peaks to nearby genes and provides functional insights into their potential regulatory roles. Users can define ",  
                strong("upstream and downstream distances"),  
                " to determine the genomic regions considered for annotation. The module generates an ",  
                strong("annotation table"),  
                " linking peaks to genes and offers multiple visualization options, including ",  
                strong("pie charts, bar charts, distance-to-TSS plots, and upset plots"),  
                ", to summarize peak distributions. This analysis helps identify ",  
                strong("gene-associated regulatory elements"),  
                " and aids in understanding their potential impact on gene expression.",  
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("mouse-pointer"), "Input Annotation Parameters"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            column(6,
                   numericInput("upstream2", "Upstream (bp):", value = 3000)),
            column(6,
                   numericInput("downstream2", "Downstream (bp):", value = 3000)),
            div(
              style = "text-align: center;",
              actionButton(
                inputId = "step4",
                label = "Run Gene Annotation",
                icon = icon("play"),
                style = "width: 50%;background-color: #6aa7a6; color: white;"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Annotation Table"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("annotationTable"),
            downloadButton(
              outputId = "downloadannotationTable",
              label = "Download"
            )  
          ),
          shinydashboard::tabBox(
            title = tagList(icon("chart-column"), "Gene Annotation Visualization"),
            width = 12,
            selected = "Annotation Pie Chart",
            side = "right",
            tabsetPanel(id = "annotationplot",
                        # Annotation Pie Chart Tab
                        tabPanel(
                          tagList(icon("chart-pie"), "Annotation Pie Chart"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(
                                     12,
                                     radioButtons(
                                       inputId = "extPlot1",
                                       label = helpText("Output Format"),
                                       choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                       inline = TRUE
                                     )
                                   )),
                                   fluidRow(
                                     column(
                                       12,
                                       downloadButton("Download_Pie", "Download"),
                                     )
                                   )
                            ),
                            column(9,
                                   plotOutput("plotAnnoPie", width = "100%") %>% withSpinner(),
                            )
                          )
                        ),
                        # Annotation Bar Chart Tab
                        tabPanel(
                          tagList(icon("chart-bar"), "Annotation Bar Chart"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(
                                     12,
                                     radioButtons(
                                       inputId = "extPlot2",
                                       label = helpText("Output Format"),
                                       choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                       inline = TRUE
                                     )
                                   )),
                                   fluidRow(
                                     column(
                                       12,
                                       downloadButton("Download_Bar", "Download"),
                                     )
                                   )
                            ),
                            column(9,
                                   plotOutput("plotAnnoBar", width = "100%") %>% withSpinner(),
                            )
                          )
                        ),
                        # Distance to TSS Tab
                        tabPanel(
                          tagList(icon("chart-line"), "Distance to TSS"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(
                                     12,
                                     radioButtons(
                                       inputId = "extPlot3",
                                       label = helpText("Output Format"),
                                       choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                       inline = TRUE
                                     )
                                   )),
                                   fluidRow(
                                     column(
                                       12,
                                       downloadButton("Download_TSS", "Download"),
                                     )
                                   )
                            ),
                            column(9,
                                   plotOutput("plotDistToTSS", width = "100%") %>% withSpinner(),
                            )
                          )
                        ),
                        # Upset Plot Tab
                        tabPanel(
                          tagList(icon("project-diagram"), "Upset Plot"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(
                                     12,
                                     radioButtons(
                                       inputId = "extPlot4",
                                       label = helpText("Output Format"),
                                       choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                       inline = TRUE
                                     )
                                   )),
                                   fluidRow(
                                     column(
                                       12,
                                       downloadButton("Download_Upset", "Download"),
                                     )
                                   )
                            ),
                            column(9,
                                   plotOutput("upsetPlot", width = "100%") %>% withSpinner(),
                            )
                          )
                        )
            )
          )
        )
      ),
      tabItem(
        tabName = "step5",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Instruction"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                strong("The Differential Results Visualization module"),  
                " provides multiple plotting tools to explore and interpret differential peak analysis results. Users can generate a ",  
                strong("Volcano Plot"),  
                " to identify and ",  
                strong("filter"),  
                " significantly enriched peaks for further analysis, with the filtered peaks available in the gene tables below. Additional visualizations include an ",  
                strong("MA Plot"),  
                " to assess fold changes relative to expression levels, a ",  
                strong("PCA Plot"),  
                " to evaluate sample clustering, and a ",  
                strong("Heatmap"),  
                " to compare peak distributions across samples. Adjustable parameters allow fine-tuning of significance thresholds, fold change cutoffs, and visualization settings, ensuring flexible and comprehensive data exploration.",  
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::tabBox(
            title = tagList(icon("chart-column"), "Differential Analysis Visualization",
                            bsButton("genecutoffhelp", label = "", icon = icon("question"), size = "extra-small"),
                            bsPopover(
                              id = "genecutoffhelp", 
                              title = "Select Significant Genes",
                              content = paste(
                                "• By adjusting the volcano plot parameters, you can filter the significant genes (displayed as red points) you wish to analyze, then submit your selection. <br><br>",
                                "• The volcano plot shows genes that are significantly different between the two groups or selected comparisons. The title of the volcano plot indicates which groups are being compared (e.g., Group1 vs Group2). <br><br>",
                                "• Genes located further from the origin (with larger absolute log2FC and lower p-values) are considered more significant. Genes to the right of the origin have higher expression in the first group, and genes to the left have higher expression in the second group. <br><br>"
                              ),
                              placement = "right", 
                              trigger = "hover", 
                              options = list(container = "body")
                            )
            ),
            width = 12,
            selected = "Volcano Plot",
            side = "right",
            tabsetPanel(id = "differential_visualization",
                        #Volcano Plot
                        tabPanel(
                          tagList(icon("chart-area"), "Volcano Plot"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(12,
                                                   numericInput("pCutoff", "-log10(p) Cutoff", value = 2),
                                                   numericInput("FCcutoff2", "Positive Log2 FC Threshold", value = 14),
                                                   numericInput("FCcutoff1", "Negative Log2 FC Threshold", value = 5),
                                                   numericInput("pointSize", "Point Size", value = 3.0),
                                                   numericInput("labSize", "Label Size", value = 4.0),
                                                   actionButton(
                                                     inputId = "step51",
                                                     label = "Submit",
                                                     icon = icon("check")
                                                   ),
                                                   br(),
                                                   radioButtons(
                                                     inputId = "extPlot51",
                                                     label = helpText("Output Format"),
                                                     choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                                     inline = TRUE
                                                   ),
                                                   downloadButton("Download_Volcano", "Download")
                                   )
                                   )
                            ),
                            column(9,
                                   plotOutput("volcanoplot", width = "100%") %>% withSpinner(),
                            ))
                        ),
                        # MA Tab
                        tabPanel(
                          tagList(icon("exchange-alt"), "MA Plot"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(12,
                                                   numericInput("size", "Label Size", value = 3),
                                                   numericInput("boxPadding", "Box Padding", value = 0.5),
                                                   numericInput("maxOverlaps", "Max Overlaps", value = 10),
                                                   numericInput("topUpGenes", "Top Upregulated Genes", value = 5),
                                                   numericInput("topDownGenes", "Top Downregulated Genes", value = 5),
                                                   br(),
                                                   radioButtons(
                                                     inputId = "extPlot52",
                                                     label = helpText("Output Format"),
                                                     choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                                     inline = TRUE
                                                   ),
                                                   downloadButton("Download_MA", "Download")
                                   ))
                            ),
                            column(9,
                                   plotOutput("maplot", width = "100%") %>% withSpinner(),
                            ))
                        ),
                        tabPanel(
                          tagList(icon("cogs"), "PCA Plot"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(
                                     12,
                                     radioButtons(
                                       inputId = "extPlot53",
                                       label = helpText("Output Format"),
                                       choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                       inline = TRUE
                                     )
                                   )),
                                   fluidRow(
                                     column(
                                       12,
                                       downloadButton("Download_PCA", "Download"),
                                     )
                                   )
                            ),
                            column(9,
                                   plotOutput("pcaplot", width = "100%") %>% withSpinner(),
                            )
                          )
                        ),
                        tabPanel(
                          tagList(icon("th"), "Heatmap"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(
                                     12,
                                     radioButtons(
                                       inputId = "extPlot54",
                                       label = helpText("Output Format"),
                                       choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                       inline = TRUE
                                     )
                                   )),
                                   fluidRow(
                                     column(
                                       12,
                                       downloadButton("Download_Heatmap", "Download"),
                                     )
                                   )
                            ),
                            column(9,
                                   plotOutput("heatmapPlot", width = "100%") %>% withSpinner(),
                            )
                          )
                        )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Up Genes Preview"),
            width = 12,
            #height = 500,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(column(3,
                            textAreaInput(
                              "upgenes_list",
                              "Upregulated Genes List",
                              value="Please first use the volcano plot to filter upregulated and downregulated genes.",
                              rows = 10
                            )
                            
            ),column(9,
                     DT::dataTableOutput("upgenes_table"),
                     downloadButton(
                       outputId = "downloadupgenes_table",
                       label = "Download"
                     )
            )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Down Genes Preview"),
            width = 12,
            #height = 500,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(column(3,
                            textAreaInput(
                              "downgenes_list",
                              "Downregulated Genes List",
                              value="Please first use the volcano plot to filter upregulated and downregulated genes.",
                              rows = 10
                            )
            ),column(9,
                     DT::dataTableOutput("downgenes_table"),
                     downloadButton(
                       outputId = "downloaddowngenes_table",
                       label = "Download"
                     )
            )
            )
          )
        )
      ),
      tabItem(
        tabName = "step6",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Instruction"),
            width = 12,
            #height = 500,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                strong("The Enrichment Analysis"), 
                " module identifies significantly enriched ",  
                strong("biological pathways and functions"),  
                " based on user-defined gene lists. It supports ",  
                strong("GO, KEGG, and GSEA analyses"),  
                ", allowing users to specify analysis parameters, adjust p-value and q-value thresholds, and input upregulated and downregulated gene lists. The module automatically performs enrichment analysis and presents results not only in ",  
                strong("interactive tables"),  
                " but also through ",  
                strong("visualization plots"),  
                " for better interpretation. Users can refine gene selections, customize analysis settings, and utilize dynamic visualizations to gain functional insights into their data.",  
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("mouse-pointer"), "Select Enrichment Analysis Parameters",
                            bsButton("genelisthelp", label = "", icon = icon("question"), size = "extra-small"),
                            bsPopover(
                              id = "genelisthelp", 
                              title = "Genes List Modification", 
                              content = "For GO or KEGG analysis, you can enter or modify the gene list using SYMBOLs to specify the genes for enrichment analysis. If GSEA is selected, the input is restricted to all genes annotated in the Gene Annotation module.",
                              placement = "right", 
                              trigger = "hover", 
                              options = list(container = "body")
                            )
            ),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(column(4,
                            selectInput("analysis_type", "Analysis Type:",
                                        choices = c("GO", "KEGG", "GSEA"),
                                        selected = "GO"),
                            conditionalPanel(
                              condition = "input.analysis_type == 'GO'",
                              selectInput("go_ontology", "GO Ontology:",
                                          choices = c("Biological Process" = "BP",
                                                      "Molecular Function" = "MF",
                                                      "Cellular Component" = "CC",
                                                      "All" = "ALL"),
                                          selected = "ALL")
                            ),
                            uiOutput("GSEA_geneset_ui"),
                            conditionalPanel(
                              condition = "input.analysis_type != 'GSEA'",
                              numericInput("pvalue_cutoff", "p-value Cutoff:",
                                           value = 0.05, min = 0, max = 1, step = 0.01),
                              numericInput("qvalue_cutoff", "q-value Cutoff:",
                                           value = 0.05, min = 0, max = 1, step = 0.01)
                            )
            ),
            column(4,
                   conditionalPanel(
                     condition = "input.analysis_type != 'GSEA'",
                     textAreaInput(
                       "upgenes_list2",
                       "Upregulated Genes List",
                       rows = 10
                     )
                   )
            ),
            column(4,
                   conditionalPanel(
                     condition = "input.analysis_type != 'GSEA'",
                     textAreaInput(
                       "downgenes_list2",
                       "Downregulated Genes List",
                       rows = 10
                     )
                   )
            ),
            column(1,
            ),
            column(6,
                   conditionalPanel(
                     condition = "input.analysis_type == 'GSEA'",
                     textAreaInput(
                       "GSEA_genelist",
                       "Up and Down Genes List",
                       rows = 10
                     )
                   )
            ),
            div(
              style = "text-align: center;",
              actionButton(
                inputId = "step6",
                label = "Run Enrichment Analysis",
                icon = icon("play"),
                style = "width: 50%;background-color: #6aa7a6; color: white;"
              )
            ))
          )
        ),
        uiOutput("dynamic_tabs")
      ),
      tabItem(
        tabName = "step7",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Instruction"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                strong("The Motif Enrichment Analysis"), 
                " module performs motif analysis on the top 200 differentially enriched peaks, selected from the volcano plot filtering step. Users can input and modify ",  
                strong("upregulated and downregulated peak lists"),  
                ", and the analysis identifies enriched transcription factor binding motifs. Results are presented in ",  
                strong("interactive tables"),  
                " and ",  
                strong("visualization plots"),  
                " such as heatmaps, with options to adjust the number of top transcription factors, enable clustering, and display motif GC content. The results can be downloaded for further analysis, offering insights into potential regulatory elements affecting gene expression.",  
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("mouse-pointer"), "Select Motif Analysis Parameters"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(4,
                     textAreaInput(
                       "upgenes_list3",
                       "Upregulated Peaks List",
                       rows = 10
                     )
              ),
              column(4,
                     textAreaInput(
                       "downgenes_list3",
                       "Downregulated Peaks List",
                       rows = 10
                     )
              ),
              column(4,
                     numericInput(
                       "num_top_tfs",
                       "Number of Top Transcription Factors to Plot",
                       value = 10,
                       min = 1
                     ),
                     radioButtons(
                       inputId = "show_motif_GC",
                       label = "Display Motif GC Content",
                       choices = c("Yes" = TRUE, "No" = FALSE),
                       selected = TRUE
                     ),
                     radioButtons(
                       inputId = "enable_clustering",
                       label = "Enable Clustering",
                       choices = c("Yes" = TRUE, "No" = FALSE),
                       selected = TRUE
                     )
              )
            ),
            div(
              style = "text-align: center;",
              actionButton(
                inputId = "step7",
                label = "Run Motif Enrichment",
                icon = icon("play"),
                style = "width: 50%;background-color: #6aa7a6; color: white;"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(
              icon("table"), "Upregulated Peaks Motif Enrichment Table",
              bsButton("motifhelp1", label = "", icon = icon("question"), size = "extra-small"),
              bsPopover(
                id = "motifhelp1", 
                title = NULL,
                content = paste(
                  "• <b>Log2 Enrichment Ratio</b>: The log2 ratio of observed to expected motif occurrences, indicating motif enrichment in the region.<br>",
                  "• <b>Adjusted P Value</b>: The P-value adjusted for multiple testing (FDR), correcting for false positives.<br>",
                  "• <b>Negative Log10 P Value</b>: The raw P-value transformed to a negative log10 scale, with smaller values indicating stronger significance.<br>",
                  "• <b>Pearson Residual</b>: A standardized measure of enrichment, similar to a z-score, showing deviation from expected motif counts.<br>",
                  "• <b>Expected Foreground Weight With Hits</b>: The expected number of foreground regions containing the motif.<br>",
                  "• <b>Sum Foreground Weight With Hits</b>: The total weighted count of foreground regions with at least one occurrence of the motif.<br>",
                  "• <b>Sum Background Weight With Hits</b>: The total weighted count of background regions with at least one occurrence of the motif.<br>",
                  "• <b>Background</b>: In this analysis, the background refers to the genome sequences used in the analysis. These sequences represent the overall genetic background of the organism and are used to calculate expected motif occurrences to compare with the observed data.<br>"
                ),
                placement = "right", 
                trigger = "hover", 
                options = list(container = "body")
              )
            ),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("motifEnrichmentTable1"), #%>% withSpinner(),
            downloadButton(
              outputId = "downloadmotifEnrichmentTable1",
              label = "Download"
            )  
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Downregulated Peaks Motif Enrichment Table",
                            bsButton("motifhelp2", label = "", icon = icon("question"), size = "extra-small"),
                            bsPopover(
                              id = "motifhelp2", 
                              title = NULL,
                              content = paste(
                                "• <b>Log2 Enrichment Ratio</b>: The log2 ratio of observed to expected motif occurrences, indicating motif enrichment in the region.<br>",
                                "• <b>Adjusted P Value</b>: The P-value adjusted for multiple testing (FDR), correcting for false positives.<br>",
                                "• <b>Negative Log10 P Value</b>: The raw P-value transformed to a negative log10 scale, with smaller values indicating stronger significance.<br>",
                                "• <b>Pearson Residual</b>: A standardized measure of enrichment, similar to a z-score, showing deviation from expected motif counts.<br>",
                                "• <b>Expected Foreground Weight With Hits</b>: The expected number of foreground regions containing the motif.<br>",
                                "• <b>Sum Foreground Weight With Hits</b>: The total weighted count of foreground regions with at least one occurrence of the motif.<br>",
                                "• <b>Sum Background Weight With Hits</b>: The total weighted count of background regions with at least one occurrence of the motif.<br>",
                                "• <b>Background</b>: In this analysis, the background refers to the genome sequences used in the analysis. These sequences represent the overall genetic background of the organism and are used to calculate expected motif occurrences to compare with the observed data.<br>"
                              ),
                              placement = "right", 
                              trigger = "hover", 
                              options = list(container = "body")
                            )),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("motifEnrichmentTable2"), #%>% withSpinner(),
            downloadButton(
              outputId = "downloadmotifEnrichmentTable2",
              label = "Download"
            )  
          ),
          shinydashboard::box(
            title = tagList(icon("sitemap"), "Motif Heatmaps"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(3,
                     div(
                       h4("Upregulated Peaks"),
                       style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                     ),
                     br(),
                     br(),
                     fluidRow(column(12, radioButtons("extPlot71", "Output Format",
                                                      c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                     fluidRow(column(12, downloadButton("Downloadmotifplot1", "Download")))
              ),
              column(9,
                     plotOutput("motifplot1", width = "100%")
              ),
              column(3,
                     div(
                       h4("Downregulated Peaks"),
                       style = "background-color: #6aa7a6; color: white; padding: 0px 3px; border-radius: 10px; display: inline-block;"
                     ),
                     br(),
                     br(),
                     fluidRow(column(12, radioButtons("extPlot72", "Output Format",
                                                      c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"), inline = TRUE))),
                     fluidRow(column(12, downloadButton("Downloadmotifplot2", "Download")))
              ),
              column(9,
                     plotOutput("motifplot2", width = "100%")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "help",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "Help"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            shiny::tags$iframe(src = "ChromTag Basic Tutorial.pdf", width = "100%", height = "600px")
          )
        )
      ),
      tabItem(
        tabName = "about",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("file-alt"), "About"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE
          )
        )
      )
    )
  ),
  footer = dashboardFooter(
    div(
      class = "footer",
      HTML(paste0(
        "</br><p style = 'text-align: center; font-size: 1.0em; color: black; line-height: 10%;'>",
        "<b>Created by</b>: XuLabGDPU | ",
        "<b>Last update</b>: 14/04/2025",
        "</p>",
        "</br><p style = 'text-align: center; font-size: 1.0em; color: black; line-height: 10%;'>",
        "<b>Address</b>: No. 280, Outer Ring East Road, Panyu District, Guangzhou City, Guangdong Province, China | ",
        "<b>Postcode</b>: 511400",
        "</p>",
        "</br><p style = 'text-align: center; font-size:1.0em; line-height: 10%;'> ",
        "<a  href = 'https://www.xulabgdpu.org.cn' target='_blank'>XuLabGDPU</a> | ",
        "<a  href = 'https://www.xulabgdpu.org.cn/signacShiny' target='_blank'>ShinySignac.UiO</a> | ",
        "<a  href = 'https://www.gdpu.edu.cn/' target='_blank'>Guangdong Pharmaceutical University</a> | ",
        "<a href='https://beian.miit.gov.cn/' target='_blank'>黑ICP备2024016624</a>",
        "</p>"
      )),
      div(
        style = "display: flex; justify-content: space-between; margin-top: 10px;",
        img(src = "logo6.png", style = "height: 50px;"),
        img(src = "yh.png", style = "height: 50px;")
      )
    )
  )
)

shinyUI(ui)