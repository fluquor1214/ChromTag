ui <- dashboardPage(
  title="ChromTag",
  dashboardHeader(
    title = shiny::tags$a(img(
      src = "logo6.png", height = 50
    )),
    titleWidth = 265
  ),
  sidebar = shinydashboard::dashboardSidebar(
    width=265,
    sidebarMenu(
      id="menuitem",
      menuItem("Home Module", tabName = "home", icon = icon("home")),
      menuItem("Peaks Visualization Module", tabName = "step1", icon = icon("chart-area")), 
      menuItem("Data Filtering and Grouping Module", tabName = "step2", icon = icon("filter")), 
      menuItem("Differential Peak Analysis Module", tabName = "step3", icon = icon("search")), 
      menuItem("Differential Peak Annotation Module", tabName = "step4", icon = icon("edit")),
      menuItem("Differential Peak Visualization Module", tabName = "step5", icon = icon("chart-bar")), 
      menuItem("Functional Enrichment Module", tabName = "step6", icon = icon("sitemap")), 
      menuItem("Motif Enrichment Module", tabName = "step7", icon = icon("dna")), 
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
      .nav-tabs > li > a {
        background-color: #F1F1F1;
        color: #888;
      }

      .nav-tabs > li.active > a {
        background-color: #4CAF50;
        color: white;
      }

      .nav-tabs {
        border-bottom: 2px solid #4CAF50;
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
            color: #b1b1b1;
            font-size: 13px
    }
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step2'] {
            color: #b1b1b1;
            font-size: 13px
    }


    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step3'] {
            color: #b1b1b1;
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step4'] {
            color: #b1b1b1;
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step5'] {
            color: #b1b1b1;
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step6'] {
            color: #b1b1b1;
            font-size: 13px
    }
    
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='step7'] {
            color: #b1b1b1;
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
        width: 265px;
      }

    .skin-blue .sidebar {
        position: fixed;
        top: 50px;
        bottom: 0;
        width:265px;
    }

    .skin-blue .main-header .logo{
        position: fixed;
        height: 50px;
        width: 265px;
        
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
              shiny::tags$img(src = "workflow1.png", style = "max-height:470px; width:auto;max-width:120%;")
            ))
            
          ),
          shinydashboard::box(
            title = tagList(icon("file-upload"), "Import Data"),
            width = 4,
            height = 530,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            selectInput("Selectdata", label = ("Data Source："), choices = list("Example dataset","Upload data"), selected = "Example dataset"),
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
              code("The Home Module"),
              " serves as the entry point of ",
              code("ChromTag"),
              ", providing an integrated interface for initializing data input and establishing the foundation for subsequent analyses. Users may choose to begin with the ",
              code("system-provided example dataset"),
              " or ",
              code("upload custom peak count matrices"),
              " that have been pre-merged across samples. This module also manages species selection and displays an immediate preview of the imported dataset, enabling users to verify data structure and completeness before progressing to downstream analytical modules. Together, these components ensure that the analysis pipeline begins with properly formatted and biologically consistent input data, thereby supporting accurate and reliable interpretation in later stages.",
              style = "font-size:15px;font-style:calibri;color:black;",
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
                strong("The Peaks Visualization Module"),
                " provides an overview of genome-wide peak distribution patterns, offering initial insights into global chromatin accessibility or histone modification profiles. By enabling users to assign distinct colors to individual samples, the module facilitates clear visual differentiation across experimental conditions. Two complementary visualization modes are supported: the ",
                strong("Chromosome Coverage Plot"),
                ", which illustrates peak density along chromosomal coordinates, and the ",
                strong("TSS Profile Plot"),
                ", which highlights signal enrichment patterns around transcription start sites. These visual summaries allow users to assess sample consistency, identify broad regulatory trends, and detect potential differences in chromatin states prior to formal statistical analysis. As an exploratory component of the workflow, this module helps contextualize downstream findings and enhances the interpretability of differential and functional analyses.",
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("palette"), "Assign Sample Colors"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(
                width = 4,
                selectInput("color_sample", "Sample:", choices = NULL)
              ),
              column(
                width = 4,
                uiOutput("color_picker_ui")
              ),
              column(
                width = 4,
                br(),
                actionButton(
                  inputId = "step10",
                  label = "Submit",
                  icon = icon("check")
                )
              )
            ),
            verbatimTextOutput("color_submit_print")
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Chromosome Coverage Plot"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(3,
                     fluidRow(
                       column(
                         12,
                         selectizeInput("weightCol1", "Select Sample(s):", choices = NULL, 
                                        multiple = TRUE,
                                        options = list(plugins = list("remove_button"))),
                         selectInput("chrs", "Select Chromosome:", choices = NULL),
                         actionButton("step11", "Plot", icon = icon("check"))
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
                     uiOutput("covplot")
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "TSS Profile Plot"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(3,
                     fluidRow(
                       column(
                         12,
                         selectizeInput("weightCol2", "Select Sample(s):", choices = NULL, 
                                        multiple = TRUE,
                                        options = list(plugins = list("remove_button"))),
                         numericInput("upstream", "Upstream (bp):", value = 3000),
                         numericInput("downstream", "Downstream (bp):", value = 3000),
                         actionButton("step12", "Plot", icon = icon("check"))
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
                     uiOutput("profileplot")
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
                strong("The Data Filtering and Grouping Module"),
                " provides essential preprocessing steps to ensure that downstream analyses are performed on high-quality and well-structured data. The ",
                strong("filtering component"),
                " removes peaks with insufficient read support based on a user-defined count cutoff, reducing noise and retaining only those genomic regions with reliable signal intensity. The grouping component allows users to assign selected samples into ",
                strong("biologically meaningful groups"),
                ", forming the basis for subsequent differential analysis. Together, these steps help establish a clean and well-organized dataset, facilitating accurate comparison across experimental conditions and improving the overall robustness of downstream computational analyses.",
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::tabBox(
            title = tagList(icon("table"), "Data Preview"),
            width = 12,
            selected = "Before Filtering",
            side = "right",
            tabsetPanel(
              id = "datapreview",
              tabPanel(
                tagList("Before Filtering"),
                fluidRow(
                  column(3,
                         div(
                           style = "display:inline-block; vertical-align:middle;",
                           shiny::tags$strong("Count Cutoff:")
                         ),
                         div(
                           style = "display:inline-block; vertical-align:middle; margin-left:5px;",
                           bsButton("thresholdHelp", label = "", icon = icon("question"), size = "extra-small", class = "tiny-button"),
                           bsButton("thresholdTip", label = "", icon = icon("exclamation"), size = "extra-small", class = "tiny-button")
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
                         bsPopover(
                           id = "thresholdTip", 
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
                tagList("After Filtering"),
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
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            
            selectizeInput(
              inputId = "samples", 
              label = "Select Sample(s):", 
              choices = NULL, 
              multiple = TRUE,
              options = list(
                plugins = list("remove_button")
              )
            ),
            
            selectInput(
              inputId = "groupName",
              label = "Select Group Label:",
              choices = c(
                "Control group" = "Control",
                "Experimental group" = "Experimental"
              ),
              selected = "Control"
            ),
            
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
                strong("The Differential Peak Analysis Module"),
                " performs statistical testing to detect significant differences in peak signal intensity across defined sample groups. The analysis is conducted using a model-based framework, and results are evaluated primarily based on statistical significance with appropriate multiple testing correction. Users may tailor the analysis by specifying parameters such as the direction of change, significance threshold, and p-value adjustment method. The module supports both pairwise and multi-group comparisons, and results are presented in an interactive tabular format for further exploration and interpretation.",
                style = "font-size:16px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("mouse-pointer"), "Differential Analysis Parameters",
                            bsButton("normalizationtip", label = "", icon = icon("exclamation"), size = "extra-small"),
                            bsPopover(
                              id = "normalizationtip", 
                              title = NULL,
                              content = paste(
                                "Differential analysis assumes comparable signal distributions across samples. Ensure appropriate experimental design and data quality before interpreting results."
                              ),
                              placement = "right", 
                              trigger = "hover", 
                              options = list(container = "body")
                            )
            ),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              uiOutput("differential_Parameters")
            ),
            fluidRow(
              column(12,
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
                strong("The Differential Peak Annotation Module"),
                " annotates differential peaks by mapping them to nearby genes based on user-defined upstream and downstream distances. After running the annotation, results are displayed in an interactive table and summarized through multiple visualization panels, including ",
                strong("pie plots"),
                ", ",
                strong("bar plots"),
                ", ",
                strong("Distance-to-TSS plots"),
                ", and ",
                strong("UpSet plots"),
                ". These outputs provide an overview of how annotated peaks are distributed across genomic features and help users explore ",
                strong("potential regulatory associations"),
                " between peaks and genes.",
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
                label = "Run Peak Annotation",
                icon = icon("play"),
                style = "width: 50%;background-color: #6aa7a6; color: white;"
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Peak Annotation Table"),
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
            title = tagList(icon("chart-column"), "Annotation Visualization"),
            width = 12,
            selected = "Annotation Pie Plot",
            side = "right",
            tabsetPanel(id = "annotationplot",
                        # Annotation Pie Plot Tab
                        tabPanel(
                          tagList(icon("chart-pie"), "Annotation Pie Plot"),
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
                        # Annotation Bar Plot Tab
                        tabPanel(
                          tagList(icon("chart-bar"), "Annotation Bar Plot"),
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
                        # Distance to TSS Plot Tab
                        tabPanel(
                          tagList(icon("chart-line"), "Distance to TSS Plot"),
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
                strong("The Differential Peak Visualization Module"),
                " offers a set of interactive plots that help users examine and interpret the outcomes of differential peak analysis. The ",
                strong("Volcano Plot"),
                " allows users to highlight and filter significantly enriched peaks, with the corresponding upregulated and downregulated gene lists displayed below for downstream analysis. Additional visualizations include the ",
                strong("MA Plot"),
                ", which summarizes fold-change patterns relative to average signal intensity, the ",
                strong("PCA Plot"),
                " for assessing sample clustering, and a ",
                strong("Heatmap"),
                " that compares peak signal profiles across samples. Adjustable parameters enable users to fine-tune significance thresholds, fold-change cutoffs, and display settings, providing a flexible and comprehensive framework for exploring differential chromatin signals.",
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
                                "• By adjusting the volcano plot parameters, you can filter the significant peaks (displayed as red points) you wish to analyze, then submit your selection. <br><br>",
                                "• Peaks located further from the origin (with larger absolute log2FC and lower p-values) are considered more significant. Peaks with a log2FC greater than 0 have higher expression in the second group, while peaks with a log2FC less than 0 have higher expression in the first group. <br><br>"
                              ),
                              placement = "right", 
                              trigger = "hover", 
                              options = list(container = "body")
                            ),
                            bsButton("thresholdstip2", label = "", icon = icon("exclamation"), size = "extra-small"),
                            bsPopover(
                              id = "thresholdstip2", 
                              title = "Tip",
                              content = paste(
                                "• Thresholds (adjusted p-value and log₂ fold change) should be defined based on data-driven considerations and should not be adjusted post hoc to obtain significant results. <br><br>",
                                "• The MA plot can be used to assess normalization quality. Strong asymmetry may indicate potential issues with data comparability."
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
                            column(
                              3,
                              fluidRow(
                                column(
                                  12,
                                  numericInput("pCutoff", "–log10(padj) Cutoff", value = 1.3),
                                  conditionalPanel(
                                    condition = "output.isMultiGroup == true",
                                    numericInput(
                                      "omnibusPadjCutoff",
                                      "Omnibus adjusted p-value cutoff",
                                      value = 0.05
                                    ),
                                    numericInput(
                                      "posthocPadjCutoff",
                                      "Post-hoc adjusted p-value cutoff across comparisons",
                                      value = 0.05
                                    )
                                  ),
                                  numericInput("FCcutoff2", "Log2FC Threshold (Positive)", value = 1),
                                  numericInput("FCcutoff1", "Log2FC Threshold (Negative)", value = 1),
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
                            column(
                              9,
                              plotOutput("volcanoplot", width = "100%") %>% withSpinner()
                            )
                          )
                        ),
                        # MA Tab
                        tabPanel(
                          tagList(icon("exchange-alt"), "MA Plot"),
                          fluidRow(
                            column(3,
                                   fluidRow(column(12,
                                                   numericInput("size", "Label Size", value = 3),
                                                   numericInput("boxPadding", "Box Padding", value = 0.5),
                                                   numericInput("maxOverlaps", "Maximum Overlaps", value = 10),
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
                strong("The Functional Enrichment Module"),
                " identifies ",
                strong("biological pathways and functional categories"),
                " associated with user-defined gene sets. It supports ",
                strong("GO"),
                ", ",
                strong("KEGG"),
                ", and ",
                strong("GSEA"),
                " analyses, allowing users to configure ontology or gene-set options, apply p-value and q-value thresholds, and use upregulated or downregulated gene lists derived from differential analysis. After running the analysis, results are presented in interactive tables and complemented by multiple visualization plots to facilitate interpretation. Users may adjust gene selections, modify analysis parameters, and explore dynamic visual outputs to obtain functional insights into the regulatory mechanisms reflected in their data.",
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
                       "Upregulated and Downregulated Gene Lists",
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
                strong("The Motif Enrichment Module"),
                " identifies ",
                strong("transcription factor binding motifs"),
                " enriched within user-defined sets of upregulated and downregulated peaks. Users may input or modify peak sets, adjust the number of top transcription factors to display, and optionally enable motif clustering or show GC-content information. After running the analysis, results are presented in interactive tables and ",
                strong("motif heatmaps"),
                " summarizing enriched transcription factor motifs for each peak set. All outputs can be downloaded for further exploration, providing insights into potential regulatory mechanisms associated with differential chromatin regions.",
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
                       "Upregulated Peak Set",
                       rows = 10
                     )
              ),
              column(4,
                     textAreaInput(
                       "downgenes_list3",
                       "Downregulated Peak Set",
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
                       label = "Show Motif GC Content",
                       choices = c("Yes" = TRUE, "No" = FALSE),
                       selected = TRUE
                     ),
                     radioButtons(
                       inputId = "enable_clustering",
                       label = "Enable TF Clustering",
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
              icon("table"), "Motif Enrichment Table (Upregulated Peaks)",
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
            title = tagList(icon("table"), "Motif Enrichment Table (Downregulated Peaks)",
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
                       h6("Motif Heatmap(Upregulated Peak Set)"),
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
                       h6("Motif Heatmap(Downregulated Peak Set)"),
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
            collapsible = TRUE,
            HTML(paste0(
              "<p style = ' font-size:17.5px; color: black;'>",
              "<b>Contact</b>",
              "<br>If you have any technical or collaboration needs, please contact:",
              "<br>Siwen Xu (siwxu@gdpu.edu.cn)",
              "<br>Qingyan Zou (1040624480@qq.com)",
              "</p>",
              "<br><p style = ' font-size:17.5px; color: black;'>",
              "<b>Code Availability</b>",
              "<br>The source code for ChromTag can be found in ","<a  href = 'https://github.com/fluquor1214/ChromTag' target='_blank'>this</a>"," repository.",
              "</p>"
            ))
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
        "<b>Last update</b>: 18/5/2026",
        "</p>",
        "</br><p style = 'text-align: center; font-size: 1.0em; color: black; line-height: 10%;'>",
        "<b>Address</b>: No. 280, Outer Ring East Road, Panyu District, Guangzhou City, Guangdong Province, China | ",
        "<b>Postcode</b>: 511400",
        "</p>",
        "</br><p style = 'text-align: center; font-size:1.0em; line-height: 10%;'> ",
        "<a  href = 'https://github.com/fluquor1214/ChromTag' target='_blank'>Github</a> | ",
        "<a  href = 'https://www.xulabgdpu.org.cn' target='_blank'>XuLabGDPU</a>",
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