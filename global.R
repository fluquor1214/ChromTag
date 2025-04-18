library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(shinyBS)
library(DT)
library(DESeq2)     
library(tidyverse) 
library(GenomicRanges)
library(ChIPseeker)
library(TxDb.Hsapiens.UCSC.hg38.knownGene)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(TxDb.Mmusculus.UCSC.mm10.knownGene)
library(TxDb.Dmelanogaster.UCSC.dm6.ensGene)
library(GenomicFeatures)
library(ggupset)
library(ggimage)
library(org.Mm.eg.db)
library(org.Hs.eg.db)
library(org.Dm.eg.db)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(pheatmap)
library(EnhancedVolcano)
library(clusterProfiler)
library(BSgenome.Hsapiens.UCSC.hg38)
library(BSgenome.Mmusculus.UCSC.mm10)
library(BSgenome.Dmelanogaster.UCSC.dm6)
library(shinyFeedback)
library(shinydashboardPlus)
library(monaLisa)
library(JASPAR2020)
library(rtracklayer)
library(TFBSTools)
library(enrichplot)
library(digest)
library(grid)

example_data_dir <- "sample.csv"
cache_dir <- "extdata/"
insects_pwms_dir <- "extdata/insects_pwms.rds"
vertebrates_pwms_dir <- "extdata/vertebrates_pwms.rds"
up_sinse_rds_dir <- "extdata/up_sinse.rds"
down_sinse_rds_dir <- "extdata/down_sinse.rds"

h_human <- read.gmt("extdata/h.all.v2024.1.Hs.symbols.gmt")
msigdb_human <- read.gmt("extdata/msigdb.v2024.1.Hs.symbols.gmt")
c1_human <- read.gmt("extdata/c1.all.v2024.1.Hs.symbols.gmt")
c2_human <- read.gmt("extdata/c2.all.v2024.1.Hs.symbols.gmt")
c3_human <- read.gmt("extdata/c3.all.v2024.1.Hs.symbols.gmt")
c4_human <- read.gmt("extdata/c4.all.v2024.1.Hs.symbols.gmt")
c5_human <- read.gmt("extdata/c5.all.v2024.1.Hs.symbols.gmt")
c6_human <- read.gmt("extdata/c6.all.v2024.1.Hs.symbols.gmt")
c7_human <- read.gmt("extdata/c7.all.v2024.1.Hs.symbols.gmt")
c8_human <- read.gmt("extdata/c8.all.v2024.1.Hs.symbols.gmt")

mh_mouse <- read.gmt("extdata/mh.all.v2024.1.Mm.symbols.gmt")
m1_mouse <- read.gmt("extdata/m1.all.v2024.1.Mm.symbols.gmt")
m2_mouse <- read.gmt("extdata/m2.all.v2024.1.Mm.symbols.gmt")
m3_mouse <- read.gmt("extdata/m3.all.v2024.1.Mm.symbols.gmt")
m5_mouse <- read.gmt("extdata/m5.all.v2024.1.Mm.symbols.gmt")
m8_mouse <- read.gmt("extdata/m8.all.v2024.1.Mm.symbols.gmt")
msigdb_mouse <- read.gmt("extdata/msigdb.v2024.1.Mm.symbols.gmt")

options(shiny.maxRequestSize = 100000 * 1024 ^ 2)

options(error = NULL)

mytheme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#6aa7a6"
  ),
  fresh::adminlte_sidebar(
    width = "250px",
    dark_bg = "#5e6364",
    dark_hover_color = "#FFF",
    dark_color = "#FFF"
  ),
  fresh::adminlte_global(
    info_box_bg = "#6aa7a6"
  )
)

enhancedVolcano <- function(
    toptable,
    lab,
    x,
    y,
    selectLab = NULL,
    xlim = c(min(toptable[[x]], na.rm=TRUE) - 1.5,
             max(toptable[[x]], na.rm=TRUE) + 1.5),
    ylim = c(0, max(-log10(toptable[[y]]), na.rm=TRUE) + 5),
    xlab = bquote(~Log[2]~ "fold change"),
    ylab = bquote(~-Log[10]~italic(P)),
    axisLabSize = 18,
    title = 'Volcano plot',
    subtitle = bquote(italic(EnhancedVolcano)),
    caption = paste0('total = ', nrow(toptable), ' variables'),
    titleLabSize = 18,
    subtitleLabSize = 14,
    captionLabSize = 14,
    pCutoff = 10e-6,
    pCutoffCol = y,
    FCcutoff1 = 1.0,
    FCcutoff2 = 1.0,
    cutoffLineType = 'longdash',
    cutoffLineCol = 'black',
    cutoffLineWidth = 0.4,
    pointSize = 2.0,
    labSize = 5.0,
    labCol = 'black',
    labFace = 'plain',
    boxedLabels = FALSE,
    parseLabels = FALSE,
    shape = 19,
    shapeCustom = NULL,
    col = c('grey30', 'forestgreen', 'royalblue', 'red2'),
    colCustom = NULL,
    colAlpha = 1/2,
    colGradient = NULL,
    colGradientBreaks = c(pCutoff, 1.0),
    colGradientLabels = c('0', '1.0'),
    colGradientLimits = c(0, 1.0),
    legendLabels = c('NS', expression(Log[2]~FC),
                     'p-value', expression(p-value~and~log[2]~FC)),
    legendPosition = 'top',
    legendLabSize = 14,
    legendIconSize = 5.0,
    legendDropLevels = TRUE,
    encircle = NULL,
    encircleCol = 'black',
    encircleFill = 'pink',
    encircleAlpha = 3/4,
    encircleSize = 2.5,
    shade = NULL,
    shadeFill = 'grey',
    shadeAlpha = 1/2,
    shadeSize = 0.01,
    shadeBins = 2,
    drawConnectors = FALSE,
    widthConnectors = 0.5,
    typeConnectors = 'closed',
    endsConnectors = 'first',
    lengthConnectors = unit(0.01, 'npc'),
    colConnectors = 'grey10',
    max.overlaps = 15,
    maxoverlapsConnectors = NULL,
    min.segment.length = 0,
    directionConnectors = 'both',
    arrowheads = TRUE,
    hline = NULL,
    hlineType = 'longdash',
    hlineCol = 'black',
    hlineWidth = 0.4,
    vline = NULL,
    vlineType = 'longdash',
    vlineCol = 'black',
    vlineWidth = 0.4,
    gridlines.major = TRUE,
    gridlines.minor = TRUE,
    border = 'partial',
    borderWidth = 0.8,
    borderColour = 'black', 
    raster = FALSE)
{
  if(!is.numeric(toptable[[x]])) {
    stop(paste(x, ' is not numeric!', sep=''))
  }
  
  if(!is.numeric(toptable[[pCutoffCol]])) {
    stop(paste(y, ' is not numeric!', sep=''))
  }
  
  if (raster) {
    
    has_ggrastr <- ! is(try(find.package("ggrastr"), silent=TRUE), "try-error")
    
    if (has_ggrastr) {
      geom_point <- ggrastr::geom_point_rast
    } else {
      warning("raster disabled, required package \"ggrastr\" not installed")
    }
  }
  
  if (!is.null(maxoverlapsConnectors)) {
    max.overlaps <- maxoverlapsConnectors
  }
  
  i <- xvals <- yvals <- Sig <- NULL
  
  toptable <- as.data.frame(toptable)
  toptable$Sig <- 'NS'
  toptable$Sig[toptable[[x]] < -FCcutoff1 | toptable[[x]] > FCcutoff2] <- 'FC'
  
  toptable$Sig[toptable[[pCutoffCol]] < pCutoff] <- 'P'
  toptable$Sig[toptable[[pCutoffCol]] < pCutoff & 
                 (toptable[[x]] < -FCcutoff1 | toptable[[x]] > FCcutoff2)] <- 'FC_P'
  toptable$Sig <- factor(toptable$Sig,
                         levels=c('NS','FC','P','FC_P'))
  # reset pCutoff to corresponding value on y
  # allowing to draw hline at the correct
  # threshold
  if (pCutoffCol != y) {
    pCutoff = max(
      toptable[which(
        toptable[pCutoffCol] <= pCutoff), y]
    )
  }
  # some software programs return 0 for very low p-values
  # These throw an error in EnhancedVolcano
  # Detect these, issue warning, and convert these to
  # machine-lowest value possible
  #####
  # New functionality in > v1.2:
  # Now convert to 10^-1 lower than lowest non-zero p-value
  if (min(toptable[[y]], na.rm=TRUE) == 0) {
    # <= v1.2
    #warning(paste("One or more P values is 0.",
    #  "Converting to minimum possible value..."),
    #  call. = FALSE)
    #toptable[which(toptable[[y]] == 0), y] <- .Machine$double.xmin
    warning(paste('One or more p-values is 0.',
                  'Converting to 10^-1 * current',
                  'lowest non-zero p-value...'),
            call. = FALSE)
    toptable[which(toptable[[y]] == 0), y] <- min(
      toptable[which(toptable[[y]] != 0), y],
      na.rm = TRUE) * 10^-1
  }
  
  toptable$lab <- lab
  toptable$xvals <- toptable[[x]]
  toptable$yvals <- toptable[[y]]
  
  # If user has supplied values in selectLab, convert labels to
  # NA and then re-set with those in selectLab
  if (!is.null(selectLab)) {
    names.new <- rep(NA, length(toptable$lab))
    indices <- which(toptable$lab %in% selectLab)
    names.new[indices] <- toptable$lab[indices]
    toptable$lab <- names.new
  }
  
  # create a base theme that will later be modified
  th <- theme_bw(base_size = 24) +
    
    theme(
      legend.background = element_rect(),
      
      # title, subtitle, and caption
      plot.title = element_text(
        angle = 0,
        size = titleLabSize,
        face = 'bold',
        vjust = 1),
      plot.subtitle = element_text(
        angle = 0,
        size = subtitleLabSize,
        face = 'plain',
        vjust = 1),
      plot.caption = element_text(
        angle = 0,
        size = captionLabSize,
        face = 'plain',
        vjust = 1),
      
      # axis text
      axis.text.x = element_text(
        angle = 0,
        size = axisLabSize,
        vjust = 1),
      axis.text.y = element_text(
        angle = 0,
        size = axisLabSize,
        vjust = 0.5),
      axis.title = element_text(
        size = axisLabSize),
      
      # legend
      legend.position = legendPosition,
      legend.key = element_blank(),
      legend.key.size = unit(0.5, 'cm'),
      legend.text = element_text(
        size = legendLabSize),
      title = element_text(
        size = legendLabSize),
      legend.title = element_blank())
  
  # Create the plot object differently based on whether colCustom 
  # and shapeCustom are NULL or not. This helps to avoid messing up
  # the legend.
  #
  # 1, both colCustom and shapeCustom are activated
  if (!is.null(colCustom) & !is.null(shapeCustom)) {
    
    plot <- ggplot(toptable, aes(x=xvals, y=-log10(yvals))) + th +
      
      # over-ride legend icon sizes for colour and shape.
      # guide_legends are separate for colour and shape;
      # so, legends will be drawn separate
      guides(
        colour = guide_legend(
          order = 1,
          override.aes = list(
            size = legendIconSize)),
        shape = guide_legend(
          order = 2,
          override.aes = list(
            size = legendIconSize))) +
      
      # include new shape and colour encodings as aes
      geom_point(
        aes(
          color = factor(names(colCustom)),
          shape = factor(names(shapeCustom))),
        alpha = colAlpha,
        size = pointSize,
        na.rm = TRUE) +
      
      # specify the colour and shape with the supplied encoding
      scale_color_manual(values = colCustom) +
      scale_shape_manual(values = shapeCustom)
    
    # 2, only colCustom is activated and 'shape' has just a single value
  } else if (!is.null(colCustom) & is.null(shapeCustom) & length(shape) == 1) {
    
    plot <- ggplot(toptable, aes(x=xvals, y=-log10(yvals))) + th +
      
      # over-ride legend icon sizes for colour and shape.
      # guide_legends are separate for colour and shape;
      # so, legends will be drawn separate IF shape is also
      # included as aes to geom_point (it is not, here)
      guides(
        colour = guide_legend(
          order = 1,
          override.aes = list(
            size = legendIconSize)),
        shape = guide_legend(
          order = 2,
          override.aes = list(
            size = legendIconSize))) +
      
      # include new colour encodings as aes.
      # 'shape' is included, but outside aes
      geom_point(
        aes(
          color = factor(names(colCustom))),
        alpha = colAlpha,
        shape = shape,
        size = pointSize,
        na.rm = TRUE) +
      
      # specify the colour with the supplied encoding
      scale_color_manual(values = colCustom) +
      
      # 'shape' is not included as aes. Specifying guide = TRUE
      # here will result in legends merging
      scale_shape_manual(guide = TRUE)
    
    # 3, only colCustom is activated and 'shape' has 4 values
  } else if (!is.null(colCustom) & is.null(shapeCustom) & length(shape) == 4) {
    
    plot <- ggplot(toptable, aes(x=xvals, y=-log10(yvals))) + th +
      
      # over-ride legend icon sizes for colour and shape.
      # guide_legends are separate for colour and shape;
      # so, legends will be drawn separate
      guides(
        colour = guide_legend(
          order = 1,
          override.aes = list(
            size = legendIconSize)),
        shape = guide_legend(
          order = 2,
          override.aes = list(
            size = legendIconSize))) +
      
      # include new colour encodings as aes.
      # 'shape' is included in aes and mapped to 4
      # categories of NS, FC, P, FC_P
      geom_point(
        aes(
          color = factor(names(colCustom)),
          shape = Sig),
        alpha = colAlpha,
        size = pointSize,
        na.rm = TRUE) +
      
      # specify the colour with the supplied encoding
      scale_color_manual(values = colCustom) +
      
      # as it is included as aes, a separate legend
      # for 'shape' will be drawn. Here, over-ride that
      # legend
      scale_shape_manual(
        values = c(
          NS = shape[1],
          FC = shape[2],
          P = shape[3],
          FC_P = shape[4]),
        labels = c(
          NS = legendLabels[1],
          FC = legendLabels[2],
          P = legendLabels[3],
          FC_P = legendLabels[4]),
        guide = TRUE,
        drop = legendDropLevels)
    
    # 4, only shapeCustom is activated
  } else if (is.null(colCustom) & !is.null(shapeCustom)) {
    
    if (is.null(colGradient)) {
      
      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +
        
        # over-ride legend icon sizes for colour and shape.
        # guide_legends are separate for colour and shape;
        # so, legends will be drawn separate
        guides(
          colour = guide_legend(
            order = 1,
            override.aes = list(
              size = legendIconSize)),
          shape = guide_legend(
            order = 2,
            override.aes = list(
              size = legendIconSize))) +
        
        # include new shape encodings as aes.
        # Standard colour for NS, FC, P, FC_P,
        # are added to aes, too.
        geom_point(
          aes(
            color = Sig,
            shape = factor(names(shapeCustom))),
          alpha = colAlpha,
          size = pointSize,
          na.rm = TRUE) +
        
        # as it is included as aes, a separate legend
        # for 'colour' will be drawn. Here, over-ride that
        # legend
        scale_color_manual(
          values = c(
            NS = col[1],
            FC = col[2],
            P = col[3],
            FC_P = col[4]),
          labels = c(
            NS = legendLabels[1],
            FC = legendLabels[2],
            P = legendLabels[3],
            FC_P = legendLabels[4]),
          drop = legendDropLevels) +
        
        # specify the shape with the supplied encoding
        scale_shape_manual(values = shapeCustom)
      
    } else {
      
      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +
        
        # over-ride legend icon sizes for colour and shape.
        # guide_legends are separate for colour and shape;
        # so, legends will be drawn separate
        guides(
          shape = guide_legend(
            order = 2,
            override.aes = list(
              size = legendIconSize))) +
        
        # include new shape encodings as aes.
        # Standard colour for NS, FC, P, FC_P,
        # are added to aes, too.
        geom_point(
          aes(
            color = Sig,
            shape = factor(names(shapeCustom))),
          alpha = colAlpha,
          size = pointSize,
          na.rm = TRUE) +
        
        scale_colour_gradient(
          low = colGradient[1],
          high = colGradient[2],
          limits = colGradientLimits,
          breaks = colGradientBreaks,
          labels = colGradientLabels)
      
      # specify the shape with the supplied encoding
      scale_shape_manual(values = shapeCustom)
      
    }
    
    # 5, both colCustom and shapeCustom are null;
    # only a single shape value specified
  } else if (is.null(colCustom) & is.null(shapeCustom) & length(shape) == 1) {
    
    if (is.null(colGradient)) {
      
      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +
        
        # over-ride legend icon sizes for colour and shape.
        # including 'shape' in the colour guide_legend here
        # results in the legends merging
        guides(colour = guide_legend(
          order = 1,
          override.aes = list(
            shape = shape,
            size = legendIconSize))) +
        
        geom_point(
          aes(color = Sig),
          alpha = colAlpha,
          shape = shape,
          size = pointSize,
          na.rm = TRUE) +
        
        scale_color_manual(
          values = c(
            NS = col[1],
            FC = col[2],
            P = col[3],
            FC_P = col[4]),
          labels = c(
            NS = legendLabels[1],
            FC = legendLabels[2],
            P = legendLabels[3],
            FC_P = legendLabels[4]),
          drop = legendDropLevels)
      
    } else {
      
      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +
        
        geom_point(
          aes(color = yvals),
          alpha = colAlpha,
          shape = shape,
          size = pointSize,
          na.rm = TRUE) +
        
        scale_colour_gradient(
          low = colGradient[1],
          high = colGradient[2],
          limits = colGradientLimits,
          breaks = colGradientBreaks,
          labels = colGradientLabels)
    }
    
    # 6, both colCustom and shapeCustom are null;
    # four shape values are specified
  } else if (is.null(colCustom) & is.null(shapeCustom) & length(shape) == 4) {
    
    if (is.null(colGradient)) {
      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +
        
        # over-ride legend icon sizes for colour and shape.
        # including 'shape' in the colour guide_legend here
        # results in the legends merging
        guides(colour = guide_legend(
          order = 1,
          override.aes = list(
            shape = c(
              NS = shape[1],
              FC = shape[2],
              P = shape[3],
              FC_P = shape[4]),
            size = legendIconSize))) +
        
        geom_point(
          aes(
            color = Sig,
            shape = Sig),
          alpha = colAlpha,
          size = pointSize,
          na.rm = TRUE) +
        
        scale_color_manual(
          values = c(
            NS = col[1],
            FC = col[2],
            P = col[3],
            FC_P = col[4]),
          labels = c(
            NS = legendLabels[1],
            FC = legendLabels[2],
            P = legendLabels[3],
            FC_P = legendLabels[4]),
          drop = legendDropLevels) +
        
        scale_shape_manual(
          values = c(
            NS = shape[1],
            FC = shape[2],
            P = shape[3],
            FC_P = shape[4]),
          guide = FALSE,
          drop = legendDropLevels)
      
    } else {
      
      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +
        
        geom_point(
          aes(
            color = yvals,
            shape = Sig),
          alpha = colAlpha,
          size = pointSize,
          na.rm = TRUE) +
        
        scale_colour_gradient(
          low = colGradient[1],
          high = colGradient[2],
          limits = colGradientLimits,
          breaks = colGradientBreaks,
          labels = colGradientLabels) +
        
        scale_shape_manual(
          values = c(
            NS = shape[1],
            FC = shape[2],
            P = shape[3],
            FC_P = shape[4]),
          guide = FALSE,
          drop = legendDropLevels)
      
    }
  }
  
  # add more elements to the plot
  plot <- plot +
    
    xlab(xlab) +
    ylab(ylab) +
    
    xlim(xlim[1], xlim[2]) +
    ylim(ylim[1], ylim[2]) +
    
    geom_vline(xintercept = c(-FCcutoff1, FCcutoff2),
               linetype = cutoffLineType,
               colour = cutoffLineCol,
               size = cutoffLineWidth) +
    
    geom_hline(yintercept = -log10(pCutoff),
               linetype = cutoffLineType,
               colour = cutoffLineCol,
               size = cutoffLineWidth)
  
  # add elements to the plot for title, subtitle, caption
  plot <- plot + labs(title = title, 
                      subtitle = subtitle, caption = caption)
  
  # add elements to the plot for vlines and hlines
  if (!is.null(vline)) {
    plot <- plot + geom_vline(xintercept = vline,
                              linetype = vlineType,
                              colour = vlineCol,
                              size = vlineWidth)
  }
  if (!is.null(hline)) {
    plot <- plot + geom_hline(yintercept = -log10(hline),
                              linetype = hlineType,
                              colour = hlineCol,
                              size = hlineWidth)
  }
  
  # Border around plot
  if (border == 'full') {
    plot <- plot + theme(panel.border = element_rect(
      colour = borderColour, fill = NA, size = borderWidth))
  } else if (border == 'partial') {
    plot <- plot + theme(axis.line = element_line(
      size = borderWidth, colour = borderColour),
      panel.border = element_blank(),
      panel.background = element_blank())
  } else {
    stop('Unrecognised value passed to \'border\'. Must be \'full\' or \'partial\'')
  }
  
  # Gridlines
  if (gridlines.major) {
    plot <- plot + theme(panel.grid.major = element_line())
  } else {
    plot <- plot + theme(panel.grid.major = element_blank())
  }
  if (gridlines.minor) {
    plot <- plot + theme(panel.grid.minor = element_line())
  } else {
    plot <- plot + theme(panel.grid.minor = element_blank())
  }
  
  # user has specified to draw with geom_text or geom_label?
  if (!boxedLabels) {
    
    # For labeling with geom_[text|label]_repel and
    # geom_[text|label] with check_overlap = TRUE, 4 possible
    # scenarios can arise
    if (drawConnectors && is.null(selectLab)) {
      
      if (arrowheads) {
        arr <- arrow(length = lengthConnectors,
                     type = typeConnectors, ends = endsConnectors)
      } else {
        arr <- NULL
      }
      
      plot <- plot + geom_text_repel(
        # 修改数据筛选条件（关键修改点）
        data = subset(toptable,
                      toptable[[y]] < pCutoff &
                        (toptable[[x]] < -FCcutoff1 |  # 左侧阈值
                           toptable[[x]] > FCcutoff2)),   # 右侧阈值
        aes(label = subset(toptable,
                           toptable[[y]] < pCutoff &
                             (toptable[[x]] < -FCcutoff1 |  # 同步修改
                                toptable[[x]] > FCcutoff2))[["lab"]]), # 同步修改
        xlim = c(NA, NA),
        ylim = c(NA, NA),
        size = labSize,
        segment.color = colConnectors,
        segment.size = widthConnectors,
        arrow = arr,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE,
        direction = directionConnectors,
        max.overlaps = max.overlaps,
        min.segment.length = min.segment.length)
    } else if (drawConnectors && !is.null(selectLab)) {
      
      if (arrowheads) {
        arr <- arrow(length = lengthConnectors,
                     type = typeConnectors, ends = endsConnectors)
      } else {
        arr <- NULL
      }
      
      plot <- plot + geom_text_repel(
        data = subset(toptable,
                      !is.na(toptable[['lab']])),
        aes(label = subset(toptable,
                           !is.na(toptable[['lab']]))[['lab']]),
        xlim = c(NA, NA),
        ylim = c(NA, NA),
        size = labSize,
        segment.color = colConnectors,
        segment.size = widthConnectors,
        arrow = arr,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE,
        direction = directionConnectors,
        max.overlaps = max.overlaps,
        min.segment.length = min.segment.length)
      
    } else if (!drawConnectors && !is.null(selectLab)) {
      
      plot <- plot + geom_text(
        data = subset(toptable,
                      !is.na(toptable[['lab']])),
        aes(
          label = subset(toptable,
                         !is.na(toptable[['lab']]))[['lab']]),
        size = labSize,
        check_overlap = TRUE,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE)
      
    } else if (!drawConnectors && is.null(selectLab)) {
      
      plot <- plot + geom_text(
        data = subset(toptable,
                      toptable[[y]] < pCutoff &
                        (toptable[[x]] < -FCcutoff1 |  # 左侧阈值
                           toptable[[x]] > FCcutoff2)),   # 右侧阈值
        aes(label = subset(toptable,
                           toptable[[y]] < pCutoff &
                             (toptable[[x]] < -FCcutoff1 |  # 同步修改
                                toptable[[x]] > FCcutoff2))[["lab"]]), # 同步修改
        size = labSize,
        check_overlap = TRUE,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE)
    }
    
  } else {
    
    # For labeling with geom_[text|label]_repel and
    # geom_[text|label] with check_overlap = TRUE, 4 possible
    # scenarios can arise
    if (drawConnectors && is.null(selectLab)) {
      
      if (arrowheads) {
        arr <- arrow(length = lengthConnectors,
                     type = typeConnectors, ends = endsConnectors)
      } else {
        arr <- NULL
      }
      
      plot <- plot + geom_label_repel(
        data = subset(toptable,
                      toptable[[y]] < pCutoff &
                        (toptable[[x]] < -FCcutoff1 |  # 左侧阈值
                           toptable[[x]] > FCcutoff2)),   # 右侧阈值
        aes(label = subset(toptable,
                           toptable[[y]] < pCutoff &
                             (toptable[[x]] < -FCcutoff1 |  # 同步修改
                                toptable[[x]] > FCcutoff2))[["lab"]]), # 同步修改
        xlim = c(NA, NA),
        ylim = c(NA, NA),
        size = labSize,
        segment.color = colConnectors,
        segment.size = widthConnectors,
        arrow = arr,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE,
        direction = directionConnectors,
        max.overlaps = max.overlaps,
        min.segment.length = min.segment.length)
      
    } else if (drawConnectors && !is.null(selectLab)) {
      
      if (arrowheads) {
        arr <- arrow(length = lengthConnectors,
                     type = typeConnectors, ends = endsConnectors)
      } else {
        arr <- NULL
      }
      
      plot <- plot + geom_label_repel(
        data = subset(toptable,
                      !is.na(toptable[['lab']])),
        aes(label = subset(toptable,
                           !is.na(toptable[['lab']]))[['lab']]),
        xlim = c(NA, NA),
        ylim = c(NA, NA),
        size = labSize,
        segment.color = colConnectors,
        segment.size = widthConnectors,
        arrow = arr,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE,
        direction = directionConnectors,
        max.overlaps = max.overlaps,
        min.segment.length = min.segment.length)
      
    } else if (!drawConnectors && !is.null(selectLab)) {
      
      plot <- plot + geom_label(
        data = subset(toptable,
                      !is.na(toptable[["lab"]])),
        aes(
          label = subset(toptable,
                         !is.na(toptable[['lab']]))[['lab']]),
        size = labSize,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE)
      
    } else if (!drawConnectors && is.null(selectLab)) {
      
      plot <- plot + geom_label(
        data = subset(toptable,
                      toptable[[y]] < pCutoff &
                        (toptable[[x]] < -FCcutoff1 |  # 左侧阈值
                           toptable[[x]] > FCcutoff2)),   # 右侧阈值
        aes(label = subset(toptable,
                           toptable[[y]] < pCutoff &
                             (toptable[[x]] < -FCcutoff1 |  # 同步修改
                                toptable[[x]] > FCcutoff2))[["lab"]]), # 同步修改
        size = labSize,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE)
      
    }
  }
  
  # encircle
  if (!is.null(encircle)) {
    
    if (is(try(find.package("ggalt"), silent=TRUE), "try-error")) {
      stop("Please install package \"ggalt\" to access the \"encircle\" features")
    }
    
    plot <- plot + 
      ggalt::geom_encircle(
        data = subset(toptable,
                      rownames(toptable) %in% encircle),
        colour = encircleCol,
        fill = encircleFill,
        alpha = encircleAlpha,
        size = encircleSize,
        show.legend = FALSE,
        na.rm = TRUE)
  }
  
  # shade
  if (!is.null(shade)) {
    plot <- plot + 
      stat_density2d(
        data = subset(toptable,
                      rownames(toptable) %in% shade),
        fill = shadeFill,
        alpha = shadeAlpha,
        geom = 'polygon',
        contour = TRUE,
        size = shadeSize,
        bins = shadeBins,
        show.legend = FALSE,
        na.rm = TRUE)
  }
  
  plot <- plot + coord_cartesian(clip = 'off')
  
  return(plot)
}

validate_genes <- function(input_genes, valid_genes) {
  input_genes <- as.character(input_genes)
  valid_genes <- as.character(valid_genes)
  if (length(input_genes) == 0 || 
      all(trimws(input_genes) == "") || 
      any(input_genes %in% c("No upregulated genes", "No downregulated genes"))) {
    return(list(valid = TRUE, invalid = character(0)))
  }
  input_genes <- trimws(input_genes)
  input_genes <- input_genes[input_genes != ""]
  invalid_genes <- setdiff(input_genes, valid_genes)
  list(
    valid = length(invalid_genes) == 0,
    invalid = invalid_genes,
    total = length(input_genes)
  )
}

covplot1 <- function(peak, weightCol=NULL,
                     xlab  = "Chromosome Size (bp)",
                     ylab  = "",
                     title = "ChIP Peaks over Chromosomes",
                     chrs  = NULL,
                     xlim  = NULL,
                     lower = 1,
                     fill_color = "black") {
  isList <- is.list(peak)
  if(!isList) {  # Note: don't support data.frame
    tm <- getChrCov1(peak = peak, weightCol = weightCol, chrs = chrs, xlim = xlim, lower = lower)
  } else {
    ltm <- lapply(peak, getChrCov1, weightCol = weightCol, chrs = chrs, xlim = xlim, lower = lower)
    if (is.null(names(ltm))) {
      nn <- paste0("peak", seq_along(ltm))
      warning("input is not a named list, set the name automatically to ", paste(nn, collapse = ' '))
      names(ltm) <- nn
    }
    tm <- dplyr::bind_rows(ltm, .id = ".id")
    chr.sorted <- sortChrName(as.character(unique(tm$chr)))
    tm$chr <- factor(tm$chr, levels = chr.sorted)
  }
  
  chr <- start <- end <- value <- .id <- NULL
  
  if(length(tm$chr) == 0){
    p <- ggplot(data.frame(x = 1)) + geom_blank()
  } else {
    p <- ggplot(tm, aes(start, value))
    
    ## p <- p + geom_segment(aes(x=start, y=0, xend=end, yend= value))
    if (isList) {
      if (length(fill_color) == length(peak) && all(is_valid_color(fill_color))){
        cols = fill_color
      } else {
        cols = generate_colors(fill_color, n = length(peak))
      }
      p <- p + geom_rect(aes(xmin = start, ymin = 0, xmax = end, ymax = value, fill = .id, color = .id)) +
        scale_color_manual(values = cols) +
        scale_fill_manual(values = cols)
    } else {
      p <- p + geom_rect(aes(xmin = start, ymin = 0, xmax = end, ymax = value), fill = fill_color, color = fill_color)
    }
    
    if(length(unique(tm$chr)) > 1) {
      p <- p + facet_grid(chr ~., scales="free")
    }
    
  }
  
  p <- p + theme_classic()
  p <- p + labs(x = xlab, y = ylab, title = title, fill = NULL, color = NULL)
  p <- p + scale_y_continuous(expand = c(0,0))
  p <- p + theme(strip.text.y=element_text(angle=360))
  p <- p + scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si("")))
  
  if (!is.null(xlim) && !all(is.na(xlim)) && is.numeric(xlim) && length(xlim) == 2) {
    p <- p + xlim(xlim)
  }
  
  return(p)
}


getChrCov1 <- function(peak, weightCol, chrs, xlim, lower = 1) {
  if (is(peak, "GRanges")) {
    peak.gr <- peak
  } else if (file.exists(peak)) {
    peak.gr <- readPeakFile(peak, as = "GRanges")
  } else {
    stop("peak should be a GRanges object or a peak file...")
  }
  if (!is.null(chrs) && length(chrs) > 0) {
    peak.gr <- peak.gr[seqnames(peak.gr) %in% chrs]
  }
  if (length(peak.gr) == 0) {
    return(data.frame())
  }
  if (is.null(weightCol)) {
    peak.cov <- coverage(peak.gr)
  } else {
    weight <- mcols(peak.gr)[[weightCol]]
    peak.cov <- coverage(peak.gr, weight = weight)
  }
  cov <- lapply(peak.cov, IRanges::slice, lower = lower)
  
  get.runValue <- function(x) {
    y <- runValue(x)
    sapply(y@listData, mean)
  }
  ldf <- lapply(1:length(cov), function(i) {
    x <- cov[[i]]
    if (length(x@ranges) == 0) {
      msg <- paste0(names(cov[i]),
                    " dosen't contain signal higher than ",
                    lower)
      message(msg)
      return(NA)
    }
    data.frame(chr = names(cov[i]),
               start = start(x),
               end = end(x),
               cnt = get.runValue(x))
  })
  ldf <- ldf[!sapply(ldf, is.null)]
  df <- do.call("rbind", ldf)
  df <- na.omit(df)
  chr.sorted <- sortChrName(as.character(unique(df$chr)))
  df$chr <- factor(df$chr, levels = chr.sorted)
  if (!is.null(xlim) && !all(is.na(xlim)) && is.numeric(xlim) && length(xlim) == 2) {
    df <- df[df$start >= xlim[1] & df$end <= xlim[2], ]
  }
  df2 <- group_by(df, chr, start, end) %>% summarise(value = sum(cnt), .groups = "drop")
  return(df2)
}


covplot2 <- function(peak, weightCol=NULL,
                     xlab  = "Chromosome Size (bp)",
                     ylab  = "",
                     title = "ChIP Peaks over Chromosomes",
                     chrs  = NULL,
                     xlim  = NULL,
                     lower = 1,
                     fill_color = "black") {
  isList <- is.list(peak)
  if(!isList) {  # Note: don't support data.frame
    tm <- getChrCov2(peak = peak, weightCol = weightCol, chrs = chrs, xlim = xlim, lower = lower)
  } else {
    ltm <- lapply(peak, getChrCov2, weightCol = weightCol, chrs = chrs, xlim = xlim, lower = lower)
    if (is.null(names(ltm))) {
      nn <- paste0("peak", seq_along(ltm))
      warning("input is not a named list, set the name automatically to ", paste(nn, collapse = ' '))
      names(ltm) <- nn
    }
    tm <- dplyr::bind_rows(ltm, .id = ".id")
    chr.sorted <- sortChrName(as.character(unique(tm$chr)))
    tm$chr <- factor(tm$chr, levels = chr.sorted)
  }
  
  chr <- start <- end <- value <- .id <- NULL
  
  if(length(tm$chr) == 0){
    p <- ggplot(data.frame(x = 1)) + geom_blank()
  } else {
    p <- ggplot(tm, aes(start, value))
    
    if (isList) {
      if (length(fill_color) == length(peak) && all(is_valid_color(fill_color))){
        cols = fill_color
      } else {
        cols = generate_colors(fill_color, n = length(peak))
      }
      p <- p + geom_rect(aes(xmin = start, ymin = 0, xmax = end, ymax = value, fill = .id, color = .id)) +
        scale_color_manual(values = cols) +
        scale_fill_manual(values = cols)
    } else {
      p <- p + geom_rect(aes(xmin = start, ymin = 0, xmax = end, ymax = value), fill = fill_color, color = fill_color)
    }
    
    if(length(unique(tm$chr)) > 1) {
      p <- p + facet_grid(chr ~., scales="free")
    }
  }
  
  p <- p + theme_classic()
  p <- p + labs(x = xlab, y = ylab, title = title, fill = NULL, color = NULL)
  p <- p + scale_y_continuous(expand = c(0,0))
  p <- p + theme(strip.text.y=element_text(angle=360))
  
  p <- p + theme(axis.text.y = element_blank(), 
                 axis.title.y = element_blank(),
                 axis.line.y = element_blank(),
                 axis.ticks.y = element_blank())
  
  p <- p + scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si("")))
  
  if (!is.null(xlim) && !all(is.na(xlim)) && is.numeric(xlim) && length(xlim) == 2) {
    p <- p + xlim(xlim)
  }
  
  return(p)
}

getChrCov2 <- function(peak, weightCol, chrs, xlim, lower=1) {
  if (is(peak, "GRanges")) {
    peak.gr <- peak
  } else if (file.exists(peak)) {
    peak.gr <- readPeakFile(peak, as="GRanges")
  } else {
    stop("peak should be a GRanges object or a peak file...")
  }
  
  if ( is.null(weightCol)) {
    peak.cov <- coverage(peak.gr)
  } else {
    weight <- mcols(peak.gr)[[weightCol]]
    peak.cov <- coverage(peak.gr, weight=weight)
  }
  
  cov <- lapply(peak.cov, IRanges::slice, lower=lower)
  
  get.runValue <- function(x) {
    y <- runValue(x)
    sapply(y@listData, mean)
    ## value <- x@subject@values
    ## value[value != 0]
  }
  
  chr <- start <- end <- cnt <- NULL
  
  ldf <- lapply(1:length(cov), function(i) {
    x <- cov[[i]]
    if (length(x@ranges) == 0) {
      msg <- paste0(names(cov[i]),
                    " dosen't contain signal higher than ",
                    lower)
      message(msg)
      return(NA)
    }
    data.frame(chr   = names(cov[i]),
               start = start(x),
               end   = end(x),
               cnt   = get.runValue(x)
               # the following versions are more slower
               # unlist(runValue(x)) 
               # sapply(x, runValue)
    )
  })
  
  ldf <- ldf[!is.na(ldf)]
  df <- do.call("rbind", ldf)
  
  chr.sorted <- sortChrName(as.character(unique(df$chr)))
  df$chr <- factor(df$chr, levels=chr.sorted)
  if (!is.null(chrs) && !all(is.na(chrs)) && all(chrs %in% chr.sorted)) {
    df <- df[df$chr %in% chrs, ]
  }
  if (!is.null(xlim) && !all(is.na(xlim)) && is.numeric(xlim) && length(xlim) == 2) {
    df <- df[df$start >= xlim[1] & df$end <= xlim[2],]
  }
  
  df2 <- group_by(df, chr, start, end) %>% summarise(value=sum(cnt), .groups = "drop")
  return(df2)
}

sortChrName <- function(chr.name, decreasing = FALSE) {
  ## universal sort function, support organisms other than human
  chr_part <- sub("^(\\D*)(\\d*)$", "\\1", chr.name)
  num_part <- as.numeric(sub("^(\\D*)(\\d*)$", "\\2", chr.name))
  chr.name[order(chr_part, num_part, decreasing = decreasing)]
}

specific_up_peaks <<- c(
  "chr3-150407290-150415877",
  "chr16-2150741-2157314",
  "chr16-88972494-88978084",
  "chr6-119343897-119351533",
  "chr12-56682168-56690455",
  "chr8-38173859-38178882",
  "chr9-128688198-128692391",
  "chr11-65420641-65428391",
  "chr10-92688412-92694886",
  "chr6-27889944-27897392",
  "chr11-86063692-86069981",
  "chr17-76733556-76740548",
  "chr7-155643280-155646807",
  "chr20-31716637-31724038",
  "chr11-119087196-119095790",
  "chr18-807132-814894",
  "chr1-35190203-35193607",
  "chr10-68954847-68959361",
  "chr5-34913446-34918468",
  "chr7-128529403-128533500",
  "chr15-50352378-50357938",
  "chr19-49487009-49491072",
  "chr16-9089987-9095167",
  "chr18-9913339-9918660",
  "chr16-21516843-21520828",
  "chr3-196076918-196082834",
  "chr6-34234609-34240423",
  "chr16-70522158-70527684",
  "chr1-51874482-51880453",
  "chr1-67684292-67689886",
  "chr9-93449542-93454975",
  "chr1-12614231-12620067",
  "chr10-17450900-17455912",
  "chr6-35917861-35922696",
  "chr5-140399191-140404653",
  "chr12-45724536-45732139",
  "chr1-224431106-224436072",
  "chr19-5620590-5625505",
  "chr2-197496596-197502737",
  "chr12-93375325-93382230",
  "chr12-79931417-79937569",
  "chr1-100035327-100040939",
  "chr1-149836921-149846332",
  "chr6-47475939-47481242",
  "chr5-890539-894979",
  "chr12-120196854-120203217",
  "chr11-62724952-62730035",
  "chr6-44245711-44249874",
  "chr6-169699703-169705133",
  "chr16-30063851-30068335",
  "chr1-211575926-211580059",
  "chr15-37096426-37103702",
  "chr3-37174568-37178041",
  "chr19-10286345-10293793",
  "chr7-92132558-92135891",
  "chr6-37169098-37176734",
  "chr1-77975618-77981351",
  "chr12-111840056-111843484",
  "chr4-15001036-15006201",
  "chr19-13097257-13103660",
  "chr19-42251667-42256260",
  "chr8-100718629-100723546",
  "chr9-135955375-135961599",
  "chr1-246929331-246933101",
  "chr1-39573140-39577551",
  "chr1-234604221-234610816",
  "chr5-1798620-1802458",
  "chr19-51548426-51553789",
  "chr3-128482750-128493915",
  "chr6-132812471-132817341",
  "chr3-40456212-40460780",
  "chr3-12838056-12842877",
  "chr6-43574522-43578455",
  "chr6-42743970-42748022",
  "chr19-47109241-47114449",
  "chr7-99497602-99501287",
  "chr10-28531558-28535112",
  "chr1-150575995-150580802",
  "chr2-10119536-10124804",
  "chr4-55395190-55399871",
  "chr2-175165370-175169731",
  "chr13-50078436-50085418",
  "chr3-152268094-152274210",
  "chr7-106281735-106286433",
  "chr13-27250559-27254275",
  "chr3-129180469-129185841",
  "chr5-28806801-28811806",
  "chr1-1405327-1408434",
  "chr8-123271363-123275982",
  "chr1-145991978-145997924",
  "chr15-92881935-92885700",
  "chr1-235125884-235129210",
  "chr1-186373460-186377793",
  "chr2-183035358-183039266",
  "chr17-48623208-48631368",
  "chr18-12700829-12704582",
  "chr7-105008719-105018149",
  "chr19-9826368-9829389",
  "chr1-108270704-108276797",
  "chr1-154999201-155005327",
  "chr5-95729036-95735607",
  "chr2-199454228-199460781",
  "chr17-42285732-42292848",
  "chr1-149237240-149241512",
  "chr1-148888390-148894087",
  "chr10-123135121-123140076",
  "chr22-19120052-19123369",
  "chr6-159725841-159730102",
  "chr11-47764961-47768532",
  "chr11-205719-210870",
  "chr7-149146151-149150344",
  "chr2-61885901-61890253",
  "chr11-85661552-85665852",
  "chr6-34886092-34891319",
  "chr6-10692861-10696683",
  "chr6-109378969-109383910",
  "chr17-75260118-75263428",
  "chr16-18786377-18791596",
  "chr2-65429856-65433509",
  "chr10-27152444-27157379",
  "chr7-108567449-108571858",
  "chr9-83977233-83983599",
  "chr11-10806522-10810178",
  "chr1-1346468-1350600",
  "chr10-45969931-45974932",
  "chr3-19944990-19949295",
  "chr16-23835430-23839642",
  "chr19-1437825-1441188",
  "chr2-186483779-186488112",
  "chr15-45401282-45404799",
  "chr2-218395730-218401713",
  "chr1-225424702-225428979",
  "chr1-231418177-231423039",
  "chr10-35334703-35339107",
  "chr9-124859699-124863945",
  "chr6-41919616-41924307",
  "chr1-28578839-28583242",
  "chr6-87697704-87703249",
  "chr1-226061189-226065384",
  "chr7-100475846-100479659",
  "chr5-135031200-135035174",
  "chr7-100824749-100829903",
  "chr21-46321473-46326754",
  "chr17-38851202-38854467",
  "chr2-86104032-86108038",
  "chr2-38599846-38603867",
  "chr15-41415077-41419051",
  "chr6-43515667-43519136",
  "chr7-76356409-76360343",
  "chr11-75399227-75402330",
  "chr16-29662516-29667171",
  "chr17-28948442-28952626",
  "chr14-49582753-49587546",
  "chr10-73250627-73254510",
  "chr7-12398905-12404996",
  "chr20-49276310-49285428",
  "chr20-38432926-38435868",
  "chr21-42877630-42880639",
  "chr1-228404722-228408453",
  "chr2-68249974-68254420",
  "chr1-109089133-109093091",
  "chr19-1406963-1410707",
  "chr10-12194121-12197779",
  "chr19-1859698-1865215",
  "chr12-98513104-98519652",
  "chr7-92244259-92248428",
  "chr16-69130685-69134833",
  "chr4-38662583-38667733",
  "chr20-56390642-56394475",
  "chr10-102112299-102115503",
  "chr7-66994089-66998235",
  "chrX-153968712-153973517",
  "chr1-110337246-110343686",
  "chr17-44216338-44221801",
  "chr19-17859307-17863093",
  "chr12-62258324-62264505",
  "chr5-217114-220118",
  "chr21-39346100-39351959",
  "chr1-150971756-150976175",
  "chr11-65496622-65507687",
  "chr17-81509182-81513759",
  "chr1-25428444-25433555",
  "chr6-166997365-167001524",
  "chr2-175179220-175182206",
  "chr11-68902224-68905673",
  "chr22-41444024-41448734",
  "chr1-111138575-111141428",
  "chr4-122920589-122927135",
  "chr2-61016084-61019227",
  "chr7-121076016-121080962",
  "chr16-4845997-4850594",
  "chr7-107044316-107048625",
  "chr7-158827657-158830841",
  "chr6-42781048-42785371",
  "chr21-32611391-32614070",
  "chr19-15376990-15380749",
  "chr2-17751947-17755633",
  "chr17-7010886-7015714",
  "chr15-75637100-75641348",
  "chr3-37242030-37245767"
)

# 创建一个包含所有的 down_peak 数据的向量
specific_down_peaks <<- c(
  "chr7-154746354-154754198",
  "chr11-20155235-20165969",
  "chr7-27100472-27112603",
  "chr1-208072367-208087339",
  "chr1-8323134-8331215",
  "chr11-118150179-118161808",
  "chr20-47665285-47675999",
  "chr14-64734713-64750256",
  "chr1-28810214-28820062",
  "chr4-182814697-182830407",
  "chr13-113397343-113411917",
  "chr8-48140771-48150138",
  "chr8-23318213-23331736",
  "chr10-99529479-99543069",
  "chr4-4399806-4411374",
  "chr13-110435512-110452306",
  "chr8-55449054-55456151",
  "chr1-160163224-160175480",
  "chr5-123096254-123101988",
  "chr4-41744183-41752531",
  "chr7-1230612-1247909",
  "chr8-23699461-23708711",
  "chrX-135432604-135439059",
  "chr12-107842265-107848232",
  "chr10-622950-631744",
  "chr5-1760534-1767005",
  "chr16-85927206-85937689",
  "chr5-1943040-1954141",
  "chr8-9895447-9907672",
  "chr14-36502387-36525944",
  "chr6-10416559-10424457",
  "chr6-170270203-170277549",
  "chr5-176769590-176781624",
  "chr2-120312315-120321034",
  "chr5-14501368-14511319",
  "chr11-134038193-134048195",
  "chr7-156357561-156367739",
  "chr7-27199959-27206474",
  "chr5-80566301-80574645",
  "chr5-3598370-3607148",
  "chr16-84914251-84921134",
  "chr21-42730797-42740417",
  "chr10-122109622-122122349",
  "chr16-49596682-49605683",
  "chrX-9285351-9297344",
  "chr2-127661050-127667924",
  "chr6-166408623-166414054",
  "chr2-176119614-176131852",
  "chr1-204244273-204255181",
  "chr7-4201427-4212086",
  "chr8-23363071-23372248",
  "chr2-71020384-71028563",
  "chr18-23656591-23664015",
  "chr2-44945668-44955196",
  "chr17-78471231-78477280",
  "chr15-93046526-93053786",
  "chr6-168212141-168220022",
  "chr2-130251132-130254530",
  "chr1-226732655-226739266",
  "chr1-227919427-227926891",
  "chr14-104385679-104397438",
  "chr2-2726534-2734923",
  "chr1-7630236-7639284",
  "chr11-134309515-134314944",
  "chr10-101214874-101233294",
  "chr9-97854827-97864505",
  "chr10-47373223-47382392",
  "chr4-153787097-153793198",
  "chr16-83851357-83861761",
  "chr2-130521813-130528731",
  "chr7-175746-188005",
  "chr11-114167949-114179302",
  "chr10-117530140-117537788",
  "chr22-18218897-18224936",
  "chr7-27138242-27160125",
  "chr8-7331188-7340537",
  "chr8-70068681-70076407",
  "chr2-10858682-10866888",
  "chr10-8057004-8065063",
  "chr13-112619716-112627894",
  "chr2-8452950-8458006",
  "chr16-56627959-56640215",
  "chr22-46405252-46415553",
  "chr6-150829040-150836351",
  "chr8-70031275-70039850",
  "chr20-50998323-51007427",
  "chr16-87602638-87609778",
  "chr8-12486900-12494733",
  "chr6-159231463-159236008",
  "chr8-38427465-38434088",
  "chr3-42870989-42877603",
  "chr6-222850-229835",
  "chr2-240712889-240722767",
  "chr10-77727560-77734874",
  "chr21-36247842-36255584",
  "chr5-17598454-17599595",
  "chr2-101615626-101623893",
  "chr13-98770933-98782799",
  "chr9-93303535-93309259",
  "chr7-158272205-158281496",
  "chr11-130396637-130430696",
  "chr16-8763194-8770806",
  "chr1-75126580-75133431",
  "chrX-137544918-137552533",
  "chr3-141047696-141057167",
  "chr16-83997334-84005141",
  "chr15-59996509-60006841",
  "chr6-89283390-89291673",
  "chr11-117774687-117785547",
  "chr1-3539752-3546226",
  "chr2-25143533-25150242",
  "chr5-32520332-32528141",
  "chr9-113669188-113678018",
  "chr11-129370426-129376959",
  "chr11-114136900-114146085",
  "chr11-71586097-71595171",
  "chr13-113040036-113048168",
  "chr8-11719627-11728828",
  "chr22-18714543-18720152",
  "chr6-100446304-100451756",
  "chr10-43211674-43220591",
  "chr2-23653053-23660547",
  "chr1-44415664-44418718",
  "chr4-186141582-186147653",
  "chr12-128857911-128865244",
  "chr16-85917688-85921835",
  "chr20-57647978-57669084",
  "chr10-1399294-1408009",
  "chr8-23100721-23108855",
  "chr7-155583374-155591922",
  "chr3-8977349-8986707",
  "chr12-109305441-109313941",
  "chr21-33100267-33106306",
  "chr16-29229418-29239232",
  "chr11-8058759-8069529",
  "chr10-350111-370903",
  "chr20-51016899-51027522",
  "chr20-21700714-21710889",
  "chr8-48124385-48132164",
  "chrX-119994654-120002265",
  "chr1-1962751-1967875",
  "chr10-43226523-43235487",
  "chr12-114392983-114416781",
  "chr17-35495122-35499951",
  "chr7-27207425-27217505",
  "chr4-183725563-183733930",
  "chr16-981949-992501",
  "chr2-39662546-39668650",
  "chr1-202263148-202271706",
  "chr2-130960797-130969398",
  "chr6-464473-476483",
  "chr12-124113010-124120547",
  "chr3-23785647-23792958",
  "chr2-1631416-1641722",
  "chr7-47376980-47385763",
  "chr10-11336689-11344836",
  "chr9-126523638-126529506",
  "chr3-42976642-42982274",
  "chr10-79979291-79986261",
  "chr12-128927158-128936788",
  "chr3-141063568-141069784",
  "chr7-157899445-157905640",
  "chr1-13582440-13588528",
  "chr1-206743735-206751567",
  "chr18-10480978-10488390",
  "chr17-35485872-35492899",
  "chr4-3383844-3390969",
  "chr2-113275582-113281176",
  "chr21-43476062-43483155",
  "chr18-10723151-10728110",
  "chr16-85908655-85916153",
  "chr11-125396764-125414982",
  "chr12-130700320-130717244",
  "chr7-131241690-131247661",
  "chr5-178391944-178400333",
  "chr1-220525721-220530256",
  "chr11-113942920-113953081",
  "chr1-111796433-111806626",
  "chr1-9390601-9405938",
  "chr16-28060606-28069480",
  "chr20-56624219-56631986",
  "chr8-8718012-8724973",
  "chr10-70507366-70513279",
  "chr6-75204195-75209366",
  "chr7-47389253-47397336",
  "chr14-56807340-56813407",
  "chr11-117794355-117800474",
  "chr21-42084869-42092631",
  "chr9-133532573-133541360",
  "chr6-41671554-41681354",
  "chr10-8041089-8052583",
  "chr2-10710943-10718573",
  "chr9-133678274-133687782",
  "chr7-1321551-1327100",
  "chr4-624031-631484",
  "chr17-916520-923847",
  "chr12-130431723-130438816",
  "chr5-38465429-38474952",
  "chr1-9268264-9274025",
  "chr18-8780928-8789214"
)
