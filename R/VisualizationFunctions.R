## Functions for visualizing model components and results

#' @import ggplot2
NULL

#' Plot specified matrix coefficients as points to compare coefficient across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' Models must have the same coefficient in the rows
#' @param matrix_name Name of model matrix to extract data from, e.g. "B"
#' @param coefficient_name Row name in the specified matrix for the line plot
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param y_title The title of y axis, excluding unit.
#' @param y_label The labels of y axis, can be "Name" or "Code".
#' @param log_scale A logical value indicating whether plotting in log scale.
#' @export
plotMatrixCoefficient <- function(model_list, matrix_name, coefficient_name, sector_to_remove, y_title, y_label, log_scale = FALSE) {
  # Prepare data frame for plot
  df <- data.frame()
  for (modelname in names(model_list)) {
    model <- model_list[[modelname]]
    # Adjust y_title
    if (unique(coefficient_name%in%model$Indicators$meta$Name)) {
      Y_title <- paste0(y_title, " (", model$Indicators$meta[match(coefficient_name, model$Indicators$meta$Name), "Unit"], "/$)")
    } else {
      Y_title <- y_title
    }
    # Generate BEA sector color mapping
    mapping <- getBEASectorColorMapping(model)
    mapping$GroupName <- mapping$SectorName
    # Generate matrix
    matrix <- model[[matrix_name]]
    matrix <- matrix[coefficient_name, , drop = FALSE]
    rownames(matrix) <- Y_title
    matrix <- as.data.frame(reshape2::melt(matrix))
    colnames(matrix) <- c("CoefficientName", "Sector", "Value")
    matrix$Sector <- toupper(gsub("/.*", "", matrix$Sector))
    # Convert matrix to df
    df_model <- data.frame()
    df_model <- rbind(matrix, df_model)
    df_model <- merge(df_model, mapping[, c(paste0(model$specs$BaseIOLevel, "Code"), "color", "GroupName")],
                      by.x = "Sector", by.y = paste0(model$specs$BaseIOLevel, "Code"))
    if (model$specs$CommodityorIndustryType=="Commodity") {
      SectorName <- model$Commodities[, c("Code", "Name")]
    } else {
      SectorName <- model$Industries[, c("Code", "Name")]
    }
    colnames(SectorName) <- c("Sector", "SectorName")
    df_model <- merge(df_model, SectorName, by = "Sector")
    df_model$modelname <- modelname
    # Remove certain sectors
    df_model <- df_model[!df_model$Sector%in%sector_to_remove, ]
    df_model <- df_model[order(df_model$GroupName), ]
    df <- rbind(df, df_model)
  }
  
  # Transform df based on user preference of Code or Name on the y axis
  # Prepare axis label color and selection of y_label
  if (y_label=="Name") {
    df_wide <- reshape2::dcast(df, CoefficientName + Sector + color + GroupName + SectorName ~ modelname, value.var = "Value")
    df <- reshape2::melt(df_wide, id.vars = c("CoefficientName", "Sector", "color", "GroupName", "SectorName"),
                         variable.name = "modelname", value.name = "Value")
    df <- df[order(df$GroupName), ]
    label_colors <- rev(unique(df[, c("SectorName", "color")])[, "color"])
    df$x <- df$SectorName
  } else {
    df_wide <- reshape2::dcast(df, CoefficientName + Sector + color + GroupName ~ modelname, value.var = "Value")
    df <- reshape2::melt(df_wide, id.vars = c("CoefficientName", "Sector", "color", "GroupName"),
                         variable.name = "modelname", value.name = "Value")
    df <- df[order(df$GroupName), ]
    label_colors <- rev(unique(df[, c("Sector", "color")])[, "color"])
    df$x <- df$Sector
  }
  df <- df[complete.cases(df), ]
  # plot
  p <- ggplot(df, aes(x = factor(x, levels = rev(unique(x))),
                      y = Value, shape = as.character(modelname)))
  if (length(model_list)>1) {
    p <- p + geom_line(aes(group = x), color='red')
  }
  p <- p + geom_point(aes(color = GroupName), size = 3) +
    scale_shape_manual(values = c(0:(length(unique(df$modelname))-1))) +
    scale_color_manual(values = unique(df$color)) +
    labs(x = "", y = Y_title) +
    scale_y_continuous(breaks = scales::pretty_breaks(),
                       sec.axis = sec_axis(~., name = Y_title, breaks = scales::pretty_breaks())) +
    coord_flip() +
    theme_linedraw(base_size = 15) +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.text.y = element_text(size = 10, color = label_colors),
          axis.title.x = element_text(size = 10), legend.title = element_blank(),
          legend.justification = c(1, 1), axis.ticks = element_blank(),
          panel.grid.minor.y = element_blank(), plot.margin = margin(c(5, 20, 5, 5)))
  if (length(coefficient_name)>1) {
    p <- p + facet_wrap(~CoefficientName, ncol = length(coefficient_name), scales = "free_x") +
      labs(y = "") + scale_y_continuous(breaks = scales::pretty_breaks(),
                                        sec.axis = sec_axis(~., name = "", breaks = scales::pretty_breaks())) +
      theme(strip.background = element_blank(), strip.placement = "outside",
            strip.text = element_text(colour = "black", size = 15))
  }
  if (log_scale) {
    max_log10_N <- ceiling(log10(max(df$Value, na.rm = TRUE)))
    min_log10_N <- ceiling(log10(min(df$Value, na.rm = TRUE)*-1))
    breaks <- c(-1*10^rev(seq(1, min_log10_N, 1)), 0, 10^seq(1, max_log10_N, 1))
    labels <- seq(-1*min_log10_N, max_log10_N, 1)
    p <- p + scale_y_continuous(trans = scales::trans_new("signed_log",
                                                          transform=function(x) sign(x)*log10(abs(x)+1E-6),
                                                          inverse=function(x) sign(x)*10^abs(x)),
                                breaks = breaks, labels = labels, name = paste(Y_title, "(10^)"),
                                sec.axis = sec_axis(~., breaks = breaks, labels = labels,
                                                    name = paste(Y_title, "(10^)")))
  }
  return(p)
}

#' Bar plot of indicator scores calculated from totals by sector and displayed by BEA Sector Level to compare scores across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' @param totals_by_sector_name The name of one of the totals by sector tables available in model$SatelliteTables$totals_by_sector
#' @param indicator_name The name of the indicator of interest from the model$Indicators$factors
#' @param sector Can be logical value or text. If non-boolean, it must be code of one or more BEA sectors.
#' @param y_title The title of y axis, excluding unit.
#' @export
barplotIndicatorScoresbySector <- function(model_list, totals_by_sector_name, indicator_name, sector, y_title) {
  # Generate BEA sector color mapping
  mapping <- getBEASectorColorMapping(model)
  # Create totals_by_sector dfs with indicator scores added and combine in a single df
  df <- data.frame()
  for (modelname in names(model_list)) {
    model <- model_list[[modelname]]
    # Calculate Indicator Scores
    df_model <- calculateIndicatorScoresforTotalsBySector(model, totals_by_sector_name, indicator_name)
    Unit <- unique(df_model$Unit)
    keep_cols <- c("Flowable", "Context", "Unit", "Sector", "IndicatorScore")
    df_model <- df_model[, keep_cols]
    # Assign sector name and colors
    df_model <- merge(df_model, mapping, by.x = "Sector", by.y = paste0(model$specs$BaseIOLevel,"Code"))
    # Aggregate 
    df_model <- stats::aggregate(IndicatorScore ~ Sector.y + Sector + SectorName + color, df_model, sum)
    df_model$Model <- modelname
    df <- rbind(df, df_model[order(df_model$SectorName), ])
  }
  # Plot
  if (sector==FALSE) {
    p <- ggplot(df, aes(x = factor(Model, levels = names(model_list)),
                        y = IndicatorScore, fill = SectorName)) +
      geom_bar(stat = "identity", width = 0.8)
  } else {
    df <- df[df$Sector.y%in%sector, ]
    p <- ggplot(df, aes(x = factor(Model, levels = names(model_list)),
                        y = IndicatorScore, fill = SectorName, group = Sector)) +
      geom_bar(stat = "identity", width = 0.8, color = "white") +
      geom_label(aes(label = Sector), position = position_stack(0.5),
                 fill = "white", color = "black", fontface = "bold", size = 5)
  }
  p <- p + scale_fill_manual(breaks = df$SectorName, values = df$color) +
    labs(x = "", y = paste0(y_title, " (", Unit, ")")) +
    scale_y_continuous(expand = c(0, 0), labels = function(x) format(x, scientific = TRUE)) +
    theme_linedraw(base_size = 15) +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
          axis.title.y = element_text(size = 15), legend.title = element_blank(),
          #legend.justification = c(1, 1), legend.position = c(0.95, 0.95),
          axis.ticks = element_blank(), panel.grid.minor.y = element_blank(),
          plot.margin = margin(rep(5.5, 3), 90))
    
  return(p)
}

#' Heatmap showing coverage of satellite tables
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param form Form of sectors in satellite table, can be"Commodity" and "Industry".
#' @export
heatmapSatelliteTableCoverage <- function(model, form="Commodity") {
  # Generate BEA sector color mapping
  mapping <- getBEASectorColorMapping(model)
  # Create df based on sat table totals_by_sector
  df <- data.frame()
  for (n in names(model$SatelliteTables$totals_by_sector)) {
    # Aggregate totals_by_sector by Industry
    df_n <- stats::aggregate(FlowAmount ~ Sector, model$SatelliteTables$totals_by_sector[[n]], sum)
    if (model$specs$CommodityorIndustryType=="Commodity" && form=="Commodity") {
      # Convert from Industry to Commodity
      df_n <- merge(df_n, model$Industries, by.x = "Sector", by.y = "Code", all.y = TRUE)
      df_n <- df_n[match(model$Industries$Code, df_n$Sector), ]
      df_n[is.na(df_n$FlowAmount), "FlowAmount"] <- 0
      rownames(df_n) <- df_n$Code_Loc
      df_n[, c("Sector", "Name", "Code_Loc")] <- NULL
      df_n <- as.data.frame(t(t(as.matrix(df_n[rownames(model$V_n), ])) %*% model$V_n))
      colnames(df_n) <- "FlowAmount"
      df_n$Sector <- gsub("/.*", "", rownames(df_n))
    }
    df_n$FlowType <- n
    df <- rbind(df, df_n)
  }
  df[df$FlowAmount==0, "Value"] <- 0
  df[df$FlowAmount>0, "Value"] <- 1
  
  df <- merge(df, mapping, by.x = "Sector", by.y = paste0(model$specs$BaseIOLevel, "Code"))
  df <- df[order(df$SectorName), ]
  # Prepare axis label and fill colors
  colors <- unique(df[, c("SectorName", "color")])[, "color"]
  # plot
  p <- ggplot(df, aes(x = factor(FlowType, levels = sort(unique(FlowType))),
                      y = factor(Sector, levels = rev(unique(Sector))),
                      alpha = Value, fill = SectorName)) + 
    geom_tile(color = "white", size = 0.1) + guides(alpha = "none") +
    scale_fill_manual(values = colors) +
    labs(x = "", y = "") +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    scale_y_discrete(expand = c(0, 0)) +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title.x = element_text(size = 12), axis.text.y = element_blank(),
          legend.title = element_blank(), legend.text = element_text(size = 15),
          legend.key.size = unit(1, "cm"), axis.ticks = element_blank(),
          plot.margin = margin(c(5, 20, 5, 5)), #strip.background = element_blank(),
          strip.text = element_text(colour = "black", size = 15), strip.placement = "outside",
          axis.title = element_blank(), panel.spacing = unit(0.1, "lines"))
  return(p)
}

#' SMM tool like heatmap showing ranking of sectors
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param matrix A matrix from model result
#' @param indicators A vector of indicators to plot
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param N_sector A numeric value indicating number of sectors to show in the ranking
#' @param x_title A string specifying desired title on the x-axis, default is NULL, the title will be "modelname indicators"
#' @export
heatmapSectorRanking <- function(model, matrix, indicators, sector_to_remove, N_sector, x_title = NULL) {
  # Generate BEA sector color mapping
  mapping <- getBEASectorColorMapping(model)
  mapping$GroupName <- mapping$SectorName
  # Prepare data frame for plot
  df <- as.data.frame(prop.table(matrix, margin = 2))
  if (model$specs$Model=="USEEIOv2.0") {
    colnames(df) <- model$Indicators$meta[order(match(model$Indicators$meta$Name, colnames(df))), "Code"]
  }
  df$Sector <- toupper(gsub("/.*", "", rownames(df)))
  df <- merge(df, mapping[, c(paste0(model$specs$BaseIOLevel, "Code"), "color", "GroupName")],
              by.x = "Sector", by.y = paste0(model$specs$BaseIOLevel, "Code"), all.x = TRUE)
  if (model$specs$CommodityorIndustryType=="Commodity") {
    SectorName <- model$Commodities[, c("Code", "Name")]
  } else {
    SectorName <- model$Industries[, c("Code", "Name")]
  }
  colnames(SectorName) <- c("Sector", "SectorName")
  df <- merge(df, SectorName, by = "Sector")
  # Subset df to keep specified indicators and remove unwanted sectors
  df <- df[!df$Sector%in%sector_to_remove, ]
  if(length(indicators)>1) {
    df$Score <- rowSums(df[, indicators])
  } else {
    df$Score <- df[, indicators]
  }
  # Rank by value
  df$ranking <- rank(-df$Score)
  df <- df[order(df$ranking), ][1:N_sector, ]
  # Reshape df
  df <- reshape2::melt(df, id.vars = c("ranking", "Sector", "SectorName", "GroupName", "color"),
                       variable.name = "Indicator", value.name = "Value")
  df <- df[df$Indicator%in%c("Score", indicators), ]
  
  # Prepare axis label color
  label_colors <- rev(unique(df[, c("SectorName", "color")])[, "color"])
  x_title <- ifelse(is.null(x_title), paste(model$specs$Model, "Indicators"), x_title)
  
  # plot
  p <- ggplot(df, aes(x = factor(Indicator, levels = c("Score", indicators)),
                      y = factor(SectorName, levels = rev(unique(SectorName))),
                      fill = Value)) +
    geom_tile(color = "black", size = 0.2) +
    scale_fill_gradient(low = "white", high = "black") +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    scale_y_discrete(expand = c(0, 0), labels = function(x) stringr::str_wrap(x, 30)) +
    labs(x = x_title, y = "") + theme_bw() +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(angle = 45, hjust = 0, vjust = 1),
          axis.text.y = element_text(size = 15, color = label_colors),
          legend.position = "none", axis.ticks = element_blank(),
          plot.margin = margin(c(5, 20, 5, 5)))
  return(p)
}

#' Proportional bar chart splitting out flows or impacts by a region and the Rest of the region
#' @param R1_calc_result A matrix from model result.
#' @param Total_calc_result A matrix from model result.
#' @param x_title The title of x axis, excluding unit.
#' @return a ggplot bar chart with horizontal orientation
#' @export
barplotFloworImpactFractionbyRegion <- function(R1_calc_result, Total_calc_result, x_title) {
  rel_diff <- as.data.frame(colSums(R1_calc_result)/colSums(Total_calc_result))
  colnames(rel_diff) <- "Fraction"
  rel_diff[["Indicator"]] <- rownames(rel_diff)
  mapping <- getIndicatorColorMapping()
  rel_diff <- merge(rel_diff, mapping, by.x = 0, by.y = "Indicator")
  rel_diff <- rel_diff[rev(match(mapping$Indicator, rel_diff$Indicator)), ]
  p <- ggplot(rel_diff, aes(y = factor(Indicator, levels = Indicator),
                            x = Fraction, fill = Indicator)) +
    scale_fill_manual(limits = rel_diff$Indicator, values = rel_diff$color) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                       breaks = scales::pretty_breaks(),
                       labels = scales::label_percent(accuracy = 1)) +
    geom_col() + labs(x = x_title, y = "") + theme_bw() +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(), legend.position = "none")
  return(p)
}

## Helper functions for plotting

#' Uses VizualizationEssentials.yml to get a mapping of the to the BEA Sector Color scheme
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @return df with mapping with model BaseIOLevel codes to BEA Sector Codes, Names, and colors
getBEASectorColorMapping <- function(model) {
  # Load VisualizationEssentials.yml and convert it to a data frame ColorLabelMapping
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package = "useeior")
  VisualizationEssentials <- configr::read.config(configfile)
  ColorLabelMapping <- as.data.frame(t(cbind.data.frame(VisualizationEssentials$BEASectorLevel$ColorLabelMapping)),
                                     stringsAsFactors = FALSE)
  colnames(ColorLabelMapping) <- c("SectorName", "Sector")
  ColorLabelMapping$color <- rownames(ColorLabelMapping)
  # Add Households, Used and Other
  # ColorLabelMapping["#FFE119", ] <- c("Households", "F010", "#FFE119") # yellow
  # ColorLabelMapping["#42D4F4", ] <- c("Used", "Used", "#42D4F4") # cyan (bright blue)
  # ColorLabelMapping["#469990", ] <- c("Other", "Other", "#469990") # teal
  # Prepare BEA Sector-modelIOLevel mapping
  mapping <- unique(model$crosswalk[, c("BEA_Sector", "USEEIO")])
  colnames(mapping) <- c("Sector", paste0(model$specs$BaseIOLevel, "Code"))
  # Merge BEA mapping with ColorLabelMapping
  mapping <- merge(mapping, ColorLabelMapping)
  mapping[, paste0(model$specs$BaseIOLevel, "Code")] <- gsub("/.*", "", mapping[, paste0(model$specs$BaseIOLevel, "Code")])
  mapping$SectorName <- factor(mapping$SectorName, levels = ColorLabelMapping$SectorName)
  mapping <- mapping[order(mapping$SectorName), ]
  return(mapping)
}

#' Uses VizualizationEssentials.yml to get a mapping of the to indicator Color scheme
#' @return df with mapping with model indicator to colors
getIndicatorColorMapping <- function() {
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package = "useeior")
  VisualizationEssentials <- configr::read.config(configfile)
  ColorLabelMapping <- as.data.frame(t(cbind.data.frame(VisualizationEssentials$Indicators$ColorLabelMapping)),
                                     stringsAsFactors = FALSE)
  colnames(ColorLabelMapping) <- "color"
  ColorLabelMapping$Indicator <- rownames(ColorLabelMapping)
  rownames(ColorLabelMapping) <- NULL
  return(ColorLabelMapping)
}
