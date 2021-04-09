## Functions for visualizing matrices

#' Plot specified matrix coefficients as points to compare coefficient across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' Models must have the same coefficient in the rows
#' @param matrix_name Name of model matrix to extract data from, e.g. "B"
#' @param coefficient_name Row name in the specified matrix for the line plot
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param y_title The title of y axis, excluding unit.
#' @export
plotMatrixCoefficient <- function(model_list, matrix_name, coefficient_name, sector_to_remove, y_title) {
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
    if (model$specs$CommoditybyIndustryType=="Commodity") {
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
  df_wide <- reshape2::dcast(df, CoefficientName + Sector + color + GroupName + SectorName ~ modelname, value.var = "Value")
  df <- reshape2::melt(df_wide, id.vars = c("CoefficientName", "Sector", "color", "GroupName", "SectorName"),
                       variable.name = "modelname", value.name = "Value")
  df <- df[order(df$GroupName), ]
  # Prepare axis label color
  label_colors <- rev(unique(df[, c("SectorName", "color")])[, "color"])
  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(SectorName, levels = rev(unique(SectorName))),
                                        y = Value, shape = as.character(modelname))) +
    ggplot2::geom_point(ggplot2::aes(color = GroupName), size = 3) +
    ggplot2::scale_shape_manual(values = c(0:(length(unique(df$modelname))-1))) +
    ggplot2::scale_color_manual(values = unique(df$color)) +
    ggplot2::labs(x = "", y = Y_title) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                sec.axis = ggplot2::sec_axis(~., name = Y_title, breaks = scales::pretty_breaks())) +
    ggplot2::coord_flip() +
    ggplot2::theme_linedraw(base_size = 15) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15),
                   axis.text.y = ggplot2::element_text(size = 10, color = label_colors),
                   axis.title.x = ggplot2::element_text(size = 10), legend.title = ggplot2::element_blank(),
                   legend.justification = c(1, 1), axis.ticks = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(), plot.margin = ggplot2::margin(c(5, 20, 5, 5)))
  if (length(coefficient_name)>1) {
    p <- p + ggplot2::facet_wrap(~CoefficientName, ncol = length(coefficient_name), scales = "free_x") +
      ggplot2::labs(y = "") + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                                          sec.axis = ggplot2::sec_axis(~., name = "", breaks = scales::pretty_breaks())) +
      ggplot2::theme(strip.background = ggplot2::element_blank(), strip.placement = "outside",
                     strip.text = ggplot2::element_text(colour = "black", size = 15))
  }
  return(p)
}

#' Bar plot of indicator scores calculated from totals by sector and displayed by BEA Sector Level to compare scores across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' @param totals_by_sector_name The name of one of the totals by sector tables available in model$SatelliteTables$totals_by_sector
#' @param indicator_code The code of the indicator of interest from the model$Indicators
#' @param sector Can be boolean value or text. If non-boolean, it must be code of one or more BEA sectors.
#' @param y_title The title of y axis, excluding unit.
#' @export
barplotIndicatorScoresbySector <- function(model_list, totals_by_sector_name, indicator_code, sector, y_title) {
  # Generate BEA sector color mapping
  mapping <- getBEASectorColorMapping(model)
  # Create totals_by_sector dfs with indicator scores added and combine in a single df
  df <- data.frame()
  for (modelname in names(model_list)) {
    model <- model_list[[modelname]]
    # Calculate Indicator Scores
    df_model <- calculateIndicatorScoresforTotalsBySector(model, totals_by_sector_name, indicator_code)
    Unit <- unique(df_model$Unit)
    df_cols_to_keep <- c("Flowable", "Context", "Unit", "Sector", "Code", "IndicatorScore")
    df_model <- df_model[,df_cols_to_keep]
    # Assign sector name and colors
    df_model <- merge(df_model, mapping, by.x = "Sector", by.y = paste0(model$specs$BaseIOLevel,"Code"))
    # Aggregate 
    df_model <- stats::aggregate(IndicatorScore ~ Sector.y + Sector + SectorName + color, df_model, sum)
    df_model$Model <- modelname
    df <- rbind(df, df_model[order(df_model$SectorName), ])
  }
  # Plot
  if (sector==FALSE) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(Model, level = names(model_list)), y = IndicatorScore, fill = SectorName)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8)
  } else {
    df <- df[df$Sector.y%in%sector, ]
    p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(Model, level = names(model_list)), y = IndicatorScore, fill = SectorName, group = Sector)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8, color = "white") +
      ggplot2::geom_label(ggplot2::aes(label = Sector),
                          position = ggplot2::position_stack(0.5), fill = "white", color = "black", fontface = "bold", size = 5)
  }
  p <- p + ggplot2::scale_fill_manual(breaks = df$SectorName, values = df$color) +
    ggplot2::labs(x = "", y = paste0(y_title, " (", Unit, ")")) +
    ggplot2::scale_y_continuous(expand = c(0, 0), labels = function(x) format(x, scientific = TRUE)) +
    ggplot2::theme_linedraw(base_size = 15) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
                   axis.title.y = ggplot2::element_text(size = 15), legend.title = ggplot2::element_blank(),
                   #legend.justification = c(1, 1), legend.position = c(0.95, 0.95),
                   axis.ticks = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(rep(5.5, 3), 90))
    
  return(p)
}

#' Heatmap showing coverage of satellite tables
#' @param model A complete EEIO model
#' @export
heatmapSatelliteTableCoverage <- function(model) {
  # Generate BEA sector color mapping
  mapping <- getBEASectorColorMapping(model)
  # Create df based on totals_by_sector
  TbS <- model$TbS
  TbS$Impact <- gsub("\\..*", "", rownames(TbS))
  # Assign category to impact
  TbS$ImpactCategory <- gsub("/.*", "", TbS$Context)
  df <- stats::aggregate(FlowAmount ~ Impact + Sector + ImpactCategory, TbS, sum)
  df_wide <- reshape2::dcast(df, Impact + ImpactCategory ~ Sector, value.var = "FlowAmount")
  df_wide[df_wide$Impact%in%c("EMP", "VADD"), "ImpactCategory"] <- "Economic"
  df_wide$ImpactCategory <- toupper(df_wide$ImpactCategory)
  df <- reshape2::melt(df_wide, id.vars = c("Impact", "ImpactCategory"), variable.name = "Sector", value.name = "FlowAmount")
  df[is.na(df$FlowAmount), "Value"] <- 0
  df[!is.na(df$FlowAmount), "Value"] <- 1
  df$Sector <- as.character(df$Sector)
  df <- merge(df, mapping, by.x = "Sector", by.y = paste0(model$specs$BaseIOLevel,"Code"))
  df <- df[order(df$SectorName), ]
  # Prepare axis label and fill colors
  colors <- unique(df[, c("SectorName", "color")])[, "color"]
  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(Impact, levels = sort(unique(Impact))),
                                        y = factor(Sector, levels = rev(unique(Sector))),
                                        alpha = Value, fill = SectorName)) + 
    ggplot2::geom_tile(color = "white", size = 0.1) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = "top") +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15),
                   axis.title.x = ggplot2::element_text(size = 12), axis.text.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 15),
                   legend.key.size = ggplot2::unit(1, "cm"), axis.ticks = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(c(5, 20, 5, 5)), #strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(colour = "black", size = 15), strip.placement = "outside",
                   axis.title = ggplot2::element_blank(), panel.spacing = ggplot2::unit(0.1, "lines")) +
    ggplot2::guides(alpha = FALSE) +
    ggplot2::facet_grid(~ImpactCategory, scales = "free_x", space = "free_x")
  return(p)
}

#' SMM tool like heatmap showing ranking of sectors
#' @param model A complete EEIO model
#' @param matrix A matrix from model result
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param y_title The title of y axis, excluding unit.
#' @param N_sector A numeric value indicating number of sectors to show in the ranking
#' @export
heatmapSectorRanking <- function(model, matrix, indicators, sector_to_remove, y_title, N_sector) {
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
  if (model$specs$CommoditybyIndustryType=="Commodity") {
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
  
  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(Indicator, levels = c("Score", indicators)),
                                        y = factor(SectorName, levels = rev(unique(SectorName))),
                                        fill = Value)) +
    ggplot2::geom_tile(color = "white", size = 0.2) +
    ggplot2::scale_fill_gradient(low = "white", high = "black") +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = "top") +
    ggplot2::scale_y_discrete(expand = c(0, 0), labels = function(x) stringr::str_wrap(x, 30)) +
    ggplot2::labs(x = model$specs$Model, y = "", fill = y_title) + ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15),
                   axis.title.x = ggplot2::element_text(size = 20),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 0, vjust = 1),
                   axis.text.y = ggplot2::element_text(size = 15, color = label_colors),
                   legend.position = "none", axis.ticks = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(c(5, 20, 5, 5)))
  return(p)
}

## Helper functions for plotting

#' Uses VizualizationEssentials.yml to get a mapping of the to the BEA Sector Color scheme
#' @param model A complete EEIO model
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
  ColorLabelMapping["#FFE119", ] <- c("Households", "F010", "#FFE119") # yellow
  ColorLabelMapping["#42D4F4", ] <- c("Used", "Used", "#42D4F4") # cyan (bright blue)
  ColorLabelMapping["#469990", ] <- c("Other", "Other", "#469990") # teal
  # Prepare BEA Sector-modelIOLevel mapping
  mapping <- unique(model$crosswalk[, c("BEA_Sector", paste0("BEA_", model$specs$BaseIOLevel))])
  colnames(mapping) <- c("Sector", paste0(model$specs$BaseIOLevel, "Code"))
  # Merge BEA mapping with ColorLabelMapping
  mapping <- merge(mapping, ColorLabelMapping, all.x = TRUE)
  mapping[, paste0(model$specs$BaseIOLevel, "Code")] <- gsub("/.*", "", mapping[, paste0(model$specs$BaseIOLevel, "Code")])
  mapping$SectorName <- factor(mapping$SectorName, levels = ColorLabelMapping$SectorName)
  mapping <- mapping[order(mapping$SectorName), ]
  return(mapping)
}

