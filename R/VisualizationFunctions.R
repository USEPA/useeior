# Functions for visualizing matrices

#' Plot specified matrix coefficients as points to compare coefficient across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' Models must have the same coefficient in the rows
#' @param matrix_name Name of model matrix to extract data from, e.g. "B"
#' @param coefficient_name Row name in the specified matrix for the line plot
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param y_title The title of y axis, excluding unit.
#' @export
plotMatrixCoefficient <- function(model_list, matrix_name, coefficient_name, sector_to_remove, y_title) {
  # Generate BEA sector color mapping
  mapping <- getBEASectorColorMapping(model_list[[1]])
  mapping$GroupName <- mapping$SectorName
  # Prepare data frame for plot
  df <- data.frame()
  for (modelname in names(model_list)) {
    df_model <- data.frame()
    model <- model_list[[modelname]]
    matrix <- model[[matrix_name]]
    matrix <- matrix[rownames(matrix)==coefficient_name, ]
    matrix <- cbind.data.frame(names(matrix), matrix)
    colnames(matrix) <- c("Sector", "Coeff")
    matrix$Sector <- toupper(gsub("/.*", "", matrix$Sector))
    df_model <- rbind(matrix, df_model)
    df_model <- merge(df_model, mapping[, c(paste0(model$specs$BaseIOLevel, "Code"), "color", "GroupName")],
                      by.x = "Sector", by.y = paste0(model$specs$BaseIOLevel, "Code"))
    df_model <- merge(df_model, model$SectorNames, by = "Sector")
    df_model$modelname <- modelname
    # Remove certain sectors
    df_model <- df_model[!df_model$Sector%in%sector_to_remove, ]
    df_model <- df_model[order(df_model$GroupName), ]
    df <- rbind(df, df_model)
  }
  #! Temp unit hardcoding - should come from flow
  y_unit <- paste0("(", model$Indicators$meta[model$Indicators$meta$FullName==coefficient_name, "Unit"], "/$)")
  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(Sector, levels = rev(df_model$Sector)),
                                        y = Coeff, shape = as.character(modelname))) +
    ggplot2::geom_point(ggplot2::aes(color = GroupName), size = 3) +
    ggplot2::scale_shape_manual(values = c(0:(length(unique(df$modelname))-1))) +
    ggplot2::scale_color_manual(values = unique(df$color)) +
    ggplot2::labs(x = "", y = paste(y_title, y_unit)) +
    ggplot2::scale_x_discrete(breaks = df$Sector, labels = stringr::str_wrap(df$SectorName, width = 100)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::coord_flip() +
    ggplot2::theme_linedraw(base_size = 15) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15),
                   axis.text.y = ggplot2::element_text(size = 10, color = df_model$color),
                   axis.title.x = ggplot2::element_text(size = 10), legend.title = ggplot2::element_blank(),
                   legend.justification = c(1, 1), #legend.position = c(0.95, 0.15),
                   axis.ticks = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(c(5, 20, 5, 5)))
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
  mapping <- getBEASectorColorMapping(model_list[[1]]$specs$BaseIOLevel)
  #Create totals_by_sector dfs with indicator scores added and combine in a single df
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
  mapping <- merge(mapping, ColorLabelMapping)
  mapping$SectorName <- factor(mapping$SectorName, levels = ColorLabelMapping$SectorName)
  mapping <- mapping[order(mapping$SectorName), ]
  return(mapping)
}

