# Functions for visualizing matrices

#' Line plot of a specified matrix coefficients to compare coefficient across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' Models must have the same coefficient in the rows
#' @param matrix_name Name of model matrix to extract data from, e.g. "B"
#' @param coefficient_name Row name in the specified matrix for the line plot
#' @export
lineplotMatrixCoefficient <- function(model_list, matrix_name, coefficient_name) {
  # Generate BEA sector color mapping
  mapping <- getBEASectorColorMapping(model_list[[1]]$specs$BaseIOLevel)
  # Prepare data frame for plot
  df <- data.frame()
  for (modelname in names(model_list)) {
    df_model <- data.frame()
    model <- model_list[[modelname]]
    matrix <- model[[matrix_name]]
    modelspecs <- model$specs
    matrix <- matrix[rownames(matrix)==coefficient_name, ]
    matrix <- cbind.data.frame(names(matrix), matrix)
    colnames(matrix) <- c("SectorCode", "Coeff")
    matrix$SectorCode <- toupper(gsub("/.*", "", matrix$SectorCode))
    df_model <- rbind(matrix, df_model)
    df_model <- merge(df_model, mapping[, c(paste0(model$specs$BaseIOLevel, "Code"), "color")],
                      by.x = "SectorCode", by.y = paste0(model$specs$BaseIOLevel, "Code"))
    df_model <- merge(df_model, model$SectorNames, by = "SectorCode")
    df_model$modelname <- modelname
    df <- rbind(df, df_model)
  }
  #! Temp unit hardcoding - should come from flow
  y_unit <- "(kg/$)"
  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(SectorCode, levels = model$SectorNames$SectorCode),
                                        y = Coeff, group = as.character(modelname))) +
    ggplot2::geom_line() + ggplot2::aes(color = as.character(modelname)) +
    ggplot2::labs(x = "", y = paste(coefficient_name, y_unit)) +
    ggplot2::scale_x_discrete(breaks = df$SectorCode, labels = df$SectorName) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) + ggplot2::coord_cartesian() +
    ggplot2::theme_linedraw(base_size = 15) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = df_model$color),
                   axis.title.y = ggplot2::element_text(size = 15), legend.title = ggplot2::element_blank(),
                   legend.justification = c(1, 1), legend.position = c(0.95, 0.95),
                   axis.ticks = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(rep(5.5, 3), 90))
  
  return(p)
}

#' Bar plot of indicator scores calculated from totals by sector and displayed by BEA Sector Level to compare scores across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' @param totals_by_sector_name The name of one of the totals by sector tables available in model$SatelliteTables$totals_by_sector
#' @param indicator_code The code of the indicator of interest from the model$Indicators
#' @param sector Can be a boolean value or a text. If non-boolean, it must be code of a BEA sector.
#' @export
barplotIndicatorScoresbySector <- function(model_list, totals_by_sector_name, indicator_code, sector) {
  # Generate BEA sector color mapping
  mapping <- getBEASectorColorMapping(model_list[[1]]$specs$BaseIOLevel)
  #Create totals_by_sector dfs with indicator scores added and combine in a single df
  df <- data.frame()
  for (modelname in names(model_list)) {
    model <- model_list[[modelname]]
    # Calculate Indicator Scores
    df_model <- calculateIndicatorScoresforTotalsBySector(model, totals_by_sector_name, indicator_code)
    Unit <- unique(df_model$Unit)
    df_cols_to_keep <- c("FlowName","Compartment","Unit","SectorCode","Code","IndicatorScore")
    df_model <- df_model[,df_cols_to_keep]
    # Assign sector name and colors
    df_model <- merge(df_model, mapping, by.x = "SectorCode", by.y = paste0(model$specs$BaseIOLevel,"Code"))
    # Aggregate 
    df_model <- stats::aggregate(IndicatorScore ~ SectorCode.y + SectorCode + SectorName + color, df_model, sum)
    df_model$Model <- modelname
    df <- rbind(df, df_model[order(df_model$SectorName), ])
  }
  # Plot
  if (sector==FALSE) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Model, y = IndicatorScore, fill = SectorName)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8)
  } else {
    df <- df[df$SectorCode.y==sector, ]
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Model, y = IndicatorScore, fill = SectorName, group = SectorCode)) +
      ggplot2::geom_bar(stat = "identity", width = 0.8, color = "white") +
      ggplot2::geom_label(ggplot2::aes(label = SectorCode),
                          position = ggplot2::position_stack(0.5), fill = "white", color = "black", fontface = "bold", size = 5)
  }
  p <- p + ggplot2::scale_fill_manual(breaks = df$SectorName, values = df$color) +
    ggplot2::labs(x = "", y = paste0(indicator_code, " (", Unit, ")")) +
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
#' @param BaseIOLevel The model$BaseIOLevel to use
#' @return df with mapping with model BaseIOLevel codes to BEA Sector Codes, Names, and colors
getBEASectorColorMapping <- function(BaseIOLevel) {
  # Load VisualizationEssentials.yml and convert it to a data frame ColorLabelMapping
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package = "useeior")
  VisualizationEssentials <- configr::read.config(configfile)
  ColorLabelMapping <- as.data.frame(t(cbind.data.frame(VisualizationEssentials$BEASectorLevel$ColorLabelMapping)))
  colnames(ColorLabelMapping) <- c("SectorName", "SectorCode")
  ColorLabelMapping$color <- rownames(ColorLabelMapping)
  # Prepare BEA Sector-BaseIOLevel mapping
  MasterCrosswalk <- useeior::MasterCrosswalk2012
  code_for_model_level <- paste("BEA_2012", BaseIOLevel, "Code", sep = "_")
  mapping <- unique(MasterCrosswalk[, c("BEA_2012_Sector_Code", code_for_model_level)])
  colnames(mapping) <- c("SectorCode", paste0(BaseIOLevel, "Code"))
  # Merge BEA mapping with ColorLabelMapping
  mapping <- merge(mapping, ColorLabelMapping)
  return(mapping)
}

