# Functions for visualizing matrices

#' Line plot of flow coefficients 
#' @param flow An index name for a flow in a model matrix (full flow name)
#' @param matrix_list List of model matrix, supports a model B, M, or U matrix
#' @param modelspecs_list List of specs for given models
#' @export
lineplotFlowCoefficients <- function(flow, matrix_list, modelspecs_list) {
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package="useeior")
  VisualizationEssentials <- configr::read.config(configfile)
  ColorLabelMapping <- as.data.frame(t(cbind.data.frame(VisualizationEssentials$BEASectorLevel$ColorLabelMapping)))
  ColorLabelMapping$color <- rownames(ColorLabelMapping)
  MasterCrosswalk <- useeior::MasterCrosswalk2012
  mapping <- unique(MasterCrosswalk[, c("BEA_2012_Sector_Code", "BEA_2012_Summary_Code", "BEA_2012_Detail_Code")])
  colnames(mapping) <- c("Sector", "Summary", "Detail")
  mapping <- mapping[mapping$Sector%in%ColorLabelMapping$V2, ]
  
  df <- data.frame()
  for (name in names(matrix_list)) {
    df_model <- data.frame()
    matrix <- matrix_list[[name]]
    modelspecs <- modelspecs_list[[name]]
    matrix <- colSums(matrix[rownames(matrix)==flow, , drop = FALSE])
    matrix <- cbind.data.frame(names(matrix), matrix)
    colnames(matrix) <- c("SectorCode", "Coeff")
    matrix$SectorCode <- toupper(gsub("/.*", "", matrix$SectorCode))
    df_model <- rbind(matrix, df_model)
    mapping <- mapping[, c(modelspecs$BaseIOLevel, "Sector")]
    mapping <- unique(mapping)
    df_model <- merge(df_model, mapping, by.x = "SectorCode", by.y = modelspecs$BaseIOLevel)
    df_model <- merge(df_model, ColorLabelMapping, by.x = "Sector", by.y = "V2")
    df_model <- merge(df_model, modelspecs$SectorCodeName, by = "SectorCode")
    df_model$modelname <- name
    df <- rbind(df, df_model)
  }
  #! Temp unit hardcoding - should come from flow
  y_unit <- "(kg/$)"
  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(SectorCode, levels = modelspecs$SectorCodeName$SectorCode),
                                        y = Coeff, group = as.character(modelname))) +
    ggplot2::geom_line() + ggplot2::aes(color = as.character(modelname)) +
    #scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) +
    ggplot2::labs(x = "", y = paste(tools::toTitleCase(flow), "Factors", y_unit)) +
    ggplot2::scale_x_discrete(breaks = df$SectorCode, labels = df$SectorName) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) + ggplot2::coord_cartesian() + #ylim = ylim # removed from coord_cartesian
    ggplot2::theme_linedraw(base_size = 15) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = df_model$color), #color = color, 
                   axis.title.y = ggplot2::element_text(size = 15), legend.title = ggplot2::element_blank(),
                   legend.justification = c(1, 1), legend.position = c(0.95, 0.95),
                   axis.ticks = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(rep(5.5, 3), 90))
  
  return(p)
}

#' Bar plot of indicator scores calculated from totals by sector and displayed by BEA Sector Level
#' @param model A EEIO model with IOdata, satellite tables, and indicators loaded
#' @param totals_by_sector_name The name of one of the totals by sector tables available in model$SatelliteTables$totals_by_sector
#' @param indicator_code The code of the indicator of interest from the model$Indicators
#' @export
barplotIndicatorScoresbySector <- function(model, totals_by_sector_name, indicator_code) {
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package="useeior")
  VisualizationEssentials <- configr::read.config(configfile)
  ColorLabelMapping <- as.data.frame(t(cbind.data.frame(VisualizationEssentials$BEASectorLevel$ColorLabelMapping)))
  ColorLabelMapping$color <- rownames(ColorLabelMapping)
  MasterCrosswalk <- useeior::MasterCrosswalk2012
  mapping <- unique(MasterCrosswalk[, c("BEA_2012_Sector_Code", "BEA_2012_Summary_Code", "BEA_2012_Detail_Code")])
  colnames(mapping) <- c("Sector", "Summary", "Detail")
  mapping <- unique(mapping[mapping$Sector%in%ColorLabelMapping$V2, c(model$specs$BaseIOLevel, "Sector")])

  
  df <- calculateIndicatorScoresforTotalsBySector(model, totals_by_sector_name, indicator_code)

  df <- merge(df, mapping, by.x = "SectorCode", by.y = model$specs$BaseIOLevel)
  df <- merge(df, ColorLabelMapping, by.x = "Sector", by.y = "V2")

  # Aggregate 
  df <- stats::aggregate(IndicatorScore ~ Code + Sector + V1, df, sum)
  
  # Convert unit from kg to million metric tons (MMT)
  #df$FlowAmount <- df$FlowAmount*1E-9
  #y_unit <- "(Million Metric Tons)"
  # Plot
 
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(Sector, levels = list(unique(Sector))), y = IndicatorScore, fill = Indicator)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "", y = paste("temp ind name", "(temp_y_unit")) +
    ggplot2::scale_y_continuous(expand = c(0, 0), labels = function(x) format(x, scientific = FALSE)) +
    ggplot2::theme_linedraw(base_size = 15) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = df$color),
                   axis.title.y = ggplot2::element_text(size = 15), legend.title = ggplot2::element_blank(),
                   legend.justification = c(1, 1), legend.position = c(0.95, 0.95),
                   axis.ticks = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(rep(5.5, 3), 90))
  return(p)
}
