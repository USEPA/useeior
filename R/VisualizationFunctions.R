# Functions for visualizing matrices

#' Line plot of a specified matrix coefficients to compare coefficient across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' Models must have the same coefficient in the rows
#' @param matrix_name Name of model matrix to extract data from, e.g. "B"
#' @param coefficient_name Row name in the specified matrix for the line plot
#' @export
lineplotMatrixCoefficient <- function(model_list,matrix_name,coefficient_name) {
  colormap <- getBEASectorColorMapping()
  
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
    #mapping <- mapping[, c(modelspecs$BaseIOLevel, "Sector")]
    #mapping <- unique(mapping)
    df_model <- merge(df_model, mapping, by.x = "SectorCode", by.y = modelspecs$BaseIOLevel)
    #df_model <- merge(df_model, ColorLabelMapping, by.x = "Sector", by.y = "V2")
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

#' Bar plot of indicator scores calculated from totals by sector and displayed by BEA Sector Level to compare scores across models
#' @param model_list List of EEIO models with IOdata, satellite tables, and indicators loaded
#' @param totals_by_sector_name The name of one of the totals by sector tables available in model$SatelliteTables$totals_by_sector
#' @param indicator_code The code of the indicator of interest from the model$Indicators
#' @export
barplotIndicatorScoresbySector <- function(model_list, totals_by_sector_name, indicator_code) {

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
    df_model <- stats::aggregate(IndicatorScore ~ Code + SectorName + color, df_model, sum)
    df_model$Model <- modelname
    df <- rbind(df, df_model)

  }
  

  
  
  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Model, y = IndicatorScore, fill = factor(SectorName, levels = mapping$SectorName))) +
    ggplot2::geom_bar(stat = "identity") + ggplot2::scale_fill_manual(values = mapping$color) +
    ggplot2::labs(x = "", y = paste0("Indicator Score of ", indicator_code, " (", Unit, ")")) +
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
getBEASectorColorMapping <- function(BaseIOLevel){
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package="useeior")
  VisualizationEssentials <- configr::read.config(configfile)
  ColorLabelMapping <- as.data.frame(t(cbind.data.frame(VisualizationEssentials$BEASectorLevel$ColorLabelMapping)))
  colnames(ColorLabelMapping) <- c("SectorName","SectorCode")
  ColorLabelMapping$color <- rownames(ColorLabelMapping)
  
  MasterCrosswalk <- useeior::MasterCrosswalk2012
  code_for_model_level <- paste0("BEA_2012_",BaseIOLevel,"_Code")
  mapping <- unique(MasterCrosswalk[, c("BEA_2012_Sector_Code", code_for_model_level)])
  colnames(mapping) <- c("SectorCode", paste0(BaseIOLevel,"Code"))
  
  mapping <- merge(mapping,ColorLabelMapping)
  
  return(mapping)
  
}

