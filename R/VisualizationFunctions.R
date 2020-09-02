# Functions for visualizing matrices

#' Line plot of flow coefficients 
#' @param flow An index name for a flow in a model matrix (full flow name)
#' @param matrix Supports a model B, M, or U matrix
#' @param modelspecs The specs for a given model
#' @export
lineplotFlowCoefficients <- function(flow, matrix, modelspecs) {
  
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package="useeior")
  VisualizationEssentials <- configr::read.config(configfile)
  ColorLabelMapping <- as.data.frame(t(cbind.data.frame(VisualizationEssentials$BEASectorLevel$ColorLabelMapping)))
  ColorLabelMapping$color <- rownames(ColorLabelMapping)
  MasterCrosswalk <- useeior::MasterCrosswalk2012
  mapping <- unique(MasterCrosswalk[, c("BEA_2012_Sector_Code", "BEA_2012_Summary_Code", "BEA_2012_Detail_Code")])
  colnames(mapping) <- c("Sector", "Summary", "Detail")
  mapping <- mapping[mapping$Sector%in%ColorLabelMapping$V2, ]
  df <- data.frame()
  matrix <- matrix[rownames(matrix)==flow, ]
  matrix <- cbind.data.frame(names(matrix), matrix)
  colnames(matrix) <- c("SectorCode", "Coeff")
  matrix$SectorCode <- toupper(gsub("/.*", "", matrix$SectorCode))
  df <- rbind(matrix, df)
  mapping <- mapping[,c(modelspecs$BaseIOLevel,"Sector")]
  mapping <- unique(mapping)
  
  df <- merge(df,mapping, by.x="SectorCode", by.y=modelspecs$BaseIOLevel)
  df <- merge(df, ColorLabelMapping, by.x = "SectorCode", by.y = "V2")
  #! Temp unit hardcoding - should come from flow
  y_unit <- "(kg/$)"
  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = SectorCode, y = Coeff, group = 1)) +
    ggplot2::geom_line() + 
    ggplot2::labs(x = "", y = paste(tools::toTitleCase(flow), "Factors", y_unit)) +
    ggplot2::scale_x_discrete(breaks = df$SectorCode, labels = df$V1) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) + ggplot2::coord_cartesian() +
    ggplot2::theme_linedraw(base_size = 15) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 15), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = df$color),   
          axis.title.y = ggplot2::element_text(size = 15), legend.title = ggplot2::element_blank(), legend.justification = c(1, 1), legend.position = c(0.95, 0.95),
          axis.ticks = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(), plot.margin = ggplot2::margin(rep(5.5, 3), 90))
  return(p)
}
