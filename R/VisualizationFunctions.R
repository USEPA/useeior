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
  p <- ggplot2::ggplot(df, aes(x = SectorCode, y = Coeff, group = 1)) +
    geom_line() + #aes(color = as.character(Year)) +
    #scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) +
    labs(x = "", y = paste(tools::toTitleCase(flow), "Factors", y_unit)) +
    scale_x_discrete(breaks = df$SectorCode, labels = df$V1) +
    scale_y_continuous(expand = c(0, 0)) + coord_cartesian() + #ylim = ylim # removed from coord_cartesian
    theme_linedraw(base_size = 15) +
    theme(axis.text = element_text(color = "black", size = 15), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = df$color),  #color = color, 
          axis.title.y = element_text(size = 15), legend.title = element_blank(), legend.justification = c(1, 1), legend.position = c(0.95, 0.95),
          axis.ticks = element_blank(), panel.grid.minor.y = element_blank(), plot.margin = margin(rep(5.5, 3), 90))
  return(p)
}
