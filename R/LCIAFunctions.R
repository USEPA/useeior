#Generates LCIA Factors
#Generate LCIA table

#' Deprecated function for generating LCIA output using LCIA_indicators static file
generateLCIA <- function (version) { # version is a string value, e.g. 'v1.2'
  lciafactlong <- loadLCIAfactors()
  #Import LCIA indicator info
  lciainfo <- utils::read.table(system.file("extdata", "USEEIO_LCIA_Indicators.csv", package = "useeior"),
                                sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  #keep version of interest
  lciainfo <- lciainfo[,c(colnames(lciainfo)[1:4],version)]
  #Rename colnames
  colnames(lciainfo) <- c("Full name","Abbreviation","Category","Units","version")
  #Remove indicators not used by that version
  lciainfo <- subset(lciainfo,version == 1)
  #Drop the version column
  lciainfo <- lciainfo[,c("Full name","Abbreviation","Category","Units")]
  #merge in info for getting indicator metadata
  lciafactlongwithmeta <- merge(lciafactlong,lciainfo, by="Abbreviation")
  lciafactorformatted <- formatLCIAforIOMB(lciafactlongwithmeta)
  return(lciafactorformatted)
}

#' Loads all LCIA factors from static source file after melting it to long file
#' Returns df with "Name""Category""Subcategory""Unit""UUID""Abbreviation""Amount"
loadLCIAfactors <- function() {
  lciafact <- utils::read.table(system.file("extdata", "USEEIO_LCIA_Factors.csv", package = "useeior"),
                                sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  #Melt these so there is one indicator score per line
  lciafactlong <- reshape2::melt(lciafact,id.vars = c(1:5))
  #Convert variable to character
  lciafactlong$variable <- as.character(lciafactlong$variable)
  #Convert values to numeric
  lciafactlong$value <- as.numeric(lciafactlong$value)
  #drop zeroes
  lciafactlong <- subset(lciafactlong,value>0)
  #Change colname for merging later
  names(lciafactlong)[names(lciafactlong) == "variable"] <- "Abbreviation"
  names(lciafactlong)[names(lciafactlong) == "value"] <- "Amount"
  return(lciafactlong)

}

#' Formats an LCIA factor table for IOMB
#'
#'
formatLCIAforIOMB <- function(lcia_factors) {
  #import LCIA fields for IOM
  lciafields <- utils::read.table(system.file("extdata", "IOMB_LCIA_fields.csv", package = "useeior"),
                                  sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  #Change col names to match those in final format
  colnames(lcia_factors) <- c("Code","Flow","Compartment","Sub.Compartment","Unit","Flow.UUID","Amount","Name","Group","Ref.Unit")
  #Reformat to meet final format
  finallcia <- lciafactlongwithmeta[,colnames(lciafields)]
  return(finallcia)
}
