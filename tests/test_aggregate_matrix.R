setwd("tests")
library(devtools)
load_all("../")


#Build a detailed model that can be aggregated to summary
## USEEIOv2.2-GHG Detail, commodity model (2017 Schema)
modelname <- "USEEIOv2.2-GHG"
cfg <- paste0("modelspecs/", modelname, ".yml")
model <- buildModel(modelname, configpaths = file.path(cfg))


# Set matrix to Use matrix
m <- model$U
from_level <- "Detail"
to_level <- "Summary"

m_agg <- aggregateMatrix(m,from_level,to_level,model)

#Build a summary model with the same year
#Check to see if the aggregated matrix is equivalent to a summary model matrix with the same data year

modelname <- "USEEIOv2.2-s-ECON"
cfg <- paste0("modelspecs/", modelname, ".yml")
sum_model <- buildIOModel(modelname, configpaths = file.path(cfg))

sum_m <- sum_model$U


#Reorder to match dimensions
m_agg <- m_agg[rownames(sum_m),colnames(sum_m)]

rel_diff <- compareMatrices(m_agg,sum_m,percentage_diff = TRUE)

validation <- formatValidationResult(rel_diff, abs_diff = FALSE, tolerance=0.01)
validation
#Fails for some cells

## Test on a multi-region EEIO model

modelname <- "GAEEIOv1.0-GHG-19" #this model is in the package modelspecs
model <- buildModel(modelname)

m_2R <- model$U

from_level <- "Summary"
to_level <- "Sector"

m_agg <- aggregateMatrix(m_2R,from_level,to_level,model)

# Fails for 2R


