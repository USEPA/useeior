library(useeior)
model <- useeior::loadIOData('USEEIOv2.0-GHG')
model <- useeior::buildEEIOModel(model)

#This should point to your local copy of https://github.com/USEPA/USEEIO_API
useeioapidir="C:/Users/Rey/USEEIO_API"

#Write the model and sector crosswalk for the API
writeModelforAPI(model,useeioapidir)
writeSectorCrosswalkforAPI(useeioapidir)

#Build and write a second model
model <- useeior::loadIOData('USEEIOv2.0.16s-GHG')
model <- useeior::buildEEIOModel(model)
writeModelforAPI(model,useeioapidir)

#test other ouput writing writing

modeloutdir="C:/Users/Rey/USEEIO"
writeModelMatrices(model,modeloutdir)
writeModelforUSEEIOPY(model)

