devtools::load_all()

settings<-list(
  stratification_col="AEBODSYS",
  group_col="ARM", 
  reference_group="Placebo",
  comparison_group="Xanomeline High Dose",
  id_col="USUBJID"
)

data <- getStats(dfAE=safetyData::adam_adae, dfDemog = safetyData::adam_adsl, mapping, stat="RR")
volcanoPlot(data)