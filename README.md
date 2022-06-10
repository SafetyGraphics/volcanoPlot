# volcano-plot

Adverse Event Volcano Plot designed for use with {safetyGraphics}. 

# Sample Code
Install the package from github with `devtools::install_github('safetyGraphics/volcanoPlot', ref="main")` then run stand-alone app:

```
devtools::install_github('safetyGraphics/volcanoPlot', ref="main")
library(volcanoPlot)
volcanoApp()
```

Or run in safetyGraphics:
```
library(volcanoPlot)

# Load standard graphics from safetyCharts + Volcano plot
charts<-c(
    safetyGraphics::makeChartConfig(),
    safetyGraphics::makeChartConfig(packages="volcanoPlot")
)

# Add default treatment columns
mapping <- list(dm=list('treatment_values--group1'="Placebo", 'treatment_values--group2'="Xanomeline High Dose"))

#Initialize SafetyGraphics app. 
safetyGraphics::safetyGraphicsApp(charts=charts, mapping=mapping)
```