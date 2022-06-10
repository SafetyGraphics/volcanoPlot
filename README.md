# volcano-plot

Adverse Event Volcano Plot designed for use with {safetyGraphics}. 

<img width="1429" alt="image" src="https://user-images.githubusercontent.com/3680095/172978390-a803a937-f156-4f49-b4e9-bae9bdb84d77.png">

# Sample Code
Install the package from github with `devtools::install_github('safetyGraphics/volcanoPlot', ref="main")` then run stand-alone app:

```
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