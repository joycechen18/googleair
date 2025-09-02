# **Google Air View Data-Dublin**
This repository stores code for the compilation of analysis-ready point data with the purpose of characterizing time-series of traffic related air pollution using mobile monitoring using Google Air Vehicle. 

![Google Air Vechile](https://smartdublin.ie/wp-content/uploads/2023/05/II_Project_Air_Release_58.jpg)


## Table of Contents
- [Data](https://data.smartdublin.ie/dataset/google-airview-data-dublin-city/resource/ea9ad286-3267-477a-96b1-70b4b2965a9e)

- [Data variables](https://data.smartdublin.ie/dataset/google-airview-data-dublin-city/resource/ea9ad286-3267-477a-96b1-70b4b2965a9e)

- [Acknowledgements](https://data.europa.eu/data/datasets/4976e11e-a015-4ef9-9179-dc7c27fb5a81?locale=en)

- [Calculate NOx at reference condition given NO and NO2](https://www.csagroupuk.org/wp-content/uploads/2015/05/TE4-Example-Calculations.pdf)

## Preview
![Moble monitoring routes](https://media.springernature.com/full/springer-static/image/art%3A10.1007%2Fs11356-024-34903-5/MediaObjects/11356_2024_34903_Fig1_HTML.png?as=webp)
Overview of the GoogleAirView mobile monitoring routes in Dublin from May 2021 to May 2022. Notes: The blue dots represent the location of the urban background, suburban background, and urban traffic stations.

## Main variables
The monitored variables (1Hz) including gas (CO, NO2, O3, nitrogen monoxide (NO), carbon dioxide (CO2)) and fine particulate matter (e.g., PM2.5). 

## Code
Data processing for this project was executed using the R progamming language. The result is reproducible if one obtains code and input data. 

## Prerequisites for running the pipeline
To run this pipeline, it is highly recommend using latest R version and the following main R libraries will be needed for the pipeline:

- dplyr
- tidyverse
- tidyquant
- lubrida
- zoo
- ggplot2
- ggplottimeseries
- ggpubr
- data.table
- tibble
- corrplot
- Hmisc
- ggcorrplot

Using R to plot map
- sf
- sp
- rgdal
- raster
- rgeos
- maptools
- osmdata
- classInt
- RColorBrewer
- ggmap
- tmap
- maps
- gripExtra
- geosphere
- spdep
- gmapsdistance
- rnaturalearth
- rnaturalearthdata

## Output
The final outputs consist of findings on background and local contributions and traffic-related pollutant hotspots in Dublin, Ireland. The associated results include:

- Time series of hourly a) PM2.5, b) CO, c) NO2, d) CO2, and e) O3 from Google Air View vehicle during May 2021 and May 2022 in Dublin city. Hourly median concentrations on each day were used for calculation, except the O3 hourly maximum was utilized for calculation.
![Time series](https://media.springernature.com/full/springer-static/image/art%3A10.1007%2Fs11356-024-34903-5/MediaObjects/11356_2024_34903_Fig2_HTML.png?as=webp)


- Diurnal trends (left) and heatmap (right) of a PM2.5, b NO2, c O3, d CO, and e CO2 by Google Air View vehicle in Dublin city (n = 1486 h). 
![Diurnal trend and heatmap](https://media.springernature.com/full/springer-static/image/art%3A10.1007%2Fs11356-024-34903-5/MediaObjects/11356_2024_34903_Fig3_HTML.png?as=webp)

- Time series decomposition of background and local contribution for PM2.5 and NO2. 
![Background and local contribution](https://media.springernature.com/full/springer-static/image/art%3A10.1007%2Fs11356-024-34903-5/MediaObjects/11356_2024_34903_Fig4_HTML.png?as=webp)
Notes: a and b refer to short-lived events for PM2.5 and NO2 (i.e., 1 h), and c and d refer to longer-lived events for PM2.5 and NO2 (i.e., ~ 8 h)

- Optimized hotspots analysis for PM2.5 and NO2
![Hotspots](https://media.springernature.com/full/springer-static/image/art%3A10.1007%2Fs11356-024-34903-5/MediaObjects/11356_2024_34903_Fig6_HTML.png?as=webp)

## Citations
- [Publication](https://link.springer.com/article/10.1007/s11356-024-34903-5)

## Authors
- [Dr. Jiayao Chen](https://people.ucd.ie/jiayao.chen)

## Feedback
If you have any feedback, please reach out to us at chenxcui@gmail.com/ jiayao.chen@ucd.ie

## 🔗 Links
[![linkedin](https://www.linkedin.com/in/joyce-jiayao-chen-535727186/?originalSubdomain=hk)](https://www.linkedin.com/)

