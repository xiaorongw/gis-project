<p align="center">
<img src="https://github.com/xiaorongw/gis-project/blob/master/project_website/static/images/logo.png" width="460" height="300">
</p>

# Identifying Neighborhoods with Childhood Developmental Risk through Geospatial Analysis of Built Spaces 

## Project Motivation
Children are the future of a nation's wellbeing. There is therefore an incentive for the nation to ensure the healthy development of a child into adulthood. 
There have been studies on the how built spaces affect the development of children. Based on recurring key factors identified across these studies, geospatial analysis of Singapore’s neighborhoods can be conducted, to map and understand geospatial gaps, pinpointing risk areas that might require intervention. 
This is especially relevant for public housing, that is sometimes at a disadvantage compared to private housing where developers are already incentivized or expected to provide some of these built environment factors (e.g. green spaces, recreation spaces). As such, identifying risk areas for childhood development in public housing will be the focus area for this project.

## Project Objective 
Through this project we aim to:
+	Investigate the spatial distribution of key factors of the built environment affecting child development, relative to public housing estates in Singapore.
+ Identify high risk neighbourhoods for healthy child development, based on built environment factors.
+ Provide possible recommendations to mitigate these risks.
+ Utilise open source data to build a proof-of-concept web-based geospatial application.


## Methodology 
The project will take the following steps in its development:
+ Identify key domains for child development (physical health and well-being, social competence, emotional maturity) and their corresponding built environment factors, based on existing research
+ Data collection from various governmental data and open data
+ Data cleaning and wrangling
+ Formulate risk matrices based on built environment factors (e.g. proximity to green spaces)
+ Spatially map and analyse built factors and their spatial relation to public housing (e.g. point pattern analysis)
+ Utilise risk matrices to compute risk scores for each HDB new town (e.g. accessibility analysis)
+ Identify at-risk areas for healthy child development in relation to public housing (e.g. localised geospatial analysis)
+ Propose recommendations for improvement and highlight key findings 

## Application System Architecture 
This project will be developed with R, as it will enable us to utilise the geospatial analysis and data manipulation libraries, in union with R Shiny to create an integrated web-based GIS application. The R Shiny application runs on a Shiny server, hosted on shinyapps.io along with the data for the application. The data and maps will be loaded by the application and displayed to the user whenever they access the application.

![Architecture](https://github.com/xiaorongw/gis-project/blob/master/project_website/static/images/architecture.png)
