<img src="xiaorongw\gis-project\master\project_website\static\images\logo.png" width="800px" height="auto">
# Tiny Blocks: Identifying Neighborhoods with Childhood Developmental Risk through Geospatial Analysis of Built Spaces 

## Project Motivation
Children are the future of a nation's wellbeing. There is therefore an incentive for the nation to ensure the healthy development of a child into adulthood. 
There have been studies on the how built spaces affect the development of children. Based on recurring key factors identified across these studies, geospatial analysis of Singapore’s neighborhoods can be conducted, to map and understand geospatial gaps, pinpointing risk areas that might require intervention. 
This is especially relevant for public housing, that is sometimes at a disadvantage compared to private housing where developers are already incentivized or expected to provide some of these built environment factors (e.g. green spaces, recreation spaces). As such, identifying risk areas for childhood development in public housing will be the focus area for this project.

## Project Objective 
Through this project we aim to:
•	Investigate the spatial distribution of key factors of the built environment affecting child development, relative to public housing estates in Singapore.
•	Identify high risk areas for childhood development, based on the built environment
•	Provide possible recommendations to mitigate these risks
•	Utilise open source data to build a proof of concept web-based geospatial application.

## Methodology 
The project will take the following steps in its development:
+ Identify key domains for child development and their corresponding built environment factors, based on existing research (Physical Health and Wellbeing, Social Competence, Emotional Maturity).
+ Retrieve Data from data sources and conduct data cleaning and transformation. 
+ Formulate risk matrices based on built environment factors (e.g. Proximity to Greenspaces).
+ Spatially map and analyse built factors and their spatial relation to public housing (e.g. Point Pattern Analysis)
+ Utilise risk matrices to create risk scores for each HDB new town (e.g. Accessibility Analysis)
+ Identify areas at risk in relation to public housing (e.g. Localised Geospatial Analysis)
+ Propose recommendation for improvement and highlight key findings 

## Application System Architecture 
The Project will be developed utilising R as it will allow us to utilise geospatial analytics and data manipulation packages in union with R Shiny to create an integrated web-based GIS application. The R shiny application runs on a shiny server, to be hosted by shinyapps.io along with the data for the application. The data and maps will be loaded by the application and displayed to the user whenever they access the application.

![Architecture](https://raw.githubusercontent.com/xiaorongw/gis-project/master/project_website/static/images/architecture.png?token=AHTAIVFGXWQJANGYGCJWIF27QIB34)
