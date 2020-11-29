<p align="center">
<img src="https://github.com/xiaorongw/tinyblocks/blob/master/shinyapp/www/logo2.png" height="300">
</p>

# Identifying Neighbourhoods that Enable Healthy Child Development through Geospatial Analysis of Built Spaces

## Project Motivation
Children are the future of a nation's well-being. There is therefore an incentive for the nation to ensure the healthy development of a child into adulthood. There have been studies on the how built spaces affect the development of children. Based on recurring key factors identified across these studies, geospatial analysis of Singapore's neighbourhoods can be conducted to map and understand gaps in spatial planning, pinpointing areas that might require intervention. In Singapore, this is especially relevant for public housing. Public estates are sometimes at a disadvantage, as compared to private housing where developers are already incentivised or expected to provide some of the built environment factors (e.g. green spaces, recreation spaces). As such, understanding how enabling different public housing are for childhood development will be the focus area for this project.

## Project Objective 
Through this project we aim to:
1.	Utilise open source data to build a proof-of-concept web-based geospatial application that is dynamic and reactive to user inputs.
2.	Identify how well HDB flats in different areas enable healthy child development, based on built environment factors.
3.	Enable more evidence-based policy decisions through a better understanding of the current spatial situation.

## Methodology 
The project will take the following steps in its development:
+ Identification of key domains for child development (physical health and well-being, social competence, emotional maturity) and their corresponding built environment factors, based on existing research
+ Data collection from various governmental data and open data
+ Data cleaning and wrangling
+ Formulation of enabling index to measure how well children in HDBs are developmentally enabled, based on key domains and built environment factors
+ Computation of enabling index utilising Hansen accessibility measures between HDBs and built factors
+ Thematic mapping of HDBs based on enabling index, to identify how well public housing in towns enable healthy child development

## Application System Architecture 
This project will be developed with R, as it will enable us to utilise the geospatial analysis and data manipulation libraries, in union with R Shiny to create an integrated web-based GIS application. The R Shiny application runs on a Shiny server, hosted on shinyapps.io along with the data for the application. The data and maps will be loaded by the application and displayed to the user whenever they access the application.

![Architecture](https://github.com/xiaorongw/gis-project/blob/master/project_website/static/images/architecture.png)

## Folder Information
+ shinyapp - You will be able to find the source code for the [geospatial application](https://tinyblocks.shinyapps.io/shinyapp/) and related data files here. (Change branch to shinyapp-1 for the version on sinyapps.io)
+ analysis - Provides rmarkdown documents of preliminary data cleaning and analysis that was conducted
+ project_website - Provides the source code for the project website
+ Report - Project report written in R code utilising rticles package 

## Project Website
You can follow our project progress via our [project website](https://tinyblocks.rbind.io/) as well. 

[![Netlify Status](https://api.netlify.com/api/v1/badges/ebd33618-12ef-46d6-8a43-2d51b31a52f0/deploy-status)](https://app.netlify.com/sites/nostalgic-bose-3a08f1/deploys)
