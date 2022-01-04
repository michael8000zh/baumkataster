# Master's thesis for MAS Data Science at ZHAW on the visualisation of threes in Zurich

This is the scripting part of my master's thesis at ZHAW's Master of Advanced Studies in Data Science. The goal of the thesis is to visualise threes in the city of Zurich. The tool should help people allergic to certain species of trees to find a neighbourhood to live where not a lot of these specific trees are growing.

The data of the Baumkataster can be obtained here: 
https://www.stadt-zuerich.ch/ted/de/index/gsz/planung-und-bau/inventare-und-grundlagen/baumkataster.html

The data of the surface area covered by each neighbourhood can be obtained here:
https://data.stadt-zuerich.ch/dataset/geo_statistische_quartiere

To run the Shiny-App, download the following files:
- dashboard.R
- flaeche.csv
- kataster.csv

They need to be placed in the same folder and the working directory set accordingly ("Session", "set Working Directory", "to Source Location") in RStudio.

The retake the steps conducted during the explorative analysis and data cleaning, download the following files:
- gsz.baumkataster_baumstandorte.csv
- quartiere_shape.zip
- baumkataster_00_explorative_analyse.R
- baumkataster_01_datacleaning.R
- baumkataster_02_flachenberechnung.R

I'm open to suggestions regarding the use of the script and other ideas on what to do with it. :-)
